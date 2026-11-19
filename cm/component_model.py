"""Component model for sport5 EuroLeague fantasy points (the site's forecast model).

Every fantasy stat is projected separately for a player's next games:
  E[stat_k] = expected minutes x per-minute rate_k x opponent factor_k(position) x home/away factor_k
  - rate_k: this season's per-minute rate, recent games weighted more (half weight ~8 games back), shrunk
    toward a prior = last season's rate (if any) shrunk toward the position average
  - opponent factor_k: per-minute stat_k the opponent allowed to that position this season vs the league
  - home/away factor_k: learned on 2024/25 + 2025/26
  - double-double / triple-double: P(stat >= 10) per category (normal approximation), categories
    independent -> 10 x P(exactly 2) + 20 x P(3 or more)
E[FP] = pts + 2 reb + 2 ast + 3 stl + 3 blk + fouls drawn - 2 turnovers - misses - fouls + DD/TD bonus
Settings were chosen on 2024/25 + 2025/26 only; 2026/27 rounds are scored out of sample (only games
before each round are used).

Usage: python3 cm/component_model.py OUTPUT_DIR [--target-round N]
Writes OUTPUT_DIR/component_predictions.json (projections for 5 rounds, backtest hits per round).
"""
import concurrent.futures as cf, json, math, os, sys, time, urllib.request
import numpy as np, pandas as pd

HERE = os.path.dirname(os.path.abspath(__file__))
SEASON, PREV = 'E2026', 'E2025'
API = 'https://api-live.euroleague.net/v2/competitions/E/seasons/{s}'
K = ['pts', 'reb', 'ast', 'stl', 'blk', 'fd', 'tov', 'fc', 'miss']
W = dict(pts=1, reb=2, ast=2, stl=3, blk=3, fd=1, tov=-2, fc=-1, miss=-1)
DDK = ['pts', 'reb', 'ast', 'stl', 'blk']
P = dict(hl=8, m0=80, m1=400, c=800, alpha=1.0)          # tuned on the two training seasons
PHI = dict(pts=3.0, reb=1.25, ast=1.5, stl=0.6, blk=0.6)  # dispersion of each count (fitted on 2025/26)
MIN_GAMES = 5                                              # same eligibility as the site's ranking

def get(url, tries=3):
    for k in range(tries):
        try:
            req = urllib.request.Request(url, headers={'User-Agent': 'EuroLeague-R-Boxscore/2.0'})
            with urllib.request.urlopen(req, timeout=40) as r:
                return json.load(r)
        except Exception:
            if k == tries - 1: raise
            time.sleep(3 + 5 * k)

def schedule(season):
    d = get(API.format(s=season) + '/games?limit=1000'); d = d.get('data', d)
    return pd.DataFrame([dict(game=g['gameCode'], round=g['round'], phase=(g.get('phaseType') or {}).get('code', ''),
                              played=bool(g.get('played')), home_team=g['local']['club']['name'],
                              away_team=g['road']['club']['name']) for g in d])

def season_rows(season, sch):
    """Box-score lines (>0 minutes) of the season's played regular-season games."""
    def one(g):
        j = get(API.format(s=season) + f'/games/{g.game}/stats')
        out = []
        for side, team, opp, home in (('local', g.home_team, g.away_team, 1), ('road', g.away_team, g.home_team, 0)):
            for p in (j.get(side) or {}).get('players') or []:
                st = p.get('stats') or {}; pl = p.get('player') or {}
                if not st.get('timePlayed'): continue
                r = dict(season=season, game=g.game, round=g['round'], phase=g.phase, team=team, opp=opp, home=home,
                         player=(pl.get('person') or {}).get('name', ''), pos=pl.get('positionName', ''))
                for k in ['timePlayed', 'points', 'fieldGoalsMade2', 'fieldGoalsAttempted2', 'fieldGoalsMade3',
                          'fieldGoalsAttempted3', 'freeThrowsMade', 'freeThrowsAttempted', 'totalRebounds',
                          'assistances', 'steals', 'turnovers', 'blocksFavour', 'foulsCommited', 'foulsReceived']:
                    r[k] = st.get(k) or 0
                out.append(r)
        return out
    played = sch[sch.played & (sch.phase == 'RS')]
    rows = []
    with cf.ThreadPoolExecutor(6) as ex:
        for r in ex.map(one, [g for _, g in played.iterrows()]): rows += r
    return pd.DataFrame(rows)

def prep(d):
    d = d.copy()
    d['min'] = d.timePlayed / 60.0
    d['pts'] = d.points; d['reb'] = d.totalRebounds; d['ast'] = d.assistances; d['stl'] = d.steals
    d['blk'] = d.blocksFavour; d['fd'] = d.foulsReceived; d['tov'] = d.turnovers; d['fc'] = d.foulsCommited
    d['miss'] = (d.fieldGoalsAttempted2 - d.fieldGoalsMade2) + (d.fieldGoalsAttempted3 - d.fieldGoalsMade3) + \
                (d.freeThrowsAttempted - d.freeThrowsMade)
    tens = sum((d[k] >= 10).astype(int) for k in DDK)
    d['fp'] = sum(W[k] * d[k] for k in K) + np.where(tens >= 3, 20, np.where(tens == 2, 10, 0))
    d['pos'] = d.pos.where(d.pos.isin(['Guard', 'Forward', 'Center']), 'Forward')
    return d

_erf = np.frompyfunc(math.erf, 1, 1)
def pge10(mean, phi):
    sd = np.sqrt(np.maximum(phi * mean, 1e-6))
    return 0.5 * (1 - _erf(((9.5 - mean) / sd) / math.sqrt(2)).astype(float))

def home_factors(train):
    hf = {}
    for k in K:
        h = train[train.home == 1]; a = train[train.home == 0]
        hf[k] = math.sqrt((h[k].sum() / h['min'].sum()) / (a[k].sum() / a['min'].sum()))
    return hf

def fp_from(E):
    """E: DataFrame with columns e_<stat>. Adds pred (expected FP), p_dd, p_td."""
    lin = sum(W[k] * E['e_' + k] for k in K)
    p10 = np.vstack([pge10(E['e_' + k].values, PHI[k]) for k in DDK]).T
    dist = np.zeros((len(E), 6)); dist[:, 0] = 1
    for j in range(5):
        p = p10[:, j:j + 1]
        dist = np.hstack([dist[:, :1] * (1 - p), dist[:, 1:] * (1 - p) + dist[:, :-1] * p])
    E = E.copy(); E['p_dd'] = dist[:, 2]; E['p_td'] = dist[:, 3:].sum(1)
    E['pred'] = lin + 10 * E.p_dd + 20 * E.p_td
    return E

class State:
    """Everything known before a round: player rates, minutes, opponent factors (from `hist` games)."""
    def __init__(self, hist, prev):
        lg = hist.groupby('pos')[K + ['min']].sum(); self.L = lg[K].div(lg['min'], axis=0)
        h = hist.sort_values(['round', 'game']).copy()
        h['n_after'] = h.groupby('player')['round'].rank(ascending=False, method='first') - 1
        h['w'] = 0.5 ** (h['n_after'] / P['hl'])
        for k in K + ['min']: h['w_' + k] = h['w'] * h[k]
        g = h.groupby('player')
        self.pw = g[['w_' + k for k in K + ['min']] + ['w']].sum()
        self.ngames = g.size(); self.team = g['team'].last(); self.pos = g['pos'].last()
        self.ngames_team = h.groupby(['player', 'team']).size()
        self.exp_min = self.pw['w_min'] / self.pw['w']
        self.oa = hist.groupby(['opp', 'pos'])[K + ['min']].sum()
        self.prev = prev.groupby('player')[K + ['min']].sum() if prev is not None and len(prev) else None

    def project(self, player, pos, opp, home, HF):
        if player not in self.pw.index: return None
        L = self.L.loc[pos] if pos in self.L.index else self.L.mean()
        prior = L
        if self.prev is not None and player in self.prev.index and self.prev.loc[player, 'min'] > 0:
            pm = self.prev.loc[player]
            prior = (pm[K] + P['m1'] * L) / (pm['min'] + P['m1'])
        wm = self.pw.loc[player, 'w_min']
        rate = (self.pw.loc[player, ['w_' + k for k in K]].values + P['m0'] * prior.values) / (wm + P['m0'])
        if opp is not None and (opp, pos) in self.oa.index:
            o = self.oa.loc[(opp, pos)]
            of = (((o[K] + P['c'] * L) / (o['min'] + P['c'])) / L).clip(0.5, 1.8).values ** P['alpha']
        else:
            of = np.ones(len(K))
        hm = np.array([HF[k] if home else 1 / HF[k] for k in K]) if home is not None else np.ones(len(K))
        mins = float(self.exp_min.loc[player])
        e = mins * rate * of * hm
        return dict(min=mins, **{'e_' + k: float(e[i]) for i, k in enumerate(K)})

def fixtures_of(sch, rnd):
    g = sch[sch['round'] == rnd]
    out = {}
    for _, x in g.iterrows():
        out[x.home_team] = (x.away_team, 1); out[x.away_team] = (x.home_team, 0)
    return out

def hits(pred_rows, actual_round):
    """Predicted top K among eligible players vs actual top K among everyone who played (ties at the cut count)."""
    a = actual_round.sort_values('fp', ascending=False)
    act = list(zip(a.player, a.team)); res = {}
    for k, key in [(10, 't10'), (30, 't30'), (50, 't50')]:
        th = a.fp.iloc[min(k, len(a)) - 1]
        top_act = set(x for x, v in zip(act, a.fp) if v >= th)
        top_pred = list(zip(pred_rows.sort_values('pred', ascending=False).player, pred_rows.sort_values('pred', ascending=False).team))[:k]
        res[key] = int(sum(1 for x in top_pred if x in top_act))
    return res

def rank_metrics(x):
    pred = (-x.pred).rank(method='average').values; act = (-x.actual).rank(method='average').values
    k = min(10, len(x)); k30 = min(30, len(x))
    tp = np.argsort(pred, kind='stable')[:k]; ta = np.argsort(act, kind='stable')[:k]
    gain = 1 / np.log2(1 + act); disc = np.log2(np.arange(1, k + 1) + 1)
    return dict(spearman=float(np.corrcoef(pred, act)[0, 1]), top10=len(set(tp) & set(ta)),
                top30=len(set(np.argsort(pred, kind='stable')[:k30]) & set(np.argsort(act, kind='stable')[:k30])),
                ndcg=float((gain[tp] / disc).sum() / (gain[ta] / disc).sum()))

def main(out_dir, target_round=None):
    t0 = time.time()
    train = prep(pd.read_csv(os.path.join(HERE, 'data', 'train_rows_2024_2026.csv.gz')))
    train = train[train.phase == 'RS']
    HF = home_factors(train)
    sch = schedule(SEASON)
    cur = prep(season_rows(SEASON, sch)); cur = cur[cur.phase == 'RS']
    prev = train[train.season == PREV]
    open_rounds = sorted(sch[(~sch.played) & (sch.phase == 'RS')]['round'].unique())
    R0 = int(target_round) if target_round else int(open_rounds[0])
    rounds = [r for r in range(R0, R0 + 5) if r in set(sch['round'])]
    print(f'component model: {len(cur)} lines of {SEASON} (rounds 1-{cur["round"].max()}), projecting rounds {rounds}', flush=True)

    # ---- projections from everything played so far --------------------------------------------
    st = State(cur, prev)
    fx = {r: fixtures_of(sch, r) for r in rounds}
    players = {}
    for pl in st.pw.index:
        team = st.team.loc[pl]; pos = st.pos.loc[pl]
        rows = []
        for r in rounds:
            f = fx[r].get(team)
            e = st.project(pl, pos, f[0] if f else None, f[1] if f else None, HF)
            rows.append(dict(round=r, playing=f is not None, **e))
        E = fp_from(pd.DataFrame(rows))
        fut = [round(float(v), 1) if pg else 0.0 for v, pg in zip(E.pred, E.playing)]
        first = E.iloc[0]
        players[f'{pl}||{team}'] = dict(
            fut=fut, min=round(first['min'], 1), games=int(st.ngames.loc[pl]),
            e={k: round(float(first['e_' + k]), 2) for k in K},
            p_dd=round(float(first.p_dd), 3), p_td=round(float(first.p_td), 4))

    # ---- out-of-sample backtest of this season (rounds 6+, each round uses earlier games only) ---
    hits_rows, bt_rows, resid = [], [], []
    complete = [r for r in sorted(cur['round'].unique()) if r >= 6 and r < R0 and
                not ((sch['round'] == r) & (~sch.played) & (sch.phase == 'RS')).any()]
    for r in complete:
        hist = cur[cur['round'] < r]; tgt = cur[cur['round'] == r]
        s = State(hist, prev); rows = []
        for _, x in tgt.iterrows():
            if s.ngames_team.get((x.player, x.team), 0) < MIN_GAMES: continue
            e = s.project(x.player, x.pos, x.opp, x.home, HF)
            if e: rows.append(dict(player=x.player, team=x.team, actual=x.fp, **e))
        E = fp_from(pd.DataFrame(rows))
        resid += list(E.actual - E.pred)
        hits_rows.append(dict(round=int(r), n_played=int(len(tgt)), m2=hits(E, tgt)))
        bt_rows.append(dict(round=int(r), **rank_metrics(E)))
        print(f'  backtest round {r}: {len(E)} players, top10 hits {hits_rows[-1]["m2"]["t10"]}', flush=True)
    resid = np.array(resid) if len(resid) else np.array([-12.0, 14.0])
    bt = pd.DataFrame(bt_rows)
    out = dict(model='component', generated=time.strftime('%Y-%m-%dT%H:%M:%SZ', time.gmtime()), rounds=rounds,
               resid=dict(lo=round(float(np.quantile(resid, .10)), 2), hi=round(float(np.quantile(resid, .90)), 2)),
               players=players, round_hits=hits_rows,
               backtest=(dict(rounds=f'{bt["round"].min()}-{bt["round"].max()}', n_rounds=len(bt),
                              top10=round(bt.top10.mean(), 2), top30=round(bt.top30.mean(), 2),
                              ndcg=round(bt.ndcg.mean(), 3), spearman=round(bt.spearman.mean(), 3)) if len(bt) else None))
    with open(os.path.join(out_dir, 'component_predictions.json'), 'w') as fh:
        json.dump(out, fh, ensure_ascii=False)
    print(f'component model written: {len(players)} players, {len(hits_rows)} backtest rounds, {time.time() - t0:.0f}s')

if __name__ == '__main__':
    a = sys.argv[1:]
    tr = int(a[a.index('--target-round') + 1]) if '--target-round' in a else None
    main(a[0], tr)
