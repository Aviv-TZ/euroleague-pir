"""Winner League (ליגת ווינר סל) site: data for docs/winner/ in the EuroLeague site's format, same component model.

Sources
  basket.co.il (the league's official site): schedule (results.asp) and box scores (game-zone.asp), past seasons
      2023/24-2025/26 kept in cm/data/wl_box_2024_2026.csv.gz, this season's pages cached in cm/.wl_cache
  dreamteam.sport5.co.il (the fantasy game, season id below): players, prices, positions, injuries, coaches

Players are followed across seasons by their Hebrew name (the league site's player ids change between seasons).
Positions: the fantasy site's; for players not in the game, from their stat profile (nearest G / F / C average).
A player with no game in the league this season or the last ones gets no projection.
Usage: python3 tools/winner.py            (writes docs/winner/data.json and docs/winner/index.html)"""
import concurrent.futures as cf, glob, gzip, html, json, math, os, re, sys, time, urllib.request
from datetime import datetime, timedelta, timezone
import numpy as np, pandas as pd

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
sys.path.insert(0, os.path.join(ROOT, 'cm'))
import component_model as C

CYEAR = 2027                          # basket.co.il season id = the year the season ends
S5_SEASON = 13                        # sport5 fantasy season (ליגת העל בכדורסל)
OUT = os.path.join(ROOT, 'docs', 'winner')
CACHE = os.path.join(ROOT, 'cm', '.wl_cache')
HIST = os.path.join(ROOT, 'cm', 'data', 'wl_box_2024_2026.csv.gz')
UA = {'User-Agent': 'Mozilla/5.0 (compatible; fantasy-stats)'}
LAST_RS_ROUND = 26                    # later rounds (2 games each) are the end-of-season series
os.makedirs(OUT, exist_ok=True); os.makedirs(CACHE, exist_ok=True)

def get(url, post=False, tries=4, text=False):
    for k in range(tries):
        try:
            req = urllib.request.Request(url, data=b'' if post else None, headers=UA, method='POST' if post else 'GET')
            with urllib.request.urlopen(req, timeout=60) as r:
                raw = r.read()
                return raw.decode('utf-8', errors='ignore') if text else json.loads(raw)
        except Exception as e:
            if k == tries - 1: print('  failed', url, e, flush=True); return None
            time.sleep(5 + 10 * k)

txt = lambda c: re.sub(r'\s+', ' ', html.unescape(re.sub(r'<[^>]+>', ' ', c))).strip()
nt = lambda s: re.sub(r'\s+', ' ', str(s).replace('\xa0', ' ')).strip()
nn = lambda s: re.sub(r"[\s'\"`׳״\-\.]", '', str(s))
num = lambda v: int(float(v)) if re.match(r'^-?\d+(\.\d+)?$', str(v or '')) else 0
def ma(v):
    m = re.match(r'(\d+)\s*/\s*(\d+)', v or ''); return (int(m.group(1)), int(m.group(2))) if m else (0, 0)

# ---------------- schedule ----------------
def il_to_utc(d, t):
    """dd/mm/yyyy + hh:mm Israel time -> UTC (Israel: +3 in summer time, +2 in winter)."""
    dt = datetime.strptime(f'{d} {t or "20:00"}', '%d/%m/%Y %H:%M')
    y = dt.year
    last_sun = lambda m: max(datetime(y, m, day) for day in range(22, 32) if day <= [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31][m - 1] and datetime(y, m, day).weekday() == 6)
    dst_start = last_sun(3) - timedelta(days=2); dst_end = last_sun(10)       # Friday before the last Sunday of March .. last Sunday of October
    off = 3 if dst_start <= dt < dst_end else 2
    return (dt - timedelta(hours=off)).replace(tzinfo=timezone.utc)

def schedule():
    s = get(f'https://basket.co.il/results.asp?cYear={CYEAR}', text=True) or ''
    games, rnd = [], None
    for m in re.finditer(r'<td class="round_break[^"]*"[^>]*>\s*מחזור\s*(\d+)\s*</td>|<tr class="row (?:odd|even)">(.*?)</tr>', s, re.S):
        if m.group(1): rnd = int(m.group(1)); continue
        row = m.group(2); gid = re.search(r'game-zone\.asp\?GameId=(\d+)', row)
        if not gid or rnd is None: continue
        tds = re.findall(r'<td[^>]*>(.*?)</td>', row, re.S)
        date = re.search(r'(\d{2}/\d{2}/\d{4})', tds[0]); tm = re.search(r'(\d{1,2}:\d{2})', tds[1] if len(tds) > 1 else '')
        teams = re.findall(r'<div class="game_item mid deskOnly">(.*?)</div>\s*<div class="game_item mid mobileOnly">(.*?)</div>', row, re.S)
        logos = re.findall(r'<div class="game_item"><img src="([^"]+)"', row)
        if not date or len(teams) < 2: continue
        utc = il_to_utc(date.group(1), tm.group(1) if tm else None)
        games.append(dict(game=int(gid.group(1)), round=rnd, utc=utc, home=nt(txt(teams[0][0])), away=nt(txt(teams[1][0])),
                          home_short=txt(teams[0][1]), away_short=txt(teams[1][1]), home_logo=logos[0] if logos else None,
                          away_logo=logos[1] if len(logos) > 1 else None, tbd='לא סופי' in tds[0]))
    return pd.DataFrame(games)

# ---------------- box scores ----------------
def parse_game(s, gid):
    title = txt((re.findall(r'<title>(.*?)</title>', s, re.S) or [''])[0])
    m = re.search(r'מחזור\s+(\d+)', title); rnd = int(m.group(1)) if m else None
    t = txt(s); dm = re.search(r'(\d{1,2})/(\d{1,2})/(\d{4}),', t)
    date = f'{dm.group(3)}-{int(dm.group(2)):02d}-{int(dm.group(1)):02d}' if dm else ''
    tabs = []
    for tm in re.finditer(r'<table[^>]*>(.*?)</table>', s, re.S):
        tr = re.findall(r'<tr[^>]*>(.*?)</tr>', tm.group(0), re.S)
        if len(tr) < 6 or 'PlayerId=' not in tm.group(0) or 'מאמן' not in tm.group(0): continue
        team = nt(re.sub(r'\(\s*מאמן.*', '', txt(tr[0])))
        players = []
        for r in tr:
            pid = re.search(r'PlayerId=(\d+)', r); c = [txt(x) for x in re.findall(r'<t[hd][^>]*>(.*?)</t[hd]>', r, re.S)]
            if pid and len(c) >= 23: players.append((int(pid.group(1)), c))
        if players: tabs.append((team, players))
    if len(tabs) != 2: return None
    rows = []
    for side, (team, players) in enumerate(tabs):
        for pid, c in players:
            mn = float(c[3]) if re.match(r'^\d+(\.\d+)?$', c[3] or '') else (num(c[3].split(':')[0]) + num(c[3].split(':')[1]) / 60 if ':' in (c[3] or '') else 0)
            if mn <= 0: continue
            f2, f3, ft = ma(c[5]), ma(c[7]), ma(c[9])
            rows.append(dict(season=CYEAR, game_id=gid, round=rnd, date=date, team=team, opp=tabs[1 - side][0], home=int(side == 0), player_id=pid,
                             name=c[1], starter=int(bool(c[2])), min=round(mn, 2), pts=num(c[4]), fg2m=f2[0], fg2a=f2[1], fg3m=f3[0], fg3a=f3[1],
                             ftm=ft[0], fta=ft[1], reb_a=num(c[11]), reb_b=num(c[12]), reb=num(c[13]), fc=num(c[14]), fd=num(c[15]),
                             stl=num(c[16]), tov=num(c[17]), ast=num(c[18]), blk=num(c[19]), blka=num(c[20]), val=num(c[21]), pm=num(c[22])))
    return rows

def season_box(sch):
    now = datetime.now(timezone.utc); rows = []
    for _, g in sch[sch.utc < now - timedelta(hours=2.5)].iterrows():
        f = os.path.join(CACHE, f'{g.game}.html.gz')
        if os.path.exists(f):
            with gzip.open(f, 'rt', encoding='utf-8') as fh: s = fh.read()
        else:
            s = get(f'https://basket.co.il/game-zone.asp?GameId={g.game}', text=True); time.sleep(0.8)
        r = parse_game(s or '', g.game) if s else None
        if r is None: continue
        if not os.path.exists(f):
            with gzip.open(f, 'wt', encoding='utf-8') as fh: fh.write(s)
        for x in r: x['round'] = int(g['round'])
        rows += r
    return pd.DataFrame(rows)

# ---------------- model rows ----------------
def to_model(b):
    b = b.copy(); b['team'] = b.team.map(nt); b['opp'] = b.opp.map(nt); b['player'] = b.name.map(nn)
    b['phase'] = np.where(b['round'] <= LAST_RS_ROUND, 'RS', 'PO')
    return pd.DataFrame(dict(season=b.season, game=b.game_id, round=b['round'], phase=b.phase, team=b.team, opp=b.opp, home=b.home,
                             player=b.player, name=b.name, timePlayed=b['min'] * 60, points=b.pts, totalRebounds=b.reb, assistances=b.ast,
                             steals=b.stl, blocksFavour=b.blk, foulsReceived=b.fd, turnovers=b.tov, foulsCommited=b.fc,
                             fieldGoalsMade2=b.fg2m, fieldGoalsAttempted2=b.fg2a, fieldGoalsMade3=b.fg3m, fieldGoalsAttempted3=b.fg3a,
                             freeThrowsMade=b.ftm, freeThrowsAttempted=b.fta, pos=''))

def positions(d, s5pos):
    agg = d.groupby('player').agg(min=('timePlayed', 'sum'), reb=('totalRebounds', 'sum'), ast=('assistances', 'sum'),
                                  blk=('blocksFavour', 'sum'), f3=('fieldGoalsAttempted3', 'sum'))
    feat = ['reb', 'ast', 'blk', 'f3']
    for k in feat: agg[k] = agg[k] / (agg['min'] / 60).clip(lower=1) * 36
    agg['known'] = agg.index.map(s5pos.get)
    kn = agg[agg.known.notna() & (agg['min'] >= 600)]
    mu, sd = agg[feat].mean(), agg[feat].std()
    cent = ((kn[feat] - mu) / sd).groupby(kn.known).mean(); z = (agg[feat] - mu) / sd
    near = {p: cent.sub(z.loc[p], axis=1).pow(2).sum(axis=1).idxmin() for p in agg.index}
    return {p: (agg.known[p] if isinstance(agg.known[p], str) else near[p]) for p in agg.index}

def main():
    t0 = time.time()
    sch = schedule()
    if sch.empty: raise SystemExit('no schedule')
    cur_box = season_box(sch)
    print(f'schedule {len(sch)} games, this season {cur_box.game_id.nunique() if len(cur_box) else 0} played', flush=True)
    hist = pd.read_csv(HIST)
    # ---- fantasy players ----
    tp = (get(f'https://dreamteam.sport5.co.il/api/Players/GetTeamsAndPlayers?seasonId={S5_SEASON}') or {}).get('data') or []
    s5teams = {t['id']: dict(name=nt(t['name']), short=t.get('displayName') or nt(t['name']), logo=t.get('teamLogoPath')) for t in tp}
    S5 = pd.DataFrame([dict(s5id=p['id'], name=nt(p['name']), teamId=p['teamId'], team=s5teams[p['teamId']]['name'], pos=p['position'],
                            price=round(p['price'] / 1e6), injured=bool((p.get('injuredStatus') or 0) > 0 or (p.get('missingStatus') or 0) > 0))
                       for t in tp for p in t['players']])
    POS = {1: 'Guard', 2: 'Forward', 3: 'Center'}
    s5pos = {nn(n): POS[p] for n, p in zip(S5.name, S5.pos) if p in POS}
    allbox = pd.concat([hist, cur_box], ignore_index=True)
    d = to_model(allbox)
    pos = positions(d, s5pos); d['pos'] = d.player.map(pos)
    d = C.prep(d)
    cur = d[(d.season == CYEAR) & (d.phase == 'RS')]; prev = d[d.season == CYEAR - 1]
    train = d[d.season.isin([CYEAR - 3, CYEAR - 2, CYEAR - 1]) & (d.phase == 'RS')]
    HF = C.home_factors(train)
    # ---- rounds to project ----
    now = datetime.now(timezone.utc)
    played_rounds = set(cur['round'].unique())
    # the round to project: the first one with most of its games still to play (a postponed game does not hold a round back)
    fut_n = sch[sch.utc > now - timedelta(hours=2.5)].groupby('round').size(); all_n = sch.groupby('round').size()
    upcoming = sorted(r for r in fut_n.index if fut_n[r] * 2 > all_n[r])
    complete = [r for r in sorted(sch['round'].unique()) if (sch[sch['round'] == r].utc < now - timedelta(hours=2.5)).all()]
    R0 = int(upcoming[0]) if upcoming else int(max(sch['round']))
    rounds = [r for r in range(R0, R0 + 5) if r in set(sch['round'])]
    fx = {}
    for r in rounds:
        g = sch[sch['round'] == r]
        fx[r] = {**{x.home: (x.away, 1, x) for _, x in g.iterrows()}, **{x.away: (x.home, 0, x) for _, x in g.iterrows()}}
    st = C.State(cur, prev) if len(cur) else None
    # earlier seasons, newest first: a player with no game this season is projected from his last season in the league
    past = [C.State(d[d.season == y], None) for y in range(CYEAR - 1, CYEAR - 4, -1) if len(d[d.season == y])]
    # ---- players ----
    players = []
    ident = 0
    for _, p in S5[S5.pos != 4].iterrows():
        key = nn(p['name']); team = p.team; ppos = POS.get(p.pos, 'Forward'); ident += 1
        mine = cur[(cur.player == key) & (cur.team == team)]
        known_cur = st is not None and key in st.pw.index
        old = next((x for x in past if key in x.pw.index), None)
        fut = None; cmx = None; e0 = None
        if known_cur or old is not None:
            src = st if known_cur else old
            vals = []
            for r in rounds:
                f = fx[r].get(team)
                if not f: vals.append(None); continue
                e = src.project(key, ppos, f[0], f[1], HF)
                vals.append(e)
            E = [C.fp_from(pd.DataFrame([dict(**e)])).iloc[0] if e else None for e in vals]
            fut = [round(float(x.pred), 1) if x is not None else 0.0 for x in E]
            e0 = E[0]
            if e0 is not None: cmx = dict(min=round(float(e0['min']), 1), e={k: round(float(e0['e_' + k]), 2) for k in C.K},
                                          p_dd=round(float(e0.p_dd), 3), p_td=round(float(e0.p_td), 4))
        gt = len(mine)
        nxt = fx[rounds[0]].get(team) if rounds else None
        reason = None
        if fut is None: reason = 'אין לו משחקים בליגה (העונה ו-3 העונות הקודמות)'
        elif gt < C.MIN_GAMES: reason = f'פחות מ-{C.MIN_GAMES} הופעות בקבוצה הנוכחית העונה'
        elif not nxt: reason = 'אין משחק במחזור'
        elif e0 is None or e0['min'] < C.MIN_EXP_MIN: reason = f'פחות מ-{C.MIN_EXP_MIN} דקות צפויות'
        team_rounds = sorted(cur[cur.team == team]['round'].unique())
        out3 = bool(gt and len(team_rounds) >= 3 and not set(team_rounds[-3:]) & set(mine['round']))
        l5 = mine.sort_values('round').tail(5)
        players.append(dict(id=ident, raw=key, name=p['name'], he=p['name'], team=team, pos=ppos, pos_he={'Guard': 'גארד', 'Forward': 'פורוורד', 'Center': 'סנטר'}[ppos],
                            games=int(gt), avg=round(float(mine.fp.mean()), 2) if gt else None, l5=round(float(l5.fp.mean()), 2) if gt else None,
                            l5n=int(len(l5)), min=round(float(mine['min'].mean()), 1) if gt else None, l5min=round(float(l5['min'].mean()), 1) if gt else None,
                            opp=nxt[0] if nxt else None, venue=('Home' if nxt[1] else 'Away') if nxt else None,
                            date=nxt[2].utc.strftime('%Y-%m-%d') if nxt else None, ranked=reason is None, reason=reason,
                            fut=fut, price=int(p.price), s5id=int(p.s5id), s5pos=ppos, injured=bool(p.injured), out3=out3, cm=cmx))
    ranked = sorted([x for x in players if x['ranked']], key=lambda x: -x['fut'][0])
    resid_lo, resid_hi = -12.0, 14.0
    for i, x in enumerate(ranked):
        pr = x['fut'][0]; m = dict(score=pr, rank=i + 1, pir=pr, lo=round(pr + resid_lo, 1), hi=round(pr + resid_hi, 1)); x['m'] = [m, m]
    # ---- coaches: expected margin from the teams' points this season (as on the EuroLeague site) ----
    pts = allbox[allbox.season == CYEAR].groupby(['game_id', 'team']).pts.sum().reset_index()
    pts['team'] = pts.team.map(nt)
    opp = pts.merge(pts, on='game_id', suffixes=('', '_o')); opp = opp[opp.team != opp.team_o]
    net = (opp.pts - opp.pts_o).groupby(opp.team).mean().to_dict()
    def coach_fut(team):
        out = []
        for r in rounds:
            f = fx[r].get(team)
            if not f or team not in net or f[0] not in net: out.append(0.0); continue
            mu = 0.6 * (net[team] - net[f[0]]) + 2.5 * (1 if f[1] else -1)
            out.append(round(mu + 5 * (2 * 0.5 * (1 + math.erf(mu / 12 / math.sqrt(2))) - 1), 1))
        return out
    coaches = []
    for _, c in S5[S5.pos == 4].iterrows():
        nxt = fx[rounds[0]].get(c.team) if rounds else None
        coaches.append(dict(raw=c['name'], he=c['name'], team=c.team, price=int(c.price), s5id=int(c.s5id), logo=s5teams[c.teamId]['logo'],
                            injured=bool(c.injured), fut=coach_fut(c.team), opp=nxt[0] if nxt else None, venue=('Home' if nxt[1] else 'Away') if nxt else None))
    # ---- opponents: fantasy points a game allowed to each position (weighted by minutes), season and last 5 ----
    pos3 = ['Guard', 'Forward', 'Center']
    def allowed(rows):
        g = rows.groupby('pos').apply(lambda z: (z.fp * z['min']).sum() / max(z['min'].sum(), 1e-9), include_groups=False)
        return {p: round(float(g.get(p, np.nan)), 3) if p in g else None for p in pos3}
    opponents = []
    for t in sorted(set(cur.opp)):
        z = cur[cur.opp == t]; last = sorted(z['round'].unique())[-5:]
        nxt = fx[rounds[0]].get(t) if rounds else None
        opponents.append(dict(team=t, season=allowed(z), last5=allowed(z[z['round'].isin(last)]),
                              next_vs=nxt[0] if nxt else None, next_venue=('Home' if nxt[1] else 'Away') if nxt else None))
    league = allowed(cur) if len(cur) else {}
    fixtures = [dict(game=str(g.game), round=int(g['round']), date=g.utc.strftime('%Y-%m-%d'), utc=g.utc.strftime('%Y-%m-%dT%H:%M:%SZ'),
                     home=g.home, away=g.away, played=bool(g['round'] in played_rounds and g.utc < now - timedelta(hours=2.5)))
                for _, g in sch.sort_values('utc').iterrows() if int(g['round']) in rounds]
    try: ab = C.absences(d[d.season == CYEAR].assign(phase=lambda z: z.phase))
    except Exception as e: ab = {}; print('absences failed:', e)
    bt = None
    bt_file = os.path.join(ROOT, 'cm', 'data', 'wl_backtest.json')
    if os.path.exists(bt_file):
        with open(bt_file) as fh: bt = json.load(fh)
    short = {s['name']: s['short'] for s in s5teams.values()}
    for _, g in sch.iterrows(): short.setdefault(g.home, g.home_short); short.setdefault(g.away, g.away_short)
    logos = {s['name']: s['logo'] for s in s5teams.values()}
    # short labels for narrow tables (by the city / club in the full name)
    AB = [('מכבי Rapyd', 'מכבי ת"א'), ('מכבי Rapyd תל אביב', 'מכבי ת"א'), ('הפועל IBI', 'הפועל ת"א'), ('ירושלים', 'י-ם'), ('נס ציונה', 'נ"צ'),
          ('ראשון לציון', 'ראשל"צ'), ('אילת', 'אילת'), ('גליל עליון', 'גליל'), ('רמת גן', 'ר"ג'), ('אשדוד', 'אשדוד'), ('הרצליה', 'הרצליה'),
          ('קריית אתא', 'ק. אתא'), ('העמק', 'העמק'), ('חולון', 'חולון'), ('ב"ש', 'ב"ש')]
    abbr = {}
    for t in set(short) | set(logos):
        abbr[t] = next((v for k, v in AB if k in t), short.get(t, t))
    meta = dict(model='component', league='winner', generated_utc=now.strftime('%Y-%m-%dT%H:%M:%SZ'), season=f'{CYEAR - 1}/{str(CYEAR)[2:]}',
                n_ranked=len(ranked), n_players=len(players), fut_rounds=rounds, next_round=R0,
                last_complete_round=int(max(complete)) if complete else None, backtest=bt, team_short=short, team_abbr=abbr, team_logo=logos)
    data = dict(meta=meta, fixtures=fixtures, players=players, round_hits=[], coaches=coaches,
                s5teams={str(k): v['name'] for k, v in s5teams.items()}, opponents=opponents, league=league, absences=ab)
    with open(os.path.join(OUT, 'data.json'), 'w', encoding='utf-8') as fh: json.dump(data, fh, ensure_ascii=False, separators=(',', ':'))
    write_live(sch, cur, R0, now)
    build_page(data)
    print(f'winner: round {R0}, {len(players)} players ({len(ranked)} ranked, {sum(1 for x in players if x["fut"] is None)} without a projection), '
          f'{len(coaches)} coaches, {time.time() - t0:.0f}s', flush=True)

def write_live(sch, cur, R0, now):
    """live.json as on the EuroLeague site (no live feed: finished games of the coming round, schedule, difficulty)."""
    done = lambda g: g.utc < now - timedelta(hours=2.5) and int(g.game) in set(cur.game)
    sched = [dict(game=str(g.game), round=int(g['round']), date=g.utc.strftime('%Y-%m-%d'), utc=g.utc.strftime('%Y-%m-%dT%H:%M:%SZ'),
                  home=g.home, away=g.away, status='final' if done(g) else 'scheduled',
                  hs=int(cur[(cur.game == g.game) & (cur.team == g.home)].points.sum()) if done(g) else None,
                  **{'as': int(cur[(cur.game == g.game) & (cur.team == g.away)].points.sum()) if done(g) else None})
             for _, g in sch.sort_values('utc').iterrows()]
    games = [x for x in sched if x['round'] == R0]
    rr = cur[cur['round'] == R0].sort_values('fp', ascending=False)
    actual = [dict(raw=x.player, team=x.team, opp=x.opp, game=str(x.game), pir=int(x.fp), min=round(float(x['min']), 1), status='final',
                   rank=int((rr.fp > x.fp).sum()) + 1) for _, x in rr.iterrows()]
    nxt = {}
    for x in sched:
        if x['status'] == 'final': continue
        for t, o, v in [(x['home'], x['away'], 'Home'), (x['away'], x['home'], 'Away')]:
            nxt.setdefault(t, dict(vs=o, venue=v, round=x['round'], date=x['date'], utc=x['utc']))
    # difficulty: fantasy points a team's players score / its opponents score, per game, at home and away
    tg = cur.groupby(['game', 'team', 'opp', 'home']).fp.sum().reset_index()
    fdr = []
    for t in sorted(set(tg.team)):
        a = tg[tg.team == t]; dd = tg[tg.opp == t]
        h, w = a[a.home == 1], a[a.home == 0]; dh, dw = dd[dd.home == 0], dd[dd.home == 1]      # allowed at home = scored by the visitors
        fdr.append(dict(team=t, n_home=int(len(h)), n_away=int(len(w)), att_home=float(h.fp.mean()) if len(h) else None,
                        att_away=float(w.fp.mean()) if len(w) else None, def_home=float(dh.fp.mean()) if len(dh) else None,
                        def_away=float(dw.fp.mean()) if len(dw) else None))
    live = dict(updated_utc=now.strftime('%Y-%m-%dT%H:%M:%SZ'), round=R0, finished_ids=[g['game'] for g in games if g['status'] == 'final'],
                schedule=sched, round_finished=sum(g['status'] == 'final' for g in games), round_games=len(games), n_played=len(actual),
                games=games, actual=actual, next_game=nxt, fdr=fdr)
    with open(os.path.join(OUT, 'live.json'), 'w', encoding='utf-8') as fh: json.dump(live, fh, ensure_ascii=False, separators=(',', ':'))

def build_page(data):
    """The EuroLeague page's template, pointed at this folder's files and with the Winner League's texts."""
    with open(os.path.join(ROOT, 'web', 'template.html'), encoding='utf-8') as fh: s = fh.read()
    season = data['meta']['season']
    s = s.replace('/*__SITE__*/null', json.dumps(dict(league='winner', dir='docs/winner', live=True, myteam=False)))
    s = s.replace('יורוליג 2026/27', f'ליגת ווינר סל {season}')
    s = s.replace('/*__EL_DATA__*/null', json.dumps(data, ensure_ascii=False, separators=(',', ':')))
    with open(os.path.join(OUT, 'index.html'), 'w', encoding='utf-8') as fh: fh.write(s)

if __name__ == '__main__':
    main()
