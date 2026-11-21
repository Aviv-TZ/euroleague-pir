"""One-off: box-score lines of complete past seasons (2022/23 - 2025/26, all phases) for the head-to-head
section of the player card. Players and clubs are identified by their EuroLeague codes, so transfers and
club renames do not break the matching. Output: cm/data/h2h_rows_2022_2026.csv.gz
Usage: python3 cm/build_h2h_history.py [cache_dir]"""
import concurrent.futures as cf, json, os, sys, time, urllib.request
import pandas as pd

HERE = os.path.dirname(os.path.abspath(__file__))
API = 'https://api-live.euroleague.net/v2/competitions/E/seasons/{s}'
SEASONS = ['E2022', 'E2023', 'E2024', 'E2025']
CACHE = sys.argv[1] if len(sys.argv) > 1 else os.path.join(HERE, '.h2h_cache')
os.makedirs(CACHE, exist_ok=True)

def get(url, tries=5):
    for k in range(tries):
        try:
            req = urllib.request.Request(url, headers={'User-Agent': 'EuroLeague-R-Boxscore/2.0'})
            with urllib.request.urlopen(req, timeout=40) as r: return json.load(r)
        except Exception:
            if k == tries - 1: raise
            time.sleep(10 + 20 * k)

def box(season, code):
    f = os.path.join(CACHE, f'{season}_{code}.json')
    if os.path.exists(f):
        with open(f) as fh: return json.load(fh)
    j = get(API.format(s=season) + f'/games/{code}/stats')
    with open(f, 'w') as fh: json.dump(j, fh)
    return j

def season_lines(season):
    d = get(API.format(s=season) + '/games?limit=1000'); d = d.get('data', d)
    games = [g for g in d if g.get('played')]
    def one(g):
        j = box(season, g['gameCode']); out = []
        for side, me, op in (('local', g['local'], g['road']), ('road', g['road'], g['local'])):
            for p in (j.get(side) or {}).get('players') or []:
                st = p.get('stats') or {}; per = (p.get('player') or {}).get('person') or {}
                if not st.get('timePlayed'): continue
                miss = (st.get('fieldGoalsAttempted2', 0) - st.get('fieldGoalsMade2', 0)) + \
                       (st.get('fieldGoalsAttempted3', 0) - st.get('fieldGoalsMade3', 0)) + \
                       (st.get('freeThrowsAttempted', 0) - st.get('freeThrowsMade', 0))
                out.append(dict(season=season, phase=(g.get('phaseType') or {}).get('code', ''), round=g['round'],
                                date=(g.get('utcDate') or '')[:10], team=me['club']['code'], opp=op['club']['code'],
                                team_name=me['club']['name'], home=int(side == 'local'),
                                pcode=per.get('code', '').strip(), player=per.get('name', ''),
                                min=round(st['timePlayed'] / 60, 1), pts=st.get('points', 0), reb=st.get('totalRebounds', 0),
                                ast=st.get('assistances', 0), stl=st.get('steals', 0), blk=st.get('blocksFavour', 0),
                                fd=st.get('foulsReceived', 0), tov=st.get('turnovers', 0), fc=st.get('foulsCommited', 0), miss=miss))
        return out
    rows = []
    with cf.ThreadPoolExecutor(3) as ex:
        for r in ex.map(one, games): rows += r
    print(season, len(games), 'games', len(rows), 'lines', flush=True)
    return rows

if __name__ == '__main__':
    rows = []
    for s in SEASONS: rows += season_lines(s)
    pd.DataFrame(rows).to_csv(os.path.join(HERE, 'data', 'h2h_rows_2022_2026.csv.gz'), index=False, compression='gzip')
    print('written', len(rows))
