"""Closing betting lines (point spread) from OddsPapi for the EuroLeague and the Israeli Super League (Winner League).
Run in GitHub Actions with the ODDSPAPI_KEY secret (never printed). Personal account details are never read.
  - fixtures of each league since 2026-01-01: 1 request per league per run (of the 250 a month)
  - price history of every finished fixture not in the file yet: free (does not use the quota), 5.5 s apart
Closing line = the last prices before the tip; the spread market whose two prices are closest to even. Pinnacle first.
Writes cm/data/odds/lines.csv (one row per fixture) and cm/data/odds/upcoming.csv (fixtures not played yet)."""
import csv, json, os, sys, time, urllib.parse, urllib.request
KEY = os.environ.get('ODDSPAPI_KEY', '').strip()
if not KEY: sys.exit('ODDSPAPI_KEY is not set')
ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
D = os.path.join(ROOT, 'cm', 'data', 'odds')
LEAGUES = {'euroleague': 138, 'israel': 1197}
BOOKS = ('pinnacle+30', 'bet365', '1xbet')
M = json.load(open(os.path.join(D, 'markets_basketball.json')))
last = {}
def call(path, wait=2.2, **q):
    if path in last: time.sleep(max(0, wait - (time.time() - last[path])))
    url = f'https://api.oddspapi.io/v4/{path}?' + urllib.parse.urlencode({**q, 'apiKey': KEY})
    for k in range(3):
        try:
            with urllib.request.urlopen(urllib.request.Request(url, headers={'Accept': 'application/json'}), timeout=90) as r:
                last[path] = time.time(); return json.load(r)
        except urllib.error.HTTPError as e:
            last[path] = time.time()
            if e.code == 429: time.sleep(8); continue
            print(f'  {path}: HTTP {e.code}', flush=True); return None
        except Exception as e:
            print(f'  {path}: {e}', flush=True); time.sleep(5)
    return None

def last_before(entries, t):
    xs = [e for e in entries if e['createdAt'] < t]
    return max(xs, key=lambda e: e['createdAt'])['price'] if xs else None

def closing(H, start):
    t = start.replace('.000Z', '').replace('Z', '')
    for bk in BOOKS:
        v = ((H or {}).get('bookmakers') or {}).get(bk)
        if not v: continue
        best, ml = None, None
        for mid, m in (v.get('markets') or {}).items():
            info = M.get(mid)
            if not info: continue
            p = [last_before(((m.get('outcomes') or {}).get(str(o)) or {}).get('players', {}).get('0') or [], t) for o in info['o']]
            if None in p: continue
            if info['t'] == 'moneyline': ml = p; continue
            gap = abs(1 / p[0] - 1 / p[1])
            if best is None or gap < best[0]: best = (gap, -info['h'], p)
        if best: return dict(margin=best[1], book=bk, p1=best[2][0], p2=best[2][1], ml1=ml[0] if ml else None, ml2=ml[1] if ml else None)
    return None

COLS = ['fixtureId', 'league', 'start', 'team1', 'team2', 'margin', 'book', 'p1', 'p2', 'ml1', 'ml2']
fn = os.path.join(D, 'lines.csv')
have = {}
if os.path.exists(fn):
    with open(fn) as fh: have = {r['fixtureId']: r for r in csv.DictReader(fh)}
upcoming = []
now = time.strftime('%Y-%m-%dT%H:%M:%SZ', time.gmtime())
for lg, tid in LEAGUES.items():
    F = call('fixtures', tournamentId=tid, **{'from': '2026-01-01T00:00:00Z', 'to': time.strftime('%Y-%m-%dT%H:%M:%SZ', time.gmtime(time.time() + 21 * 86400))}) or []
    todo = [f for f in F if f.get('statusId') == 2 and f['fixtureId'] not in have]
    upcoming += [dict(fixtureId=f['fixtureId'], league=lg, start=f['startTime'], team1=f.get('participant1Name'), team2=f.get('participant2Name'),
                      hasOdds=f.get('hasOdds')) for f in F if f.get('statusId') == 0]
    print(f'{lg}: {len(F)} fixtures, {len(todo)} finished without a line yet', flush=True)
    for i, f in enumerate(todo):
        H = call('historical-odds', wait=5.5, fixtureId=f['fixtureId'], bookmakers=','.join(BOOKS))
        c = closing(H, f['startTime'])
        have[f['fixtureId']] = dict(fixtureId=f['fixtureId'], league=lg, start=f['startTime'], team1=f.get('participant1Name'),
                                    team2=f.get('participant2Name'), **(c or {}))
        if (i + 1) % 25 == 0:
            print(f'  {lg}: {i + 1}/{len(todo)}', flush=True)
            with open(fn, 'w', newline='') as fh:
                w = csv.DictWriter(fh, fieldnames=COLS); w.writeheader(); w.writerows(sorted(have.values(), key=lambda r: r['start']))
with open(fn, 'w', newline='') as fh:
    w = csv.DictWriter(fh, fieldnames=COLS); w.writeheader(); w.writerows(sorted(have.values(), key=lambda r: r['start']))
with open(os.path.join(D, 'upcoming.csv'), 'w', newline='') as fh:
    w = csv.DictWriter(fh, fieldnames=['fixtureId', 'league', 'start', 'team1', 'team2', 'hasOdds']); w.writeheader(); w.writerows(upcoming)
print(f'lines: {len(have)} fixtures, {sum(1 for r in have.values() if r.get("margin") not in (None, ""))} with a closing spread', flush=True)
