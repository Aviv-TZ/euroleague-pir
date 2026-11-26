"""OddsPapi check (run in GitHub Actions with the ODDSPAPI_KEY secret; the key is never printed).
Uses about 5 of the monthly requests: tournaments, markets, fixtures x2 (+ the free account / historical-odds calls).
Writes cm/data/odds/probe.json (no key inside) and prints a summary."""
import json, os, sys, time, urllib.parse, urllib.request
KEY = os.environ.get('ODDSPAPI_KEY', '').strip()
if not KEY: sys.exit('ODDSPAPI_KEY is not set')
BASE = 'https://api.oddspapi.io/v4/'
OUT = os.path.join(os.path.dirname(os.path.dirname(os.path.abspath(__file__))), 'cm', 'data', 'odds'); os.makedirs(OUT, exist_ok=True)
last = {}
def call(path, wait=2.2, **q):
    ep = path.split('?')[0]
    if ep in last: time.sleep(max(0, wait - (time.time() - last[ep])))
    q['apiKey'] = KEY
    url = BASE + path + '?' + urllib.parse.urlencode(q)
    try:
        with urllib.request.urlopen(urllib.request.Request(url, headers={'Accept': 'application/json'}), timeout=60) as r:
            last[ep] = time.time(); return json.load(r)
    except urllib.error.HTTPError as e:
        last[ep] = time.time(); body = e.read().decode('utf-8', 'ignore')[:300]
        print(f'  {path}: HTTP {e.code} {body}'); return None

out = {}
acc = call('account'); out['account'] = {k: v for k, v in (acc or {}).items() if 'key' not in k.lower()}
print('account:', json.dumps(out['account'])[:400])
T = call('tournaments', sportId=11) or []
hits = [t for t in T if any(w in json.dumps(t, ensure_ascii=False).lower() for w in ['euroleague', 'israel', 'ligat', 'winner', 'super league', 'premier league'])]
out['tournaments'] = hits
print(f'basketball tournaments: {len(T)}; matching:')
for t in hits: print('  ', json.dumps({k: t.get(k) for k in ('tournamentId', 'tournamentName', 'categoryName', 'categorySlug', 'futureFixtures', 'upcomingFixtures', 'liveFixtures')}, ensure_ascii=False))
M = call('markets', sportId=11) or []
keep = [m for m in M if m.get('marketType') in ('spreads', 'moneyline', 'totals') and m.get('period') in ('result', 'fulltime') and not m.get('playerProp')]
out['markets'] = keep
print(f'markets: {len(M)} basketball, {len(keep)} kept (full-game spreads / moneyline / totals)')
el = next((t for t in hits if 'euroleague' in json.dumps(t).lower() and 'women' not in json.dumps(t).lower()), None)
il = next((t for t in hits if 'israel' in json.dumps(t, ensure_ascii=False).lower() and 'women' not in json.dumps(t).lower()), None)
for lab, t in [('euroleague', el), ('israel', il)]:
    if not t: print(lab, ': no tournament found'); continue
    F = call('fixtures', tournamentId=t['tournamentId'], **{'from': '2026-01-01T00:00:00Z', 'to': time.strftime('%Y-%m-%dT%H:%M:%SZ', time.gmtime(time.time() + 14 * 86400))}) or []
    out[f'fixtures_{lab}'] = F
    fin = [f for f in F if f.get('statusId') == 2]
    print(f'{lab} ({t.get("tournamentName")}): {len(F)} fixtures since 2026-01-01, {len(fin)} finished, {sum(1 for f in F if f.get("statusId") == 0)} upcoming, '
          f'with odds: {sum(1 for f in F if f.get("hasOdds"))}; first {F[0].get("startTime") if F else None} last {F[-1].get("startTime") if F else None}')
    for f in fin[-2:]:
        H = call('historical-odds', wait=5.5, fixtureId=f['fixtureId'], bookmakers='pinnacle+30,bet365,1xbet')
        out.setdefault('historical', {})[f['fixtureId']] = H
        bm = (H or {}).get('bookmakers') or {}
        print(f'   historical {f.get("participant1Name")} - {f.get("participant2Name")} {f.get("startTime")}: bookmakers {list(bm)}, '
              f'markets {[len((v or {}).get("markets") or {}) for v in bm.values()]}')
with open(os.path.join(OUT, 'probe.json'), 'w') as fh: json.dump(out, fh, ensure_ascii=False)
acc = call('account'); print('account after:', json.dumps({k: v for k, v in (acc or {}).items() if 'key' not in k.lower()})[:400])
