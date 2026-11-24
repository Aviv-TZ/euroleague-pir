"""Fantasy ownership (how many teams hold each player / coach), refreshed by how close the transfer window is to closing:
  window open, more than 12 hours to go: every 3 hours
  6-12 hours to go: every hour
  last 6 hours: every 30 minutes
  round in progress (squads locked): every 6 hours
The window closes at the first tip of the next round and opens after the last game of the previous one.

Also keeps docs/own_base.json: ownership as it was when the window opened (the last reading taken while the previous
round was locked), so the site can show who was bought / sold the most since then.

Usage: python3 tools/ownership.py [--force]       (run every 30 minutes; it decides by itself whether a refresh is due)
Prints 'updated' when docs/own.json changed."""
import json, os, sys, time, urllib.request
from datetime import datetime, timedelta, timezone

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
DOCS = os.path.join(ROOT, 'docs')
API = 'https://dreamteam.sport5.co.il/api'
WORKER = 'https://el-fantasy.aviv-tzafrir.workers.dev'
UA = {'User-Agent': 'Mozilla/5.0 (compatible; fantasy-ownership)'}
GAME_H = 2.5            # a game is over about 2.5 hours after the tip
SLACK = timedelta(minutes=8)   # scheduled runs drift; a refresh a few minutes early still counts as due

def now_utc(): return datetime.now(timezone.utc)
def parse(t): return datetime.strptime(t[:19], '%Y-%m-%dT%H:%M:%S').replace(tzinfo=timezone.utc)
def iso(t): return t.strftime('%Y-%m-%dT%H:%M:%SZ')

def get(url, post=False, tries=4):
    for k in range(tries):
        try:
            req = urllib.request.Request(url, data=b'' if post else None, headers=UA, method='POST' if post else 'GET')
            with urllib.request.urlopen(req, timeout=40) as r: return json.load(r)
        except Exception:
            if k == tries - 1: return None
            time.sleep(3 + 5 * k)

def load(name):
    try:
        with open(os.path.join(DOCS, name)) as f: return json.load(f)
    except Exception: return None

def save(name, obj):
    with open(os.path.join(DOCS, name), 'w') as f: json.dump(obj, f, separators=(',', ':'))

def rounds():
    """first / last tip of every round, from the site's fixtures and live schedule"""
    tips = {}
    for src in ((load('data.json') or {}).get('fixtures') or [], (load('live.json') or {}).get('schedule') or []):
        for g in src:
            if g.get('utc') and g.get('round') is not None:
                tips.setdefault(int(g['round']), set()).add(parse(g['utc']))
    return {r: (min(t), max(t)) for r, t in tips.items()}

def state(now):
    """(locked?, next round to lock, hours until it locks)"""
    rr = rounds()
    locked = any(a <= now < b + timedelta(hours=GAME_H) for a, b in rr.values())
    nxt = min((r for r, (a, b) in rr.items() if a > now), default=None)
    hours = (rr[nxt][0] - now).total_seconds() / 3600 if nxt else None
    return locked, nxt, hours

def cadence(locked, hours):
    if locked or hours is None: return timedelta(hours=6)
    if hours > 12: return timedelta(hours=3)
    if hours > 6: return timedelta(hours=1)
    return timedelta(minutes=30)

def fetch():
    lg = get(f'{API}/Leagues/GetLeagues') or {}
    sid = next((l['currentSeasonId'] for l in lg.get('data') or [] if l.get('urlName') == 'fantasyeuroleague'), None)
    tp = get(f'{API}/Players/GetTeamsAndPlayers?seasonId={sid}') if sid else None
    size = (get(f'{WORKER}/size') or {}).get('leagueSize')
    if not tp or not size: return None
    ids = [p['id'] for t in tp.get('data') or [] for p in t.get('players') or []]
    own, inj = {}, []
    for pid in ids:
        d = get(f'{API}/Players/GetPlayerData?seasonId={sid}&playerId={pid}', post=True)
        dd = (d or {}).get('data') or {}
        v = dd.get('timesSelected')
        if v is not None: own[str(pid)] = v
        pl = dd.get('player') or {}
        if (pl.get('injuredStatus') or 0) > 0 or (pl.get('missingStatus') or 0) > 0: inj.append(pid)   # injured / missing on the fantasy site
        time.sleep(0.1)
    if len(own) < 0.8 * len(ids): return None   # a partial reading would show fake drops
    return {'updated_utc': iso(now_utc()), 'size': size, 'own': own, 'inj': inj}

def wait_seconds():
    """For the workflow's loop: seconds until the next refresh is due, while the window closes within 6 hours
    (GitHub starts scheduled runs late or skips them, so in the last hours one run stays and refreshes on time).
    0 = no need to wait in this run."""
    now = now_utc(); locked, nxt, hours = state(now)
    if locked or hours is None or hours > 6.5: return 0
    cur = load('own.json')
    if not cur: return 1
    due = parse(cur['updated_utc']) + cadence(locked, hours)
    return max(1, int((due - now).total_seconds()) + 30)

def main():
    if '--wait' in sys.argv: print(wait_seconds()); return
    force = '--force' in sys.argv
    now = now_utc()
    locked, nxt, hours = state(now)
    cur = load('own.json')
    age = now - parse(cur['updated_utc']) if cur and cur.get('updated_utc') else None
    due = force or age is None or age >= cadence(locked, hours) - SLACK
    base = load('own_base.json')
    print(f'locked={locked} next_round={nxt} hours_to_lock={hours and round(hours, 1)} '
          f'age={age} cadence={cadence(locked, hours)} due={due}', flush=True)
    # the window has opened for round nxt but no reading was kept as its start: use the last reading (best effort)
    if not locked and nxt and cur and (not base or base.get('round') != nxt):
        save('own_base.json', {**cur, 'round': nxt}); print('base: last reading kept as the window start for round', nxt)
    if not due: return
    new = fetch()
    if not new: print('ownership: no reading (site unavailable)'); return
    save('own.json', new); print(f"updated: {len(new['own'])} players, league size {new['size']}")
    if locked and nxt:   # while squads are locked, every reading is the start point of the coming window
        save('own_base.json', {**new, 'round': nxt}); print('base: window start for round', nxt)

if __name__ == '__main__':
    main()
