// Cloudflare Worker: look up a sport5 EuroLeague fantasy team by name.
// Secrets (Settings → Variables and Secrets, type "Secret"): S5_EMAIL, S5_PASSWORD, DEBUG_KEY
// Public endpoints (used by the GitHub Pages site):
//   /lookup?name=<team name>   → rank, league size, scores and the squad (sanitised, no personal data)
//   /lookup?userId=<id>        → same, when several teams share a name
// Private endpoint (for setup checks only, needs DEBUG_KEY):
//   /debug?key=<DEBUG_KEY>&name=<team name>
//
// Load on sport5 is kept minimal: one login reused, results cached for 1 hour, league size
// cached for 12 hours, and a global limit on upstream lookups per minute.

const API = 'https://dreamteam.sport5.co.il/api/';
const ALLOWED_ORIGIN = 'https://aviv-tz.github.io';
const MAX_LOOKUPS_PER_MIN = 20;

let COOKIE = null, SEASON = null, SIZE = null, SIZE_AT = 0;
const FALLBACK_SEASON = 11;   // sport5 EuroLeague fantasy season 2026/27
// Same headers a browser on the fantasy site sends
const BH = {
  'Accept': 'application/json, text/plain, */*',
  'Accept-Language': 'he-IL,he;q=0.9,en;q=0.8',
  'Origin': 'https://fantasyeuroleague.sport5.co.il',
  'Referer': 'https://fantasyeuroleague.sport5.co.il/',
  'User-Agent': 'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/130.0 Safari/537.36'
};
let windowStart = 0, windowCount = 0, sWindowStart = 0, sWindowCount = 0;
const MAX_SEARCHES_PER_MIN = 40;

function cors(extra = {}) {
  return { 'Access-Control-Allow-Origin': ALLOWED_ORIGIN, 'Content-Type': 'application/json; charset=utf-8', ...extra };
}
const json = (obj, status = 200, headers = {}) => new Response(JSON.stringify(obj), { status, headers: cors(headers) });

function cookiesFrom(res) {
  let list = [];
  if (typeof res.headers.getSetCookie === 'function') list = res.headers.getSetCookie();
  else if (typeof res.headers.getAll === 'function') list = res.headers.getAll('Set-Cookie');
  else { const h = res.headers.get('set-cookie'); if (h) list = [h]; }
  return list.map(c => c.split(';')[0]).filter(Boolean).join('; ');
}

async function login(env) {
  const res = await fetch(API + 'Account/Login', {
    method: 'POST', redirect: 'manual',
    headers: { ...BH, 'Content-Type': 'application/json' },
    body: JSON.stringify({ email: env.S5_EMAIL, password: env.S5_PASSWORD, googleToken: null, facebookToken: null, adminKey: null })
  });
  const c = cookiesFrom(res);
  let ok = false; try { ok = (await res.json()).result === true; } catch (e) {}
  if (!c || !ok) throw new Error('login failed (status ' + res.status + ')');
  COOKIE = c;
}

// GET with the session cookie; logs in again once if the session expired.
async function api(env, path, retry = true) {
  if (!COOKIE) await login(env);
  const res = await fetch(API + path, { redirect: 'manual', headers: { ...BH, Cookie: COOKIE } });
  const text = await res.text();
  if (res.status >= 300 || text.trimStart().startsWith('<')) {
    if (!retry) throw new Error('not authorised (status ' + res.status + ')');
    COOKIE = null; return api(env, path, false);
  }
  return JSON.parse(text);
}

async function season(env) {
  if (SEASON) return SEASON;
  try {
    const r = await fetch(API + 'Leagues/GetLeagues', { headers: BH }).then(r => r.json());
    const l = (r.data || []).find(x => (x.urlName || '').toLowerCase() === 'fantasyeuroleague');
    if (l) return (SEASON = l.currentSeasonId);
  } catch (e) {}
  return (SEASON = FALLBACK_SEASON);
}

const leaguePage = async (env, page, search = '') =>
  api(env, `CustomLeagues/GetLeagueData?seasonId=${await season(env)}&isPerRound=false&page=${page}&searchText=${encodeURIComponent(search)}`);
const teamsOf = r => (r && r.data && r.data.teams) || [];

// League size: binary search for the last page (about 15-20 requests), cached for 12 hours.
async function leagueSize(env) {
  if (SIZE && Date.now() - SIZE_AT < 12 * 3600e3) return SIZE;
  const n = teamsOf(await leaguePage(env, 0)).length;
  if (!n) throw new Error('empty league table');
  let hi = 1;
  while (hi < 1 << 20 && teamsOf(await leaguePage(env, hi)).length === n) hi *= 2;
  let lo = hi >> 1;
  while (hi - lo > 1) { const m = (lo + hi) >> 1; if (teamsOf(await leaguePage(env, m)).length === n) lo = m; else hi = m; }
  let last = teamsOf(await leaguePage(env, hi));
  if (!last.length) { hi -= 1; last = teamsOf(await leaguePage(env, hi)); }
  SIZE = hi * n + last.length; SIZE_AT = Date.now();
  return SIZE;
}

// Squad: keep only player/coach objects and only non-personal fields.
const PLAYER_KEYS = /^(id|playerId|name|price|position|teamId|teamName|shirtNumber)$|captain|bench|sub|starter|order|role|lineup/i;
function squadFrom(obj) {
  const found = [];
  (function walk(o, path) {
    if (Array.isArray(o)) { o.forEach((x, i) => walk(x, path + '[' + i + ']')); return; }
    if (!o || typeof o !== 'object') return;
    if (typeof o.price === 'number' && (o.position != null || o.positionId != null)) {
      const p = { path };
      for (const k of Object.keys(o)) if (PLAYER_KEYS.test(k) && (o[k] === null || typeof o[k] !== 'object')) p[k] = o[k];
      found.push(p); return;
    }
    for (const k of Object.keys(o)) walk(o[k], path + '.' + k);
  })(obj, '');
  return found;
}
const flagsOf = obj => {   // top-level booleans/ids that may mark captain / bench (no personal data)
  const out = {};
  (function walk(o, d) {
    if (!o || typeof o !== 'object' || d > 3) return;
    for (const [k, v] of Object.entries(o)) {
      if (/captain|bench|sub/i.test(k) && (typeof v !== 'object' || v === null)) out[k] = v;
      else if (typeof v === 'object') walk(v, d + 1);
    }
  })(obj, 0);
  return out;
};
const teamInfo = t => ({ userId: t.userId, name: t.name, position: (t.position ?? 0) + 1,
  totalScore: t.totalScore, roundScore: t.roundScore, weeklyPosition: t.weeklyPosition != null ? t.weeklyPosition + 1 : null });

function rateLimited() {
  const now = Date.now();
  if (now - windowStart > 60e3) { windowStart = now; windowCount = 0; }
  return ++windowCount > MAX_LOOKUPS_PER_MIN;
}

async function lookup(env, url) {
  const name = (url.searchParams.get('name') || '').trim();
  const userId = url.searchParams.get('userId');
  if (!name && !userId) return json({ error: 'missing name' }, 400);
  if (rateLimited()) return json({ error: 'busy, try again in a minute' }, 429);
  const norm = s => (s || '').replace(/\s+/g, ' ').trim().toLowerCase();
  const found = teamsOf(await leaguePage(env, 0, name || ''));
  let matches = userId ? found.filter(t => String(t.userId) === String(userId))
                       : found.filter(t => norm(t.name) === norm(name));
  if (!matches.length && userId && name === '') return json({ error: 'not found' }, 404);
  if (matches.length > 1) return json({ choose: matches.map(teamInfo) });
  if (!matches.length) return json({ suggestions: found.slice(0, 10).map(teamInfo) });
  const t = matches[0];
  const squadRaw = await api(env, `UserTeam/GetUserAndTeam?seasonId=${await season(env)}&userId=${t.userId}`);
  const size = await leagueSize(env);
  const info = teamInfo(t);
  const ut = (squadRaw && squadRaw.data && squadRaw.data.userTeam) || {};
  const players = (ut.userTeamPlayers || []).filter(x => x && x.player && x.isActive !== false && !x.isRemoved).map(x => ({
    s5id: x.player.id, name: x.player.name, price: x.player.price / 1e6, pos: x.player.position,   // 1 G, 2 F, 3 C, 4 coach
    teamId: x.player.teamId, reserve: !!x.isReserve,
    injured: (x.player.injuredStatus || 0) > 0 || (x.player.missingStatus || 0) > 0,
    lastRound: x.player.lastRoundPlayerStats ? x.player.lastRoundPlayerStats.points : null }));
  return json({ team: info, leagueSize: size, percentile: Math.round(1000 * info.position / size) / 10,
                squad: players, captainId: ut.captainId ?? null, subCaptainId: ut.subCaptainId ?? null },
              200, { 'Cache-Control': 'public, max-age=900' });
}

export default {
  async fetch(request, env, ctx) {
    const url = new URL(request.url);
    try {
      if (url.pathname === '/lookup') {
        const cache = caches.default, key = new Request(url.toString());
        const hit = await cache.match(key); if (hit) return hit;
        const res = await lookup(env, url);
        if (res.status === 200) ctx.waitUntil(cache.put(key, res.clone()));
        return res;
      }
      if (url.pathname === '/search') {    // autocomplete: one request per query, cached 15 min
        const q = (url.searchParams.get('q') || '').trim();
        if (q.length < 3) return json({ teams: [] });
        const key = new Request(url.origin + '/search?q=' + encodeURIComponent(q.toLowerCase()));
        const hit = await caches.default.match(key); if (hit) return hit;
        const now = Date.now(); if (now - sWindowStart > 60e3) { sWindowStart = now; sWindowCount = 0; }
        if (++sWindowCount > MAX_SEARCHES_PER_MIN) return json({ error: 'busy' }, 429);
        const teams = teamsOf(await leaguePage(env, 0, q)).slice(0, 12).map(teamInfo);
        const res = json({ teams }, 200, { 'Cache-Control': 'public, max-age=900' });
        ctx.waitUntil(caches.default.put(key, res.clone()));
        return res;
      }
      if (url.pathname === '/size') {      // league size only (cached 6 h) - used for ownership %
        const cache = caches.default, key = new Request(url.origin + '/size');
        const hit = await cache.match(key); if (hit) return hit;
        const res = json({ leagueSize: await leagueSize(env) }, 200, { 'Cache-Control': 'public, max-age=21600' });
        ctx.waitUntil(cache.put(key, res.clone()));
        return res;
      }
      if (url.pathname === '/debug') {
        if (!env.DEBUG_KEY || url.searchParams.get('key') !== env.DEBUG_KEY) return json({ error: 'forbidden' }, 403);
        const name = url.searchParams.get('name') || '';
        const out = { secrets: { S5_EMAIL: !!env.S5_EMAIL, S5_PASSWORD: !!env.S5_PASSWORD } };
        const peek = async (label, res) => { const t = await res.text();
          out[label] = { status: res.status, type: res.headers.get('content-type'), location: res.headers.get('location'),
                         isJson: !t.trimStart().startsWith('<'), head: t.slice(0, 160).replace(/"(email|password)":"[^"]*"/gi, '"$1":"[hidden]"') };
          return t; };
        // 1. public endpoint
        await peek('step1_getLeagues', await fetch(API + 'Leagues/GetLeagues', { headers: BH }));
        // 2. login
        const lr = await fetch(API + 'Account/Login', { method: 'POST', redirect: 'manual', headers: { ...BH, 'Content-Type': 'application/json' },
          body: JSON.stringify({ email: env.S5_EMAIL, password: env.S5_PASSWORD, googleToken: null, facebookToken: null, adminKey: null }) });
        const ck = cookiesFrom(lr);
        await peek('step2_login', lr);
        out.step2_login.cookieNames = ck ? ck.split('; ').map(c => c.split('=')[0]) : [];
        if (!ck) return json(out);
        COOKIE = ck;
        // 3. search the general league
        const sr = await fetch(API + `CustomLeagues/GetLeagueData?seasonId=${await season(env)}&isPerRound=false&page=0&searchText=${encodeURIComponent(name)}`,
                               { redirect: 'manual', headers: { ...BH, Cookie: COOKIE } });
        const st = await peek('step3_search', sr);
        let teams = []; try { teams = teamsOf(JSON.parse(st)); } catch (e) {}
        out.step3_search.count = teams.length; out.step3_search.first = teams[0] ? teamInfo(teams[0]) : null;
        if (!teams[0]) return json(out);
        // 4. squad of the first match
        const qr = await fetch(API + `UserTeam/GetUserAndTeam?seasonId=${await season(env)}&userId=${teams[0].userId}`,
                               { redirect: 'manual', headers: { ...BH, Cookie: COOKIE } });
        const qt = await peek('step4_squad', qr);
        try { const raw = JSON.parse(qt);
          out.step4_squad.squad = squadFrom(raw); out.step4_squad.flags = flagsOf(raw);
          out.step4_squad.rawSample = JSON.stringify(raw, (k, v) => /mail|phone|password|token|username|firstname|lastname|fullname|birth/i.test(k) ? '[hidden]' : v).slice(0, 3000);
        } catch (e) {}
        delete out.step4_squad.head;
        return json(out);
      }
      return json({ ok: true, endpoints: ['/lookup?name=', '/search?q=', '/size', '/debug?key=&name='] });
    } catch (e) {
      return json({ error: String(e.message || e) }, 500);
    }
  }
};
