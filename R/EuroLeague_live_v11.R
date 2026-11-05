# EuroLeague v11 - LIVE / IN-ROUND LAYER (no training, no model re-scoring)
#
# Called by EuroLeague_auto_update_v11.R on every scheduled run. Writes
# <site_dir>/live.json, which the page reads on top of the frozen round predictions:
#   * actual PIR / minutes / real rank for players whose game in the current round is FINISHED
#   * live PIR / minutes for games IN PROGRESS (clearly flagged as live)
#   * opponent-by-position PIR table recomputed with every finished game
#   * each team's real NEXT game (after a team played this round -> its next round game)
# The heavy box-score download only happens when a new game has finished.

el_live_helpers <- local({
  val <- function(x, keys) { if (!is.list(x)) return(NULL)
    for (k in keys) if (!is.null(x[[k]]) && length(x[[k]])) return(x[[k]]); NULL }
  str1 <- function(x) {
    if (is.null(x) || !length(x)) return('')
    if (!is.list(x)) return(as.character(x[1]))
    v <- val(x, c('fullName','name','officialName','clubPermanentName','displayName',
                  'shortName','personName','code'))
    if (!is.null(v)) return(str1(v))
    paste(Filter(nzchar, c(str1(x$firstName), str1(x$lastName))), collapse=' ')
  }
  number <- function(x) {
    if (is.null(x) || !length(x)) return(NA_real_)
    if (is.list(x)) return(number(val(x, c('value','total','seconds'))))
    z <- trimws(as.character(x[1]))
    if (grepl('^[0-9]+:[0-9]{2}$', z)) { p <- as.numeric(strsplit(z, ':', fixed=TRUE)[[1]]); return(p[1]*60+p[2]) }
    suppressWarnings(as.numeric(z))
  }
  list(val=val, str1=str1, number=number)
})

el_get_json <- function(path, season='E2026') {
  bases <- c(paste0('https://api-live.euroleague.net/v2/competitions/E/seasons/', season),
             paste0('https://feeds.incrowdsports.com/provider/euroleague-feeds/v2/competitions/E/seasons/', season))
  for (b in bases) {
    r <- tryCatch(httr::GET(paste0(b, '/', path), httr::user_agent('EuroLeague-R-Boxscore/2.0'),
                            httr::add_headers(Accept='application/json'), httr::timeout(20)), error=identity)
    if (!inherits(r, 'error') && httr::status_code(r) == 200) {
      x <- tryCatch(jsonlite::fromJSON(httr::content(r, 'text', encoding='UTF-8'), simplifyVector=FALSE),
                    error=identity)
      if (!inherits(x, 'error') && is.list(x)) return(x)
    }
  }
  NULL
}

# Player lines of ONE game box score (used only for games in progress).
el_live_box <- function(game_id, home, away) {
  h <- el_live_helpers
  box <- el_get_json(paste0('games/', game_id, '/stats'))
  if (is.null(box)) {
    el_live_debug(game_id, 'v2', list(ok=FALSE, err='no response'))
    return(el_live_box_legacy(game_id, home, away))
  }
  if (!is.null(box$data) && is.list(box$data) && is.null(box$local)) box <- box$data
  out <- list()
  for (side in c('local','road')) {
    sec <- h$val(box, c(side, if (side == 'local') 'home' else 'away'))
    if (is.null(sec)) next
    plys <- h$val(sec, c('players','playerStats','playerStatistics'))
    if (!is.null(plys$data)) plys <- plys$data
    for (line in plys) {
      person <- h$val(line, c('player','person')); if (is.null(person)) person <- list()
      stats <- h$val(line, c('stats','statistics')); if (is.null(stats)) stats <- line
      name <- h$str1(h$val(person, c('person')) %||% person)
      secs <- h$number(h$val(stats, c('timePlayed','minutesPlayed','minutes','min')))
      pir <- h$number(h$val(stats, c('valuation','pir','PIR','rating')))
      pts <- h$number(h$val(stats, c('points','pts','Points')))
      if (!nzchar(name) || !is.finite(secs) || secs <= 0 || !is.finite(pir)) next
      out[[length(out)+1L]] <- data.frame(Game=game_id, Player=name,
        Team=if (side == 'local') home else away, Opponent=if (side == 'local') away else home,
        Minutes=secs/60, PIR=pir, Points=if (is.finite(pts)) pts else 0, Side=side,
        stringsAsFactors=FALSE)
    }
  }
  res <- if (length(out)) do.call(rbind, out) else NULL
  el_live_debug(game_id, 'v2', list(ok=TRUE, top=names(box),
    local=names(h$val(box, c('local','home'))), rows=if (is.null(res)) 0L else nrow(res)))
  if (is.null(res)) res <- el_live_box_legacy(game_id, home, away)
  res
}

# Fallback: the older live box-score service (built for games in progress).
el_live_box_legacy <- function(game_id, home, away, season='E2026') {
  h <- el_live_helpers
  u <- 'https://live.euroleague.net/api/Boxscore'
  r <- tryCatch(httr::GET(u, query=list(gamecode=game_id, seasoncode=season),
                          httr::user_agent('EuroLeague-R-Boxscore/2.0'), httr::timeout(20)), error=identity)
  if (inherits(r, 'error') || httr::status_code(r) != 200) {
    el_live_debug(game_id, 'legacy', list(ok=FALSE, err=if (inherits(r,'error')) conditionMessage(r) else httr::status_code(r)))
    return(NULL)
  }
  x <- tryCatch(jsonlite::fromJSON(httr::content(r, 'text', encoding='UTF-8'), simplifyVector=FALSE), error=identity)
  if (inherits(x, 'error') || !is.list(x)) { el_live_debug(game_id, 'legacy', list(ok=FALSE, err='bad json')); return(NULL) }
  el_parse_legacy_box(x, game_id, home, away)
}
el_parse_legacy_box <- function(x, game_id, home, away) {
  h <- el_live_helpers
  st <- h$val(x, c('Stats','stats'))
  out <- list()
  for (k in seq_along(st)) {
    side <- if (k == 1) 'local' else 'road'
    for (pl in h$val(st[[k]], c('PlayersStats','playersStats'))) {
      name <- h$str1(h$val(pl, c('Player','player')))
      secs <- h$number(h$val(pl, c('Minutes','minutes')))
      pir <- h$number(h$val(pl, c('Valuation','valuation')))
      pts <- h$number(h$val(pl, c('Points','points')))
      if (!nzchar(name) || toupper(name) %in% c('TEAM','TOTAL') || !is.finite(secs) || secs <= 0 || !is.finite(pir)) next
      out[[length(out)+1L]] <- data.frame(Game=game_id, Player=trimws(name),
        Team=if (side == 'local') home else away, Opponent=if (side == 'local') away else home,
        Minutes=secs/60, PIR=pir, Points=if (is.finite(pts)) pts else 0, Side=side, stringsAsFactors=FALSE)
    }
  }
  res <- if (length(out)) do.call(rbind, out) else NULL
  el_live_debug(game_id, 'legacy', list(ok=TRUE, top=names(x), teams=length(st),
    first=if (length(st)) names(h$val(st[[1]], c('PlayersStats'))[[1]]) else NULL,
    rows=if (is.null(res)) 0L else nrow(res)))
  res
}
.el_dbg <- new.env()
el_live_debug <- function(game, src, info) assign(paste(game, src), c(list(game=game, src=src), info), envir=.el_dbg)
`%||%` <- function(a, b) if (is.null(a)) b else a

el_opponent_tables <- function(raw, meta) {
  pos3 <- c('Guard','Forward','Center')
  wavg <- function(d) if (nrow(d) && sum(d$Minutes) > 0) round(sum(d$PIR_x_Minutes)/sum(d$Minutes), 3) else NULL
  meta <- meta[order(meta$GameDate, suppressWarnings(as.numeric(meta$Game)), decreasing=TRUE),,drop=FALSE]
  teams <- sort(unique(raw$Opponent))
  opponents <- lapply(teams, function(t) {
    g <- meta$Game[(meta$Home == t | meta$Away == t) & meta$Game %in% raw$Game[raw$Opponent == t]]
    recent <- raw[raw$Opponent == t & raw$Game %in% head(g, 5),,drop=FALSE]
    list(team=t,
         season=setNames(lapply(pos3, function(p) wavg(raw[raw$Opponent == t & raw$Position == p,,drop=FALSE])), pos3),
         last5=setNames(lapply(pos3, function(p) wavg(recent[recent$Position == p,,drop=FALSE])), pos3))
  })
  league <- setNames(lapply(pos3, function(p) wavg(raw[raw$Position == p,,drop=FALSE])), pos3)
  list(opponents=opponents, league=league)
}

el_live_update <- function(schedule, site_dir, code_dir, now=Sys.time()) {
  data_file <- file.path(site_dir, 'data.json')
  if (!file.exists(data_file)) return(invisible(FALSE))
  d <- jsonlite::fromJSON(data_file, simplifyVector=FALSE)
  live_file <- file.path(site_dir, 'live.json')
  prev <- if (file.exists(live_file)) jsonlite::fromJSON(live_file, simplifyVector=FALSE) else list()

  s <- schedule[is.finite(schedule$Round) & nzchar(schedule$Game),,drop=FALSE]
  R <- suppressWarnings(as.numeric(d$meta$next_round))
  if (!length(R) || !is.finite(R)) {
    fx_games <- vapply(d$fixtures, function(x) as.character(x$game), '')
    R <- suppressWarnings(min(s$Round[s$Game %in% fx_games]))
    if (!is.finite(R)) R <- min(s$Round[!s$Played])
  }
  rg <- s[s$Round == R,,drop=FALSE]
  finished <- sort(s$Game[s$Played])
  live_ids <- s$Game[!s$Played & !is.na(s$Utc) & s$Utc <= now & now <= s$Utc + 3.5*3600]

  # ---- finished games: full box-score collection, only when something new finished ----
  same_finished <- identical(as.character(unlist(prev$finished_ids)), as.character(finished)) &&
                   identical(as.numeric(prev$round), as.numeric(R))
  final_rows <- NULL; tables <- NULL
  if (!same_finished) {
    if (!exists('build_euroleague_2026_27', mode='function'))
      source(file.path(code_dir, 'EuroLeague_ranking_models_v11.R'))
    col <- tryCatch(build_euroleague_2026_27(file.path(tempdir(), 'el_live_collect'), mode='collect'),
                    error=function(e) { message('Live collect failed: ', conditionMessage(e)); NULL })
    if (!is.null(col)) {
      raw <- col$raw[col$raw$Game %in% finished,,drop=FALSE]   # in-progress games never enter the tables
      tables <- el_opponent_tables(raw, col$meta[col$meta$Game %in% finished,,drop=FALSE])
      fr <- raw[raw$Game %in% rg$Game,,drop=FALSE]
      final_rows <- if (nrow(fr)) data.frame(Game=fr$Game, Player=fr$Player, Team=fr$Team,
        Opponent=fr$Opponent, Minutes=fr$Minutes, PIR=fr$PIR, stringsAsFactors=FALSE) else
        data.frame(Game=character(), Player=character(), Team=character(), Opponent=character(),
                   Minutes=numeric(), PIR=numeric())
    } else same_finished <- TRUE   # keep previous values
  }
  if (same_finished) {
    pa <- Filter(function(x) identical(x$status, 'final'), prev$actual %||% list())
    final_rows <- if (length(pa)) data.frame(Game=vapply(pa, function(x) as.character(x$game), ''),
      Player=vapply(pa, `[[`, '', 'raw'), Team=vapply(pa, `[[`, '', 'team'),
      Opponent=vapply(pa, `[[`, '', 'opp'), Minutes=vapply(pa, function(x) as.numeric(x$min), 0),
      PIR=vapply(pa, function(x) as.numeric(x$pir), 0), stringsAsFactors=FALSE) else NULL
    tables <- list(opponents=prev$opponents, league=prev$league)
  }

  # ---- games in progress (only current-round games are displayed as live) ----
  live_rows <- NULL
  for (g in intersect(live_ids, rg$Game)) {
    x <- rg[rg$Game == g,]
    b <- tryCatch(el_live_box(g, x$Home, x$Away), error=function(e) NULL)
    if (!is.null(b)) live_rows <- rbind(live_rows, b)
  }
  live_score <- list()   # live score = sum of player points per side
  if (!is.null(live_rows) && nrow(live_rows)) {
    for (g in unique(live_rows$Game)) {
      z <- live_rows[live_rows$Game == g,,drop=FALSE]
      live_score[[g]] <- c(hs=sum(z$Points[z$Side == 'local']), as=sum(z$Points[z$Side == 'road']))
    }
    live_rows <- live_rows[, c('Game','Player','Team','Opponent','Minutes','PIR')]
  }

  # ---- actual results + real rank among everyone who played in the round so far ----
  rows <- rbind(if (!is.null(final_rows) && nrow(final_rows)) cbind(final_rows, status='final'),
                if (!is.null(live_rows) && nrow(live_rows)) cbind(live_rows, status='live'))
  actual <- list()
  if (!is.null(rows) && nrow(rows)) {
    rows$rank <- rank(-rows$PIR, ties.method='min')
    actual <- lapply(seq_len(nrow(rows)), function(i) list(raw=rows$Player[i], team=rows$Team[i],
      opp=rows$Opponent[i], game=rows$Game[i], pir=rows$PIR[i], min=round(rows$Minutes[i], 1),
      status=rows$status[i], rank=rows$rank[i]))
  }

  # ---- each team's real next game ----
  upcoming <- s[!s$Played & !(s$Game %in% live_ids),,drop=FALSE]
  upcoming <- upcoming[order(upcoming$Utc, suppressWarnings(as.numeric(upcoming$Game))),,drop=FALSE]
  teams <- unique(c(s$Home, s$Away)); teams <- teams[nzchar(teams)]
  nxt <- list()
  for (t in teams) {
    k <- which(upcoming$Home == t | upcoming$Away == t)[1]
    if (is.na(k)) next
    u <- upcoming[k,]
    nxt[[t]] <- list(vs=if (u$Home == t) u$Away else u$Home, venue=if (u$Home == t) 'Home' else 'Away',
                     round=u$Round, date=u$Date,
                     utc=if (!is.na(u$Utc)) format(u$Utc, '%Y-%m-%dT%H:%M:%SZ', tz='UTC') else NULL)
  }
  games <- lapply(seq_len(nrow(rg)), function(i) list(game=rg$Game[i], home=rg$Home[i], away=rg$Away[i],
    utc=if (!is.na(rg$Utc[i])) format(rg$Utc[i], '%Y-%m-%dT%H:%M:%SZ', tz='UTC') else NULL,
    status=if (rg$Played[i]) 'final' else if (rg$Game[i] %in% live_ids) 'live' else 'scheduled',
    hs=if (!is.null(live_score[[rg$Game[i]]])) live_score[[rg$Game[i]]][['hs']] else
       if (rg$Played[i] && is.finite(rg$HomeScore[i])) rg$HomeScore[i] else NULL,
    as=if (!is.null(live_score[[rg$Game[i]]])) live_score[[rg$Game[i]]][['as']] else
       if (rg$Played[i] && is.finite(rg$AwayScore[i])) rg$AwayScore[i] else NULL))

  # full schedule from the current round onwards (for browsing future rounds)
  fut <- s[s$Round >= R,,drop=FALSE]
  fut <- fut[order(fut$Round, fut$Utc, suppressWarnings(as.numeric(fut$Game))),,drop=FALSE]
  schedule_out <- lapply(seq_len(nrow(fut)), function(i) list(game=fut$Game[i], round=fut$Round[i],
    date=fut$Date[i], home=fut$Home[i], away=fut$Away[i],
    utc=if (!is.na(fut$Utc[i])) format(fut$Utc[i], '%Y-%m-%dT%H:%M:%SZ', tz='UTC') else NULL,
    status=if (fut$Played[i]) 'final' else if (fut$Game[i] %in% live_ids) 'live' else 'scheduled',
    hs=if (fut$Played[i] && is.finite(fut$HomeScore[i])) fut$HomeScore[i] else NULL,
    as=if (fut$Played[i] && is.finite(fut$AwayScore[i])) fut$AwayScore[i] else NULL))

  out <- list(round=R, finished_ids=as.list(finished), schedule=schedule_out,
              round_finished=sum(rg$Played), round_games=nrow(rg),
              n_played=if (is.null(rows)) 0L else nrow(rows),
              games=games, actual=actual, next_game=nxt,
              opponents=tables$opponents, league=tables$league,
              debug=unname(as.list(.el_dbg)))
  body <- jsonlite::toJSON(out, auto_unbox=TRUE, null='null', na='null', digits=NA)
  prev_body <- if (length(prev)) { prev$updated_utc <- NULL
    jsonlite::toJSON(prev, auto_unbox=TRUE, null='null', na='null', digits=NA) } else ''
  if (identical(as.character(body), as.character(prev_body))) {
    message('Live: no change'); return(invisible(FALSE))
  }
  out <- c(list(updated_utc=format(now, '%Y-%m-%dT%H:%M:%SZ', tz='UTC')), out)
  writeLines(jsonlite::toJSON(out, auto_unbox=TRUE, null='null', na='null', digits=NA),
             live_file, useBytes=TRUE)
  message('Live: round ', R, ' - ', sum(rg$Played), '/', nrow(rg), ' finished, ',
          length(intersect(live_ids, rg$Game)), ' live, ', length(actual), ' player lines')
  invisible(TRUE)
}
