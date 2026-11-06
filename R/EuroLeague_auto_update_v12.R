# EuroLeague v12 (FANTASY POINTS) - AUTOMATIC ROUND UPDATE (no training)
#
# Run this every hour, and every 10 minutes during game hours. It:
#   1. downloads only the 2026/27 schedule (one light request);
#   2. finds the latest round whose games are ALL played;
#   3. if that round ended >= 12 hours ago and was not published yet,
#      runs run_euroleague_next_round() (prediction only, uses the saved .rds)
#      and rebuilds the web page;
#   4. on EVERY run updates live.json (finished games of the current round, games in
#      progress, opponent-by-position table, each team's next game) - see EuroLeague_live_v12.R.
#
#   Rscript EuroLeague_auto_update_v12.R            # normal (only when due)
#   Rscript EuroLeague_auto_update_v12.R --force    # run now regardless
#
# Paths (optional environment variables; defaults = repository layout):
#   EL_CODE_DIR    folder with the R files            (default: this script's folder)
#   EL_OUTPUT_DIR  folder with the .rds + outputs     (default: <code>/../output)
#   EL_SITE_DIR    folder for index.html / data.json  (default: <code>/../docs)
#   EL_DELAY_HOURS hours to wait after the round ends (default: 12)
#   EL_GAME_HOURS  assumed game length from tip-off   (default: 2.5)

EL_SEASON <- 'E2026'

el_script_dir <- function() {
  f <- grep('^--file=', commandArgs(FALSE), value=TRUE)
  if (length(f)) return(dirname(normalizePath(sub('^--file=', '', f[1]))))
  if (!is.null(sys.frames()[[1]]$ofile)) return(dirname(normalizePath(sys.frames()[[1]]$ofile)))
  getwd()
}

el_schedule <- function(season=EL_SEASON) {
  bases <- c(paste0('https://api-live.euroleague.net/v2/competitions/E/seasons/', season),
             paste0('https://feeds.incrowdsports.com/provider/euroleague-feeds/v2/competitions/E/seasons/', season))
  for (b in bases) for (attempt in 1:3) {
    r <- tryCatch(httr::GET(paste0(b, '/games'), query=list(limit=500, offset=0),
                            httr::user_agent('EuroLeague-R-Boxscore/2.0'),
                            httr::add_headers(Accept='application/json'), httr::timeout(30)),
                  error=identity)
    if (!inherits(r, 'error') && httr::status_code(r) == 200) {
      x <- tryCatch(jsonlite::fromJSON(httr::content(r, 'text', encoding='UTF-8'),
                                       simplifyVector=FALSE), error=identity)
      if (!inherits(x, 'error')) return(el_parse_schedule(x))
    }
    Sys.sleep(2*attempt)
  }
  stop('Could not download the EuroLeague schedule')
}

el_parse_schedule <- function(x) {
  # Same field conventions as build_euroleague_2026_27().
  val <- function(x, keys) { if (!is.list(x)) return(NULL)
    for (k in keys) if (!is.null(x[[k]]) && length(x[[k]])) return(x[[k]]); NULL }
  str1 <- function(x) {
    if (is.null(x) || !length(x)) return('')
    if (!is.list(x)) return(as.character(x[1]))
    v <- val(x, c('fullName','name','officialName','clubPermanentName','displayName',
                  'shortName','personName','positionName','description','code'))
    if (!is.null(v)) return(str1(v)); ''
  }
  number <- function(x) {
    if (is.null(x) || !length(x)) return(NA_real_)
    if (is.list(x)) return(number(val(x, c('value','total','seconds'))))
    suppressWarnings(as.numeric(x[1]))
  }
  club <- function(g, side) {
    keys <- if (side == 'local') c('local','home','localClub','homeClub') else c('road','away','roadClub','awayClub')
    z <- val(g, keys); str1(val(z, c('club','team')) %||% z)
  }
  `%||%` <- function(a, b) if (is.null(a)) b else a
  played <- function(g) {
    st <- tolower(str1(val(g, c('status','gameStatus'))))
    isTRUE(val(g, c('played','isPlayed'))) || st %in% c('played','finished','final','closed')
  }
  games <- if (!is.null(x$data)) x$data else if (!is.null(x$items)) x$items else x
  if (!length(games)) stop('Empty schedule')
  do.call(rbind, lapply(games, function(g) {
    utc_raw <- str1(val(g, c('utcDate')))
    utc <- as.POSIXct(substr(utc_raw, 1, 19), format='%Y-%m-%dT%H:%M:%S', tz='UTC')
    raw_date <- str1(val(g, c('utcDate','date','localDate','confirmedDate')))
    d <- regmatches(raw_date, regexpr('[0-9]{4}-[0-9]{2}-[0-9]{2}', raw_date))
    if (is.na(utc) && length(d))   # date only: assume a late (22:00 Israel) game
      utc <- as.POSIXct(paste(d, '22:00:00'), tz='Asia/Jerusalem')
    side_score <- function(keys, flat) {
      z <- number(val(g, flat)); if (is.finite(z)) return(z)
      number(val(val(g, keys), c('score','points')))
    }
    data.frame(Game=str1(val(g, c('gameCode','gamecode','game_code'))),
               Round=number(val(g, c('round','roundNumber'))),
               Date=if (length(d)) d else '', Utc=utc,
               Home=club(g, 'local'), Away=club(g, 'road'),
               Played=played(g),
               HomeScore=side_score(c('local','home'), c('localScore','homeScore')),
               AwayScore=side_score(c('road','away'), c('roadScore','awayScore')),
               stringsAsFactors=FALSE)
  }))
}

# Returns list(due, round, ended_utc, due_utc, reason)
el_round_status <- function(schedule, last_published, delay_hours=12, game_hours=2.5,
                            now=Sys.time()) {
  s <- schedule[is.finite(schedule$Round) & schedule$Round >= 1 & nzchar(schedule$Game),,drop=FALSE]
  done <- tapply(s$Played, s$Round, all)
  complete <- sort(as.numeric(names(done)[done]))
  if (!length(complete)) return(list(due=FALSE, round=NA, reason='No completed round yet'))
  r <- max(complete)
  last_tip <- max(s$Utc[s$Round == r], na.rm=TRUE)
  ended <- last_tip + game_hours*3600
  due_at <- ended + delay_hours*3600
  fmt <- function(t) format(t, '%Y-%m-%d %H:%M', tz='Asia/Jerusalem')
  if (is.finite(last_published) && r <= last_published)
    return(list(due=FALSE, round=r, ended_utc=ended, due_utc=due_at,
                reason=paste0('Round ', r, ' already published')))
  if (now < due_at)
    return(list(due=FALSE, round=r, ended_utc=ended, due_utc=due_at,
                reason=paste0('Round ', r, ' ended ~', fmt(ended), ' (Israel); update due at ', fmt(due_at))))
  list(due=TRUE, round=r, ended_utc=ended, due_utc=due_at,
       reason=paste0('Round ', r, ' ended ~', fmt(ended), ' (Israel) - updating'))
}

el_auto_update <- function(force=FALSE) {
  for (pkg in c('httr','jsonlite','openxlsx'))
    if (!requireNamespace(pkg, quietly=TRUE)) install.packages(pkg, repos='https://cloud.r-project.org')
  code_dir <- Sys.getenv('EL_CODE_DIR', el_script_dir())
  output_dir <- Sys.getenv('EL_OUTPUT_DIR', file.path(code_dir, '..', 'output'))
  site_dir <- Sys.getenv('EL_SITE_DIR', file.path(code_dir, '..', 'docs'))
  delay <- as.numeric(Sys.getenv('EL_DELAY_HOURS', '12'))
  game_hours <- as.numeric(Sys.getenv('EL_GAME_HOURS', '2.5'))
  template <- file.path(code_dir, '..', 'web', 'template.html')
  if (!file.exists(template)) template <- file.path(code_dir, 'template.html')
  state_file <- file.path(site_dir, 'state.json')
  state <- if (file.exists(state_file)) jsonlite::fromJSON(state_file) else list(last_round=NA)
  last <- suppressWarnings(as.numeric(state$last_round))
  if (!length(last)) last <- NA_real_

  schedule <- el_schedule()
  st <- el_round_status(schedule, last, delay, game_hours)
  message(st$reason)
  source(file.path(code_dir, 'EuroLeague_live_v12.R'))
  run_live <- function() tryCatch(el_live_update(schedule, site_dir, code_dir),
                                  error=function(e) message('Live update failed: ', conditionMessage(e)))
  if (!st$due && !force) { run_live(); return(invisible(FALSE)) }
  if (force) message('--force: running now')

  source(file.path(code_dir, 'EuroLeague_ranking_models_v12.R'))
  source(file.path(code_dir, 'EuroLeague_run_next_round_v12.R'))
  source(file.path(code_dir, 'EuroLeague_web_export_v12.R'))

  run_euroleague_next_round(output_dir)    # prediction only - NO training

  # Make sure the finished round was really parsed (box scores sometimes lag).
  dbg <- readLines(file.path(output_dir, 'EuroLeague_2026-27_debug.txt'), warn=FALSE)
  line <- grep('^Fully completed rounds with parsed box scores:', dbg, value=TRUE)
  parsed <- suppressWarnings(as.numeric(strsplit(sub('.*:', '', line[1]), ',')[[1]]))
  round_ok <- is.finite(st$round) && st$round %in% parsed
  late <- !is.null(st$due_utc) && Sys.time() > st$due_utc + 24*3600
  if (!round_ok && !late && !force) {
    message('Round ', st$round, ' box scores not complete in the API yet - will retry next run')
    run_live(); return(invisible(FALSE))
  }
  export_euroleague_web(output_dir, site_dir, template, schedule=schedule,
                        extra_meta=list(last_complete_round=if (round_ok) st$round else max(parsed, na.rm=TRUE)))
  jsonlite::write_json(list(last_round=if (is.finite(st$round)) st$round else last,
                            updated_utc=format(Sys.time(), '%Y-%m-%dT%H:%M:%SZ', tz='UTC')),
                       state_file, auto_unbox=TRUE, pretty=TRUE)
  run_live()
  invisible(TRUE)
}

if (sys.nframe() == 0L) {
  el_auto_update(force='--force' %in% commandArgs(TRUE))
}
