# EuroLeague v12 (FANTASY POINTS) - WEB EXPORT
# Reads the files produced by run_euroleague_next_round() (NO training, NO API calls)
# and writes the interactive page: <site_dir>/index.html (self-contained) + data.json.
#
#   source('EuroLeague_web_export_v12.R')
#   export_euroleague_web(output_dir, site_dir, template_file)
#
# Sheets are read by POSITION (not by Hebrew name) so this works in any R locale.
# Positions follow the `sheets` vector in build_euroleague_2026_27().

EL_SHEET <- list(opp_season=1, opp_last5=2, all_candidates=8, player_avgs=10, next_fixtures=11, backtest=15)

el_web_format_name <- function(x) {
  # "VEZENKOV, SASHA" -> "Sasha Vezenkov"
  cap <- function(s) {
    s <- tolower(trimws(s))
    gsub("(^|[ \\-'.])([a-z])", "\\1\\U\\2", s, perl=TRUE)
  }
  vapply(x, function(n) {
    p <- strsplit(n, ',', fixed=TRUE)[[1]]
    if (length(p) >= 2) paste(cap(paste(p[-1], collapse=' ')), cap(p[1])) else cap(n)
  }, character(1), USE.NAMES=FALSE)
}

el_web_components <- function(d, w) {
  # Exact re-implementation of predict_score(), split into its components.
  sign <- ifelse(d$Venue == 'Home', 1, -1)
  base <- (1-w[['PlayerLast5Weight']])*d$AvgPIR + w[['PlayerLast5Weight']]*d$Last5AvgPIR
  mraw <- (1-w[['OpponentLast5Weight']])*d$MatchupSeasonFactor +
          w[['OpponentLast5Weight']]*d$MatchupLast5Factor
  matchup <- pmax(.10, 1 + w[['OpponentStrength']]*(mraw-1))
  mcomb <- (1-w[['Last5MinutesWeight']])*d$MinutesRatio + w[['Last5MinutesWeight']]*d$Last5MinutesRatio
  minf <- 1 - w[['MinutesPenalty']]*(1-mcomb)
  home <- exp(w[['HomeAdvantage']]*sign)
  data.frame(base=base, mraw=mraw, matchup=matchup, mcomb=mcomb, minf=minf,
             home=home, score=base*matchup*minf*home)
}

el_web_calibration <- function(hist, score_col) {
  # Maps a model Score to an expected PIR (given the player plays >= 1 minute),
  # fitted on the 2026/27 out-of-sample backtest rows. Refreshed every round.
  ok <- hist$CleanEligible %in% c(TRUE, 'TRUE') & is.finite(hist$PIR) & is.finite(hist[[score_col]])
  h <- hist[ok,,drop=FALSE]
  if (nrow(h) < 50) return(list(a=0, b=1, lo=NA_real_, hi=NA_real_, n=nrow(h), r2=NA_real_))
  m <- stats::lm(h$PIR ~ h[[score_col]])
  r <- stats::resid(m)
  list(a=unname(coef(m)[1]), b=unname(coef(m)[2]),
       lo=unname(stats::quantile(r, .10)), hi=unname(stats::quantile(r, .90)),
       n=nrow(h), r2=summary(m)$r.squared)
}

# ---- sport5 fantasy prices --------------------------------------------------
# Players are matched to EuroLeague by club + shirt number (validated: ~95% matched;
# the rest are fringe players missing from the EuroLeague roster).
EL_S5_TEAMS <- c('אנדולו אפס'='IST','באסקוניה'='BAS','הכוכב האדום'='RED','דובאי'='DUB','מילאנו'='MIL',
  'ברצלונה'='BAR','באיירן מינכן'='MUN',"פנרבחצ'ה"='ULK','הפועל ת"א'='HTA','וילרבאן'='ASV',
  'מכבי ת"א'='TEL','אולימפיאקוס'='OLY','פנאתינייקוס'='PAN','פריז באסקט'='PRS','פרטיזן בלגרד'='PAR',
  'ריאל מדריד'='MAD','ולנסיה'='PAM','וירטוס בולוניה'='VIR',"ז'לגיריס קובנה"='ZAL','בשיקטש'='BES')
el_web_prices <- function(season='E2026') {
  get <- function(u) { r <- tryCatch(httr::GET(u, httr::timeout(40), httr::user_agent('EuroLeague-R/2.0')), error=identity)
    if (inherits(r,'error') || httr::status_code(r) != 200) return(NULL)
    tryCatch(jsonlite::fromJSON(httr::content(r,'text',encoding='UTF-8'), simplifyVector=FALSE), error=function(e) NULL) }
  lg <- get('https://dreamteam.sport5.co.il/api/Leagues/GetLeagues')
  sid <- NULL
  for (l in lg$data) if (identical(l$urlName, 'fantasyeuroleague')) sid <- l$currentSeasonId
  if (is.null(sid)) { message('Prices: fantasy league not found'); return(NULL) }
  tp <- get(paste0('https://dreamteam.sport5.co.il/api/Players/GetTeamsAndPlayers?seasonId=', sid))
  pe <- get(paste0('https://api-live.euroleague.net/v2/competitions/E/seasons/', season, '/people?limit=2000&offset=0'))
  if (is.null(tp) || is.null(pe)) { message('Prices: download failed'); return(NULL) }
  pe <- if (!is.null(pe$data)) pe$data else pe
  key <- function(code, dorsal) paste(code, trimws(as.character(dorsal)), sep='#')
  pl <- Filter(function(p) identical(p$type, 'J'), pe)
  name_by <- setNames(vapply(pl, function(p) p$person$name, ''), vapply(pl, function(p) key(p$club$code, p$dorsal), ''))
  club_by <- setNames(vapply(pe, function(p) p$club$name, ''), vapply(pe, function(p) p$club$code, ''))
  coach_by <- unlist(lapply(Filter(function(p) identical(p$type, 'E'), pe), function(p) setNames(p$person$name, p$club$code)))
  out <- list()
  for (t in tp$data) {
    code <- unname(EL_S5_TEAMS[enc2utf8(t$name)]); if (!length(code) || is.na(code)) { message('Prices: unknown team ', t$name); next }
    for (p in t$players) {
      coach <- identical(as.integer(p$position), 4L)
      raw <- if (coach) unname(coach_by[code]) else unname(name_by[key(code, p$shirtNumber)])
      out[[length(out)+1L]] <- data.frame(raw=if (length(raw) && !is.na(raw)) raw else NA_character_,
        team=unname(club_by[code]), he=p$name, price=p$price/1e6,
        s5pos=c('Guard','Forward','Center','Coach')[as.integer(p$position)],
        injured=isTRUE(p$injuredStatus > 0) || isTRUE(p$missingStatus > 0), stringsAsFactors=FALSE)
    }
  }
  res <- do.call(rbind, out)
  message('Prices: ', nrow(res), ' entries, ', sum(!is.na(res$raw)), ' matched to EuroLeague')
  # EuroLeague headshots for every rostered player (used by the court view and the comparison)
  img <- function(p) { u <- p$images$headshot; if (is.null(u)) u <- p$person$images$headshot; if (is.null(u)) NA_character_ else u }
  attr(res, 'images') <- data.frame(raw=vapply(pl, function(p) p$person$name, ''),
    team=vapply(pl, function(p) unname(club_by[p$club$code]), ''), img=vapply(pl, img, ''), stringsAsFactors=FALSE)
  res
}

# Hits per round: predicted top K (model) vs actual top K among ALL players who played that round.
el_web_round_hits <- function(hist, raw) {
  if (!nrow(hist) || !nrow(raw)) return(list())
  game_round <- unique(hist[,c('Game','Round')])
  raw$Round <- game_round$Round[match(as.character(raw$Game), as.character(game_round$Game))]
  lapply(sort(unique(hist$Round)), function(r) {
    h <- hist[hist$Round == r,,drop=FALSE]; a <- raw[raw$Round %in% r,,drop=FALSE]
    a <- a[order(-a$PIR),,drop=FALSE]
    act_key <- paste(a$Player, a$Team)
    hits <- function(score_col) setNames(lapply(c(10,30,50), function(k) {
      th <- a$PIR[min(k, nrow(a))]
      top_act <- act_key[a$PIR >= th]
      top_pred <- head(paste(h$Player, h$Team)[order(-h[[score_col]], -h$AvgMinutes)], k)
      sum(top_pred %in% top_act) }), c('t10','t30','t50'))
    list(round=r, n_played=nrow(a), m1=hits('FixedScore'), m2=hits('WalkForwardScore'))
  })
}

export_euroleague_web <- function(output_dir, site_dir, template_file,
                                  schedule=NULL, extra_meta=list(),
                                  trained_model_file=NULL) {
  for (pkg in c('jsonlite','openxlsx'))
    if (!requireNamespace(pkg, quietly=TRUE)) install.packages(pkg, repos='https://cloud.r-project.org')
  if (is.null(trained_model_file))
    trained_model_file <- file.path(output_dir, 'EuroLeague_historical_models_2024_2026_v12_fantasy.rds')
  xlsx <- file.path(output_dir, 'EuroLeague_2026-27_FP_real_data.xlsx')
  if (!file.exists(xlsx)) stop('Missing ', xlsx, ' - run run_euroleague_next_round() first')
  dir.create(site_dir, recursive=TRUE, showWarnings=FALSE)

  artifact <- readRDS(trained_model_file)
  W <- list(artifact$weights, artifact$walk_weights)

  read_sheet <- function(i) tryCatch(openxlsx::read.xlsx(xlsx, i), error=function(e) data.frame())
  cand <- read_sheet(EL_SHEET$all_candidates)
  avgs <- read_sheet(EL_SHEET$player_avgs)
  fixt <- read_sheet(EL_SHEET$next_fixtures)
  hist <- read_sheet(EL_SHEET$backtest)
  if (!nrow(avgs)) stop('Player averages sheet is empty')
  if (nrow(cand) && !all(c('Score','Score_WalkForward','MatchupSeasonFactor') %in% names(cand)))
    stop('Unexpected candidates sheet layout - sheet order changed?')

  # ---- model components, ranks and PIR predictions --------------------------
  calib <- list(m1=el_web_calibration(hist, 'FixedScore'),
                m2=el_web_calibration(hist, 'WalkForwardScore'))
  model_rows <- list()
  if (nrow(cand)) {
    for (k in 1:2) {
      comp <- el_web_components(cand, W[[k]])
      target <- if (k == 1) cand$Score else cand$Score_WalkForward
      if (max(abs(comp$score - target)) > 1e-6)
        warning('Model ', k, ' component re-computation differs from R output (max diff ',
                signif(max(abs(comp$score-target)),3), '); using R output score.')
      comp$score <- target
      ord <- order(-comp$score, -cand$AvgMinutes, cand$Player)
      comp$rank <- integer(nrow(cand)); comp$rank[ord] <- seq_along(ord)
      cb <- calib[[k]]
      comp$pir <- cb$a + cb$b*comp$score
      comp$pir_lo <- comp$pir + cb$lo
      comp$pir_hi <- comp$pir + cb$hi
      model_rows[[k]] <- comp
    }
  }
  cand_key <- paste(cand$Player, cand$Team, sep='||')

  # ---- next fixtures per team (also for players who are not ranked) ---------
  fx_by_team <- if (nrow(fixt)) split(fixt, fixt$Team) else list()

  pos_he <- c(Guard='גארד', Forward='פורוורד', Center='סנטר')
  num <- function(x, d=3) if (is.null(x) || !length(x) || !is.finite(x)) NULL else round(x, d)

  prices <- tryCatch(el_web_prices(), error=function(e) { message('Prices failed: ', conditionMessage(e)); NULL })
  # Did the player appear (>0 minutes) in any of his team's last 3 games?
  raw_all <- tryCatch(utils::read.csv(file.path(output_dir, 'EuroLeague_2026-27_players.csv'),
                                      fileEncoding='UTF-8', stringsAsFactors=FALSE), error=function(e) NULL)
  last3_played <- function(player, team) {
    if (is.null(raw_all)) return(NA)
    g <- unique(raw_all$Game[raw_all$Team == team])
    ord <- if (!is.null(schedule)) order(schedule$Utc[match(as.character(g), schedule$Game)], decreasing=TRUE)
           else order(suppressWarnings(as.numeric(g)), decreasing=TRUE)
    last3 <- head(g[ord], 3)
    any(raw_all$Player == player & raw_all$Team == team & raw_all$Game %in% last3)
  }
  players <- vector('list', nrow(avgs))
  for (i in seq_len(nrow(avgs))) {
    a <- avgs[i,]
    key <- paste(a$Player, a$Team, sep='||')
    j <- match(key, cand_key)
    fx <- fx_by_team[[a$Team]]
    p <- list(id=i, raw=a$Player, name=el_web_format_name(a$Player), team=a$Team,
              pos=a$Position, pos_he=unname(pos_he[a$Position]),
              games=a$Games, avg=num(a$AvgPIR,2), l5=num(a$Last5AvgPIR,2), l5n=a$Last5GamesUsed,
              min=num(a$AvgMinutes,1), l5min=num(a$Last5AvgMinutes,1),
              opp=if (!is.null(fx)) fx$Opponent[1] else NULL,
              venue=if (!is.null(fx)) fx$Venue[1] else NULL,
              date=if (!is.null(fx)) as.character(fx$Date[1]) else NULL,
              ranked=!is.na(j))
    if (isFALSE(last3_played(a$Player, a$Team))) p$out3 <- TRUE
    if (!is.null(prices)) {
      q <- which(prices$raw == a$Player & prices$team == a$Team)[1]
      if (is.na(q)) q <- which(prices$raw == a$Player)[1]
      if (!is.na(q)) { p$price <- prices$price[q]; p$he <- prices$he[q]; p$s5pos <- prices$s5pos[q]
                       p$injured <- prices$injured[q] }
      im <- attr(prices, 'images')
      q2 <- which(im$raw == a$Player & im$team == a$Team)[1]; if (is.na(q2)) q2 <- which(im$raw == a$Player)[1]
      if (!is.na(q2) && !is.na(im$img[q2])) p$img <- im$img[q2]
    }
    if (!is.na(j)) {
      c1 <- cand[j,]
      p$inputs <- list(msf=num(c1$MatchupSeasonFactor,4), ml5f=num(c1$MatchupLast5Factor,4),
                       mr=num(c1$MinutesRatio,4), l5mr=num(c1$Last5MinutesRatio,4))
      p$m <- lapply(1:2, function(k) {
        r <- model_rows[[k]][j,]
        list(score=num(r$score), rank=r$rank, pir=num(r$pir,1),
             lo=num(r$pir_lo,1), hi=num(r$pir_hi,1),
             base=num(r$base), mraw=num(r$mraw,4), matchup=num(r$matchup,4),
             mcomb=num(r$mcomb,4), minf=num(r$minf,4), home=num(r$home,4))
      })
    } else {
      p$reason <- if (a$Games < 5) 'פחות מ-5 הופעות בקבוצה הנוכחית' else
                  if (is.finite(a$AvgMinutes) && a$AvgMinutes < 10) 'ממוצע דקות נמוך מ-10' else
                  if (is.null(fx)) 'לקבוצה אין משחק במחזור הקרוב' else
                  'לא עומד בתנאי הזכאות של המודל'
    }
    players[[i]] <- p
  }

  # ---- fixtures of the coming round ----------------------------------------
  fixtures <- list()
  if (!is.null(schedule) && nrow(schedule) && nrow(fixt)) {
    rnd <- unique(schedule$Round[schedule$Game %in% as.character(fixt$Game)])
    rnd <- rnd[is.finite(rnd)]
    if (length(rnd)) {
      s <- schedule[schedule$Round == min(rnd),,drop=FALSE]
      s <- s[order(s$Utc, suppressWarnings(as.numeric(s$Game))),,drop=FALSE]
      fixtures <- lapply(seq_len(nrow(s)), function(i) list(
        game=s$Game[i], round=s$Round[i], date=s$Date[i],
        utc=if (!is.na(s$Utc[i])) format(s$Utc[i], '%Y-%m-%dT%H:%M:%SZ', tz='UTC') else NULL,
        home=s$Home[i], away=s$Away[i], played=isTRUE(s$Played[i])))
      extra_meta$next_round <- min(rnd)
    }
  }
  if (!length(fixtures) && nrow(fixt)) {
    h <- fixt[fixt$Venue == 'Home',,drop=FALSE]
    h <- h[order(h$Date, suppressWarnings(as.numeric(h$Game))),,drop=FALSE]
    fixtures <- lapply(seq_len(nrow(h)), function(i) list(
      game=as.character(h$Game[i]), date=as.character(h$Date[i]),
      home=h$Team[i], away=h$Opponent[i], played=FALSE))
  }

  # ---- backtest summary (2026/27 out-of-sample, conditional on playing) -----
  backtest <- list()
  rc_file <- file.path(output_dir, 'EuroLeague_2026-27_round_comparison.csv')
  if (file.exists(rc_file)) {
    rc <- utils::read.csv(rc_file, fileEncoding='UTF-8', stringsAsFactors=FALSE)
    rc <- rc[rc$EvaluationType == 'Conditional_1min_played',,drop=FALSE]
    for (k in 1:2) {
      z <- rc[rc$Model == c('Model1_Frozen','Model2_FrozenRecency')[k],,drop=FALSE]
      if (nrow(z)) backtest[[paste0('m',k)]] <- list(
        rounds=paste(range(z$Round), collapse='-'), n_rounds=nrow(z),
        top10=num(mean(z$Top10Hits),2), top30=num(mean(z$Top30Hits),2),
        ndcg=num(mean(z$NDCG10),3), spearman=num(mean(z$Spearman),3))
    }
  }

  # ---- opponent PIR allowed by position (season + last 5 games) -------------
  opp_s <- read_sheet(EL_SHEET$opp_season)
  opp_r <- read_sheet(EL_SHEET$opp_last5)
  pos3 <- c('Guard','Forward','Center')
  opponents <- list(); league <- list()
  if (nrow(opp_s)) {
    names(opp_s)[1] <- 'Team'; if (nrow(opp_r)) names(opp_r)[1] <- 'Team'
    opponents <- lapply(seq_len(nrow(opp_s)), function(i) {
      t <- opp_s$Team[i]; r <- if (nrow(opp_r)) opp_r[opp_r$Team == t,,drop=FALSE] else opp_r
      fx <- fx_by_team[[t]]
      list(team=t,
           season=setNames(lapply(pos3, function(p) num(opp_s[[p]][i], 3)), pos3),
           last5=setNames(lapply(pos3, function(p) if (nrow(r)) num(r[[p]][1], 3) else NULL), pos3),
           next_vs=if (!is.null(fx)) fx$Opponent[1] else NULL,
           next_venue=if (!is.null(fx)) fx$Venue[1] else NULL)
    })
  }
  raw_csv <- file.path(output_dir, 'EuroLeague_2026-27_players.csv')
  if (file.exists(raw_csv)) {
    raw <- utils::read.csv(raw_csv, fileEncoding='UTF-8', stringsAsFactors=FALSE)
    league <- setNames(lapply(pos3, function(p) {
      d <- raw[raw$Position == p,,drop=FALSE]
      if (nrow(d)) num(sum(d$PIR_x_Minutes)/sum(d$Minutes), 3) else NULL }), pos3)
  }

  now <- Sys.time()
  meta <- c(list(
    generated_utc=format(now, '%Y-%m-%dT%H:%M:%SZ', tz='UTC'),
    season='2026/27', n_ranked=nrow(cand), n_players=nrow(avgs),
    weights=list(m1=as.list(W[[1]]), m2=as.list(W[[2]])),
    calib=lapply(calib, function(c) lapply(c, function(v) if (is.finite(v)) round(v,4) else NULL)),
    backtest=backtest), extra_meta)

  raw_csv2 <- file.path(output_dir, 'EuroLeague_2026-27_players.csv')
  round_hits <- if (file.exists(raw_csv2)) el_web_round_hits(hist,
    utils::read.csv(raw_csv2, fileEncoding='UTF-8', stringsAsFactors=FALSE)) else list()
  coaches <- if (!is.null(prices)) { cp <- prices[prices$s5pos == 'Coach',,drop=FALSE]
    lapply(seq_len(nrow(cp)), function(i) list(raw=cp$raw[i], team=cp$team[i], he=cp$he[i], price=cp$price[i])) } else list()
  data <- list(meta=meta, fixtures=fixtures, players=players, round_hits=round_hits, coaches=coaches,
               opponents=opponents, league=league)
  json <- jsonlite::toJSON(data, auto_unbox=TRUE, null='null', na='null', digits=NA, pretty=FALSE)
  writeLines(json, file.path(site_dir, 'data.json'), useBytes=TRUE)

  tpl <- paste(readLines(template_file, encoding='UTF-8', warn=FALSE), collapse='\n')
  safe <- gsub('</', '<\\/', json, fixed=TRUE)
  parts <- strsplit(tpl, '/*__EL_DATA__*/null', fixed=TRUE)[[1]]
  if (length(parts) != 2) stop('Template placeholder /*__EL_DATA__*/null not found exactly once')
  html <- paste0(parts[1], safe, parts[2])
  con <- file(file.path(site_dir, 'index.html'), open='w', encoding='UTF-8')
  writeLines(html, con, useBytes=TRUE); close(con)
  message('Web page written: ', file.path(site_dir, 'index.html'),
          ' (', length(players), ' players, ', nrow(cand), ' ranked, ', length(fixtures), ' fixtures)')
  invisible(data)
}
