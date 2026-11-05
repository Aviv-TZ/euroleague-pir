# EuroLeague v11 - WEB EXPORT
# Reads the files produced by run_euroleague_next_round() (NO training, NO API calls)
# and writes the interactive page: <site_dir>/index.html (self-contained) + data.json.
#
#   source('EuroLeague_web_export_v11.R')
#   export_euroleague_web(output_dir, site_dir, template_file)
#
# Sheets are read by POSITION (not by Hebrew name) so this works in any R locale.
# Positions follow the `sheets` vector in build_euroleague_2026_27().

EL_SHEET <- list(all_candidates=8, player_avgs=10, next_fixtures=11, backtest=15)

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

export_euroleague_web <- function(output_dir, site_dir, template_file,
                                  schedule=NULL, extra_meta=list(),
                                  trained_model_file=NULL) {
  for (pkg in c('jsonlite','openxlsx'))
    if (!requireNamespace(pkg, quietly=TRUE)) install.packages(pkg, repos='https://cloud.r-project.org')
  if (is.null(trained_model_file))
    trained_model_file <- file.path(output_dir, 'EuroLeague_historical_models_2024_2026_v11.rds')
  xlsx <- file.path(output_dir, 'EuroLeague_2026-27_PIR_real_data.xlsx')
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

  now <- Sys.time()
  meta <- c(list(
    generated_utc=format(now, '%Y-%m-%dT%H:%M:%SZ', tz='UTC'),
    season='2026/27', n_ranked=nrow(cand), n_players=nrow(avgs),
    weights=list(m1=as.list(W[[1]]), m2=as.list(W[[2]])),
    calib=lapply(calib, function(c) lapply(c, function(v) if (is.finite(v)) round(v,4) else NULL)),
    backtest=backtest), extra_meta)

  data <- list(meta=meta, fixtures=fixtures, players=players)
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
