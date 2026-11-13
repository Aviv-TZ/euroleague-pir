# EuroLeague v12 (FANTASY): identical to v11, but the TARGET is sport5 fantasy points instead of PIR.
# The column named `PIR` holds the target metric (fantasy points); the official EuroLeague
# PIR (valuation) is kept in `EL_PIR`. Everything else (features, cleaning, loss, models) is unchanged.
# Fantasy rules (sport5 EuroLeague): point +1, rebound +2, assist +2, steal +3, block +3,
# foul drawn +1, turnover -2, missed shot (2pt, 3pt and free throw) -1, foul committed -1,
# double-double +10, triple-double +20 (instead of the double-double bonus). Categories for
# doubles: points, rebounds, assists, steals, blocks.
el_fantasy_points <- function(pts, reb, ast, stl, blk, tov, fd, fc, miss) {
  z <- function(x) ifelse(is.finite(x), x, 0)
  pts <- z(pts); reb <- z(reb); ast <- z(ast); stl <- z(stl); blk <- z(blk)
  tens <- (pts>=10) + (reb>=10) + (ast>=10) + (stl>=10) + (blk>=10)
  pts + 2*reb + 2*ast + 3*stl + 3*blk + z(fd) - 2*z(tov) - z(miss) - z(fc) +
    ifelse(tens>=3, 20, ifelse(tens>=2, 10, 0))
}
# EuroLeague v11: two ranking-first models, 5-match windows, venue, cleaning; NO team-form.
# EuroLeague PIR: ranking-first models with round-based chronological learning.
# Historical features are built by competition ROUND; no current/future round enters training.
el_fit_historical_weights <- function(raw, meta, log_it=function(...) invisible(NULL),
                                       complete_rounds=NULL, historical_features=NULL, features_only=FALSE) {
  # Both models minimize ranking-position error, never numerical PIR prediction error.
  defaults <- c(PlayerLast5Weight=.40,OpponentLast5Weight=.60,
                OpponentStrength=1,MinutesPenalty=.40,Last5MinutesWeight=.50,HomeAdvantage=0)
  predict_score <- function(d,w) {
    base <- (1-w['PlayerLast5Weight'])*d$AvgPIR + w['PlayerLast5Weight']*d$Last5AvgPIR
    matchup <- (1-w['OpponentLast5Weight'])*d$MatchupSeasonFactor +
      w['OpponentLast5Weight']*d$MatchupLast5Factor
    matchup <- pmax(.10,1+w['OpponentStrength']*(matchup-1))
    minute_ratio <- (1-w['Last5MinutesWeight'])*d$MinutesRatio +
      w['Last5MinutesWeight']*d$Last5MinutesRatio
    as.numeric(base*matchup*(1-w['MinutesPenalty']*(1-minute_ratio))*
      exp(w['HomeAdvantage']*d$HomeSign))
  }
  empty <- data.frame()
  fallback <- function(reason,hist=empty,checks=empty) {
    if (!ncol(checks)) checks <- data.frame(Metric='Model status',Value=reason)
    log_it('Ranking optimization: default weights retained - ',reason)
    list(weights=defaults,walk_weights=defaults,history=hist,checks=checks,
         round_metrics=data.frame(),error_summary=data.frame(),walk_coefficients=data.frame(),status=reason,
         predict=predict_score)
  }
  if (is.null(historical_features)) {
  meta <- meta[!duplicated(meta$Game),c('Game','GameDate','Home','Away','Round'),drop=FALSE]
  meta <- meta[is.finite(meta$Round) & meta$Round>=1,,drop=FALSE]
  if(!is.null(complete_rounds))
    meta <- meta[meta$Round %in% complete_rounds,,drop=FALSE]
  meta <- meta[grepl('^[0-9]{4}-[0-9]{2}-[0-9]{2}$',meta$GameDate) &
                 meta$GameDate!='0000-00-00',,drop=FALSE]
  meta <- meta[order(meta$GameDate,suppressWarnings(as.numeric(meta$Game))),,drop=FALSE]
  raw <- merge(raw,meta,by='Game',all=FALSE,sort=FALSE)
  if (!nrow(raw))
    return(fallback('No dated historical games'))
  raw$PlayerKey <- paste(raw$Team,raw$Player,raw$Position,sep='||')
  raw$GameKey <- paste(raw$Opponent,raw$Game,sep='||')
  history <- list()
  rounds <- sort(unique(raw$Round[raw$Round>=4]))
  # One snapshot per COMPLETE round: no results from the same round are used.
  for (round_id in rounds) {
    current <- raw[raw$Round==round_id,,drop=FALSE]
    # Delayed matches in earlier rounds must also be earlier than this round's first game.
    first_day <- min(current$GameDate)
    prev <- raw[raw$Round<round_id & raw$GameDate<first_day,,drop=FALSE]
    if (!nrow(prev) || !nrow(current)) next
    # Candidate round: both teams must have played at least five games before today.
    team_games <- rbind(unique(prev[,c('Team','Game')]),
                        setNames(unique(prev[,c('Opponent','Game')]),c('Team','Game')))
    team_counts <- table(unique(team_games)$Team)
    # Player historical stats, including exact last five appearances.
    prev <- prev[order(prev$GameDate,suppressWarnings(as.numeric(prev$Game)),decreasing=TRUE),,drop=FALSE]
    player_groups <- split(prev,prev$PlayerKey)
    player_stats <- lapply(player_groups,function(x) {
      x <- x[!duplicated(x$Game),,drop=FALSE]
      c(AvgPIR=mean(x$PIR),Last5AvgPIR=mean(head(x$PIR,5)),
        AvgMinutes=mean(x$Minutes),Last5AvgMinutes=mean(head(x$Minutes,5)),Games=nrow(x))
    })
    stats <- as.data.frame(do.call(rbind,player_stats),stringsAsFactors=FALSE)
    stats$PlayerKey <- rownames(stats)
    eligible <- stats[stats$Games>=5,,drop=FALSE]
    if (!nrow(eligible)) next
    keys <- strsplit(eligible$PlayerKey,'||',fixed=TRUE)
    eligible$Team <- vapply(keys,`[`,character(1),1)
    eligible$Position <- vapply(keys,`[`,character(1),3)
    eligible$TeamPos <- paste(eligible$Team,eligible$Position,sep='||')
    maximum <- tapply(eligible$AvgMinutes,eligible$TeamPos,max)
    eligible$MinutesRatio <- pmin(1,eligible$AvgMinutes / pmax(.01,as.numeric(maximum[eligible$TeamPos])))
    max_recent <- tapply(eligible$Last5AvgMinutes,eligible$TeamPos,max)
    eligible$Last5MinutesRatio <- pmin(1,eligible$Last5AvgMinutes /
      pmax(.01,as.numeric(max_recent[eligible$TeamPos])))
    # League position PIR baseline at that point in time (minutes weighted).
    by_pos <- split(prev,prev$Position)
    league <- vapply(by_pos,function(x) sum(x$PIR_x_Minutes)/sum(x$Minutes),numeric(1))
    # Opponent's previous five games are determined across ALL positions.
    opponent_games <- lapply(split(prev,prev$Opponent),function(x) head(unique(x$Game),5))
    # Opponent allowed PIR calculated from prior games, with last FIVE distinct games.
    by_oppos <- split(prev,paste(prev$Opponent,prev$Position,sep='||'))
    opp_stats <- lapply(by_oppos,function(x) {
      x <- x[order(x$GameDate,suppressWarnings(as.numeric(x$Game)),decreasing=TRUE),,drop=FALSE]
      last_ids <- opponent_games[[x$Opponent[1]]]
      recent <- x[x$Game %in% last_ids,,drop=FALSE]
      if (!nrow(recent)) recent <- x
      c(Season=sum(x$PIR_x_Minutes)/sum(x$Minutes),
        Recent=sum(recent$PIR_x_Minutes)/sum(recent$Minutes),
        Games=length(unique(x$Game)))
    })
    if (!length(opp_stats)) next
    op <- as.data.frame(do.call(rbind,opp_stats),stringsAsFactors=FALSE)
    op$OppPos <- rownames(op)
    # Pregame universe: build candidates from historical appearances and today's fixtures.
    # NOT from today's boxscore, because non-playing players must remain selectable.
    fixtures <- unique(current[,c('Game','GameDate','Round','Team','Opponent','Home','Away'),drop=FALSE])
    # Do not carry old-club records of a player after a recorded transfer.
    latest_player_team <- prev[!duplicated(prev$Player),c('Player','Team'),drop=FALSE]
    eligible$Player <- vapply(strsplit(eligible$PlayerKey,'||',fixed=TRUE),`[`,character(1),2)
    eligible <- merge(eligible,latest_player_team,by=c('Player','Team'),sort=FALSE)
    candidates <- merge(fixtures,eligible,by='Team',sort=FALSE)
    if (!nrow(candidates)) next
    outcomes <- current[,c('Game','PlayerKey','PIR','Minutes'),drop=FALSE]
    outcomes <- outcomes[!duplicated(outcomes[,c('Game','PlayerKey')]),,drop=FALSE]
    current <- merge(candidates,outcomes,by=c('Game','PlayerKey'),all.x=TRUE,sort=FALSE)
    current$PlayedMinutes <- current$Minutes
    current$Participated <- is.finite(current$PlayedMinutes) & current$PlayedMinutes>=1
    # Retain non-participants in pregame-universe audit, never for fitted labels.
    current$PIR[is.na(current$PIR)] <- 0
    current$HomeSign <- ifelse(current$Team==current$Home,1,
                         ifelse(current$Team==current$Away,-1,NA_real_))
    # A zero PIR here means no boxscore appearance, not verified availability.
    current <- current[current$AvgMinutes>=10 &
                         as.numeric(team_counts[current$Team])>=5 &
                         as.numeric(team_counts[current$Opponent])>=5,,drop=FALSE]
    if (!nrow(current)) next
    current$OppPos <- paste(current$Opponent,current$Position,sep='||')
    current <- merge(current,op,by='OppPos',sort=FALSE,suffixes=c('','_Opp'))
    current <- current[current$Games_Opp>=2,,drop=FALSE]
    if (!nrow(current)) next
    baselines <- as.numeric(league[current$Position])
    ok <- is.finite(baselines) & baselines>0
    current <- current[ok,,drop=FALSE]
    baselines <- baselines[ok]
    if (!nrow(current)) next
    current$MatchupSeasonFactor <- pmax(.65,pmin(1.35,current$Season/baselines))
    current$MatchupLast5Factor <- pmax(.65,pmin(1.35,current$Recent/baselines))
    required_history <- c('Game','GameDate','Round','Team','Player','Position',
      'Opponent','PIR','AvgPIR','Last5AvgPIR','AvgMinutes','Last5AvgMinutes','Games',
      'MatchupSeasonFactor','MatchupLast5Factor','MinutesRatio','Last5MinutesRatio',
      'HomeSign','PlayedMinutes','Participated')
    missing_history <- setdiff(required_history,names(current))
    if (length(missing_history)) stop(paste('Historical candidate columns missing in round',
                                          round_id, ':',paste(missing_history,collapse=', ')))
    history[[length(history)+1L]] <- current[,c('Game','GameDate','Round','Team','Player','Position',
      'Opponent','PIR','AvgPIR','Last5AvgPIR','AvgMinutes','Last5AvgMinutes','Games',
      'MatchupSeasonFactor','MatchupLast5Factor','MinutesRatio','Last5MinutesRatio',
      'HomeSign','PlayedMinutes','Participated')]
  }
  if (!length(history)) return(fallback('No eligible pregame observations with five prior appearances'))
  hist <- do.call(rbind,history)
  hist <- hist[order(hist$Round,hist$Game,hist$Player),,drop=FALSE]
  rownames(hist) <- NULL
  } else {
    hist <- historical_features
    if (!nrow(hist)) return(fallback('No historical features provided'))
    hist <- hist[order(hist$Round,hist$Game,hist$Player),,drop=FALSE]
    rownames(hist) <- NULL
  }
  # Outlier policy: remove only implausible/corrupt observation labels, NOT real high PIR games.
  # Minutes below 1 are removed from fitting/evaluation conditional on participation.
  hist$CleanEligible <- hist$Participated & is.finite(hist$PIR) &
    is.finite(hist$PlayedMinutes) & hist$PlayedMinutes<=65 &
    abs(hist$PIR)<=150 & is.finite(hist$HomeSign)
  hist$CleanReason <- ifelse(!hist$Participated,'DNP_or_under_1_min',
    ifelse(!is.finite(hist$PlayedMinutes) | hist$PlayedMinutes>65,
      'Invalid_minutes',ifelse(!is.finite(hist$PIR) | abs(hist$PIR)>150,
      'Invalid_PIR',ifelse(!is.finite(hist$HomeSign),'Unknown_venue','Included'))))
  if (features_only) return(list(history=hist,status='features_only'))
  hist <- hist[hist$CleanEligible,,drop=FALSE]
  if (!nrow(hist)) return(fallback('No clean historical training candidates'))
  # Scores are evaluated within complete competition rounds, not dates.
  # All candidate rankings use the same pregame historical universe.
  rank_metrics <- function(d,score_col) {
    rounds <- split(d,d$Round)
    out <- lapply(rounds,function(x) {
      if (nrow(x)<2L) return(NULL)
      pred <- rank(-x[[score_col]],ties.method='average')
      actual <- rank(-x$PIR,ties.method='average')
      k <- min(10L,nrow(x))
      k30 <- min(30L,nrow(x))
      top_pred <- order(pred)[seq_len(k)]
      top_actual <- order(actual)[seq_len(k)]
      top30_pred <- order(pred)[seq_len(k30)]
      top30_actual <- order(actual)[seq_len(k30)]
      hit10 <- length(intersect(top_pred,top_actual))
      hit30 <- length(intersect(top30_pred,top30_actual))
      # Normalized discounted gain based on ACTUAL ranks, not PIR magnitude.
      gain <- 1/log2(1+actual)
      dcg <- sum(gain[top_pred]/log2(seq_len(k)+1))
      ideal <- sum(gain[top_actual]/log2(seq_len(k)+1))
      data.frame(Round=x$Round[1],N=nrow(x),
        Spearman=suppressWarnings(cor(pred,actual,method='spearman')),
        Top10Hits=hit10,Top10Eligible=k,Top10Overlap=hit10/k,
        Top30Hits=hit30,Top30Eligible=k30,Top30Overlap=hit30/k30,
        NDCG10=if(ideal>0) dcg/ideal else NA_real_)
    })
    z <- Filter(Negate(is.null),out)
    if(length(z)) do.call(rbind,z) else data.frame()
  }
  # Ranking objective: mean absolute difference between predicted and actual
  # positions WITHIN EACH ROUND. No numeric PIR prediction error is used.
  # Candidate weights are searched using reproducible bounded exploration,
  # since ranks are discontinuous and gradient optimization is unsuitable.
  rank_errors <- function(d, score_col) {
    out <- rep(NA_real_,nrow(d))
    for(ix in split(seq_len(nrow(d)),d$Round)) {
      out[ix] <- abs(rank(-d[[score_col]][ix],ties.method='average') -
                     rank(-d$PIR[ix],ties.method='average'))
    }
    out
  }
  rank_loss <- function(d,w,recency=FALSE,anchor=defaults) {
    scores <- predict_score(d,w)
    per_round <- lapply(split(seq_len(nrow(d)),d$Round),function(ix) {
      predicted <- rank(-scores[ix],ties.method='average')
      observed <- rank(-d$PIR[ix],ties.method='average')
      errors <- abs(predicted-observed)/max(1,length(ix)-1)
      # BOTH models: elite weighting in the OPTIMIZED ranking loss.
      # Top 10 = x7, positions 11-30 = x4, other players = x1.
      # High priority if either predicted or actual rank is in a priority band.
      priority <- ifelse(pmin(predicted,observed)<=10,7,
                  ifelse(pmin(predicted,observed)<=30,4,1))
      sum(errors*priority)/sum(priority)
    })
    vals <- unlist(per_round,use.names=FALSE)
    if(!length(vals)) return(Inf)
    if(recency) {
      importance <- 0.90^(rev(seq_along(vals))-1L)
      # Regularization stabilizes small-sample walk-forward coefficient jumps.
      regularization <- .035*mean(((w-anchor)/(walk_upper-walk_lower))^2)
      return(weighted.mean(vals,importance)+regularization)
    }
    mean(vals)
  }
  lower <- c(0,0,0,0,0,-.35)
  upper <- c(1,1,2,1,1,.35)
  # Model 2 constraints avoid removing whole components on scant history.
  walk_lower <- c(.15,.15,.20,.05,.15,-.25)
  walk_upper <- c(.85,.85,1.50,.75,.85,.25)
  fit <- function(d,recency=FALSE,anchor=defaults) {
    if(!nrow(d)) return(defaults)
    lo <- if(recency) walk_lower else lower
    hi <- if(recency) walk_upper else upper
    initials <- rbind(unname(defaults),c(.65,.35,1,.6,.8,0),
                      c(.2,.8,.5,.25,.2,.10),c(.5,.5,1,.5,.5,-.10),unname(anchor))
    for(j in seq_len(320L)) {
      f <- function(z) (j * z * 0.6180339887498949) %% 1
      initials <- rbind(initials, lo+(hi-lo)*c(f(1),f(3),f(7),f(11),f(17),f(23)))
    }
    initials <- t(apply(initials,1,function(x) pmin(hi,pmax(lo,x))))
    values <- apply(initials,1,function(w)
      rank_loss(d,setNames(w,names(defaults)),recency,anchor))
    best <- initials[which.min(values),]
    best_loss <- min(values)
    for(step in c(.25,.10,.04,.015)) for(iter in seq_len(3L)) {
      improved <- FALSE
      for(k in seq_along(best)) for(sign_dir in c(-1,1)) {
        trial <- best
        trial[k] <- min(hi[k],max(lo[k],trial[k]+sign_dir*step*(hi[k]-lo[k])))
        v <- rank_loss(d,setNames(trial,names(defaults)),recency,anchor)
        if(is.finite(v) && v < best_loss-1e-10) {
          best <- trial; best_loss <- v; improved <- TRUE
        }
      }
      if(!improved) break
    }
    setNames(as.numeric(best),names(defaults))
  }
  unique_rounds <- sort(unique(hist$Round))
  # Any non-empty historical rank data is used, even a single eligible round.
  if(!nrow(hist)) return(fallback('No rankable historical candidates',hist))
  # Model 1: fixed coefficient set, elite-weighted rank error (top10 x7,
  # 11-30 x4, rest x1) with equal weighting of historical rounds.
  # Model 2 uses the SAME elite priority but adds chronological walk-forward,
  # recency weighting and coefficient stabilization.
  weights <- fit(hist,recency=FALSE)
  hist$Model1Score <- predict_score(hist,weights)
  hist$FixedScore <- NA_real_
  hist$WalkForwardScore <- NA_real_
  hist$WalkForwardReady <- FALSE
  hist$Model1RankError <- NA_real_
  hist$Model2RankError <- NA_real_
  coeffs <- list()
  metrics <- list()
  # Model 1 retrospective leave-one-round-out diagnostic; NOT prospective.
  # This supplies one evaluation for every available round without training
  # on that round's outcomes. Other (future) rounds may enter its training.
  for(r in unique_rounds) {
    ix <- which(hist$Round==r)
    other <- hist[hist$Round!=r,,drop=FALSE]
    w1 <- if(nrow(other)) fit(other,recency=FALSE) else defaults
    hist$FixedScore[ix] <- predict_score(hist[ix,,drop=FALSE],w1)
  }
  # Model 2: walk-forward only. At round 4 no labeled training rounds exist;
  # after each completed round, update using rounds STRICTLY < r.
  prior_weights <- defaults
  for(r in unique_rounds) {
    past <- hist[hist$Round<r,,drop=FALSE]
    ix <- which(hist$Round==r)
    w <- if(nrow(past)) fit(past,recency=TRUE,anchor=prior_weights) else defaults
    hist$WalkForwardScore[ix] <- predict_score(hist[ix,,drop=FALSE],w)
    hist$WalkForwardReady[ix] <- TRUE
    coeffs[[length(coeffs)+1L]] <- data.frame(Round=r,TrainingRounds=length(unique(past$Round)),
      TrainingCandidates=nrow(past),as.list(w),check.names=FALSE)
    prior_weights <- w
  }
  # Errors for every player in every eligible round.
  hist$Model1RankError <- rank_errors(hist,'FixedScore')
  ready_ix <- which(hist$WalkForwardReady)
  if(length(ready_ix)) {
    err <- rank_errors(hist[ready_ix,,drop=FALSE],'WalkForwardScore')
    hist$Model2RankError[ready_ix] <- err
  }
  # All eligible rounds 4..last are displayed for BOTH models.
  # Model 1 = retrospective leave-one-round-out (may use future rounds).
  # Model 2 = genuinely chronological walk-forward, no future labels.
  comparable <- hist[hist$WalkForwardReady,,drop=FALSE]
  if(nrow(comparable)) {
    fixed_m <- rank_metrics(comparable,'FixedScore')
    walk_m <- rank_metrics(comparable,'WalkForwardScore')
    if(nrow(fixed_m)) metrics[[length(metrics)+1L]] <-
      data.frame(Model='Model1_Frozen',EvaluationType='Retrospective_LOO',fixed_m)
    if(nrow(walk_m)) metrics[[length(metrics)+1L]] <-
      data.frame(Model='Model2_FrozenRecency',EvaluationType='Chronological_OOS',walk_m)
  }
  round_metrics <- if(length(metrics)) do.call(rbind,metrics) else data.frame()
  error_summary <- do.call(rbind,lapply(split(hist,hist$Round),function(d) {
    usable <- d[!is.na(d$Model2RankError),,drop=FALSE]
    if(!nrow(usable)) return(NULL)
    describe <- function(x) c(MAE=mean(x),Median=median(x),P80=unname(quantile(x,.8)),P90=unname(quantile(x,.9)),Max=max(x))
    data.frame(Round=d$Round[1],Candidates=nrow(d),
      Model1_MAE=describe(usable$Model1RankError)[1],
      Model1_P80=describe(usable$Model1RankError)[3],
      Model1_P90=describe(usable$Model1RankError)[4],
      Model2_MAE=describe(usable$Model2RankError)[1],
      Model2_P80=describe(usable$Model2RankError)[3],
      Model2_P90=describe(usable$Model2RankError)[4],row.names=NULL)
  }))
  if(is.null(error_summary)) error_summary <- data.frame()

  walk_coefficients <- if(length(coeffs)) do.call(rbind,coeffs) else data.frame()
  # Next-round model 2 trained on all completed eligible rounds,
  # anchored to the last set of previous-round weights for stability.
  walk_weights <- fit(hist,recency=TRUE,anchor=prior_weights)
  checks <- data.frame(Metric=c('Historical candidates','Eligible backtest rounds',
    'First backtest round','Last backtest round','Comparable displayed rounds',
    'Model 1 testing method','Model 2 testing method',
    'Model 1 mean NDCG10','Model 2 mean NDCG10',
    'Model 1 mean Spearman','Model 2 mean Spearman',
    'Both models top-rank weighting','Model 2 coefficient stabilization','Model 1 objective','Model 1 mean Top10 hits','Model 1 mean Top30 hits','Model 2 mean Top10 hits','Model 2 mean Top30 hits'),
    Value=as.character(c(nrow(hist),length(unique_rounds),min(unique_rounds),max(unique_rounds),
      length(unique(comparable$Round)),
      'Leave-one-round-out (retrospective; potentially uses future rounds)',
      'Walk-forward: only rounds STRICTLY before prediction round',
      if(nrow(round_metrics)) round(mean(round_metrics$NDCG10[round_metrics$Model=='Model1_Frozen'],na.rm=TRUE),4) else NA,
      if(nrow(round_metrics)) round(mean(round_metrics$NDCG10[round_metrics$Model=='Model2_FrozenRecency'],na.rm=TRUE),4) else NA,
      if(nrow(round_metrics)) round(mean(round_metrics$Spearman[round_metrics$Model=='Model1_Frozen'],na.rm=TRUE),4) else NA,
      if(nrow(round_metrics)) round(mean(round_metrics$Spearman[round_metrics$Model=='Model2_FrozenRecency'],na.rm=TRUE),4) else NA,
      'Top 10: x7; ranks 11-30: x4; ranks 31+: x1',
      'Bounded coefficients and 0.035 squared coefficient-change penalty',
      'Equal-round elite-weighted normalized absolute rank error',
      if(nrow(round_metrics)) round(mean(round_metrics$Top10Hits[round_metrics$Model=='Model1_Frozen'],na.rm=TRUE),3) else NA,
      if(nrow(round_metrics)) round(mean(round_metrics$Top30Hits[round_metrics$Model=='Model1_Frozen'],na.rm=TRUE),3) else NA,
      if(nrow(round_metrics)) round(mean(round_metrics$Top10Hits[round_metrics$Model=='Model2_FrozenRecency'],na.rm=TRUE),3) else NA,
      if(nrow(round_metrics)) round(mean(round_metrics$Top30Hits[round_metrics$Model=='Model2_FrozenRecency'],na.rm=TRUE),3) else NA)))
  log_it('Ranking optimization completed; comparison rounds: ',length(unique(comparable$Round)))
  list(weights=weights,walk_weights=walk_weights,history=hist,checks=checks,
    round_metrics=round_metrics,error_summary=error_summary,walk_coefficients=walk_coefficients,
    status='Model 1 optimized on all available rounds; Model 2 walk-forward',predict=predict_score)

}

# Evaluate TWO frozen models on E2026, without fitting on E2026 labels.
el_evaluate_frozen_models <- function(raw,meta,complete_rounds,artifact,
                                      log_it=function(...) invisible(NULL)) {
  defaults <- c(PlayerLast5Weight=.40,OpponentLast5Weight=.60,
                OpponentStrength=1,MinutesPenalty=.40,Last5MinutesWeight=.50,HomeAdvantage=0)
  w1 <- artifact$weights
  w2 <- artifact$walk_weights
  if (!setequal(names(w1),names(defaults)) || !setequal(names(w2),names(defaults)))
    stop('Saved trained model contains incompatible weights')
  predict_score <- function(d,w) {
    base <- (1-w['PlayerLast5Weight'])*d$AvgPIR+w['PlayerLast5Weight']*d$Last5AvgPIR
    matchup <- (1-w['OpponentLast5Weight'])*d$MatchupSeasonFactor+
      w['OpponentLast5Weight']*d$MatchupLast5Factor
    matchup <- pmax(.10,1+w['OpponentStrength']*(matchup-1))
    minutes <- (1-w['Last5MinutesWeight'])*d$MinutesRatio+
      w['Last5MinutesWeight']*d$Last5MinutesRatio
    as.numeric(base*matchup*(1-w['MinutesPenalty']*(1-minutes))*
      exp(w['HomeAdvantage']*d$HomeSign))
  }
  blank <- el_fit_historical_weights(raw,meta,log_it,complete_rounds,
                                      features_only=TRUE)
  hist <- blank$history
  if (is.null(hist) || !nrow(hist)) {
    log_it('No complete E2026 rounds with eligible evaluation players yet.')
    return(list(weights=w1,walk_weights=w2,history=data.frame(),
      checks=data.frame(Metric='Testing status',Value='No eligible complete E2026 rounds'),
      round_metrics=data.frame(),error_summary=data.frame(),
      walk_coefficients=data.frame(),status='No eligible 2026/27 rounds',predict=predict_score))
  }
  # No future participation info is used in scoring, only in conditional evaluation.
  # Keep the complete candidate universe for a second honest deployment-style test.
  full_hist <- hist[is.finite(hist$HomeSign),,drop=FALSE]
  hist <- hist[hist$CleanEligible,,drop=FALSE]
  if (!nrow(hist)) stop('No clean 2026/27 evaluation observations')
  hist$Model1Score <- predict_score(hist,w1)
  hist$FixedScore <- hist$Model1Score
  hist$WalkForwardScore <- predict_score(hist,w2)
  hist$WalkForwardReady <- TRUE
  for (model in c('FixedScore','WalkForwardScore')) {
    err <- numeric(nrow(hist))
    for (ix in split(seq_len(nrow(hist)),hist$Round)) {
      err[ix] <- abs(rank(-hist[[model]][ix],ties.method='average')-
                       rank(-hist$PIR[ix],ties.method='average'))
    }
    hist[[if(model=='FixedScore') 'Model1RankError' else 'Model2RankError']] <- err
  }
  calc_metrics <- function(d,score_col,model_label) {
    lapply(split(d,d$Round),function(x) {
      if (nrow(x)<2L) return(NULL)
      pred <- rank(-x[[score_col]],ties.method='average')
      actual <- rank(-x$PIR,ties.method='average')
      k <- min(10L,nrow(x)); k30 <- min(30L,nrow(x))
      top_pred <- order(pred)[seq_len(k)]
      top_actual <- order(actual)[seq_len(k)]
      top30_pred <- order(pred)[seq_len(k30)]
      top30_actual <- order(actual)[seq_len(k30)]
      h10 <- length(intersect(top_pred,top_actual))
      h30 <- length(intersect(top30_pred,top30_actual))
      gain <- 1/log2(1+actual)
      dcg <- sum(gain[top_pred]/log2(seq_len(k)+1))
      ideal <- sum(gain[top_actual]/log2(seq_len(k)+1))
      data.frame(Model=model_label,EvaluationType='Chronological_OOS_Frozen',
        Round=x$Round[1],N=nrow(x),
        Spearman=suppressWarnings(cor(pred,actual,method='spearman')),
        Top10Hits=h10,Top10Eligible=k,Top10Overlap=h10/k,
        Top30Hits=h30,Top30Eligible=k30,Top30Overlap=h30/k30,
        NDCG10=if(ideal>0) dcg/ideal else NA_real_)
    })
  }
  full_hist$FixedScore <- predict_score(full_hist,w1)
  full_hist$WalkForwardScore <- predict_score(full_hist,w2)
  # Observed PIR = 0 for non-participants. These remain in the full prediction universe.
  deployment_metrics <- c(
    calc_metrics(full_hist,'FixedScore','Model1_Frozen'),
    calc_metrics(full_hist,'WalkForwardScore','Model2_FrozenRecency'))
  deployment_metrics <- Filter(Negate(is.null),deployment_metrics)
  deployment_metrics <- if(length(deployment_metrics)) do.call(rbind,deployment_metrics) else data.frame()
  if(nrow(deployment_metrics)) deployment_metrics$EvaluationType <- 'Full_pregame_universe'
  metrics <- c(calc_metrics(hist,'FixedScore','Model1_Frozen'),
               calc_metrics(hist,'WalkForwardScore','Model2_FrozenRecency'))
  metrics <- Filter(Negate(is.null),metrics)
  round_metrics <- if(length(metrics)) do.call(rbind,metrics) else data.frame()
  if(nrow(round_metrics)) round_metrics$EvaluationType <- 'Conditional_1min_played'
  if(nrow(deployment_metrics)) round_metrics <- rbind(round_metrics,deployment_metrics)
  error_summary <- do.call(rbind,lapply(split(hist,hist$Round),function(x) {
    data.frame(Round=x$Round[1],Candidates=nrow(x),
      Model1_MAE=mean(x$Model1RankError),
      Model1_P80=unname(quantile(x$Model1RankError,.8)),
      Model1_P90=unname(quantile(x$Model1RankError,.9)),
      Model2_MAE=mean(x$Model2RankError),
      Model2_P80=unname(quantile(x$Model2RankError,.8)),
      Model2_P90=unname(quantile(x$Model2RankError,.9)))
  }))
  walk_coefficients <- do.call(rbind,lapply(sort(unique(hist$Round)),function(r) {
    data.frame(Round=r,TrainingRounds=length(unique(artifact$training_rounds)),
      TrainingCandidates=artifact$training_candidates,as.list(w2),check.names=FALSE)
  }))
  mean_metric <- function(m,model,col) {
    z <- m[m$Model==model & m$EvaluationType=='Conditional_1min_played',col]
    if(!length(z) || all(is.na(z))) return(NA_real_)
    round(mean(z,na.rm=TRUE),4)
  }
  m1 <- 'Model1_Frozen'; m2 <- 'Model2_FrozenRecency'
  checks <- data.frame(Metric=c('Historical training seasons','Historical training candidates',
    'Evaluated season','Eligible backtest rounds','First backtest round','Last backtest round',
    'Model 1 testing method','Model 2 testing method',
    'Model 1 mean NDCG10','Model 2 mean NDCG10',
    'Model 1 mean Spearman','Model 2 mean Spearman',
    'Both models top-rank weighting','Model 2 coefficient stabilization',
    'Model 1 objective','Model 1 mean Top10 hits','Model 1 mean Top30 hits',
    'Model 2 mean Top10 hits','Model 2 mean Top30 hits'),
    Value=as.character(c('2024/25, 2025/26',artifact$training_candidates,
      '2026/27',length(unique(hist$Round)),min(hist$Round),max(hist$Round),
      'Frozen historical weights; chronological out-of-sample',
      'Frozen historical recency weights; chronological out-of-sample',
      mean_metric(round_metrics,m1,'NDCG10'),mean_metric(round_metrics,m2,'NDCG10'),
      mean_metric(round_metrics,m1,'Spearman'),mean_metric(round_metrics,m2,'Spearman'),
      'Top 10: x7; ranks 11-30: x4; ranks 31+: x1',
      'Bounded coefficients and 0.035 squared coefficient-change penalty',
      'Equal-round elite-weighted normalized absolute rank error',
      mean_metric(round_metrics,m1,'Top10Hits'),mean_metric(round_metrics,m1,'Top30Hits'),
      mean_metric(round_metrics,m2,'Top10Hits'),mean_metric(round_metrics,m2,'Top30Hits'))))
  log_it('Frozen out-of-sample evaluation on E2026 rounds: ',
         paste(sort(unique(hist$Round)),collapse=', '))
  full_hist$Model1Score <- full_hist$FixedScore
  full_hist$WalkForwardReady <- TRUE
  full_hist$Model1RankError <- NA_real_
  full_hist$Model2RankError <- NA_real_
  keys <- paste(hist$Round,hist$Game,hist$Team,hist$Player,sep='||')
  match_index <- match(paste(full_hist$Round,full_hist$Game,full_hist$Team,full_hist$Player,sep='||'),keys)
  ok_ix <- which(!is.na(match_index))
  full_hist$Model1RankError[ok_ix] <- hist$Model1RankError[match_index[ok_ix]]
  full_hist$Model2RankError[ok_ix] <- hist$Model2RankError[match_index[ok_ix]]
  list(weights=w1,walk_weights=w2,history=full_hist,checks=checks,
       round_metrics=round_metrics,error_summary=error_summary,
       walk_coefficients=walk_coefficients,
       status='Both models frozen from E2024/E2025; E2026 test only',predict=predict_score)
}

# Ranking-first player selection, two models and round-level evaluation.

build_euroleague_2026_27 <- function(output_dir, season='E2026', mode='predict',
                                       trained_model_file=NULL, target_round=NULL) {
  if (!mode %in% c('predict','collect')) stop('mode must be predict or collect')
  if (mode=='predict' && is.null(trained_model_file))
    trained_model_file <- file.path(output_dir,'EuroLeague_historical_models_2024_2026_v12_fantasy.rds')

  if (missing(output_dir) || !is.character(output_dir) || length(output_dir) != 1L ||
      is.na(output_dir) || !nzchar(trimws(output_dir))) {
    stop("Provide one folder path: build_euroleague_2026_27('C:/path/to/folder')")
  }
  output_dir <- path.expand(output_dir)
  if (!dir.exists(output_dir)) {
    if (!dir.create(output_dir, recursive=TRUE, showWarnings=FALSE)) {
      stop(paste("Cannot create output folder:", output_dir))
    }
  }
  output_dir <- normalizePath(output_dir, winslash='/', mustWork=TRUE)
  if (file.access(output_dir, 2) != 0) stop(paste("Folder is not writable:", output_dir))

  packages <- c('httr', 'jsonlite', 'openxlsx')
  for (pkg in packages) if (!requireNamespace(pkg, quietly=TRUE))
    install.packages(pkg, repos='https://cloud.r-project.org')

  base_urls <- c(paste0('https://api-live.euroleague.net/v2/competitions/E/seasons/',season),
                 paste0('https://feeds.incrowdsports.com/provider/euroleague-feeds/v2/competitions/E/seasons/',season))
  output <- file.path(output_dir, 'EuroLeague_2026-27_FP_real_data.xlsx')
  csv_path <- file.path(output_dir, 'EuroLeague_2026-27_players.csv')
  picks_path <- file.path(output_dir, 'EuroLeague_2026-27_top10_recommendations.csv')
  debug_path <- file.path(output_dir, 'EuroLeague_2026-27_debug.txt')
  log_lines <- character()
  log_it <- function(...) {
    msg <- paste0(..., collapse='')
    cat(msg, '\n'); log_lines <<- c(log_lines,msg)
  }
  finish_log <- function() writeLines(log_lines,debug_path,useBytes=TRUE)
  on.exit(finish_log(), add=TRUE)  # Also save the log on errors.

  get_json <- function(path, query=NULL, quiet=FALSE) {
    failures <- character()
    for (b in base_urls) {
      u <- paste0(b,'/',path)
      for (attempt in 1:2) {
        r <- tryCatch(httr::GET(u,query=query,httr::user_agent('EuroLeague-R-Boxscore/2.0'),
                       httr::add_headers(Accept='application/json'),httr::timeout(20)),error=identity)
        if (!inherits(r,'error')) {
          status <- httr::status_code(r)
          if (status==200) {
            x <- tryCatch(jsonlite::fromJSON(httr::content(r,'text',encoding='UTF-8'),
                                             simplifyVector=FALSE),error=identity)
            if (!inherits(x,'error') && is.list(x)) return(x)
            failures <- c(failures,paste0(u,': invalid JSON'))
          } else failures <- c(failures,paste0(u,': HTTP ',status))
          if (status %in% c(400,401,403,404)) break
        } else failures <- c(failures,paste0(u,': ',conditionMessage(r)))
        Sys.sleep(attempt)
      }
    }
    if (!quiet) log_it('FAILED ',path,' => ',paste(unique(failures),collapse=' | '))
    NULL
  }
  val <- function(x,keys,default=NULL) {
    if (!is.list(x)) return(default)
    for (k in keys) if (!is.null(x[[k]]) && length(x[[k]])>0) return(x[[k]])
    default
  }
  str1 <- function(x) {
    if (is.null(x) || !length(x)) return('')
    if (!is.list(x)) return(as.character(x[1]))
    v <- val(x,c('fullName','name','officialName','clubPermanentName','displayName',
                 'shortName','personName','positionName','description','code'))
    if (!is.null(v)) return(str1(v))
    paste(Filter(nzchar,c(str1(x$firstName),str1(x$lastName))),collapse=' ')
  }
  number <- function(x) {
    if (is.null(x) || !length(x)) return(NA_real_)
    if (is.list(x)) return(number(val(x,c('value','total','seconds'))))
    z <- trimws(as.character(x[1]))
    if (grepl('^[0-9]+:[0-9]{2}$',z)) {
      p <- as.numeric(strsplit(z,':',fixed=TRUE)[[1]])
      return(p[1]*60+p[2])
    }
    suppressWarnings(as.numeric(z))
  }
  position <- function(x) {
    p <- tolower(trimws(str1(x)))
    # IMPORTANT: EuroLeague's numeric codes are 1=Guard, 2=Forward, 3=Center.
    # They are NOT the familiar basketball 1..5 court-position numbering.
    if (p %in% c('1','guard','g','pg','sg') || grepl('guard',p)) return('Guard')
    if (p %in% c('2','forward','f','sf','pf') || grepl('forward',p)) return('Forward')
    if (p %in% c('3','center','centre','c') || grepl('cent(er|re)',p)) return('Center')
    ''
  }
  list_items <- function(x) {
    if (is.null(x)) return(list())
    if (is.list(x) && !is.null(x$data)) x <- x$data
    if (is.list(x) && !is.null(x$items)) x <- x$items
    if (!is.list(x)) return(list())
    # A single JSON object is not a collection.
    if (!is.null(names(x)) && any(names(x) %in% c('gameCode','person','stats','local','road'))) return(list(x))
    unname(x)
  }
  club <- function(game,side) {
    keys <- if(side=='local') c('local','home','localClub','homeClub') else
                               c('road','away','roadClub','awayClub')
    x <- val(game,keys)
    str1(val(x,c('club','team'),default=x))
  }
  played <- function(g) {
    st <- tolower(str1(val(g,c('status','gameStatus'))))
    p <- val(g,c('played','isPlayed'))
    isTRUE(p) || st %in% c('played','finished','final','closed') ||
      (is.finite(number(val(g,c('localScore','homeScore')))) &&
       is.finite(number(val(g,c('roadScore','awayScore')))))
  }

  log_it('Downloading games from official EuroLeague API, season ',season)
  schedule <- get_json('games',query=list(limit=500,offset=0))
  if (is.null(schedule)) { finish_log(); stop(paste('Could not download schedule. See', debug_path)) }
  games <- list_items(schedule)
  total <- number(val(schedule,c('total')))
  if (is.finite(total) && length(games)<total) {
    for (offset in seq(500,total-1,by=500)) {
      page <- get_json('games',query=list(limit=500,offset=offset))
      games <- c(games,list_items(page))
    }
  }
  log_it('Games returned: ',length(games),' / API total: ',total)
  if (length(games)) log_it('First game fields: ',paste(names(games[[1]]),collapse=', '))
  ids <- vapply(games,function(g) str1(val(g,c('gameCode','gamecode','game_code'))),'')
  done <- vapply(games,played,logical(1))
  # Schedule status fields may differ; if necessary try every past game with an ID.
  if (!any(done & nzchar(ids))) {
    log_it('No completed games identified by flags; probing game IDs for available box scores.')
    done <- nzchar(ids)
  }
  todo <- games[done & nzchar(ids)]
  log_it('Candidate games: ',length(todo))
  if (!length(todo)) { finish_log(); stop('No game IDs found. Inspect debug log.') }

  # Optional player-position lookup from the SAME official EuroLeague API.
  # This helps if a particular box-score omits a player's position.
  people <- get_json('people',query=list(limit=2000,offset=0),quiet=TRUE)
  roster <- list_items(people)
  position_map <- new.env(hash=TRUE,parent=emptyenv())
  for (r in roster) {
    p <- position(val(r,c('position','positionName'),default=val(r,c('person'))))
    for (key in c(str1(val(r,c('personCode','code','id'))),
                  str1(val(val(r,c('person'),list()),c('code','id'))))) {
      if(nzchar(key) && nzchar(p)) assign(key,p,envir=position_map)
    }
  }
  log_it('Roster records available: ',length(roster))
  rows <- list(); game_meta <- list(); fail <- character(); skipped <- character(); processed <- 0L
  for (i in seq_along(todo)) {
    g <- todo[[i]]; id <- str1(val(g,c('gameCode','gamecode','game_code')))
    box <- get_json(paste0('games/',id,'/stats'),quiet=TRUE)
    if (is.null(box)) { fail <- c(fail,id); next }
    if (!is.null(box$data) && is.list(box$data) && is.null(box$local)) box <- box$data
    if (processed==0) log_it('First valid box score fields: ',paste(names(box),collapse=', '))
    processed <- processed+1L
    # Schedule date is recorded for ordering games rather than individual player rows.
    raw_date <- str1(val(g,c('utcDate','date','localDate','confirmedDate')))
    match_date <- regmatches(raw_date,regexpr('[0-9]{4}-[0-9]{2}-[0-9]{2}',raw_date))
    if (!length(match_date) || !nzchar(match_date)) match_date <- '0000-00-00'
    game_meta[[length(game_meta)+1L]] <- data.frame(
      Game=id, GameDate=match_date, Round=number(val(g,c('round','roundNumber'))),
      Home=club(g,'local'), Away=club(g,'road'),
      stringsAsFactors=FALSE)
    for (side in c('local','road')) {
      oppside <- if(side=='local') 'road' else 'local'
      sec <- val(box,c(side,if(side=='local') 'home' else 'away'),default=list())
      opp <- club(g,oppside)
      if (!nzchar(opp)) opp <- str1(val(val(box,c(oppside),list()),c('team','club')))
      own_team <- club(g,side)
      if (!nzchar(own_team)) own_team <- str1(val(sec,c('team','club')))
      plys <- list_items(val(sec,c('players','playerStats','playerStatistics')))
      if (processed==1) log_it('Game ',id,' ',side,': ',length(plys),' players; opponent=',opp,
                               '; section fields=',paste(names(sec),collapse=','))
      for (line in plys) {
        person <- val(line,c('player','person'),default=list())
        stats <- val(line,c('stats','statistics'),default=line)
        pname <- str1(val(person,c('person'),default=person))
        pos_raw <- val(person,c('position','positionName'),
                   default=val(line,c('position','positionName')))
        pos <- position(pos_raw)
        if(!nzchar(pos)) {
          code <- str1(val(val(person,c('person'),default=person),c('code','id','personCode')))
          if(exists(code,envir=position_map,inherits=FALSE)) pos <- get(code,envir=position_map)
        }
        secs <- number(val(stats,c('timePlayed','minutesPlayed','minutes','min')))
        el_pir <- number(val(stats,c('valuation','pir','PIR','rating')))
        sv <- function(k) number(val(stats,k))
        pir <- el_fantasy_points(sv(c('points')), sv(c('totalRebounds')), sv(c('assistances','assists')),
          sv(c('steals')), sv(c('blocksFavour','blocks')), sv(c('turnovers')), sv(c('foulsReceived')),
          sv(c('foulsCommited','foulsCommitted')),
          (sv('fieldGoalsAttempted2')-sv('fieldGoalsMade2')) + (sv('fieldGoalsAttempted3')-sv('fieldGoalsMade3')) +
          (sv('freeThrowsAttempted')-sv('freeThrowsMade')))
        if (!is.finite(el_pir)) pir <- NA_real_
        if (!nzchar(pos) || !nzchar(opp) || !nzchar(own_team) || !is.finite(secs) || secs<=0 || !is.finite(pir)) {
          if(length(skipped)<100) skipped <- c(skipped,paste('game',id,'player',pname,
            'position',str1(pos_raw),'seconds',secs,'PIR',pir,'opponent',opp))
          next
        }
        rows[[length(rows)+1L]] <- data.frame(Game=id,Team=own_team,Player=pname,Position=pos,
            Opponent=opp,Minutes=secs/60,PIR=pir,PIR_x_Minutes=secs/60*pir,EL_PIR=el_pir)
      }
    }
    if (i%%10==0) log_it('Checked ',i,'/',length(todo),' games; valid player rows: ',length(rows))
    Sys.sleep(0.15)
  }
  log_it('Successful box scores: ',processed,'; failed game requests: ',length(fail),
         '; valid rows: ',length(rows),'; skipped records sampled: ',length(skipped))
  if(length(skipped)) log_it('First skipped lines: \n',paste(head(skipped,12),collapse='\n'))
  if(length(fail)) log_it('Failed game IDs: ',paste(fail,collapse=', '))
  finish_log()
  if(!length(rows)) stop(paste('No valid player data. Read', debug_path))
  raw <- do.call(rbind,rows)
  counts <- table(factor(raw$Position, levels=c('Guard','Forward','Center')))
  log_it('Player rows by position: ',paste(names(counts),as.integer(counts),sep='=',collapse='; '))
  if (any(counts==0L)) {
    finish_log()
    stop(paste0('Missing position data: ',paste(names(counts)[counts==0L],collapse=', '),
                '. Check position mapping in ',debug_path))
  }
  clubs_data <- get_json('clubs',quiet=TRUE)
  clubs <- vapply(list_items(clubs_data),str1,'')
  teamnames <- sort(unique(c(clubs[nzchar(clubs)],raw$Opponent)))
  summary <- data.frame('יריבה'=teamnames,check.names=FALSE)
  for (p in c('Guard','Forward','Center')) summary[[p]] <- vapply(teamnames,function(t) {
    r <- raw[raw$Opponent==t & raw$Position==p,,drop=FALSE]
    if(!nrow(r)) return(NA_real_)
    sum(r$PIR_x_Minutes)/sum(r$Minutes)
  },numeric(1))
  # Determine the last 5 DISTINCT games played by each opponent (not last 5 player records).
  meta <- do.call(rbind,game_meta)
  meta <- meta[!duplicated(meta$Game),,drop=FALSE]
  valid_game_ids <- unique(raw$Game)
  meta <- meta[meta$Game %in% valid_game_ids,,drop=FALSE]
  # Some older API responses use dates without an ISO prefix; gameCode is the tiebreaker.
  suppressWarnings(meta$GameNumber <- as.numeric(meta$Game))
  meta$GameNumber[is.na(meta$GameNumber)] <- 0
  meta <- meta[order(meta$GameDate,meta$GameNumber,decreasing=TRUE),,drop=FALSE]
  # A game belongs to an opponent only if at least one valid opposing player row exists.
  recent_keys <- character()
  latest_games <- list()
  for (t in teamnames) {
    candidates <- meta[meta$Home==t | meta$Away==t,,drop=FALSE]
    # Protect against potential inconsistencies in schedule naming.
    candidates <- candidates[candidates$Game %in% raw$Game[raw$Opponent==t],,drop=FALSE]
    candidates <- head(candidates,5)
    if (nrow(candidates)) {
      recent_keys <- c(recent_keys,paste(candidates$Game,t,sep='||'))
      latest_games[[length(latest_games)+1L]] <- data.frame(
        Opponent=t, Game=candidates$Game, Date=candidates$GameDate,
        stringsAsFactors=FALSE)
    }
  }
  recent_raw <- raw[paste(raw$Game,raw$Opponent,sep='||') %in% recent_keys,,drop=FALSE]
  recent_summary <- data.frame('יריבה'=teamnames,check.names=FALSE)
  for (p in c('Guard','Forward','Center')) recent_summary[[p]] <- vapply(teamnames,function(t) {
    r <- recent_raw[recent_raw$Opponent==t & recent_raw$Position==p,,drop=FALSE]
    if(!nrow(r)) return(NA_real_)
    sum(r$PIR_x_Minutes)/sum(r$Minutes)
  },numeric(1))
  make_top8 <- function(dat) {
    out <- list()
    for (t in teamnames) for (p in c('Guard','Forward','Center')) {
      x <- dat[dat$Opponent==t & dat$Position==p,,drop=FALSE]
      if (!nrow(x) || sum(x$Minutes)<=0) next
      out[[length(out)+1L]] <- data.frame(
        Opponent=t, Position=p, Weighted_PIR=sum(x$PIR_x_Minutes)/sum(x$Minutes),
        Games=length(unique(x$Game)), Player_appearances=nrow(x),
        Total_minutes=sum(x$Minutes),stringsAsFactors=FALSE)
    }
    if (!length(out)) return(data.frame(Rank=integer(),Opponent=character(),
       Position=character(),Weighted_PIR=numeric(),Games=integer(),
       Player_appearances=integer(),Total_minutes=numeric()))
    x <- do.call(rbind,out)
    x <- x[order(-x$Weighted_PIR,-x$Total_minutes,x$Opponent,x$Position),,drop=FALSE]
    x <- head(x,8)
    data.frame(Rank=seq_len(nrow(x)),x,check.names=FALSE,row.names=NULL)
  }
  top8_all <- make_top8(raw)
  top8_recent <- make_top8(recent_raw)
  recent_games <- if(length(latest_games)) do.call(rbind,latest_games) else
    data.frame(Opponent=character(),Game=character(),Date=character())
  log_it('Recent-game window: ',length(unique(recent_games$Game)),
         ' unique games across ',length(unique(recent_games$Opponent)),' opponents')

  # Historical score components are calculated using only PRIOR ROUNDS.
  # A whole round enters history only after all its scheduled games are finished
  # AND their box scores were successfully downloaded. No partially completed slates.
  schedule_rounds <- vapply(games,function(g) number(val(g,c('round','roundNumber'))),numeric(1))
  schedule_done <- vapply(games,played,logical(1))
  schedule_ids <- vapply(games,function(g) str1(val(g,c('gameCode','gamecode','game_code'))),'')
  complete_rounds <- sort(unique(schedule_rounds[is.finite(schedule_rounds) &
    schedule_rounds>=1]))
  complete_rounds <- complete_rounds[vapply(complete_rounds,function(r) {
    ix <- which(schedule_rounds==r & nzchar(schedule_ids))
    length(ix)>0L && all(schedule_done[ix]) && all(schedule_ids[ix] %in% raw$Game)
  },logical(1))]
  log_it('Fully completed rounds with parsed box scores: ',paste(complete_rounds,collapse=', '))
  if (mode=='collect') {
    return(invisible(list(raw=raw,meta=meta,complete_rounds=complete_rounds,
                          scheduled_rounds=sort(unique(schedule_rounds[is.finite(schedule_rounds) & schedule_rounds>=1])),
                          season=season,successful_boxscores=processed,
                          failed_boxscores=length(fail),debug=debug_path)))
  }
  if (!file.exists(trained_model_file))
    stop(paste('Trained weights not found:',trained_model_file,
               '. Run train_euroleague_historical_models() first.'))
  artifact <- readRDS(trained_model_file)
  if (!identical(artifact$version,12L)) stop('Incompatible trained artifact: retrain using v12 (fantasy)')
  if (!identical(as.character(artifact$seasons),c('E2024','E2025')))
    stop('Unexpected training seasons in model artifact')
  training <- el_evaluate_frozen_models(raw,meta,complete_rounds,artifact,log_it)

  learned <- training$weights
  walk_learned <- training$walk_weights
  if (is.null(walk_learned) || !length(walk_learned)) {
    stop("Walk-forward model returned no weights")
  }

  # ------------------ NEXT FIXTURES + PLAYER RECOMMENDATIONS ------------------
  # All dates are calendar dates in the published schedule. A date-only feed does
  # not reliably encode tip-off time, so never fabricate a time.
  calendar <- lapply(games, function(g) {
    raw_date <- str1(val(g,c('utcDate','date','localDate','confirmedDate')))
    iso <- regmatches(raw_date, regexpr('[0-9]{4}-[0-9]{2}-[0-9]{2}',raw_date))
    if (!length(iso)) iso <- ''
    data.frame(Game=str1(val(g,c('gameCode','gamecode','game_code'))),
               Date=iso, Round=number(val(g,c('round','roundNumber'))),
               Home=club(g,'local'), Away=club(g,'road'),
               Finished=played(g),stringsAsFactors=FALSE)
  })
  calendar <- do.call(rbind,calendar)
  today <- as.Date(Sys.time(),tz='Asia/Jerusalem')
  calendar$ParsedDate <- as.Date(calendar$Date,format='%Y-%m-%d')
  upcoming <- calendar[!calendar$Finished & !is.na(calendar$ParsedDate) &
                         calendar$ParsedDate >= today & nzchar(calendar$Home) &
                         nzchar(calendar$Away),,drop=FALSE]
  upcoming <- upcoming[order(upcoming$ParsedDate, suppressWarnings(as.numeric(upcoming$Game))),,drop=FALSE]
  upcoming <- upcoming[!duplicated(upcoming$Game),,drop=FALSE]
  # One common competition round, rather than the next fixture of each individual club.
  if(nrow(upcoming) && any(is.finite(upcoming$Round))) {
    first_round <- min(upcoming$Round[is.finite(upcoming$Round)])
    # target_round: project a later round (used mid-round for the NEXT round's preview)
    if (!is.null(target_round) && is.finite(target_round)) first_round <- target_round
    upcoming <- upcoming[upcoming$Round==first_round,,drop=FALSE]
  }
  next_fixtures <- if (nrow(upcoming)) {
    # Each club's very next match. This avoids picking a later appealing fixture.
    x <- rbind(data.frame(Team=upcoming$Home,Opponent=upcoming$Away,
                          Date=upcoming$Date,Game=upcoming$Game,Venue='Home'),
               data.frame(Team=upcoming$Away,Opponent=upcoming$Home,
                          Date=upcoming$Date,Game=upcoming$Game,Venue='Away'))
    x <- x[order(x$Date,suppressWarnings(as.numeric(x$Game))),,drop=FALSE]
    x[!duplicated(x$Team),,drop=FALSE]
  } else data.frame(Team=character(),Opponent=character(),Date=character(),
                    Game=character(),Venue=character())
  # Diagnose name mismatches before attempting candidate selection.
  log_it('Distinct teams in player data: ',length(unique(raw$Team)),
         '; in next fixtures: ',length(unique(next_fixtures$Team)))
  unmatched <- setdiff(unique(raw$Team),unique(next_fixtures$Team))
  if(length(unmatched)) log_it('Teams with NO matching next fixture: ',paste(unmatched,collapse=' | '))
  if (!any(raw$Team %in% next_fixtures$Team)) {
    stop('No team names match between player box scores and schedule. See debug log.')
  }
  # ------------------ PLAYER PIR: SEASON + PERSONAL LAST 3 GAMES ------------------
  # Each player is grouped by team + name + position. This preserves separate
  # histories for a player who transferred clubs; below we keep the latest club.
  # The season PIR is the ordinary arithmetic mean of PIR per player appearance.
  # The last-5 PIR is the ordinary arithmetic mean of the FIVE most recent
  # DISTINCT appearances BY THAT PLAYER (not the opponent's last 5 matches).
  # meta is already ordered latest first by date, with game number as tiebreaker.
  player_summary <- list()
  player_pir_all <- list()  # Every player, even with fewer than 5 appearances.
  keys <- unique(paste(raw$Team,raw$Player,raw$Position,sep='||'))
  for (k in keys) {
    v <- strsplit(k,'||',fixed=TRUE)[[1]]
    d <- raw[raw$Team==v[1] & raw$Player==v[2] & raw$Position==v[3],,drop=FALSE]
    # גם שחקן עם משחק אחד או שניים מופיע בטבלת ממוצעי השחקנים.
    # לשחקן כזה ממוצע "5 אחרונים" יחושב על המשחקים הזמינים בלבד,
    # ועמודת Last5GamesUsed תציין בבירור את גודל המדגם בפועל.
    n <- length(unique(d$Game))
    player_game_ids <- head(meta$Game[meta$Game %in% d$Game],5L)
    last5 <- d[d$Game %in% player_game_ids,,drop=FALSE]
    if (!length(player_game_ids) || !nrow(last5)) next
    row <- data.frame(
      Team=v[1],Player=v[2],Position=v[3],Games=n,
      AvgPIR=mean(d$PIR),Last5AvgPIR=mean(last5$PIR),
      Last5GamesUsed=length(player_game_ids),AvgMinutes=mean(d$Minutes),Last5AvgMinutes=mean(last5$Minutes),
      LastGame=player_game_ids[1],stringsAsFactors=FALSE)
    player_pir_all[[length(player_pir_all)+1L]] <- row
    # רק שחקנים עם חמש הופעות לפחות נכנסים לדירוג ההמלצות.
    if (n >= 5L && length(player_game_ids)==5L)
      player_summary[[length(player_summary)+1L]] <- row
  }
  players <- if(length(player_summary)) do.call(rbind,player_summary) else
    data.frame(Team=character(),Player=character(),Position=character(),Games=integer(),
               AvgPIR=numeric(),Last5AvgPIR=numeric(),Last5GamesUsed=integer(),
               AvgMinutes=numeric(),Last5AvgMinutes=numeric(),LastGame=character())
  # Transfer safety: the eligible history is team-specific, but a player with
  # fewer than 5 appearances at a NEW team must not be recommended for the OLD team.
  # Determine most recent observed team using ALL player records (including <5).
  # This does not borrow statistics from the former club.
  latest_team_by_player <- setNames(character(),character())
  if (nrow(raw)) {
    recent_player_rows <- merge(raw[,c('Game','Player','Team'),drop=FALSE],
      meta[,c('Game','GameDate'),drop=FALSE],by='Game',all.x=TRUE,sort=FALSE)
    recent_player_rows <- recent_player_rows[order(recent_player_rows$GameDate,
      suppressWarnings(as.numeric(recent_player_rows$Game)),decreasing=TRUE),,drop=FALSE]
    recent_player_rows <- recent_player_rows[!duplicated(recent_player_rows$Player),,drop=FALSE]
    latest_team_by_player <- stats::setNames(recent_player_rows$Team,recent_player_rows$Player)
  }
  if (nrow(players)) {
    eligible_latest <- !is.na(latest_team_by_player[players$Player]) &
       players$Team == unname(latest_team_by_player[players$Player])
    players <- players[eligible_latest,,drop=FALSE]
  }
  # The current team is inferred by the latest completed match in the schedule.
  # Exclude prior clubs for transferred players rather than double-recommend them.
  if (nrow(players)) {
    game_order <- match(players$LastGame,meta$Game)
    for (nm in unique(players$Player)) {
      ix <- which(players$Player==nm)
      if (length(ix)>1) {
        keep <- ix[which.min(game_order[ix])]
        players <- players[-setdiff(ix,keep),,drop=FALSE]
        game_order <- match(players$LastGame,meta$Game)
      }
    }
  }
  # בנפרד מדירוג ההמלצות, בונים סיכום PIR עבור כל השחקנים שנקלטו.
  # אם שחקן עבר קבוצה, נציג רק את קבוצתו האחרונה לפי המשחק האחרון.
  all_players <- if(length(player_pir_all)) do.call(rbind,player_pir_all) else players
  if (nrow(all_players)) {
    for (nm in unique(all_players$Player)) {
      ix <- which(all_players$Player==nm)
      if (length(ix)>1L) {
        order_ix <- match(all_players$LastGame[ix],meta$Game)
        keep <- ix[which.min(order_ix)]
        all_players <- all_players[-setdiff(ix,keep),,drop=FALSE]
      }
    }
  }
  recommendations <- list()
  league_position_baseline <- sapply(c('Guard','Forward','Center'),function(p) {
    d <- raw[raw$Position==p,,drop=FALSE]
    if(nrow(d)) sum(d$PIR_x_Minutes)/sum(d$Minutes) else NA_real_
  })
  if (nrow(players) && nrow(next_fixtures)) for (i in seq_len(nrow(players))) {
    a <- players[i,,drop=FALSE]
    fixture <- next_fixtures[next_fixtures$Team==a$Team,,drop=FALSE]
    if (!nrow(fixture) || !is.finite(a$AvgMinutes) || a$AvgMinutes < 10) next
    opp <- fixture$Opponent[1]; pos <- a$Position
    same <- players[players$Team==a$Team & players$Position==pos,,drop=FALSE]
    same <- same[same$AvgMinutes > 0,,drop=FALSE]
    if (!nrow(same)) next
    # Same-position teammate data is used ONLY to compare minutes, never PIR.
    team_max_minutes <- max(same$AvgMinutes)
    d <- raw[raw$Opponent==opp & raw$Position==pos,,drop=FALSE]
    recent_d <- recent_raw[recent_raw$Opponent==opp & recent_raw$Position==pos,,drop=FALSE]
    if (!nrow(d) || !is.finite(league_position_baseline[pos]) ||
        league_position_baseline[pos] <= 0 || length(unique(d$Game)) < 2) next
    opponent_season <- sum(d$PIR_x_Minutes)/sum(d$Minutes)
    # If last-five-game data is unavailable, use the season baseline as fallback.
    opponent_recent <- if (nrow(recent_d)) sum(recent_d$PIR_x_Minutes)/sum(recent_d$Minutes) else opponent_season
    matchup_season_factor <- max(0.65,min(1.35,opponent_season/league_position_baseline[pos]))
    matchup_recent_factor <- max(0.65,min(1.35,opponent_recent/league_position_baseline[pos]))
    # Ranking model scores. Both include recent-five-game minutes.
    base_pir <- (1-learned['PlayerLast5Weight'])*a$AvgPIR +
       learned['PlayerLast5Weight']*a$Last5AvgPIR
     raw_matchup <- (1-learned['OpponentLast5Weight'])*matchup_season_factor +
       learned['OpponentLast5Weight']*matchup_recent_factor
     matchup_factor <- max(.10,1+learned['OpponentStrength']*(raw_matchup-1))
     minutes_ratio <- min(1,a$AvgMinutes/team_max_minutes)
     recent_max <- max(same$Last5AvgMinutes,na.rm=TRUE)
     last5_minutes_ratio <- min(1,a$Last5AvgMinutes/max(.01,recent_max))
     combined_minutes_ratio <- (1-learned['Last5MinutesWeight'])*minutes_ratio +
       learned['Last5MinutesWeight']*last5_minutes_ratio
     minutes_factor <- 1-learned['MinutesPenalty']*(1-combined_minutes_ratio)
     score_before_minutes <- base_pir*matchup_factor
     home_sign <- if(fixture$Venue[1]=='Home') 1 else -1
     home_factor <- exp(learned['HomeAdvantage']*home_sign)
     score <- as.numeric(score_before_minutes*minutes_factor*home_factor)
     # Model 2: same input features but newly optimized round-by-round weights.
     score_walk <- as.numeric(training$predict(data.frame(
       AvgPIR=a$AvgPIR,Last5AvgPIR=a$Last5AvgPIR,
       MatchupSeasonFactor=matchup_season_factor,
       MatchupLast5Factor=matchup_recent_factor,
       MinutesRatio=minutes_ratio,Last5MinutesRatio=last5_minutes_ratio,
       HomeSign=home_sign),walk_learned))

    # ייצוא: פרטים כלליים + שני חלקי המדד + המקדמים + התוצאה.
    # לא שומרים עמודות Points ישנות המערבבות PIR אישי עם התאמה.
    # נתוני העזר הגולמיים ממשיכים לשמש בחישוב, אך לא מעמיסים על הדוח.
    recommendations[[length(recommendations)+1L]] <- data.frame(
      Player=a$Player,Team=a$Team,Position=pos,Opponent=opp,
      Date=fixture$Date[1],Venue=fixture$Venue[1],
      Score=score,Score_WalkForward=score_walk,AvgPIR=a$AvgPIR,Last5AvgPIR=a$Last5AvgPIR,
      BasePIR=base_pir,AvgMinutes=a$AvgMinutes,Last5AvgMinutes=a$Last5AvgMinutes,Games=a$Games,
      MatchupSeasonFactor=matchup_season_factor,
      MatchupLast5Factor=matchup_recent_factor,
      MatchupFactor=matchup_factor,MinutesRatio=minutes_ratio,
       Last5MinutesRatio=last5_minutes_ratio,MinutesFactor=minutes_factor,
      ScoreBeforeMinutes=score_before_minutes,HomeFactor=as.numeric(home_factor),
      stringsAsFactors=FALSE)
  }
  picks <- if(length(recommendations)) do.call(rbind,recommendations) else
    data.frame(Player=character(),Team=character(),Position=character(),
               Opponent=character(),Date=character(),Venue=character(),
               Score=numeric(),Score_WalkForward=numeric(),AvgPIR=numeric(),Last5AvgPIR=numeric(),
               BasePIR=numeric(),AvgMinutes=numeric(),Last5AvgMinutes=numeric(),Games=integer(),
               MatchupSeasonFactor=numeric(),MatchupLast5Factor=numeric(),
               MatchupFactor=numeric(),MinutesRatio=numeric(),Last5MinutesRatio=numeric(),
               MinutesFactor=numeric(),ScoreBeforeMinutes=numeric(),HomeFactor=numeric())
  picks <- picks[order(-picks$Score,-picks$AvgMinutes,picks$Player),,drop=FALSE]
  picks_walk <- picks[order(-picks$Score_WalkForward,-picks$AvgMinutes,picks$Player),,drop=FALSE]
  top10_walk <- head(picks_walk,10)
  top10_walk <- data.frame(Rank=seq_len(nrow(top10_walk)),top10_walk,row.names=NULL,check.names=FALSE)
  top10 <- head(picks,10)
  top10 <- data.frame(Rank=seq_len(nrow(top10)),top10,row.names=NULL,check.names=FALSE)
  log_it('Upcoming games: ',nrow(upcoming),'; next team fixtures: ',nrow(next_fixtures),
         '; eligible recommendations: ',nrow(picks),'; top picks: ',nrow(top10))

  wb <- openxlsx::createWorkbook()
  sheets <- c('סיכום לפי יריבה','5 משחקים אחרונים','טופ 8 כל העונה',
              'טופ 8 אחרונים','נתוני שחקנים','משחקים אחרונים',
               'טופ 10 המלצות','כל המועמדים','דירוג Score מלא','ממוצעי PIR שחקנים',
               'לוח קרוב','שיטת דירוג','משקלים שנלמדו','בדיקת מודל',
               'Backtest היסטורי','סטטוס','טופ 10 מודל 2','דירוג מודל 2',
                'השוואה לפי מחזור','דיוק טופ 10 ו-30','משקלים מודל 2 לפי מחזור','משקלים מודל 2',
               'טווח טעות דירוג','ניקוי תצפיות')
  for(s in sheets) openxlsx::addWorksheet(wb,s,gridLines=FALSE)
  header <- openxlsx::createStyle(fgFill='#142642',fontColour='#FFFFFF',textDecoration='bold',
                                   halign='center',border='Bottom',borderColour='#5E7694')
  sub <- openxlsx::createStyle(fgFill='#EAF2FB')
  write_table <- function(sheet,df,widths=NULL,score_cols=NULL,filter=TRUE) {
    openxlsx::writeData(wb,sheet,df,headerStyle=header,withFilter=filter)
    if (!is.null(widths)) openxlsx::setColWidths(wb,sheet,cols=seq_along(widths),widths=widths)
    if (nrow(df)>0) {
      openxlsx::addStyle(wb,sheet,sub,rows=seq(2,nrow(df)+1,by=2),
                         cols=seq_len(ncol(df)),gridExpand=TRUE,stack=TRUE)
      if (!is.null(score_cols)) {
        fmt <- openxlsx::createStyle(numFmt='0.00')
        openxlsx::addStyle(wb,sheet,fmt,rows=2:(nrow(df)+1),cols=score_cols,
                           gridExpand=TRUE,stack=TRUE)
        openxlsx::conditionalFormatting(wb,sheet,cols=score_cols,
          rows=2:(nrow(df)+1),type='colourScale',
          style=c('#F6CACA','#FFF0AD','#A9E2C2'))
      }
    }
    openxlsx::freezePane(wb,sheet,firstActiveRow=2)
  }
  write_table('סיכום לפי יריבה',summary,c(36,19,19,19),2:4)
  write_table('5 משחקים אחרונים',recent_summary,c(36,19,19,19),2:4)
  write_table('טופ 8 כל העונה',top8_all,c(10,36,16,19,12,22,19),4)
  write_table('טופ 8 אחרונים',top8_recent,c(10,36,16,19,12,22,19),4)
  write_table('נתוני שחקנים',raw,c(12,32,30,16,34,14,12,18),c(6,7))
  write_table('משחקים אחרונים',recent_games,c(38,16,20))
  write_table('טופ 10 המלצות',top10,rep(20,ncol(top10)),which(names(top10)=='Score'))
  write_table('כל המועמדים',picks,rep(20,ncol(picks)),which(names(picks)=='Score'))
  write_table('טופ 10 מודל 2',top10_walk,rep(20,ncol(top10_walk)),which(names(top10_walk)=='Score_WalkForward'))
  write_table('דירוג מודל 2',data.frame(Rank=seq_len(nrow(picks_walk)),picks_walk),
              rep(20,ncol(picks_walk)+1),which(names(picks_walk)=='Score_WalkForward')+1)
  if(nrow(training$error_summary)) write_table('טווח טעות דירוג',training$error_summary,
    rep(20,ncol(training$error_summary)))
  if(nrow(training$round_metrics)) {
    write_table('השוואה לפי מחזור',training$round_metrics,
      rep(20,ncol(training$round_metrics)))
    top_accuracy <- training$round_metrics[,c('Round','Model','EvaluationType','N',
      'Top10Hits','Top10Eligible','Top10Overlap','Top30Hits','Top30Eligible','Top30Overlap'),drop=FALSE]
    write_table('דיוק טופ 10 ו-30',top_accuracy,rep(21,ncol(top_accuracy)),
      which(names(top_accuracy) %in% c('Top10Hits','Top30Hits')))
  }
  if(nrow(training$walk_coefficients)) write_table('משקלים מודל 2 לפי מחזור',
    training$walk_coefficients,rep(22,ncol(training$walk_coefficients)))
  write_table('משקלים מודל 2',data.frame(Parameter=names(walk_learned),
    Value=unname(walk_learned)),c(35,20),2)
  # Full Score ranking for every candidate satisfying the existing eligibility rules.
  all_scores <- data.frame(Rank=seq_len(nrow(picks)),
                           picks[,c('Player','Team','Position','Opponent','Date',
                                    'Score','Score_WalkForward','Games','AvgPIR','Last5AvgPIR','BasePIR',
                                    'AvgMinutes','MatchupSeasonFactor','MatchupLast5Factor',
                                    'MatchupFactor','MinutesRatio','MinutesFactor',
                                    'ScoreBeforeMinutes','Last5AvgMinutes','Last5MinutesRatio','HomeFactor'),drop=FALSE],
                           row.names=NULL,check.names=FALSE)
  write_table('דירוג Score מלא',all_scores,rep(20,ncol(all_scores)),
              which(names(all_scores)=='Score'))
  # טבלה לכל שחקן: ממוצע העונה, ממוצע עד 3 משחקיו האחרונים וגודל המדגם.
  # Exclude LastGame: it is only an internal key used for transfer resolution.
  player_pir_table <- all_players[,c('Player','Team','Position','Games',
                                  'AvgPIR','Last5AvgPIR','Last5GamesUsed','AvgMinutes','Last5AvgMinutes'),drop=FALSE]
  player_pir_table <- player_pir_table[order(-player_pir_table$Last5AvgPIR,
                                                 -player_pir_table$AvgPIR),,drop=FALSE]
  write_table('ממוצעי PIR שחקנים',player_pir_table,
              rep(20,ncol(player_pir_table)),c(5,6))
  write_table('לוח קרוב',next_fixtures,c(34,34,17,15,15))
  # פירוט מלא של כל שלב בנוסחה החדשה (ללא ספירה כפולה של PIR).
  method <- data.frame(Item=c('Target','Ranking universe','Model 1',
     'Model 2','Chronology','Recent minutes','Ranking loss','Comparison','Reported metrics',
     'Player eligibility','Important limitation'),Explanation=c(
     'Rank players by actual game PIR; numerical PIR prediction error is NOT optimized.',
     'League-wide players eligible in each complete competition round.',
     'Fixed model trained on 2024/25 and 2025/26 only; frozen across 2026/27 backtest.' ,
     'Recency-weighted model trained on 2024/25 and 2025/26 only; weights frozen during 2026/27 evaluation.',
     'Historical snapshots use strictly earlier round numbers and earlier dates to prevent delayed-match leakage.',
     'Last5AvgMinutes from last 5 player appearances, plus recent/season minute share relative to eligible same-position teammates.',
     'Venue uses exp(HomeAdvantage * HomeSign); no team-form factor. Both models optimize top-10 x7 / ranks 11-30 x4 / others x1 weighted normalized absolute rank error. Model 2 also penalizes unstable coefficients.',
     'Both models evaluated chronologically out-of-sample in 2026/27, with frozen trained weights and strictly prior-round features.',
     'NDCG@10, Spearman, Top10/Top30 hits; report conditional played>=1min AND full pregame candidate universe.',
     'At least 5 previous appearances for current club, >=10 average minutes; training targets require >=1 played minute.',
     'Training excludes nonparticipants/<1 minute; conditional and full-candidate 2026/27 metrics BOTH reported; player availability is not known at prediction time.'),
     stringsAsFactors=FALSE)
  weight_table <- data.frame(Parameter=names(learned),Value=unname(learned),
                             stringsAsFactors=FALSE)
  write_table('משקלים שנלמדו',weight_table,c(34,22),2)
  if(ncol(training$history) && 'CleanReason' %in% names(training$history)) {
    cleaning <- as.data.frame(table(training$history$Round,training$history$CleanReason),
                              stringsAsFactors=FALSE)
    names(cleaning) <- c('Round','Reason','Count')
    write_table('ניקוי תצפיות',cleaning,c(18,32,16))
  }
  write_table('בדיקת מודל',training$checks,c(42,48),filter=FALSE)
  if (ncol(training$history))
    write_table('Backtest היסטורי',training$history,
                rep(21,ncol(training$history)),
                which(names(training$history) %in% c('PIR','Model1Score','FixedScore','WalkForwardScore')))
  write_table('שיטת דירוג',method,c(30,110),filter=FALSE)
  status <- data.frame(Item=c('Season','Training seasons','Training model file','Player rows','Successful box scores',
                              'Failed box scores','Recent window per opponent',
                              'Calculation','Upcoming fixture count','Recommendation candidates','Debug file'),
                       Value=c(season,'E2024; E2025',trained_model_file,nrow(raw),processed,length(fail),'5 most recent played games',
                               'sum(PIR * minutes) / sum(minutes)',nrow(next_fixtures),nrow(picks),debug_path))
  status <- rbind(status,data.frame(Item=paste0('Position rows: ',names(counts)),
                                    Value=as.character(as.integer(counts))))
  write_table('סטטוס',status,c(37,70),filter=FALSE)
  openxlsx::saveWorkbook(wb,output,overwrite=TRUE)
  write.csv(raw,csv_path,row.names=FALSE,fileEncoding='UTF-8')
  write.csv(top10,picks_path,row.names=FALSE,fileEncoding='UTF-8')
  write.csv(top10_walk,file.path(output_dir,'EuroLeague_2026-27_top10_walkforward.csv'),
    row.names=FALSE,fileEncoding='UTF-8')
  if(nrow(training$round_metrics)) write.csv(training$round_metrics,
    file.path(output_dir,'EuroLeague_2026-27_round_comparison.csv'),
    row.names=FALSE,fileEncoding='UTF-8')
  log_it('Created ',output,' with ',nrow(raw),' rows and 2 Top 8 rankings; position counts: ',paste(names(counts),as.integer(counts),collapse=', '))
  finish_log()
  cat('\nFiles saved in: ', output_dir, '\n', sep='')
  cat('Excel: ', output, '\nCSV: ', csv_path, '\nTop 10 CSV: ',picks_path, '\nDebug: ', debug_path, '\n', sep='')
  invisible(list(excel=output, csv=csv_path, top10_csv=picks_path, top10=top10, top10_walk=top10_walk,
                 candidates=picks, next_fixtures=next_fixtures, debug=debug_path,
                 player_rows=nrow(raw), top8_all=top8_all, top8_recent=top8_recent,
                 calibration=training))

}
