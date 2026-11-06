# STEP 1 (infrequent): train TWO weight sets using ALL eligible completed rounds
# from seasons E2024 (2024/25) and E2025 (2025/26).
# Run source('.../EuroLeague_ranking_models_v12.R') first.
train_euroleague_historical_models <- function(output_dir,force_download=FALSE) {
  if(!exists('el_fit_historical_weights',mode='function') ||
     !exists('build_euroleague_2026_27',mode='function'))
    stop('Source EuroLeague_ranking_models_v12.R first')
  dir.create(output_dir,recursive=TRUE,showWarnings=FALSE)
  output_dir <- normalizePath(output_dir,winslash='/',mustWork=TRUE)
  cache_dir <- file.path(output_dir,'EuroLeague_training_cache_v12_fantasy')
  dir.create(cache_dir,recursive=TRUE,showWarnings=FALSE)
  seasons <- c('E2024','E2025')
  histories <- vector('list',length(seasons))
  download_status <- vector('list',length(seasons))
  for(i in seq_along(seasons)) {
    season <- seasons[i]
    cache_path <- file.path(cache_dir,paste0(season,'_complete_boxscores.rds'))
    if (force_download || !file.exists(cache_path)) {
      message('Downloading full EuroLeague season ',season)
      season_dir <- file.path(cache_dir,season)
      dat <- build_euroleague_2026_27(output_dir=season_dir,
                                      season=season,mode='collect')
      saveRDS(dat,cache_path)
    } else {
      message('Reading cached EuroLeague season ',season)
      dat <- readRDS(cache_path)
    }
    if(!identical(dat$season,season)) stop('Wrong season in cached data: ',cache_path)
    if(!length(dat$complete_rounds)) stop('No complete rounds in ',season)
    # Prevent silent incomplete-season training if the API returned a partial season.
    scheduled <- dat$scheduled_rounds
    if (is.null(scheduled) || !length(scheduled) ||
        !setequal(dat$complete_rounds,scheduled))
      stop('Incomplete season ',season,': expected rounds ',
           paste(scheduled,collapse=','),' but completed ',
           paste(dat$complete_rounds,collapse=','),
           '. Re-run with force_download=TRUE or inspect debug log.')
    if (length(dat$complete_rounds)<30L)
      stop('Too few complete rounds for ',season,': ',length(dat$complete_rounds),
           '. Check API coverage and saved debug logs.')
    snap <- el_fit_historical_weights(dat$raw,dat$meta,
       complete_rounds=dat$complete_rounds,features_only=TRUE)
    if (is.null(snap$history) || !nrow(snap$history))
      stop('No eligible training features for ',season)
    h <- snap$history
    h$Season <- season
    h$SeasonRound <- h$Round
    # Unique chronological round IDs across seasons. Features stay season-specific.
    h$Round <- (i-1L)*100L+h$SeasonRound
    histories[[i]] <- h
    download_status[[i]] <- data.frame(Season=season,
      CompleteRounds=length(dat$complete_rounds),
      EligibleRounds=length(unique(h$SeasonRound)),
      TrainingCandidates=sum(h$CleanEligible),ExcludedCandidates=sum(!h$CleanEligible),
      SuccessfulBoxscores=dat$successful_boxscores,
      FailedBoxscores=dat$failed_boxscores)
  }
  hist <- do.call(rbind,histories)
  cleaning_summary <- as.data.frame(table(hist$Season,hist$CleanReason),stringsAsFactors=FALSE)
  names(cleaning_summary) <- c('Season','Reason','Candidates')
  write.csv(cleaning_summary,file.path(output_dir,'EuroLeague_v12_fantasy_training_cleaning.csv'),row.names=FALSE)
  rownames(hist) <- NULL
  message('Optimizing both weight sets on ',nrow(hist),
          ' player-round observations across ',length(unique(hist$Round)),' rounds.')
  fit <- el_fit_historical_weights(raw=NULL,meta=NULL,
                                    historical_features=hist)
  if (!identical(fit$status,'Model 1 optimized on all available rounds; Model 2 walk-forward'))
    stop('Training did not finish successfully: ',fit$status)
  stopifnot(!('TeamFormStrength' %in% names(fit$weights)),
            !('TeamFormStrength' %in% names(fit$walk_weights)))
  artifact <- list(version=12L,target='sport5 fantasy points',seasons=seasons,trained_at=Sys.time(),
    weights=fit$weights,walk_weights=fit$walk_weights,
    training_candidates=sum(hist$CleanEligible),training_candidates_before_cleaning=nrow(hist),training_rounds=sort(unique(hist$Round)),
    season_status=do.call(rbind,download_status),
    training_checks=fit$checks)
  model_file <- file.path(output_dir,'EuroLeague_historical_models_2024_2026_v12_fantasy.rds')
  saveRDS(artifact,model_file)
  write.csv(data.frame(Parameter=names(fit$weights),
    Model1=as.numeric(fit$weights),Model2=as.numeric(fit$walk_weights)),
    file.path(output_dir,'EuroLeague_historical_weights_2024_2026_v12_fantasy.csv'),
    row.names=FALSE,fileEncoding='UTF-8')
  write.csv(artifact$season_status,
    file.path(output_dir,'EuroLeague_historical_training_coverage_v12_fantasy.csv'),
    row.names=FALSE,fileEncoding='UTF-8')
  # Optional training diagnostics, kept SEPARATE from 2026/27 OOS evaluation.
  if(nrow(fit$round_metrics)) write.csv(fit$round_metrics,
    file.path(output_dir,'EuroLeague_historical_training_diagnostics_v12_fantasy.csv'),
    row.names=FALSE,fileEncoding='UTF-8')
  message('Training artifact saved: ',model_file)
  invisible(list(model_file=model_file,weights=fit$weights,
                 walk_weights=fit$walk_weights,coverage=artifact$season_status))
}
