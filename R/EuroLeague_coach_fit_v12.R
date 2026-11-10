# EuroLeague v12 - COACH MODEL FIT (expected game margin)
# Builds pre-game team features from EARLIER games only (same season) and fits
# margin(home - away) on them. Feature sets are compared by out-of-sample error:
#   CV  = leave-one-season-out over 2024/25 and 2025/26 (regular season)
#   TEST = 2026/27 games so far (never used for fitting the final weights)
# Usage: source('EuroLeague_auto_update_v12.R'); source(this); el_coach_fit(cache_dir, players_csv)

el_coach_games <- function(season, raw) {
  s <- el_schedule(season)
  s <- s[s$Played & is.finite(s$HomeScore) & is.finite(s$AwayScore) & nzchar(s$Home),,drop=FALSE]
  s <- s[order(s$Utc, suppressWarnings(as.numeric(s$Game))),,drop=FALSE]
  fp <- aggregate(PIR ~ Game + Team, raw, sum)          # team fantasy points in each game
  key <- paste(fp$Game, fp$Team)
  s$fpH <- fp$PIR[match(paste(s$Game, s$Home), key)]
  s$fpA <- fp$PIR[match(paste(s$Game, s$Away), key)]
  s$Season <- season
  s[is.finite(s$fpH) & is.finite(s$fpA),,drop=FALSE]
}

# Pre-game features: everything from the team's earlier games in the same season.
el_coach_features <- function(g, min_games=5) {
  out <- list()
  hist <- list()   # team -> data.frame of past games (margin, fpFor, fpAgainst, home)
  for (i in seq_len(nrow(g))) {
    h <- g$Home[i]; a <- g$Away[i]
    H <- hist[[h]]; A <- hist[[a]]
    if (!is.null(H) && !is.null(A) && nrow(H) >= min_games && nrow(A) >= min_games) {
      l5 <- function(d) tail(d, 5)
      m <- function(d, col) mean(d[[col]])
      out[[length(out)+1L]] <- data.frame(Season=g$Season[i], Round=g$Round[i], Game=g$Game[i], Home=h, Away=a,
        margin = g$HomeScore[i] - g$AwayScore[i],
        d_season = m(H,'mg') - m(A,'mg'),
        d_last5  = m(l5(H),'mg') - m(l5(A),'mg'),
        d_split  = (if (sum(H$home) >= 2) mean(H$mg[H$home]) else m(H,'mg')) -
                   (if (sum(!A$home) >= 2) mean(A$mg[!A$home]) else m(A,'mg')),      # home team at home vs away team away
        d_fpfor  = m(H,'ff') - m(A,'ff'),
        d_fpfor5 = m(l5(H),'ff') - m(l5(A),'ff'),
        d_fpag   = m(H,'fa') - m(A,'fa'),            # higher = worse defence
        d_fpag5  = m(l5(H),'fa') - m(l5(A),'fa'))
    }
    add <- function(t, mg, ff, fa, home) hist[[t]] <<- rbind(hist[[t]], data.frame(mg=mg, ff=ff, fa=fa, home=home))
    add(h, g$HomeScore[i] - g$AwayScore[i], g$fpH[i], g$fpA[i], TRUE)
    add(a, g$AwayScore[i] - g$HomeScore[i], g$fpA[i], g$fpH[i], FALSE)
  }
  do.call(rbind, out)
}

el_coach_fit <- function(cache_dir, players_csv) {
  regular <- c(E2024=34, E2025=38, E2026=99)
  all <- list()
  for (season in c('E2024','E2025')) {
    dat <- readRDS(file.path(cache_dir, paste0(season, '_complete_boxscores.rds')))
    g <- el_coach_games(season, dat$raw); g <- g[g$Round <= regular[[season]],,drop=FALSE]
    all[[season]] <- el_coach_features(g)
  }
  raw26 <- utils::read.csv(players_csv, fileEncoding='UTF-8', stringsAsFactors=FALSE); raw26$Game <- as.character(raw26$Game)
  all[['E2026']] <- el_coach_features(el_coach_games('E2026', raw26))
  d <- do.call(rbind, all)
  feats <- c('d_season','d_last5','d_split','d_fpfor','d_fpfor5','d_fpag','d_fpag5')
  train <- d[d$Season != 'E2026',]; test <- d[d$Season == 'E2026',]
  cv_mae <- function(vars) {          # leave-one-season-out on the two training seasons
    e <- c(); for (s in unique(train$Season)) {
      tr <- train[train$Season != s,]; te <- train[train$Season == s,]
      f <- if (length(vars)) stats::lm(stats::reformulate(vars, 'margin'), tr) else stats::lm(margin ~ 1, tr)
      e <- c(e, abs(stats::predict(f, te) - te$margin)) }
    mean(e) }
  # forward selection by CV error
  chosen <- c(); path <- data.frame(step=0, added='(home court only)', cv_mae=cv_mae(c()))
  repeat {
    left <- setdiff(feats, chosen); if (!length(left)) break
    sc <- vapply(left, function(v) cv_mae(c(chosen, v)), 0)
    best <- names(which.min(sc))
    if (sc[[best]] >= tail(path$cv_mae, 1) - 0.01) break       # stop when it no longer helps
    chosen <- c(chosen, best); path <- rbind(path, data.frame(step=length(chosen), added=best, cv_mae=sc[[best]]))
  }
  final <- stats::lm(stats::reformulate(if (length(chosen)) chosen else '1', 'margin'), train)
  base_pred <- function(x) 0.6*x$d_season + 2.5                      # the original formula
  list(n=table(d$Season), path=path, chosen=chosen, coef=stats::coef(final), model=final,
       test_mae_model=mean(abs(stats::predict(final, test) - test$margin)),
       test_mae_base=mean(abs(base_pred(test) - test$margin)), data=d,
       all_coef=stats::coef(stats::lm(stats::reformulate(feats, 'margin'), train)))
}
