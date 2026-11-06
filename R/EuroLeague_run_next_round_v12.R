# STEP 2 (every round): download ONLY 2026/27, evaluate all complete
# 2026/27 rounds out-of-sample, and produce the original Excel and CSV outputs.
# Source this file after EuroLeague_ranking_models_v12.R.
run_euroleague_next_round <- function(output_dir) {
  if (!exists('build_euroleague_2026_27',mode='function'))
    stop('Source EuroLeague_ranking_models_v12.R first')
  build_euroleague_2026_27(output_dir=output_dir,season='E2026',mode='predict',
    trained_model_file=file.path(output_dir,'EuroLeague_historical_models_2024_2026_v12_fantasy.rds'))
}
