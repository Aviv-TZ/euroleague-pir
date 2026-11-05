@echo off
REM Windows alternative to GitHub: schedule this file hourly in Task Scheduler.
REM Edit the paths below. index.html is self-contained, so it opens directly from the share.
set EL_CODE_DIR=C:\PATH\TO\EL\EL_web\R
set EL_OUTPUT_DIR=C:\PATH\TO\EL\NEW
set EL_SITE_DIR=C:\PATH\TO\EL\site
"C:\Program Files\R\R-4.4.1\bin\Rscript.exe" "%EL_CODE_DIR%\EuroLeague_auto_update_v11.R" %* >> "%EL_SITE_DIR%\update_log.txt" 2>&1
