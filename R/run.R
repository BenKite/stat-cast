## This runs the report generating process.

system("python ../Python/get-team-info.py")
system("python ../Python/stat-cast-request.py")
source("pitcher_summary.R")
system("python ../Python/player-images.py")
source("pitcher_report.R")


