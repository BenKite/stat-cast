# stat-cast
Analysis of Statcast data from baseballsavant.mlb.com.

This repo is a work in progress, therefore the code provided here may
not run completely on your machine.

Currently the bulk of the work here is for downloading data on
pitchers and using it to generate a scouting report.

My workflow to generate the report is:

Python/stat-cast-request.py
Python/handedness.py
R/pitcher_summary.R
Python/player_images.py
pitcher_report.R

Once the scouting.tex file is written I compile it with pdflatex

