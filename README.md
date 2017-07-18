# stat-cast
Analysis of Statcast data from baseballsavant.mlb.com.

This repo is a work in progress, therefore the code provided here may
not run completely on your machine.

Currently the bulk of the work here is for downloading data on
pitchers and using it to generate a scouting report.

My workflow to generate the report is:

Python/stat-cast-request.py

R/pitcher_summary.R

Python/player_images.py

R/pitcher_report.R

Once the scouting.tex file is written I compile it with pdflatex. 
I complie it twice to ensure that the table of contents is
correct. There are usually a few files missing when compiling (e.g., a
pitcher with one appearance and no balls put into play).

