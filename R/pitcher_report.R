## Ben Kite

## install.packages("plyr")
library(plyr)

## install.packages("xtable")
library(xtable)

patdir <- "../data/pitchers/plots"
rdir <- "../report"

pfiles <- list.files(patdir)

## Try by team instead

teamdat <- read.csv("../data/teamdat.csv", stringsAsFactors = FALSE)

header <-"
\\documentclass[english]{article}
\\usepackage[T1]{fontenc}
\\usepackage[latin9]{inputenc}
\\usepackage{geometry}
\\geometry{verbose,tmargin=1in,bmargin=1in}
\\usepackage{float}
\\usepackage{graphicx}
\\usepackage{babel}
\\usepackage{titlesec}
\\usepackage{titletoc}

\\usepackage{hyperref}
\\hypersetup{
    colorlinks,
    citecolor=blue,
    filecolor=blue,
    linkcolor=blue,
    urlcolor=blue
}
\\begin{document}
\\title{MLB Pitchers Scouting Report}
\\author{Ben Kite}
\\maketitle

\\newpage
This document contains basic information summarizing pitchers in 2018.
All data are from pitches thrown in 2018 captured by Statcast. The data were collected from baseballsavant.mlb.com.
Currently, each pitcher featured has two pages in the report.
The first page contains tables summarizing the types of pitches each player has thrown, how often, average release speed, and average spin rate.
The averages for release speed and spin rate also have the pitcher's percentile reported for reference.
The percentile indicates what percentage of pitchers have an average speed or spin rate lower than the given pitcher (rounding will cause percentiles near 100 to display as 100).
Having a higher percentile is not always better (e.g., percentile for spin rate on a knuckle ball), but these values do indicate how the given pitcher compares to others.
This is especially useful for the spin rate information provided by Statcast, which is in rotations per minute units.
There are also tables providing information about how pitch selection changes based on the count, and how often balls hit into play are ground balls, line drives, flyballs, and popups.\\par
The second page provides a plot showing the release points for each of the pitches in the player's arsenal, and then two plots showing pitch location relative to the strike zone for right and left handed batters.
The release point plots show the average release point from the view of home plate.
The pitchers mound was drawn on manually to provide a concrete reference point for the release points.
The points are designed to show where pitches are typically released, and to indicate how consistent average release points are across pitch types.
In the pitch location plots, darker shades of red indicate a higher concentration of pitches. The black dots in the middle of the light red circles indicate pitch locations that are atypical. I am always looking to make improvements; feel free to email suggestions to: kitebena@gmail.com
\\newpage
\\tableofcontents
\\thispagestyle{empty}
"

footer <- "
\\end{document}
"

tchunk <- "
\\newpage
\\section{FANCYT}
\\begin{figure}[H]
\\centering
\\includegraphics[width=2in,height=2in,keepaspectratio]{../images/TEAM.png}
\\end{figure}
\\begin{table}[H]
\\input{../data/teaminfo/TEAM.tex}
\\end{table}
"


pchunk <- "
\\newpage
\\subsection{FNOMBRE}
\\noindent\\rule{6.5in}{0.4pt}

\\begin{figure}[H]
\\centering
\\includegraphics[width=2in,height=2in,keepaspectratio]{../data/pitchers/plots/NAME/headshot.png}
\\end{figure}

\\subsubsection*{Pitches}

\\begin{table}[H]
\\centering
\\input{../data/pitchers/plots/NAME/table1.tex}
\\end{table}

\\subsubsection*{Pitch Preferences Conditional on Count}

\\begin{table}[H]
\\centering
\\input{../data/pitchers/plots/NAME/table2.tex}
\\end{table}

\\subsubsection*{Balls in Play Percentages}

\\begin{table}[H]
\\centering
\\input{../data/pitchers/plots/NAME/table3.tex}
\\end{table}

\\newpage

\\subsection*{FNOMBRE}
\\noindent\\rule{6.5in}{0.4pt}
\\subsubsection*{Release Points}

\\begin{figure}[H]
\\centering
\\includegraphics[scale=0.60]{../data/pitchers/plots/NAME/plot2.pdf}
\\end{figure}

\\subsubsection*{Location over Plate}
\\begin{figure}[H]
\\includegraphics[scale=0.45]{../data/pitchers/plots/NAME/plot3.pdf}\\includegraphics[scale=0.45]{../data/pitchers/plots/NAME/plot4.pdf}

\\end{figure}

"

teamdat <- teamdat[teamdat$Team != "MON",]

ugnames <- sort(unique(teamdat$Team))
fancynames <- c("Arizona Diamondbacks",
                "Atlanta Braves",
                "Baltimore Orioles",
                "Boston Red Sox",
                "Chicago Cubs",
                "Chicago White Sox",
                "Cincinnati Reds",
                "Cleveland Indians",
                "Colorado Rockies",
                "Detroit Tigers",
                "Houston Astros",
                "Kansas City Royals",
                "Los Angeles Angels of Anaheim",
                "Los Angeles Dodgers",
                "Miami Marlins",
                "Milwaukee Brewers",
                "Minnesota Twins",
                "New York Mets",
                "New York Yankees",
                "Oakland A's",
                "Philadelphia Phillies",
                "Pittsburgh Pirates",
                "San Diego Padres",
                "Seattle Mariners",
                "San Franciso Giants",
                "St. Louis Cardinals",
                "Tampa Bay Rays",
                "Texas Rangers",
                "Toronto Blue Jays",
                "Washington Nationals")

names(fancynames) <- ugnames


reportMaker <- function(teams){
    ugnames <- teams

    tbody <- list()
    for (t in ugnames){
        tmptchunk <- gsub("FANCYT", fancynames[t], tchunk)
        tmptchunk <- gsub("TEAM", t, tmptchunk)
        tmpx <- read.csv(paste0("../data/teaminfo/", t, ".csv"), stringsAsFactors = FALSE)
        players <- tmpx$StatcastName
        players <- gsub(" ", "_", players)
        body <- list()
        for (p in players){
            fname <- gsub("_", " ", p)
            tmpchunk <- gsub("NAME", p, pchunk)
            tmpchunk <- gsub("FNOMBRE", fname, tmpchunk)
            body[[p]] <- tmpchunk
        }
        middle <- paste0(body, collapse = "\n")
        tbody[[t]] <- paste0(tmptchunk, middle, collapse = "\n")
    }

    tmiddle <- paste0(tbody, collapse = "\n")

    doc <- paste0(header, tmiddle, footer, collapse = "\n")

    if(dir.exists(rdir) == FALSE){
        dir.create(rdir)
    }

    fileConn<-file(paste0(rdir, "/scouting.tex"))
    writeLines(doc, fileConn)
    close(fileConn)
}

## Specify what teams should be included in the report

reportMaker(ugnames)
