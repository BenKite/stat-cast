head## Ben Kite

## install.packages("plyr")
library(plyr)

## install.packages("xtable")
library(xtable)

patdir <- "../data/batter/plots"
rdir <- "../report"

pfiles <- list.files(patdir)

## Try by team instead

teamdat <- read.csv("../data/teamdat_batting.csv", stringsAsFactors = FALSE)

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
\\title{MLB Batters Scouting Report}
\\author{Ben Kite}
\\maketitle

\\newpage
This is the first draft of my batter's scouting report. For now I am keeping things simple and just reporting information about a batters ability to make contact, ground ball spray percentages, and line drive spray percentages. I am providing this information for left-handed and right-handed pitchers. I am going to add more information to help inform how a pitcher should attack each batter, and how the defense should position itself to be in the best position to field ground balls and line drives.
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
\\input{../data/teaminfo/TEAM_batting.tex}
\\end{table}
"


pchunk <- "
\\newpage
\\subsection{FNOMBRE}
\\noindent\\rule{6.5in}{0.4pt}

\\begin{figure}[H]
\\centering
\\includegraphics[width=2in,height=2in,keepaspectratio]{../data/batters/plots/NAME/headshot.png}
\\end{figure}

\\newpage
\\subsection*{FNOMBRE Against Left Handed Pitchers}
\\noindent\\rule{6.5in}{0.4pt}
\\subsubsection*{Contact Percentage on Swings}
\\begin{table}[H]
\\input{../data/batters/plots/NAME/contact_L.tex}
\\end{table}
\\begin{figure}[H]
\\centering
\\includegraphics[scale=0.45]{../data/batters/plots/NAME/ground_ball_L.pdf}\\includegraphics[scale=0.45]{../data/batters/plots/NAME/line_drive_L.pdf}
\\end{figure}

\\newpage
\\subsection*{FNOMBRE Against Right Handed Pitchers}
\\noindent\\rule{6.5in}{0.4pt}
\\subsubsection*{Contact Percentage on Swings}
\\begin{table}[H]
\\input{../data/batters/plots/NAME/contact_R.tex}
\\end{table}
\\begin{figure}[H]
\\centering
\\includegraphics[scale=0.45]{../data/batters/plots/NAME/ground_ball_R.pdf}\\includegraphics[scale=0.45]{../data/batters/plots/NAME/line_drive_R.pdf}
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
        tmpx <- read.csv(paste0("../data/teaminfo/", t, "_batting.csv"), stringsAsFactors = FALSE)
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

    fileConn<-file(paste0(rdir, "/scouting_batting.tex"))
    writeLines(doc, fileConn)
    close(fileConn)
}

reportMaker("KCR")
