## Ben Kite

## install.packages("plyr")
library(plyr)

## install.packages("xtable")
library(xtable)

datdir <- "../data/pitchers/"

pfiles <- list.files(datdir, pattern = ".csv")

## Be patient with this line
## It reads in over 500 .csv files
pdat <- lapply(paste0(datdir, pfiles), read.csv, stringsAsFactors = FALSE)

## Now make a single data.frame
dat <- do.call("rbind.fill", pdat)

dnames <- names(dat)

## Again, be patient
dat <- apply(dat, 2, function(x) ifelse(x == "null", NA, x))
dat <- data.frame(dat, stringsAsFactors = FALSE)

names(dat) <- dnames

## Just want to reformat release point information, speed, and date
dat$release_pos_x <- as.numeric(dat$release_pos_x)
dat$release_pos_z <- as.numeric(dat$release_pos_z)

dat$plate_x <- as.numeric(dat$plate_x)
dat$plate_z <- as.numeric(dat$plate_z)

dat$sz_top <- as.numeric(dat$sz_top)
dat$sz_bot <- as.numeric(dat$sz_bot)

dat$release_speed <- as.numeric(dat$release_speed)

dat$game_date <- as.Date(dat$game_date, "%m/%d/%y")

pitches <- unique(dat$pitch_type)
pitches <- pitches[!is.na(pitches)]
pitches <- pitches[pitches != ""]
pitches <- pitches[!pitches %in% c("IN", "EP", "PO", "FO", "AB", "UN")]

dat <- dat[dat$pitch_type %in% pitches,]

ptypes <- c("FF" = "Four Seam",
            "FT" = "Two Seam",
            "SL" = "Slider",
            "CH" = "Change-Up",
            "CU" = "Curveball",
            "FC" = "Cutter",
            "SI" = "Sinker",
            "KN" = "Knuckleball",
            #"EP" = "Eephus",
            #"PO" = "Pitch Out",
            "FS" = "Fastball",
            "KC" = "Knucklecurve",
            #"UN" = "Unidentified",
            "SC" = "SC"
            #"FO" = "Pitch Out",
            #"AB" = "AB",
            #"IN" = "IN"
            )

colors <- palette(rainbow(length(ptypes)))

dat$pitch_col <- mapvalues(dat$pitch_type, ptypes, colors)

dat$pitch_type <- mapvalues(dat$pitch_type, names(ptypes), ptypes)

pitcherSummary <- function(dat, directory, pitcherid = NULL, pitcher = NULL){
    if (!dir.exists(directory)){
        dir.create(directory)
    }

    if (is.null(pitcherid)){
        tdat <- dat[dat$player_name == pitcher,]
        pid <- gsub(" ", "_", pitcher)
    }
    if (is.null(pitcher)){
        tdat <- dat[dat$pitcher == pitcherid,]
        pid <- pitcherid
    }

    pdir <- paste0(directory, pid, "/")

    if (!dir.exists(pdir)){
        dir.create(pdir)
    }

    ## release points for pitches over and under 90mph.
    pdf(paste0(pdir, "/plot1.pdf"))
    plot(tdat$release_pos_x, tdat$release_pos_z, type = "p", xlim = c(-6, 6), ylim = c(0, 8),
         col = ifelse(tdat$release_speed > 90, "red", "blue"), xlab = "Horizontal Position", ylab = "Vertical Position")
    dev.off()

    ## Zoom in on where pitches are typically released, then plot by type
    pdf(paste0(pdir, "/plot2.pdf"))
    ptypes <- unique(tdat$pitch_type)
    xmin <- min(tdat$release_pos_x, na.rm = TRUE) - .25
    xmax <- max(tdat$release_pos_x, na.rm = TRUE) + .25
    ymin <- min(tdat$release_pos_z, na.rm = TRUE) - .25
    ymax <- max(tdat$release_pos_z, na.rm = TRUE) + .25
    colors <- aggregate(pitch_col ~ pitch_type, data = tdat, function(x) names(table(x)))
    means <- aggregate(cbind(release_pos_x, release_pos_z) ~ pitch_type, data = tdat, FUN = mean)
    means <- merge(means, colors, by = "pitch_type", all = TRUE)
    plot(means$release_pos_x, means$release_pos_z, type = "p", xlim = c(-6, 6), ylim = c(0, 8),
         col = means$pitch_col, lwd = 20, xlab = "Horizontal Position", ylab = "Vertical Position")
    legend("topright", means[,"pitch_type"], col = means[,"pitch_col"], pch = 1, lwd = 5)
    dev.off()

    rdat <- tdat[tdat$stand == "R",]
    ## Strike zone location
    pdf(paste0(pdir, "/plot3.pdf"))
    plot(rdat$plate_x, rdat$plate_z, type = "p", xlim = c(-2, 2), ylim = c(0, 4),
         col = means$pitch_col, lwd = 1, xlab = "Horizontal Position", ylab = "Vertical Position",
         main = "Right Handed Batters")
    legend("topright", means[,"pitch_type"], col = means[,"pitch_col"], pch = 1, lwd = 5)
    segments(x0 = -.75, y0 = 3.5, x1 = .75, y1 = 3.5, col = "black")
    segments(x0 = -.75, y0 = 3.5, x1 = -.75, y1 = 1.5, col = "black")
    segments(x0 = .75, y0 = 3.5, x1 = .75, y1 = 1.5, col = "black")
    segments(x0 = -.75, y0 = 1.5, x1 = .75, y1 = 1.5, col = "black")
    segments(x0 = -.75, y0 = 0, x1 = .75, y1 = 0, col = "black")
    segments(x0 = -.75, y0 = 0, x1 = -.75, y1 = -1, col = "black")
    segments(x0 = .75, y0 = 0, x1 = .75, y1 = -1, col = "black")
    dev.off()

    ldat <- tdat[tdat$stand == "L",]
    ## Strike zone location
    pdf(paste0(pdir, "/plot4.pdf"))
    plot(ldat$plate_x, ldat$plate_z, type = "p", xlim = c(-2, 2), ylim = c(0, 4),
         col = means$pitch_col, lwd = 1, xlab = "Horizontal Position", ylab = "Vertical Position",
         main = "Left Handed Batters")
    legend("topright", means[,"pitch_type"], col = means[,"pitch_col"], pch = 1, lwd = 5)
    segments(x0 = -.75, y0 = 3.5, x1 = .75, y1 = 3.5, col = "black")
    segments(x0 = -.75, y0 = 3.5, x1 = -.75, y1 = 1.5, col = "black")
    segments(x0 = .75, y0 = 3.5, x1 = .75, y1 = 1.5, col = "black")
    segments(x0 = -.75, y0 = 1.5, x1 = .75, y1 = 1.5, col = "black")
    segments(x0 = -.75, y0 = 0, x1 = .75, y1 = 0, col = "black")
    segments(x0 = -.75, y0 = 0, x1 = -.75, y1 = -1, col = "black")
    segments(x0 = .75, y0 = 0, x1 = .75, y1 = -1, col = "black")
    dev.off()

    ## Pitch proportions
    ptable <- data.frame(table(tdat$pitch_type)/nrow(tdat))
    pspeeds <- aggregate(release_speed ~ pitch_type, data = tdat, FUN = mean)
    names(ptable) <- c("Type", "Percentage of Pitches")
    names(pspeeds) <- c("Type", "Average Release Speed")
    ptable <- merge(ptable, pspeeds, by = "Type")
    ptable[,"Percentage of Pitches"] <- paste0(round(ptable[,"Percentage of Pitches"], 2)*100, "%")
    ptable[,"Average Release Speed"] <- round(ptable[,"Average Release Speed"], 2)
    t1 <- xtable(ptable)
    print(t1, file = paste0(pdir, "/table1.tex"), include.rownames = FALSE, floating = FALSE)

    ## By count
    count <- expand.grid("balls" = unique(tdat$balls), "strikes" = unique(tdat$strikes))
    count[,unique(tdat$pitch_type)] <- NA
    count[,"N"] <- NA
    count <- count[!is.na(count$balls),]
    count <- count[!is.na(count$strikes),]
    row.names(count) <- seq(1, nrow(count))
    for (i in 1:nrow(count)){
        balls <- count[i,"balls"]
        strikes <- count[i, "strikes"]
        thrown <- tdat[(tdat$balls == balls) & (tdat$strikes == strikes), "pitch_type"]
        xx <- table(thrown)/length(thrown)
        count[i, names(xx)] <- xx
        count[i, "N"] <- length(thrown)
    }
    count <- apply(count, c(1, 2), function(x) ifelse(is.na(x), 0, as.numeric(x)))
    count <- count[,!colMeans(count) == 0]
    count[,unique(tdat$pitch_type)] <- paste0(round(count[,unique(tdat$pitch_type)], 2)*100, "%")
    count
    t2 <- xtable(count)
    print(t2, file = paste0(pdir, "/table2.tex"), include.rownames = FALSE, floating = FALSE)
}

pitcherSummary(dat, paste0(datdir, "plots/"), pitcher = "Madison Bumgarner")

dat <- dat[!is.na(dat$player_name),]

for (n in unique(dat$player_name)){
    pitcherSummary(dat, paste0(datdir, "plots/"), pitcher = n)
}


## Make tables for each team
teamfiles <- list.files("../data/teaminfo", "_pitching.csv")

teamdat <- lapply(paste0("../data/teaminfo/", teamfiles), read.csv, stringsAsFactors = FALSE)

teamdat <- do.call("rbind.fill", teamdat)

for (i in 1:nrow(teamdat)){
    tmp <- teamdat$Name[i]
    if (substr(tmp, 1, 5) == "Rank_"){
        tmp <- "Rank_in"
    }
    teamdat$Name[i] <- strsplit(tmp, "_\\(")[[1]][1]
}

teamdat <- teamdat[!teamdat$Name %in% c("Team_Totals", "Rank_in"),]

write.csv(teamdat, "../data/teamdat.csv")

for (t in unique(teamdat$Team)){
    usedat <- teamdat[teamdat$Team == t,]
    usedat <- usedat[, c("Pos", "Name", "Age", "W", "L", "ERA", "G", "GS", "IP", "BB", "SO")]
    xx <- xtable(usedat)
    print(xx, file = paste0("../data/teaminfo/", t, ".tex"), include.rownames = FALSE, floating = FALSE)
}
