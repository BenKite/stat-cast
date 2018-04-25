## Ben Kite

## install.packages("plyr")
library(plyr)

## install.packages("xtable")
library(xtable)

##install.packages("hexbin")

library(ggplot2)
library(png)

library(shape)

datdir <- "../data/pitchers/"

pfiles <- list.files(datdir, pattern = "2018.csv")

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
dat$release_spin_rate <- as.numeric(dat$release_spin_rate)

dat$game_date <- as.Date(dat$game_date, "%m/%d/%y")

pitches <- unique(dat$pitch_type)
pitches <- pitches[!is.na(pitches)]
pitches <- pitches[pitches != ""]
pitches <- pitches[!pitches %in% c("IN", "EP", "PO", "FO", "AB", "UN", "SC")]

dat <- dat[dat$pitch_type %in% pitches,]

ptypes <- c("FF" = "Four_Seam",
            "FT" = "Two_Seam",
            "SL" = "Slider",
            "CH" = "Change_Up",
            "CU" = "Curveball",
            "FC" = "Cutter",
            "SI" = "Sinker",
            #"KN" = "Knuckleball",
            "FS" = "Fastball",
            "KC" = "Knucklecurve"
            )

dat$pitch_type <- mapvalues(dat$pitch_type, names(ptypes), ptypes)

colors <- rainbow(n = length(unique(dat$pitch_type)))
colors <- c("red", "orange", "yellow", "purple", "green", "pink", "blue", #"gray",
            "black", "brown")
dat$pitch_col <- mapvalues(dat$pitch_type, unique(dat$pitch_type), colors)

## Rankings by speed
sumdat <- dat
p2 <- unique(sumdat$pitch_type)
means <- paste0("avg_speed_", p2)
names(means) <- p2
for (p in p2){
    sumdat[,paste0("avg_speed_", p)] <- ifelse(sumdat$pitch_type == p, sumdat$release_speed, NA)
}

players <- unique(sumdat$player_name)
plist <- list()

for (p in players){
    xx <- sumdat[which(sumdat$player_name == p),c("player_name", grep("avg_speed_", names(sumdat), value = TRUE))]
    tmp <- t(colMeans(xx[,2:ncol(xx)], na.rm = TRUE))
    plist[[p]] <- data.frame(p, tmp)
}

pranks <- do.call("rbind.fill", plist)
names(pranks) <- c("player_name", p2)
pranks[order(pranks$Four_Seam, decreasing = TRUE),]
for (p in p2){
    pranks <- pranks[order(pranks[,p], decreasing = TRUE), ]
    pranks[,paste0(p, "speed_rank")] <- seq(1, nrow(pranks))
    pranks[,paste0(p, "speed_rank")] <- ifelse(is.na(pranks[,p]), NA, pranks[,paste0(p, "speed_rank")])
    pranks[,paste0(p, "speed_%")] <- pranks[,paste0(p, "speed_rank")]/sum(!is.na(pranks[,paste0(p, "speed_rank")]))
    pranks[,paste0(p, "speed_%")] <- paste0(100*round(1 - pranks[,paste0(p, "speed_%")], 2), "%")
}

head(pranks)
write.csv(pranks, "speed_ranks.csv")


## Rankings by spinrate
means <- paste0("avg_spin_", p2)
names(means) <- p2
for (p in p2){
    sumdat[,paste0("avg_spin_", p)] <- ifelse(sumdat$pitch_type == p, sumdat$release_spin_rate, NA)
}
players <- unique(sumdat$player_name)
plist <- list()
for (p in players){
    xx <- sumdat[which(sumdat$player_name == p), c("player_name", grep("avg_spin_", names(sumdat), value = TRUE))]
    tmp <- t(colMeans(xx[,2:ncol(xx)], na.rm = TRUE))
    plist[[p]] <- data.frame(p, tmp)
}
spinrates <- do.call("rbind.fill", plist)
names(spinrates) <- c("player_name", p2)

for (p in p2){
    spinrates <- spinrates[order(spinrates[,p], decreasing = TRUE), ]
    spinrates[,paste0(p, "spin_rank")] <- seq(1, nrow(spinrates))
    spinrates[,paste0(p, "spin_rank")] <- ifelse(is.na(spinrates[,p]), NA, spinrates[,paste0(p, "spin_rank")])
    spinrates[,paste0(p, "spin_%")] <- spinrates[,paste0(p, "spin_rank")]/sum(!is.na(spinrates[,paste0(p, "spin_rank")]))
    spinrates[,paste0(p, "spin_%")] <- paste0(100*round(1 - spinrates[,paste0(p, "spin_%")], 2), "%")
}

names(spinrates)
head(spinrates)

write.csv(spinrates, "spin_ranks.csv")

speedranks <- pranks
spinranks <- spinrates


pitcherSummary <- function(dat, directory, speedranks, spinranks, pitcherid = NULL, pitcher = NULL, plots = NULL, quickReport = TRUE){
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

    tdat <- tdat[tdat$pitch_type %in% names(table(tdat$pitch_type))[table(tdat$pitch_type) > 5],]

    if (nrow(tdat) == 0){
        return("No rows!")
    }

    if (isTRUE(plots)){
        ## Compare release points by pitch type
        pdf(paste0(pdir, "/plot2.pdf"))
        ptypes <- unique(tdat$pitch_type)
        xmin <- min(tdat$release_pos_x, na.rm = TRUE) - .25
        xmax <- max(tdat$release_pos_x, na.rm = TRUE) + .25
        ymin <- min(tdat$release_pos_z, na.rm = TRUE) - .25
        ymax <- max(tdat$release_pos_z, na.rm = TRUE) + .25
        colors <- aggregate(pitch_col ~ pitch_type, data = tdat,
                            function(x) names(table(x)))
        means <- aggregate(cbind(release_pos_x, release_pos_z) ~ pitch_type,
                           data = tdat, FUN = mean)
        means <- merge(means, colors, by = "pitch_type", all = TRUE)

        plot(means$release_pos_x, means$release_pos_z, type = "n",
             xlim = c(-13, 12), ylim = c(-15.25, 13), col = means$pitch_col,
             lwd = 2, xlab = NA, ylab = NA, axes = FALSE,
             main = "Typical Release Points")
        box()
        if(isTRUE(quickReport)){
            lines(x = c(-3.3, 3.3), y = c(-11.5, -11.5))
            lines(x = c(-3.3, -3.4), y = c(-11.5, -12.5))
            lines(x = c(3.3, 3.4), y = c(-11.5, -12.5))
            lines(x = c(3.4, 0), y = c(-12.5, -13))
            lines(x = c(-3.4, 0), y = c(-12.5, -13))
            plotellipse(mid = c(0, .75), rx = 9, ry = .75, col = "burlywood")
            filledrectangle(mid = c(0, 1.25), wx = 2, wy = .1, col = "white")
        }else{
            ima <- readPNG("../images/pitching_backdrop1.png")
            lim <- par()
            rasterImage(ima, lim$usr[1], lim$usr[3], lim$usr[2], lim$usr[4])
            filledrectangle(mid = c(0, 1.5), wx = 1.5, wy = .2, col = "white")
        }
        points(means$release_pos_x, means$release_pos_z, type = "p",
             col = means$pitch_col, lwd = 2)
        pitches <- gsub("_", " ", means[,"pitch_type"])
        legend("topright", pitches, col = means[,"pitch_col"],
               pch = 1, lwd = 2, lty = NA, bg = "grey")

        dev.off()

        rdat <- tdat[tdat$stand == "R",]
        ## Strike zone location
        pdf(paste0(pdir, "/plot3.pdf"))
        plot(rdat$plate_x, rdat$plate_z, type = "n", xlim = c(-2.8, 2.8),
             ylim = c(-1, 6), col = "black", lwd = 1,
             xlab = NA, ylab = NA,
             main = "Right Handed Batters", axes = FALSE)
        box()
        if(isTRUE(quickReport)){
            segments(x0 = -.75, y0 = 0, x1 = .75, y1 = 0, col = "black")
            segments(x0 = -.75, y0 = 0, x1 = -.8, y1 = -.25, col = "black")
            segments(x0 = .75, y0 = 0, x1 = .8, y1 = -.25, col = "black")
            segments(x0 = -.8, y0 = -.25, x1 = 0, y1 = -.4, col = "black")
            segments(x0 = .8, y0 = -.25, x1 = 0, y1 = -.4, col = "black")
            plotellipse(mid = c(0, 3), rx = 2, ry = .2, col = "burlywood")
            filledrectangle(mid = c(0, 3.1), wx = .5, wy = .05, col = "white")
        }else{
            ima <- readPNG("../images/pitching_backdrop1.png")
            lim <- par()
            rasterImage(ima, lim$usr[1], lim$usr[3], lim$usr[2], lim$usr[4])
            filledrectangle(mid = c(0, 1.5), wx = 1.5, wy = .2, col = "white")
        }
        points(rdat$plate_x, rdat$plate_z, type = "p",
               col = alpha("white", .5), cex = 3, pch = 16)
        par(new = T)
        if(nrow(rdat) > 0){
            smoothScatter(xy.coords(x = rdat$plate_x, y = rdat$plate_z),
                          xlim = c(-2.8, 2.8), ylim = c(-1, 6),
                          add = TRUE, useRaster = TRUE,
                          colramp = colorRampPalette(c(rgb(1, 1, 1, 0),
                                                       rgb(1, 0, 0, 1)), alpha = TRUE))
        }
        segments(x0 = -.75, y0 = 3.5, x1 = .75, y1 = 3.5, col = "blue")
        segments(x0 = -.75, y0 = 3.5, x1 = -.75, y1 = 1.5, col = "blue")
        segments(x0 = .75, y0 = 3.5, x1 = .75, y1 = 1.5, col = "blue")
        segments(x0 = -.75, y0 = 1.5, x1 = .75, y1 = 1.5, col = "blue")
        dev.off()


        ldat <- tdat[tdat$stand == "L",]
        pdf(paste0(pdir, "/plot4.pdf"))
        plot(ldat$plate_x, ldat$plate_z, type = "n", xlim = c(-2.8, 2.8),
             ylim = c(-1, 6), col = "black", lwd = 1,
             xlab = NA, ylab = NA,
             main = "Left Handed Batters", axes = FALSE)
        box()
        if(isTRUE(quickReport)){
            segments(x0 = -.75, y0 = 0, x1 = .75, y1 = 0, col = "black")
            segments(x0 = -.75, y0 = 0, x1 = -.8, y1 = -.25, col = "black")
            segments(x0 = .75, y0 = 0, x1 = .8, y1 = -.25, col = "black")
            segments(x0 = -.8, y0 = -.25, x1 = 0, y1 = -.4, col = "black")
            segments(x0 = .8, y0 = -.25, x1 = 0, y1 = -.4, col = "black")
            plotellipse(mid = c(0, 3), rx = 2, ry = .2, col = "burlywood")
            filledrectangle(mid = c(0, 3.1), wx = .5, wy = .05, col = "white")
        }else{
            ima <- readPNG("../images/pitching_backdrop1.png")
            lim <- par()
            rasterImage(ima, lim$usr[1], lim$usr[3], lim$usr[2], lim$usr[4])
            filledrectangle(mid = c(0, 1.5), wx = 1.5, wy = .2, col = "white")
        }
        points(ldat$plate_x, ldat$plate_z, type = "p",
               col = alpha("white", .5), cex = 3, pch = 16)
        par(new = T)
        if(nrow(ldat) > 0){
            smoothScatter(xy.coords(x = ldat$plate_x, y = ldat$plate_z),
                          xlim = c(-2.8, 2.8), ylim = c(-1, 6),
                          add = TRUE, useRaster = TRUE,
                          colramp = colorRampPalette(c(rgb(1, 1, 1, 0),
                                                       rgb(1, 0, 0, 1)), alpha = TRUE))
        }
        segments(x0 = -.75, y0 = 3.5, x1 = .75, y1 = 3.5, col = "blue")
        segments(x0 = -.75, y0 = 3.5, x1 = -.75, y1 = 1.5, col = "blue")
        segments(x0 = .75, y0 = 3.5, x1 = .75, y1 = 1.5, col = "blue")
        segments(x0 = -.75, y0 = 1.5, x1 = .75, y1 = 1.5, col = "blue")
        dev.off()
    }


    ## Pitch proportions

    speedinfo <- speedranks[which(speedranks$player_name == pitcher),]
    spininfo <- spinranks[which(spinranks$player_name == pitcher),]

    ptable <- data.frame(table(tdat$pitch_type)/nrow(tdat), stringsAsFactors = FALSE)
    ptable$Var1 <- levels(ptable$Var1)
    pspeeds <- aggregate(release_speed ~ pitch_type, data = tdat, FUN = mean)
    pspins <- aggregate(release_spin_rate ~ pitch_type, data = tdat, FUN = mean, na.rm = TRUE)
    names(ptable) <- c("Type", "% of Pitches")
    names(pspeeds) <- c("Type", "Average Release Speed (%tile)")
    names(pspins) <- c("Type", "Average Spin Rate (%tile)")
    ptable <- merge(ptable, pspeeds, by = "Type")
    ptable <- merge(ptable, pspins, by = "Type")
    ptable <- ptable[order(ptable[,"% of Pitches"], decreasing = TRUE),]
    ptable[,"% of Pitches"] <- paste0(round(ptable[,"% of Pitches"], 2)*100, "%")
    ptable[,"Average Release Speed (%tile)"] <- paste0(round(ptable[,"Average Release Speed (%tile)"], 2), "(", speedinfo[,paste0(ptable$Type, "speed_%")], ")")
    ptable[,"Average Spin Rate (%tile)"] <- paste0(round(ptable[,"Average Spin Rate (%tile)"], 2), "(", spininfo[,paste0(ptable$Type, "spin_%")], ")")
    tmpptable <- ptable
    tmpptable[,"Type"] <- gsub("_", " ", tmpptable[,"Type"])
    t1 <- xtable(tmpptable, align = c("l", rep("r", ncol(tmpptable))))
    print(t1, file = paste0(pdir, "/table1.tex"), include.rownames = FALSE, floating = FALSE)

    ## By count
    count <- expand.grid("balls" = unique(tdat$balls), "strikes" = unique(tdat$strikes))
    count[,"Number of Pitches"] <- NA
    count[,ptable[,"Type"]] <- NA
    count <- count[!is.na(count$balls),]
    count <- count[!is.na(count$strikes),]
    row.names(count) <- seq(1, nrow(count))
    for (i in 1:nrow(count)){
        balls <- count[i,"balls"]
        strikes <- count[i, "strikes"]
        thrown <- tdat[(tdat$balls == balls) & (tdat$strikes == strikes), "pitch_type"]
        xx <- table(thrown)/length(thrown)
        count[i, names(xx)] <- xx
        count[i, "Number of Pitches"] <- length(thrown)
    }
    count <- apply(count, c(1, 2), function(x) ifelse(is.na(x), 0, as.numeric(x)))
    #count <- count[,!colMeans(count) == 0]
    count[,unique(tdat$pitch_type)] <- paste0(round(count[,unique(tdat$pitch_type)], 2)*100, "%")
    countdat <- data.frame(count, stringsAsFactors = FALSE)
    countdat <- countdat[order(countdat[,c("balls")]),]
    countdat <- countdat[order(countdat[,c("strikes")]),]
    countinfo <- paste0(countdat[,"balls"], "-", countdat[,"strikes"])
    countdat$balls <- NULL
    countdat$strikes <- NULL
    countdat <- data.frame(Count = countinfo, countdat)
    names(countdat) <- sapply(names(countdat), function(x) gsub("\\.", " ", x))
    names(countdat) <- sapply(names(countdat), function(x) gsub("_", " ", x))
    t2 <- xtable(countdat, align = c("l", rep("r", ncol(countdat))))
    print(t2, file = paste0(pdir, "/table2.tex"), include.rownames = FALSE, floating = FALSE)

    ##bb type
    bbdat <- tdat[tdat$bb_type %in% c("ground_ball", "fly_ball", "line_drive", "popup"),]
    if (nrow(bbdat) > 0){
        b3 <- table(bbdat$bb_type)/nrow(bbdat)
        b3 <- data.frame(b3, stringsAsFactors = FALSE)
        names(b3) <- c("Hit Type", "Percentage of Balls in Play")
        b3[,"Hit Type"] <- gsub("_", " ", b3[,"Hit Type"])
        b3[,"Percentage of Balls in Play"] <- paste0(round(b3[,"Percentage of Balls in Play"], 2)*100, "%")
        t3 <- xtable(b3, align = c("l", rep("r", ncol(b3))))
        print(t3, file = paste0(pdir, "/table3.tex"), include.rownames = FALSE, floating = FALSE)
    }

}

pitcherSummary(dat, paste0(datdir, "plots/"), speedranks = speedranks,
               spinranks = spinranks, pitcher = "Clayton Kershaw", plots = TRUE, quickReport = FALSE)

dat <- dat[!is.na(dat$player_name),]

for (n in unique(dat$player_name)){
    pitcherSummary(dat, paste0(datdir, "plots/"), speedranks = speedranks, spinranks = spinranks, pitcher = n, plots = TRUE, quickReport = TRUE)
}

fnames <- unique(dat$player_name)

## Make tables for each team
teamfiles <- list.files("../data/teaminfo", "_pitching.csv")

teamdat <- lapply(paste0("../data/teaminfo/", teamfiles), read.csv, stringsAsFactors = FALSE)

teamdat <- do.call("rbind.fill", teamdat)

teamdat$Name2 <- NA
teamdat$Last <- NA
teamdat$FirstI <- NA
for (i in 1:nrow(teamdat)){
    tmp <- teamdat$Name[i]
    if (substr(tmp, 1, 5) == "Rank_"){
        tmp <- "Rank_in"
    }
    teamdat$Name[i] <- strsplit(tmp, "_\\(")[[1]][1]
    teamdat$FirstI[i] <- substr(tmp, 1, 1)
    teamdat$Last[i] <- strsplit(teamdat$Name[i], "_")[[1]][length(strsplit(teamdat$Name[i], "_")[[1]])]
    teamdat$Name2[i] <- gsub("_", " ", teamdat$Name[i])
}

## First initial and last name in Statcast data
dat$FirstI <- substr(dat$player_name, 1, 1)

lasts <- rep(NA, length(fnames))
firsts <- rep(NA, length(fnames))
handedness <- rep(NA, length(fnames))
for (f in fnames){
    firsts[f] <- substr(f, 1, 1)
    lasts[f] <- strsplit(f, " ")[[1]][length(strsplit(f, " ")[[1]])]
    handedness[f] <- unique(dat[dat$player_name == f,"p_throws"])
}

scnames <- data.frame(firsts, lasts, fnames, handedness)
names(scnames) <- c("FirstI", "Last", "StatcastName", "Throws")

teamdat <- merge(teamdat, scnames, by = c("FirstI", "Last"))

teamdat$pfolder <- gsub(" ", "_", teamdat$StatcastName)

teamdat <- teamdat[!teamdat$Name %in% c("Team_Totals", "Rank_in"),]

write.csv(teamdat, "../data/teamdat.csv")

for (t in unique(teamdat$Team)){
    usedat <- teamdat[teamdat$Team == t,]
    usedat <- usedat[, c("Pos", "Name", "Throws", "Age", "W", "L", "ERA", "G", "GS", "IP", "BB", "SO", "StatcastName")]
    for (i in 1:nrow(usedat)){
        pname <- usedat[i,"Name"]
        pname <- gsub("_", " ", pname)
        #insert <- paste0("\\ref[", pname, "]{", pname, "}")
        usedat[i, "Name"] <- pname
    }
    usedat <- unique(usedat)
    starters <- usedat[usedat$Pos == "SP",]
    starters <- starters[order(starters$ERA),]
    closers <- usedat[usedat$Pos == "CL",]
    closers <- closers[order(closers$ERA),]
    relievers <- usedat[usedat$Pos == "RP",]
    relievers <- relievers[order(relievers$ERA),]
    others <- usedat[!usedat$Pos %in% c("SP", "CL", "RP"),]
    others <- others[order(others$ERA),]
    usedat <- rbind(starters, closers, relievers, others)
    usedat[,"Age"] <- as.character(usedat[,"Age"])
    write.csv(usedat, paste0("../data/teaminfo/", t, ".csv"))
    usedat$StatcastName <- NULL
    xx <- xtable(usedat)
    print(xx, file = paste0("../data/teaminfo/", t, ".tex"), include.rownames = FALSE, floating = FALSE)
}
