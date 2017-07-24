## Ben Kite

## install.packages("plyr")
library(plyr)

## install.packages("xtable")
library(xtable)

##install.packages("hexbin")

library(ggplot2)
library(png)

library(shape)

datdir <- "../data/batters/"

pfiles <- list.files(datdir, pattern = "2017.csv")

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

dat$plate_x <- as.numeric(dat$plate_x)
dat$plate_z <- as.numeric(dat$plate_z)

dat$sz_top <- as.numeric(dat$sz_top)
dat$sz_bot <- as.numeric(dat$sz_bot)

dat$hc_x <- as.numeric(dat$hc_x)
dat$hc_y <- as.numeric(dat$hc_y)

dat$release_speed <- as.numeric(dat$release_speed)
dat$release_spin_rate <- as.numeric(dat$release_spin_rate)

dat$game_date <- as.Date(dat$game_date, "%m/%d/%y")

dat$swing <- ifelse(dat$description %in% c("hit_into_play",
                                           "foul",
                                           "foul_bunt",
                                           "swinging_strike_blocked",
                                           "hit_into_play_no_out",
                                           "hit_into_play_score",
                                           "missed_bunt",
                                           "swinging_strike"), 1, 0)
dat$contact <- ifelse(dat$description %in% c("hit_into_play",
                                           "foul",
                                           "foul_bunt",

                                           "hit_into_play_no_out",
                                           "hit_into_play_score"), 1, 0)


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
            "KN" = "Knuckleball",
            "FS" = "Fastball",
            "KC" = "Knucklecurve"
            )

dat$pitch_type <- mapvalues(dat$pitch_type, names(ptypes), ptypes)

dat$sprayx <- dat$hc_x - 125
dat$sprayy <- (dat$hc_y - 200) * -1

dat$launchAngle_x <- acos(dat$sprayx)/(dat$sprayy)

plot(dat$sprayx, dat$sprayy, xlim = c(-200, 200), ylim = c(-20, 400))

batterSummary <- function(dat, directory, speedranks, spinranks, batterid = NULL, batter = NULL, plots = NULL){
    if (!dir.exists(directory)){
        dir.create(directory)
    }

    if (is.null(batterid)){
        tdat <- dat[dat$player_name == batter,]
        pid <- gsub(" ", "_", batter)
    }
    if (is.null(batter)){
        tdat <- dat[dat$batter == batterid,]
        pid <- batterid
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
        ## Swings and misses
        outcomes <- c("called_strike", "swinging_strike")
        for (o in outcomes){
            rdat <- tdat[tdat$description == o,]
            ## Strike zone location
            pdf(paste0(pdir, "/", o, ".pdf"))
            plot(rdat$plate_x, rdat$plate_z, type = "n", xlim = c(-2.8, 2.8),
                 ylim = c(-1, 6), col = "black", lwd = 1,
                 xlab = NA, ylab = NA,
                 main = toupper(o), axes = FALSE)
            ima <- readPNG("../images/pitching_backdrop1.png")
            lim <- par()
            rasterImage(ima, lim$usr[1], lim$usr[3], lim$usr[2], lim$usr[4])
            points(rdat$plate_x, rdat$plate_z, type = "p",
                   col = alpha("red", .5), cex = 3, pch = 16)
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
        }

        contact <- list()
        for (h in c("L", "R")){
            hdat <- tdat[which(tdat$p_throws == h),]
            cont <- matrix(NA, length(unique(hdat$pitch_type)), ncol = 3)
            cont[,1] <-  unique(hdat$pitch_type)
            for (p in 1:length(unique(hdat$pitch_type))){
                pdat <- hdat[which(hdat$pitch_type == cont[p,1]),]
                cont[p,2] <- sum(pdat$contact)/sum(pdat$swing)
                cont[p,3] <- sum(pdat$swing)
                ##t1 <- xtable(tmpptable, align = c("l", rep("r", ncol(tmpptable))))
                ##print(t1, file = paste0(pdir, "/table1.tex"), include.rownames = FALSE, floating = FALSE)

            }
            contact[[h]] <- cont
        }
    }
    tdat$scol <- ifelse(tdat$sprayx > 0, "red", "blue")
    ##tdat$scol <- ifelse(tdat$launchAngle_x > 0, "red", "blue")
    for (b in unique(tdat$bb_type)){
        if (!is.na(b)){
            tmpdat <- tdat[which(tdat$bb_type == b),]
            pdf(paste0(pdir, "spray_", b, ".pdf"))
            plot(tmpdat$sprayx, tmpdat$sprayy, main = b, xlim = c(-100, 100), ylim = c(-10, 200))
            segments(0, 0, tmpdat$sprayx, tmpdat$sprayy, col = tmpdat$scol)
            dev.off()
            pdf(paste0(pdir, "launch_", b, ".pdf"))
            #hist(tmpdat$launchAngle_x, main = b, xlim = c(-60, 60))
            dev.off()
        }
    }

}

batterSummary(dat, paste0(datdir, "plots/"), speedranks = speedranks,
               spinranks = spinranks, batter = "Mike Moustakas", plots = TRUE)

dat <- dat[!is.na(dat$player_name),]

for (n in unique(dat$player_name)){
    batterSummary(dat, paste0(datdir, "plots/"), speedranks = speedranks, spinranks = spinranks, batter = n, plots = TRUE)
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
