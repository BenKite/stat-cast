## Ben Kite

## install.packages("plyr")
library(plyr)

## install.packages("xtable")
library(xtable)

datdir <- "../data/pitchers/"

pfiles <- list.files(datdir)

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

dat$release_speed <- as.numeric(dat$release_speed)

dat$game_date <- as.Date(dat$game_date, "%m/%d/%y")

pitches <- unique(dat$pitch_type)
pitches <- pitches[!is.na(pitches)]

ptypes <- c("FF" = "Four Seam",
            "FT" = "Two Seam",
            "SL" = "Slider",
            "CH" = "Change-Up",
            "CU" = "Curveball",
            "FC" = "Cutter",
            "SI" = "Sinker",
            "KN" = "Knuckleball",
            "EP" = "Eephus",
            "PO" = "Pitch Out",
            "FS" = "Fastball",
            "KC" = "Knucklecurve",
            "UN" = "Unidentified",
            "SC" = "SC",
            "FO" = "Pitch Out",
            "AB" = "AB",
            "IN" = "IN")

colors <- palette(rainbow(length(ptypes)))

dat$pitch_col <- mapvalues(dat$pitch_type, pitches, colors)

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

    tdat <- tdat[!is.na(tdat$pitch_type),]

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
    plot(tdat$release_pos_x, tdat$release_pos_z, type = "p", xlim = c(xmin, xmax), ylim = c(ymin, ymax),
         col = tdat$pitch_col, lwd = 5, xlab = "Horizontal Position", ylab = "Vertical Position")
    colors <- aggregate(pitch_col ~ pitch_type, data = tdat, function(x) names(table(x)))
    legend("topright", colors[,"pitch_type"], col = colors[,"pitch_col"], pch = 1, lwd = 5)
    dev.off()

    ## Pitch proportions
    ptable <- table(tdat$pitch_type)/nrow(tdat)
    xtable(ptable)

    ## By count
    count <- expand.grid("balls" = unique(tdat$balls), "strikes" = unique(tdat$strikes))
    count[,unique(ptypes)] <- NA
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
    xtable(count)
}

pitcherSummary(dat, paste0(datdir, "plots/"), pitcher = "Madison Bumgarner")

for (n in unique(dat$player_name)){
    pitcherSummary(dat, paste0(datdir, "plots/"), pitcher = n)
}
