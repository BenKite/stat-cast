## Ben Kite
## 2017-05-28

## This is an example script of how to plot StatCast data for a pitchers release point
## The example uses data from early in the 2017 season for Clayton Kershaw

## install.packages("plyr")
library(plyr)

datdir <- "data/"
## This is the Clayton Kershaw data that I have in the repo
pdat <-  paste0(datdir, "477132_data.csv")

dat <- read.csv(pdat, stringsAsFactors = FALSE)
dnames <- names(dat)

dat <- apply(dat, c(1, 2), function(x) ifelse(x == "null", NA, x))
dat <- data.frame(dat, stringsAsFactors = FALSE)

names(dat) <- dnames

## Just want to reformat release point information, speed, and date
dat$release_pos_x <- as.numeric(dat$release_pos_x)
dat$release_pos_z <- as.numeric(dat$release_pos_z)

dat$release_speed <- as.numeric(dat$release_speed)

dat$game_date <- as.Date(dat$game_date, "%m/%d/%y")

pitches <- unique(dat$pitch_type)
pitches <- pitches[!is.na(pitches)]

ptypes <- c("FF" = "Four Seam", "SL" = "Slider", "CH" = "Change-Up",
"CU" = "Curveball")

colors <- c("red", "purple", "green", "blue")
dat$pitch <- dat$pitch_type

dat$pitch <- mapvalues(dat$pitch, pitches, colors)

## Lease points for pitches over and under 90mph.
plot(dat$release_pos_x, dat$release_pos_z, type = "p", xlim = c(-5, 5), ylim = c(0, 7),
     col = ifelse(dat$release_speed > 90, "red", "blue"))

## Zoom in on where pitches are typically released, then plot by type
plot(dat$release_pos_x, dat$release_pos_z, type = "p", xlim = c(0.25, 1.5), ylim = c(6, 7),
     col = dat$pitch, lwd = 5, xlab = "Horizontal Position", ylab = "Vertical Position")
legend(1.2, 7, ptypes, col = colors, pch = 1, lwd = 5)




