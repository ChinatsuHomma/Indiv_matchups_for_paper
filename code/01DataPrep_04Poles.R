##### Data preparation for poles #####
#### initial settings ####
rm(list=ls(all=TRUE)) # clear all objects
saveFiles <- TRUE     # Do you want save result file?

#### Read data ####
dPoles <- read.csv("data/03Poles.csv")

#### initial objects ####
# matchup radius (m) which extracts opponents
radius <- 3

#### organize subject individuals ####
dPoles <- dPoles[dPoles$LD2017 == "L", ]
dPoles <- dPoles[!is.na(dPoles$dbh2017), ]
dPoles <- dPoles[dPoles$dbh2017 >= 1 & dPoles$dbh2017 < 5, ] # 1 <= DBH < 5 (cm)

# sort focused individuals to avoid edge effect
## individuals on the edge of the plot were only used for opponents, not for focal
rowN <- nrow(dPoles) # count for loop process
i <- 1
for(i in 1:rowN){
    if(
        (dPoles$global_y[i] < radius)       ||  # bottom of the plot,
        (dPoles$global_x[i] > 170 - radius) ||  # right side,
        (dPoles$global_y[i] > 380 - radius) ||  # upper side,
        (dPoles$global_x[i] < radius)       ||  # left side, and some edges (following conditions)
        (dPoles$global_x[i] > 160 - radius & dPoles$global_y[i] > 350 - radius) ||
        (dPoles$global_x[i] > 150 - radius & dPoles$global_y[i] > 370 - radius) ||
        (dPoles$global_x[i] <  70 + radius & dPoles$global_y[i] > 150 - radius)
    ){dPoles$edge[i] <- 1} else {dPoles$edge[i] <- 0}
}

if(saveFiles){
    write.csv(dPoles, "output/01subjects/04SubPoles.csv", row.names = FALSE)
}
