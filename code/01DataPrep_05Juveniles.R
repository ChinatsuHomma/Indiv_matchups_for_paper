##### Data preparation for juveniles #####
#### initial settings ####
rm(list=ls(all=TRUE)) # clear all objects
saveFiles <- TRUE     # Do you want save result file?

#### Read data ####
dJuveniles <- read.csv("data/04Adults.csv")

#### initial objects ####
# matchup radius (m) which extracts opponents
radius <- 5

#### organize subject individuals ####
# sizes
dJuveniles <- dJuveniles[dJuveniles$LD2019 == "L", ]
dJuveniles <- dJuveniles[!is.na(dJuveniles$dbh2019), ]
dJuveniles <- dJuveniles[dJuveniles$dbh2019 >= 5 & dJuveniles$dbh2019 < 10, ] # 5 <= DBH < 10 (cm)

# sort focused individuals to avoid edge effect
## individuals on the edge of the plot were only used for opponents, not for focal
rowN <- nrow(dJuveniles) # count for loop process
i <- 1
for(i in 1:rowN){
    if(
        (dJuveniles$global_y[i] < radius)       ||  # bottom of the plot,
        (dJuveniles$global_x[i] > 170 - radius) ||  # right side,
        (dJuveniles$global_y[i] > 380 - radius) ||  # upper side,
        (dJuveniles$global_x[i] < radius)       ||  # left side, and some edges (following conditions)
        (dJuveniles$global_x[i] > 160 - radius & dJuveniles$global_y[i] > 350 - radius) ||
        (dJuveniles$global_x[i] > 150 - radius & dJuveniles$global_y[i] > 370 - radius) ||
        (dJuveniles$global_x[i] <  70 + radius & dJuveniles$global_y[i] > 150 - radius) ||
        (dJuveniles$topo[i] == "R") # individuals on terrace (topo = "R") also regarded as on edges
    ){dJuveniles$edge[i] <- 1} else {dJuveniles$edge[i] <- 0}
}

if(saveFiles){
    write.csv(dJuveniles, "output/01subjects/05SubJuveniles.csv", row.names = FALSE)
}
