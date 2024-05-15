##### Data preparation for subadults #####
#### initial settings ####
rm(list=ls(all=TRUE)) # clear all objects
saveFiles <- TRUE     # Do you want save result file?

#### Read data ####
dSubadults <- read.csv("data/04Adults.csv")

#### initial objects ####
# matchup radius (m) which extracts opponents
radius <- 7

#### organize subject individuals ####
# sizes
dSubadults <- dSubadults[dSubadults$LD2019 == "L", ]
dSubadults <- dSubadults[!is.na(dSubadults$dbh2019), ]
dSubadults <- dSubadults[dSubadults$dbh2019 >= 10 & dSubadults$dbh2019 < 20, ] # 10 <= DBH < 20 (cm)

# sort focused individuals to avoid edge effect
## individuals on the edge of the plot were only used for opponents, not for focal
rowN <- nrow(dSubadults) # count for loop process
i <- 1
for(i in 1:rowN){
    if(
        (dSubadults$global_y[i] < radius)       ||  # bottom of the plot,
        (dSubadults$global_x[i] > 170 - radius) ||  # right side,
        (dSubadults$global_y[i] > 380 - radius) ||  # upper side,
        (dSubadults$global_x[i] < radius)       ||  # left side, and some edges (following conditions)
        (dSubadults$global_x[i] > 160 - radius & dSubadults$global_y[i] > 350 - radius) ||
        (dSubadults$global_x[i] > 150 - radius & dSubadults$global_y[i] > 370 - radius) ||
        (dSubadults$global_x[i] <  70 + radius & dSubadults$global_y[i] > 150 - radius) ||
        (dSubadults$topo[i] == "R") # individuals on terrace (topo = "R") also regarded as on edges
    ){dSubadults$edge[i] <- 1} else {dSubadults$edge[i] <- 0}
}

if(saveFiles){
    write.csv(dSubadults, "output/01subjects/06SubSubadults.csv", row.names = FALSE)
}
