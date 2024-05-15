##### Data preparation for large saplings #####
#### initial settings ####
rm(list=ls(all=TRUE)) # clear all objects
saveFiles <- TRUE     # Do you want save result file?

#### Read data ####
# There are two data with different schemes we used in this life stage
dLargeSaplings2x2 <- read.csv("data/02Saplings2x2.csv") # quadrate based
dLargeSaplingsFP  <- read.csv("data/02SaplingsFloodPlain.csv") # census

#### organize subject individuals from 2x2 ####
# use measured individuals
dLargeSaplings2x2 <- dLargeSaplings2x2[!is.na(dLargeSaplings2x2$dbh18) | !is.na(dLargeSaplings2x2$H18), ]

# not use current year seedlings
dLargeSaplings2x2 <- dLargeSaplings2x2[dLargeSaplings2x2$current == 0, ]

# use large, but not too large, individuals (DBH < 1 cm and height >= 100 cm)
dLargeSaplings2x2 <- dLargeSaplings2x2[is.na(dLargeSaplings2x2$dbh18) | dLargeSaplings2x2$dbh18  < 1, ]
dLargeSaplings2x2 <- dLargeSaplings2x2[is.na(dLargeSaplings2x2$H18) | dLargeSaplings2x2$H18 >= 100, ]

#### organize subject individuals from flood plain ####
# use measured individuals
dLargeSaplingsFP$dbh2019 <- (dLargeSaplingsFP$dbh1_2019 + dLargeSaplingsFP$dbh2_2019)/2
dLargeSaplingsFP <- dLargeSaplingsFP[!is.na(dLargeSaplingsFP$dbh2019) | !is.na(dLargeSaplingsFP$height2019), ]

# not use current year seedlings
dLargeSaplingsFP <- dLargeSaplingsFP[dLargeSaplingsFP$current == 0, ]

# use large, but not too large, individuals (DBH < 1 cm and height >= 100 cm)
dLargeSaplingsFP <- dLargeSaplingsFP[is.na(dLargeSaplingsFP$dbh2019) |dLargeSaplingsFP$dbh2019 < 1, ]
dLargeSaplingsFP <- dLargeSaplingsFP[dLargeSaplingsFP$height2019 >= 100, ]

if(saveFiles){
  write.csv(dLargeSaplings2x2, "output/01subjects/03SubLargeSaplings2x2.csv", row.names = FALSE)
  write.csv(dLargeSaplingsFP,  "output/01subjects/03SubLargeSaplingsFP.csv",  row.names = FALSE)
}
