##### Data preparation for small saplings #####
#### initial settings ####
rm(list=ls(all=TRUE)) # clear all objects
saveFiles <- TRUE     # Do you want save result file?

#### Read data ####
# There are two data with different schemes we used in this life stage
dSmallSaplings2x2 <- read.csv("data/02Saplings2x2.csv") # quadrate based
dSmallSaplingsFP  <- read.csv("data/02SaplingsFloodPlain.csv") # census

#### organize subject individuals from 2x2 ####
# use measured individuals
dSmallSaplings2x2 <- dSmallSaplings2x2[is.na(dSmallSaplings2x2$H18) == FALSE, ]

# not use current year seedlings
dSmallSaplings2x2 <- dSmallSaplings2x2[dSmallSaplings2x2$current == 0, ]

# use small individuals (GBH and DBH not measured and 30 < height < 100 cm)
dSmallSaplings2x2 <- dSmallSaplings2x2[is.na(dSmallSaplings2x2$gbh18), ]
dSmallSaplings2x2 <- dSmallSaplings2x2[is.na(dSmallSaplings2x2$dbh18), ]
dSmallSaplings2x2 <- dSmallSaplings2x2[dSmallSaplings2x2$H18 >= 30, ]
dSmallSaplings2x2 <- dSmallSaplings2x2[dSmallSaplings2x2$H18 < 100, ]

#### organize subject individuals from flood plain ####
# use measured individuals
dSmallSaplingsFP <- dSmallSaplingsFP[is.na(dSmallSaplingsFP$height2019) == FALSE, ]

# not use current year seedlings
dSmallSaplingsFP <- dSmallSaplingsFP[dSmallSaplingsFP$current == 0, ]

# use small individuals (DBH not measured and 30 < height < 100 cm)
dSmallSaplingsFP <- dSmallSaplingsFP[is.na(dSmallSaplingsFP$dbh2019), ]
dSmallSaplingsFP <- dSmallSaplingsFP[is.na(dSmallSaplingsFP$dbh1_2019), ]
dSmallSaplingsFP <- dSmallSaplingsFP[is.na(dSmallSaplingsFP$dbh2_2019), ]
dSmallSaplingsFP <- dSmallSaplingsFP[dSmallSaplingsFP$height2019 >= 30, ]
dSmallSaplingsFP <- dSmallSaplingsFP[dSmallSaplingsFP$height2019 < 100, ]

if(saveFiles){
  write.csv(dSmallSaplings2x2, "output/01subjects/02SubSmallSaplings2x2.csv", row.names = FALSE)
  write.csv(dSmallSaplingsFP,  "output/01subjects/02SubSmallSaplingsFP.csv",  row.names = FALSE)
}
