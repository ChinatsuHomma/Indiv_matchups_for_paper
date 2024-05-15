##### Data preparation for seedlings #####
#### initial settings ####
rm(list=ls(all=TRUE)) # clear all objects
saveFiles <- TRUE     # Do you want save result file?

#### Read data ####
dSeedling <- read.csv("data/01Seedlings.csv")
keep      <- dSeedling # for debug

#### initial objects ####
years  <- 2009:2018     # vector of year used
yearsN <- length(years) # num years used

# counting for loop process
rowN <- nrow(dSeedling)
colN <- ncol(dSeedling)

#### organize subject individuals ####
# add cols about subjecting
## column Tyyyy indicate whether individuals are subjected (=1) or not (=0) in a year "yyyy".
tmpDF <- as.data.frame.matrix(
  matrix(rep(NA), nrow = rowN, ncol = yearsN)
)
colnames(tmpDF) <- c(paste("S", years, sep = ""))
dSeedling <- cbind(dSeedling, tmpDF)

# loop process for sorting subject individuals (new or height < 30)
i <- 1
## loop for each year
for(i in 1:yearsN){
  ## column name of heights in year i
  currentH <- paste("H", years[i], sep = "")

  ## column name of heights in year (i-1)
  if(i != 1){
    lastH  <- paste("H", years[i-1], sep = "")
  } else {
    lastH  <- "H2007" # a previous investigation in 2009 is 2007
  }

  ## column name for extracting subject individual in year i
  subject <- paste("S", years[i], sep = "")
  
  ## loop for each individual
  j <- 1
  for(j in 1:rowN){
    if(is.na(dSeedling[j, currentH]) == FALSE){
      ## height in year i is exist
      if(dSeedling[j, currentH] <  30 | is.na(dSeedling[j, lastH]) == TRUE){
        dSeedling[j, subject] <- 1
      } else {
        dSeedling[j, subject] <- 0
      }
    } else {         
      ## height in year i is NOT exist
      dSeedling[j, subject] <- 0
    }
  }
}

if(saveFiles){
  write.csv(dSeedling, "output/01subjects/01SubSeedlings.csv", row.names = FALSE)
}
