##### Counting samples for make Table 2 #####
#### initial settings ####
rm(list=ls(all=TRUE)) # clear all objects
saveFiles <- TRUE     # Do you want save files?

#### initial objects ####
seedlings     <- list()
smallSaplings <- list()
largeSaplings <- list()
poles         <- list()
juveniles     <- list()
subadults     <- list()


#### read data ####
seedlings$subjectData        <- read.csv("output/01subjects/01SubSeedlings.csv")
smallSaplings$subjectData2x2 <- read.csv("output/01subjects/02SubSmallSaplings2x2.csv")
smallSaplings$subjectDataFP  <- read.csv("output/01subjects/02SubSmallSaplingsFP.csv")
largeSaplings$subjectData2x2 <- read.csv("output/01subjects/03SubLargeSaplings2x2.csv")
largeSaplings$subjectDataFP  <- read.csv("output/01subjects/03SubLargeSaplingsFP.csv")
poles$subjectData            <- read.csv("output/01subjects/04SubPoles.csv")
juveniles$subjectData        <- read.csv("output/01subjects/05SubJuveniles.csv")
subadults$subjectData        <- read.csv("output/01subjects/06SubSubadults.csv")


##### counting samples for each life stage #####
#### Seedlings ####
# make base objects into this list
seedlings$spList  <- unique(seedlings$subjectData$Sp)
seedlings$years   <- 2009:2018                             # vector of year used
seedlings$yearsN  <- length(seedlings$years)               # num years used
seedlings$rowN    <- nrow(seedlings$subjectData)           # num for loop process
seedlings$colN    <- ncol(seedlings$subjectData)           # num for loop process
seedlings$colList <- paste("S", seedlings$years, sep = "") # target column names for counting


# make objects for results into this list
## sample sizes in each year
seedlings$eachYear  <- data.frame("Sp" = seedlings$spList)

# loop for each year
i <- 1
for(i in 1:seedlings$yearsN){
  seedlings$tmp    <- data.frame(
    tapply(
      seedlings$subjectData[ , seedlings$colList[i]], 
      seedlings$subjectData$Sp,
      sum
    )
  )
  seedlings$tmp$Sp <- rownames(seedlings$tmp)
  colnames(seedlings$tmp)[1] <- paste("Num", seedlings$years[i], sep = "")

  seedlings$eachYear <- merge(
      seedlings$eachYear, seedlings$tmp,
      all.x = TRUE, all.y = TRUE,
      by = "Sp"
  )
}

# sum up
seedlings$result <- data.frame(
  "Sp"        = seedlings$eachYear$Sp,
  "Seedlings" = rowSums(seedlings$eachYear[ ,-1], na.rm = TRUE)
)


#### Small saplings ####
# make base objects into this list
smallSaplings$spList2x2 <- unique(smallSaplings$subjectData2x2$Sp)
smallSaplings$spListFP  <- unique(smallSaplings$subjectDataFP$Sp)
smallSaplings$spList    <- unique(c(smallSaplings$spList2x2, smallSaplings$spListFP))

# count by each scheme
smallSaplings$samples2x2 <- data.frame(table(smallSaplings$subjectData2x2$Sp))
colnames(smallSaplings$samples2x2) <- c("Sp", "SS2x2") # SS = SmallSaplings

smallSaplings$samplesFP <- data.frame(table(smallSaplings$subjectDataFP$Sp))
colnames(smallSaplings$samplesFP) <- c("Sp", "SSFP") # SS = SmallSaplings

smallSaplings$eachScheme <- merge(
    smallSaplings$samples2x2, smallSaplings$samplesFP,
    all.x = TRUE, all.y = TRUE,
    by = "Sp"
)

# sum up
smallSaplings$result <- data.frame(
  "Sp"            = smallSaplings$eachScheme$Sp,
  "SmallSaplings" = rowSums(smallSaplings$eachScheme[ ,-1], na.rm = TRUE)
)


#### Large saplings ####
# make base objects into this list
largeSaplings$spList2x2 <- unique(largeSaplings$subjectData2x2$Sp)
largeSaplings$spListFP  <- unique(largeSaplings$subjectDataFP$Sp)
largeSaplings$spList    <- unique(c(largeSaplings$spList2x2, largeSaplings$spListFP))

# count by each scheme
largeSaplings$samples2x2 <- data.frame(table(largeSaplings$subjectData2x2$Sp))
colnames(largeSaplings$samples2x2) <- c("Sp", "LS2x2") # LS = LargeSaplings

largeSaplings$samplesFP <- data.frame(table(largeSaplings$subjectDataFP$Sp))
colnames(largeSaplings$samplesFP) <- c("Sp", "LSFP") # LS = LargeSaplings

largeSaplings$eachScheme <- merge(
    largeSaplings$samples2x2, largeSaplings$samplesFP,
    all.x = TRUE, all.y = TRUE,
    by = "Sp"
)

# sum up
largeSaplings$result <- data.frame(
  "Sp"            = largeSaplings$eachScheme$Sp,
  "LargeSaplings" = rowSums(largeSaplings$eachScheme[ ,-1], na.rm = TRUE)
)


#### Poles ####
# exclude edge individuals
poles$notEdge <- poles$subjectData[poles$subjectData$edge == 0, ]

# make base objects into this list
poles$spList <- unique(poles$notEdge$Sp)

# count
poles$result <- data.frame(table(poles$notEdge$Sp))
colnames(poles$result) <- c("Sp", "Poles")


#### Juveniles ####
# exclude edge individuals
juveniles$notEdge <- juveniles$subjectData[juveniles$subjectData$edge == 0, ]

# make base objects into this list
juveniles$spList <- unique(juveniles$notEdge$Sp)

# count
juveniles$result <- data.frame(table(juveniles$notEdge$Sp))
colnames(juveniles$result) <- c("Sp", "Juveniles")


#### Subadults ####
# exclude edge individuals
subadults$notEdge <- subadults$subjectData[subadults$subjectData$edge == 0, ]

# make base objects into this list
subadults$spList <- unique(subadults$notEdge$Sp)

# count
subadults$result <- data.frame(table(subadults$notEdge$Sp))
colnames(subadults$result) <- c("Sp", "Subadults")


##### merge output for each life stage into Table 2 #####
# seedlings + smallSaplings
Table2 <- merge(
  seedlings$result,
  smallSaplings$result,
  all.x = TRUE, all.y = TRUE,
  by = "Sp"
)

# + largeSaplings
Table2 <- merge(
  Table2,
  largeSaplings$result,
  all.x = TRUE, all.y = TRUE,
  by = "Sp"
)

# + poles
Table2 <- merge(
  Table2,
  poles$result,
  all.x = TRUE, all.y = TRUE,
  by = "Sp"
)

# + juveniles
Table2 <- merge(
  Table2,
  juveniles$result,
  all.x = TRUE, all.y = TRUE,
  by = "Sp"
)

# + subadults
Table2 <- merge(
  Table2,
  subadults$result,
  all.x = TRUE, all.y = TRUE,
  by = "Sp"
)

# Totalization and ordering
Table2$Total <- rowSums(Table2[ ,-1], na.rm = TRUE)
Table2       <- Table2[order(Table2$Total, decreasing = TRUE), ]
rownames(Table2) <- 1:nrow(Table2)

# Appendix
Appendix1 <- rbind(
  Table2[which(Table2$Sp %in% c("PR", "FC", "AP", "AT", "UL", "CJ", "QC")), ],
  Table2[which(Table2$Sp %in% c("Assp")), ]
)
Appendix1 <- rbind(
  Appendix1,
  Table2[which(Table2$Sp %in% c("Acam", "Acja", "Acsi")), ]
)
Appendix1 <- rbind(
  Appendix1,
  Table2[-which(Table2$Sp %in% c("PR", "FC", "AP", "AT", "UL", "CJ", "QC", "Assp", "Acam", "Acja", "Acsi")), ]
)

# replace NA with "-"
Appendix1[Appendix1 == 0] <- "-"
Appendix1[is.na(Appendix1)] <- "-"
#Appendix1[is.na(Appendix1)] <- 0 


# Shortening for Table2
OtherSp <- Table2[-which(Table2$Sp %in% c("PR", "FC", "AP", "AT", "UL", "CJ", "QC")), ]
Table2  <- Table2[which(Table2$Sp %in% c("PR", "FC", "AP", "AT", "UL", "CJ", "QC")), ]
OtherSp <- data.frame("Sp" = "Other species", t(colSums(OtherSp[ ,-1], na.rm = TRUE)))
Table2  <- rbind(Table2, OtherSp)

# replace NA with "-"
Table2[Table2 == 0] <- "-"
Table2[is.na(Table2)] <- "-"
#Table2[is.na(Table2)] <- 0 

if(saveFiles){
  write.csv(Table2, "output/01subjects/00Table2_sampleSizes.csv", row.names = FALSE)
  write.csv(Appendix1, "output/01subjects/00Appendix1_sampleSizes.csv", row.names = FALSE)
}
