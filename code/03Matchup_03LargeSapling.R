##### Matchup list for saplings #####
#### initial settings ####
rm(list=ls(all=TRUE)) # clear all objects
saveFiles <- TRUE     # Do you want save result file?

#### Read data ####
subject2x2 <- read.csv("output/01subjects/03SubLargeSaplings2x2.csv")
subjectFP  <- read.csv("output/01subjects/03SubLargeSaplingsFP.csv")
sampleSize <- read.csv("output/01subjects/00Appendix1_sampleSizes.csv")

# species list
sampleSize[sampleSize == "-"] <- 0
spList  <- sampleSize$Sp
spOrder <- data.frame("Sp" = sampleSize$Sp, "Order" = 1:length(spList))

#### initial objects ####
# radius
radius <- 1.5

# counting for loop process
rowN2x2 <- nrow(subject2x2)
rowNFP  <- nrow(subjectFP)
spN     <- length(spList)

# frame of resultTable
matchupList <- data.frame(
  "NumTrial"    = NA, # number of trials
  "DataFrom"    = NA, # data type (2x2 or FP)
  "Focal_Plot"  = NA, # a quadrat where focal individual in
  "Focal_x"     = NA, # global x-coordinate of a quadrat where focal individual in
  "Focal_y"     = NA, # global y-coordinate of a quadrat where focal individual in
  "Focal_ID"    = NA, # ID of a focal individual
  "Focal_Sp"    = NA, # species code of a focal individual
  "Focal_H"     = NA, # height of a focal individual
  "Focal_DBH"   = NA, # dbh of a focal individual
  "Opponent_ID" = NA, # ID of an opponent individual
  "Opponent_Sp" = NA, # species code of an opponent individual
  "Opponent_H"  = NA, # height of an opponent individual
  "Opponent_DBH"= NA, # dbh of an opponent individual
  "Distance"    = NA  # distance between a focal and an opponent
)

#### loop processes for extracting "matchups" ####
Trial     <- 0 # repetition time
listRowsN <- 0 # number of rows in matchup list

# from 2x2
i <- 1
for(i in 1:rowN2x2){
    # counting repetitions
    Trial <- Trial + 1

    # target an individual as focal
    focal_tmp <- subject2x2[i, ]

    # other individuals that could be opponents
    opponent_tmp <- subject2x2[-i, ]

    # extract opponents
    opponent_tmp <- opponent_tmp[opponent_tmp$Plot == focal_tmp$Plot, ]

    # store results
    opponentN_tmp <- nrow(opponent_tmp)
    if(opponentN_tmp == 0){
        # this focal is isolated individual
        result_tmp <- data.frame(
            "NumTrial"    = Trial,
            "DataFrom"    = "2x2",
            "Focal_Plot"  = focal_tmp$Plot,
            "Focal_x"     = NA,
            "Focal_y"     = NA,
            "Focal_ID"    = focal_tmp$ID, 
            "Focal_Sp"    = focal_tmp$Sp, 
            "Focal_H"     = focal_tmp$H18,
            "Focal_DBH"   = focal_tmp$dbh18,
            "Opponent_ID" = 0,
            "Opponent_Sp" = "Isolation",
            "Opponent_H"  = NA,
            "Opponent_DBH"= NA,
            "Distance"    = NA  
        )
    } else {
        result_tmp <- data.frame(
            "NumTrial"    = rep(Trial,          opponentN_tmp),
            "DataFrom"    = rep("2x2",          opponentN_tmp),
            "Focal_Plot"  = rep(focal_tmp$Plot, opponentN_tmp),
            "Focal_x"     = rep(NA,             opponentN_tmp),
            "Focal_y"     = rep(NA,             opponentN_tmp),
            "Focal_ID"    = rep(focal_tmp$ID,   opponentN_tmp), 
            "Focal_Sp"    = rep(focal_tmp$Sp,   opponentN_tmp), 
            "Focal_H"     = rep(focal_tmp$H18,  opponentN_tmp), 
            "Focal_DBH"   = rep(focal_tmp$dbh18,opponentN_tmp), 
            "Opponent_ID" = opponent_tmp$ID,
            "Opponent_Sp" = opponent_tmp$Sp,
            "Opponent_H"  = opponent_tmp$H18,
            "Opponent_DBH"= opponent_tmp$dbh18,
            "Distance"    = rep(NA,             opponentN_tmp)
        )
    }

    # store
    result_tmpN <- nrow(result_tmp)
    if(!identical(colnames(result_tmp), colnames(matchupList))){
        stop("Colnames of 'result_tmp' differ from that of 'matchupList.\n")
    }
    matchupList[(listRowsN + 1):(listRowsN + result_tmpN), ] <- result_tmp

    # for next repetition
    listRowsN <- nrow(matchupList)
}

# from FP
i <- 1
for(i in 1:rowNFP){
    # counting repetitions
    Trial <- Trial + 1

    # target an individual as focal
    focal_tmp <- subjectFP[i, ]

    # other individuals that could be opponents
    opponent_tmp <- subjectFP[-i, ]

    # extract opponents
    opponent_tmp$delta_x <- opponent_tmp$global_x - focal_tmp$global_x
    opponent_tmp$delta_y <- opponent_tmp$global_y - focal_tmp$global_y
    opponent_tmp$dist    <- sqrt(
        (opponent_tmp$delta_x)^2 + (opponent_tmp$delta_y)^2
    )
    opponent_tmp <- opponent_tmp[opponent_tmp$dist <= radius, ]

    # store results
    opponentN_tmp <- nrow(opponent_tmp)
    if(opponentN_tmp == 0){
        # this focal is isolated individual
        result_tmp <- data.frame(
            "NumTrial"    = Trial,
            "DataFrom"    = "FP",
            "Focal_Plot"  = NA,
            "Focal_x"     = focal_tmp$global_x,
            "Focal_y"     = focal_tmp$global_y,
            "Focal_ID"    = focal_tmp$ID, 
            "Focal_Sp"    = focal_tmp$Sp, 
            "Focal_H"     = focal_tmp$height2019,
            "Focal_DBH"   = focal_tmp$dbh2019,
            "Opponent_ID" = 0,
            "Opponent_Sp" = "Isolation",
            "Opponent_H"  = NA,
            "Opponent_DBH"= NA,
            "Distance"    = NA
        )
    } else {
        result_tmp <- data.frame(
            "NumTrial"    = rep(Trial, opponentN_tmp),
            "DataFrom"    = rep("FP", opponentN_tmp),
            "Focal_Plot"  = NA,
            "Focal_x"     = rep(focal_tmp$global_x, opponentN_tmp),
            "Focal_y"     = rep(focal_tmp$global_y, opponentN_tmp),
            "Focal_ID"    = rep(focal_tmp$ID, opponentN_tmp), 
            "Focal_Sp"    = rep(focal_tmp$Sp, opponentN_tmp), 
            "Focal_H"     = rep(focal_tmp$height2019, opponentN_tmp),
            "Focal_DBH"   = rep(focal_tmp$dbh2019, opponentN_tmp),
            "Opponent_ID" = opponent_tmp$ID,
            "Opponent_Sp" = opponent_tmp$Sp,
            "Opponent_H"  = opponent_tmp$height2019,
            "Opponent_DBH"= opponent_tmp$dbh2019,
            "Distance"    = opponent_tmp$dist
        )
    }

    # store
    result_tmpN <- nrow(result_tmp)
    if(!identical(colnames(result_tmp), colnames(matchupList))){
        stop("Colnames of 'result_tmp' differ from that of 'matchupList.\n")
    }
    matchupList[(listRowsN + 1):(listRowsN + result_tmpN), ] <- result_tmp

    # for next repetition
    listRowsN <- nrow(matchupList)
}


if(saveFiles){
  write.csv(matchupList, "output/02matchupList/03MatchupListLargeSaplings.csv", row.names = FALSE)
}


#### make matchup table ####
matchupTable <- as.data.frame.matrix(table(matchupList$Focal_Sp, matchupList$Opponent_Sp))

# Order rows
matchupTable$Sp <- rownames(matchupTable)
matchupTable <- merge(
    matchupTable,
    spOrder,
    all.x = TRUE, all.y = TRUE,
    by = "Sp"
)
matchupTable <- matchupTable[order(matchupTable$Order), ]
rownames(matchupTable) <- matchupTable$Sp
matchupTable <- matchupTable[ , !(colnames(matchupTable) %in% c("Sp", "Order"))]

# Order columns
matchupTable <- data.frame(t(matchupTable))
matchupTable$Sp <- rownames(matchupTable)
matchupTable <- merge(
    matchupTable,
    spOrder,
    all.x = TRUE, all.y = TRUE,
    by = "Sp"
)
matchupTable <- matchupTable[order(matchupTable$Order), ]
rownames(matchupTable) <- matchupTable$Sp
matchupTable <- matchupTable[ , !(colnames(matchupTable) %in% c("Sp", "Order"))]
matchupTable <- data.frame(t(matchupTable))

matchupTable[is.na(matchupTable)] <- 0


if(saveFiles){
  write.csv(matchupTable, "output/03matchupTable/03MatchupTableLargeSaplings.csv", row.names = TRUE)
}
