##### Matchup list for seedlings #####
#### initial settings ####
rm(list=ls(all=TRUE)) # clear all objects
saveFiles <- TRUE     # Do you want save result file?

#### Read data ####
subject <- read.csv("output/01subjects/01SubSeedlings.csv")
sampleSize <- read.csv("output/01subjects/00Appendix1_sampleSizes.csv")

# species list
sampleSize[sampleSize == "-"] <- 0
spList  <- sampleSize$Sp
spOrder <- data.frame("Sp" = sampleSize$Sp, "Order" = 1:length(spList))

#### initial objects ####
years  <- 2009:2018     # vector of year used
yearsN <- length(years) # num years used

# counting for loop process
rowN <- nrow(subject)
spN  <- length(spList)

# frame of resultTable
matchupList <- data.frame(
  "NumTrial"    = NA, # number of trials
  "Year"        = NA, # a year when a focal individual was observed
  "Focal_x"     = NA, # global x-coordinate of a quadrat where focal individual in
  "Focal_y"     = NA, # global y-coordinate of a quadrat where focal individual in
  "Focal_ID"    = NA, # ID of a focal individual
  "Focal_Sp"    = NA, # species code of a focal individual
  "Focal_H"     = NA, # height of a focal individual
  "Opponent_ID" = NA, # ID of a opponent individual
  "Opponent_Sp" = NA, # species code of a opponent individual
  "Opponent_H"  = NA  # height of a opponent individual
)

#### loop processes for extracting "matchups" ####
Trial     <- 0 # repetition time
listRowsN <- 0 # number of rows in matchup list
y <- 1 # repeat "yearsN" times

for(y in 1:yearsN){
    # target column name in a loop
    targetCol  <- c(
        "Sp", "ID", "Plot_x", "Plot_y",
        paste("H", years[y], sep = ""),
        paste("S", years[y], sep = "")
    )
    
    # only y year
    yYearsOnly <- subject[ , colnames(subject) %in% targetCol]

    # only target individuals in year y
    yYearsOnly <- yYearsOnly[yYearsOnly[ ,6] == 1, ]

    # count target individuals in year y
    indivNy <- nrow(yYearsOnly)

    # rownames renamed
    rownames(yYearsOnly) <- c(1:indivNy)

    i <- 1
    for(i in 1:indivNy){
        # counting repetitions
        Trial <- Trial + 1

        # target an individual as focal
        focal_tmp <- yYearsOnly[i, ]

        # other individuals that could be opponents
        opponent_tmp <- yYearsOnly[-i, ]

        # extract opponents
        opponent_tmp <- opponent_tmp[opponent_tmp$Plot_x == focal_tmp$Plot_x & opponent_tmp$Plot_y == focal_tmp$Plot_y, ]

        # store results
        opponentN_tmp <- nrow(opponent_tmp)
        if(opponentN_tmp == 0){
            # this focal is isolated individual
            result_tmp <- data.frame(
                "NumTrial"    = Trial,
                "Year"        = years[y],
                "Focal_x"     = focal_tmp$Plot_x,
                "Focal_y"     = focal_tmp$Plot_y, 
                "Focal_ID"    = focal_tmp$ID, 
                "Focal_Sp"    = focal_tmp$Sp, 
                "Focal_H"     = focal_tmp[ ,5],
                "Opponent_ID" = NA,
                "Opponent_Sp" = "Isolation",
                "Opponent_H"  = NA  
            )
        } else {
            result_tmp <- data.frame(
                "NumTrial"    = rep(Trial,            opponentN_tmp),
                "Year"        = rep(years[y],         opponentN_tmp),
                "Focal_x"     = rep(focal_tmp$Plot_x, opponentN_tmp),
                "Focal_y"     = rep(focal_tmp$Plot_y, opponentN_tmp), 
                "Focal_ID"    = rep(focal_tmp$ID,     opponentN_tmp), 
                "Focal_Sp"    = rep(focal_tmp$Sp,     opponentN_tmp),
                "Focal_H"     = rep(focal_tmp[ ,5],   opponentN_tmp),
                "Opponent_ID" = opponent_tmp$ID,
                "Opponent_Sp" = opponent_tmp$Sp,
                "Opponent_H"  = opponent_tmp[ ,5]  
            )
        }

        # store
        result_tmpN <- nrow(result_tmp)
        matchupList[(listRowsN + 1):(listRowsN + result_tmpN), ] <- result_tmp

        # for next repetition
        listRowsN <- nrow(matchupList)
    }
}


if(saveFiles){
  write.csv(matchupList, "output/02matchupList/01MatchupListSeedlings.csv", row.names = FALSE)
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
  write.csv(matchupTable, "output/03matchupTable/01MatchupTableSeedlings.csv", row.names = TRUE)
}
