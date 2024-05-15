##### Prepare for GLM #####
#### initial settings ####
rm(list=ls(all=TRUE)) # clear all objects
saveFiles <- TRUE     # Do you want save result file?
#saveFiles <- FALSE

#### Library ####
#library("lme4")
#library("car")

#### initial object ####
LifeStage <- c(
    "Seedlings", "SmallSaplings", "LargeSaplings",
    "Poles", "Juveniles", "Subadults"
)
Sp <- c("AS", "PR", "FC", "AP", "AT", "UL", "CJ", "QC")
Files <- list.files("output/02matchupList", full.names = TRUE)

#### Read data ####
MatchupList <- list()
MatchupList <- lapply(Files, read.csv)
names(MatchupList) <- LifeStage

#### make some objects for extraction ####
# extract the data for study species
N <- length(MatchupList)
for(i in 1:N){
    MatchupList[[i]] <- MatchupList[[i]][MatchupList[[i]]$Focal_Sp %in% Sp, ]
} # OK

AllFocalIndiv <- rowSums(data.frame(lapply(MatchupList, nrow))) 

for(i in 1:N){
    MatchupList[[i]]$LifeStage <- LifeStage[i]

    if(i == 1){
        AllMatchupList <- MatchupList[[i]][ ,colnames(MatchupList[[i]]) %in% c("NumTrial", "LifeStage", "Focal_ID", "Focal_Sp", "Opponent_ID", "Opponent_Sp")]
    } else {
        AllMatchupList <- rbind(
            AllMatchupList, MatchupList[[i]][ ,colnames(MatchupList[[i]]) %in% c("NumTrial", "LifeStage", "Focal_ID", "Focal_Sp", "Opponent_ID", "Opponent_Sp")]
        )
    }
} # OK

rownames(AllMatchupList) <- c(1:nrow(AllMatchupList))

AllMatchupList$TrialAndLifeStage <- paste(
    AllMatchupList$NumTrial, AllMatchupList$LifeStage,
    sep = "_"
)

#### make data for GLM ####
# make dataframe for results
IndivList <- unique(AllMatchupList$TrialAndLifeStage)
NumIndiv  <- length(IndivList)

Result <- as.data.frame.matrix(matrix(rep(NA), nrow = NumIndiv, ncol = 6))
colnames(Result) <- c(
    "TrialAndLifeStage", "LifeStage", "Focal_ID", "Focal_Sp", "BinConspecific", "BinIsolation"
)

# iterate over all Focal individuals
i <- 1 # choose each individual
for(i in 1:NumIndiv){
    Indiv_tmp <- AllMatchupList[AllMatchupList$TrialAndLifeStage == IndivList[i], ]
    Focal_Sp_tmp <- unique(Indiv_tmp$Focal_Sp)

    if(length(Focal_Sp_tmp) != 1){
        stop("2 or more focal species were chosen.\n")
    } else {

        # this individual was isolated?
        if(all(Indiv_tmp$Opponent_Sp == "Isolation")){
            # this is isolated individual
            Result[i, ] <- c(
                IndivList[i],
                unique(Indiv_tmp$LifeStage),
                unique(Indiv_tmp$Focal_ID),
                Focal_Sp_tmp,
                0, # this individual did NOT face conspecific matchup
                1  # this individual was isolated
            )
        } else {
            # this is not isolated individual, 
            # and did this individual face conspecific matchups?
            if(any(Indiv_tmp$Opponent_Sp == Focal_Sp_tmp)){
                # this individual faced conspecific matchup
                Result[i, ] <- c(
                    IndivList[i],
                    unique(Indiv_tmp$LifeStage),
                    unique(Indiv_tmp$Focal_ID),
                    Focal_Sp_tmp,
                    1, # this individual faced conspecific matchup
                    0  # this individual was NOT isolated
                )
            } else {
                # this individual did NOT face conspecific matchup
                Result[i, ] <- c(
                    IndivList[i],
                    unique(Indiv_tmp$LifeStage),
                    unique(Indiv_tmp$Focal_ID),
                    Focal_Sp_tmp,
                    0, # this individual faced conspecific matchup
                    0  # this individual was NOT isolated
                )
            }
        }
    }
}

if(saveFiles){
    write.csv(Result, "output/07GLM/00dataForGLM.csv", row.names = FALSE)
}
