##### Prepare for GLM #####
#### initial settings ####
rm(list=ls(all=TRUE)) # clear all objects
saveFiles <- TRUE     # Do you want save result file?
#saveFiles <- FALSE

#### Library ####
#library("lme4")
#library("car")
library("stringr")

#### initial object ####
LifeStage <- c(
    "Seedlings", "SmallSaplings", "LargeSaplings",
    "Poles", "Juveniles", "Subadults"
)
Sp <- c("PR", "FC", "AP", "AT", "UL", "CJ", "QC")
Files <- list.files("output/02matchupList", full.names = TRUE)

Chart_coreX <- data.frame(
    "Focal_x" = c(1:11),
    "xtmp"    = LETTERS[8:(8+10)]
)

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

    if(length(MatchupList[[i]]$Focal_x) > 0){
        if(i == 1){ # seedlings
            MatchupList[[i]] <- merge(
                MatchupList[[i]], Chart_coreX,
                by = "Focal_x",
                all.x = TRUE, all.y = FALSE
            )
            MatchupList[[i]]$ytmp <- MatchupList[[i]]$Focal_y + 25
        } else {
            MatchupList[[i]]$xtmp <- LETTERS[trunc(MatchupList[[i]]$Focal_x / 10) + 1]
            MatchupList[[i]]$ytmp <- str_pad(trunc(MatchupList[[i]]$Focal_y / 10) + 1, 2, pad = 0)
        }
    }

    if(i == 2 | i == 3){
        MatchupList[[i]]$xtmp[is.na(MatchupList[[i]]$xtmp)] <- str_extract(MatchupList[[i]]$Focal_Plot, "^[:UPPER:]{1}")[is.na(MatchupList[[i]]$xtmp)]
        MatchupList[[i]]$ytmp[is.na(MatchupList[[i]]$ytmp)] <- str_extract(MatchupList[[i]]$Focal_Plot, "[:digit:]{2}$")[is.na(MatchupList[[i]]$ytmp)]
    }

    MatchupList[[i]]$Location <- paste(MatchupList[[i]]$xtmp, MatchupList[[i]]$ytmp, sep = "")

    if(i == 1){
        AllMatchupList <- MatchupList[[i]][ ,colnames(MatchupList[[i]]) %in% c("NumTrial", "LifeStage", "Focal_ID", "Focal_Sp", "Opponent_ID", "Opponent_Sp", "Location")]
    } else {
        AllMatchupList <- rbind(
            AllMatchupList, MatchupList[[i]][ ,colnames(MatchupList[[i]]) %in% c("NumTrial", "LifeStage", "Focal_ID", "Focal_Sp", "Opponent_ID", "Opponent_Sp", "Location")]
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

Result <- as.data.frame.matrix(matrix(rep(NA), nrow = NumIndiv, ncol = 8))
colnames(Result) <- c(
    "TrialAndLifeStage", "LifeStage", "Focal_ID", "Focal_Sp", "Conspecific", "Heterospecific", "BinIsolation", "Location"
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
                0, # this individual did NOT face heterospecific matchup
                1, # this individual was isolated
                unique(Indiv_tmp$Location)
            )
        } else {
            # this is not isolated individual, 
            # and how many conspecific matchups did this individual face?
            Result[i, ] <- c(
                IndivList[i],
                unique(Indiv_tmp$LifeStage),
                unique(Indiv_tmp$Focal_ID),
                Focal_Sp_tmp,
                sum(Indiv_tmp$Opponent_Sp == Focal_Sp_tmp), # conspecific matchup
                sum(Indiv_tmp$Opponent_Sp != Focal_Sp_tmp), # heterospecific matchup
                0, # this individual was NOT isolated
                unique(Indiv_tmp$Location)
            )
        }
    }
}

if(saveFiles){
    write.csv(Result, "output/07GLM/00dataForGLM.csv", row.names = FALSE)
}
