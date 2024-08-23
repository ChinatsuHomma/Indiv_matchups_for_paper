##### Overview of Matchups #####
#### initial settings ####
rm(list=ls(all=TRUE)) # clear all objects
saveFiles <- TRUE     # Do you want save result file?

#### initial object ####
LifeStage <- c(
    "Seedlings", "SmallSaplings", "LargeSaplings",
    "Poles", "Juveniles", "Subadults"
)
Files <- list.files("output/02matchupList", full.names = TRUE)
# years of seedlings
Year <- c(2009:2018)
YearN <- length(Year)

#### Read data ####
Table2 <- read.csv("output/01subjects/00Table2_sampleSizes.csv")
Appendix1 <- read.csv("output/01subjects/00Appendix1_sampleSizes.csv")
MatchupList <- lapply(Files, read.csv)
names(MatchupList) <- LifeStage

StudySp  <- Table2$Sp[-length(Table2$Sp)]
StudySpN <- length(StudySp)

#### result table ####
# summarize how many *species* were matched-up per individual of focal species
NumMatchupSp <- as.data.frame.matrix(matrix(rep(NA), nrow = StudySpN, ncol = 6))
colnames(NumMatchupSp) <- LifeStage
rownames(NumMatchupSp) <- StudySp

# summarize how many *individuals* were matched-up per individual of focal species
NumMatchupIndiv <- NumMatchupSp # Use same frame

# for summing results for each year (saplings)
EachYear <- as.data.frame.matrix(
    matrix(rep(NA), nrow = YearN, ncol = 2)
)
colnames(EachYear) <- c("NumMatchupSp", "NumMatchupIndiv")
rownames(EachYear) <- Year


#### Summarize matchups ####
l <- 1 # choose Life stage
s <- 1 # choose species
y <- 1 # choose years (seedling only)

for(l in 1:6){
    List_tmp <- MatchupList[[l]]

    for(s in 1:StudySpN){
        # extract a study species
        List_Sp_tmp <- List_tmp[List_tmp$Focal_Sp == StudySp[s], ]

        if(l == 1){
            # only sapling
            for(y in 1:YearN){
                if(y == 1){
                    EachYear_tmp <- EachYear
                }

                # extract data of a year
                List_Sp_Year_tmp <- List_Sp_tmp[List_Sp_tmp$Year == Year[y], ]

                if(nrow(List_Sp_Year_tmp) == 0){
                    # no applicable data
                    EachYear_tmp[y, c(1:2)] <- NA
                } else {
                    Result_tmp <- as.data.frame.matrix(
                        table(List_Sp_Year_tmp$Focal_ID, List_Sp_Year_tmp$Opponent_Sp)
                    )
                    Result_tmp <- Result_tmp[ , colnames(Result_tmp) != "Isolation"]

                    if(is.null(nrow(Result_tmp))){
                        # Result_tmp is vector
                        EachYear_tmp$NumMatchupSp[y] <- sum(Result_tmp != 0, na.rm = TRUE) /length(Result_tmp)
                        EachYear_tmp$NumMatchupIndiv[y] <- mean(Result_tmp)
                    } else {
                        # summarize about matchup *species*
                        NumMatchupSp_tmp  <- rowSums(Result_tmp != 0, na.rm = TRUE)
                        EachYear_tmp$NumMatchupSp[y] <- mean(NumMatchupSp_tmp)

                        # summarize about matchup *individuals*
                        NumMatchupIndiv_tmp  <- rowSums(Result_tmp, na.rm = TRUE)
                        EachYear_tmp$NumMatchupIndiv[y] <- mean(NumMatchupIndiv_tmp)
                    }
                }            
            }
            # mean for each year
            # summarize about matchup *species*
            NumMatchupSp[s,l] <- mean(EachYear_tmp$NumMatchupSp, na.rm = TRUE)

            # summarize about matchup *individuals*
            NumMatchupIndiv[s,l] <- mean(EachYear_tmp$NumMatchupIndiv, na.rm = TRUE)
        } else {
            # other life stages
            # summarize
            Result_tmp <- as.data.frame.matrix(
                table(List_Sp_tmp$Focal_ID, List_Sp_tmp$Opponent_Sp)
            )
            Result_tmp <- Result_tmp[ , colnames(Result_tmp) != "Isolation"]

            # summarize about matchup *species*
            NumMatchupSp_tmp  <- rowSums(Result_tmp != 0, na.rm = TRUE)
            NumMatchupSp[s,l] <- mean(NumMatchupSp_tmp)

            # summarize about matchup *individuals*
            NumMatchupIndiv_tmp  <- rowSums(Result_tmp, na.rm = TRUE)
            NumMatchupIndiv[s,l] <- mean(NumMatchupIndiv_tmp)
        }
    }
}


if(saveFiles){
    write.csv(NumMatchupSp,    "output/05forFigure/00DataForFig2_01MatchupsForEachIndiv_Sp.csv")
    write.csv(NumMatchupIndiv, "output/05forFigure/00DataForFig2_02MatchupsForEachIndiv_Indiv.csv")
}
