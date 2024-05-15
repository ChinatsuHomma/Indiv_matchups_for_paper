##### Merge All Matchup Tables #####
#### initial settings ####
rm(list=ls(all=TRUE)) # clear all objects
saveFiles <- TRUE     # Do you want save result file?

#### initial object ####
LifeStage <- c(
    "Seedlings", "SmallSaplings", "LargeSaplings",
    "Poles", "Juveniles", "Subadults"
)
Files <- list.files("output/03matchupTable", full.names = TRUE)

#### Read data ####
Table2 <- read.csv("output/01subjects/00Table2_sampleSizes.csv")
Appendix1 <- read.csv("output/01subjects/00Appendix1_sampleSizes.csv")
MatchupTable <- lapply(Files, read.csv)
names(MatchupTable) <- LifeStage

#### cleanup data ####
l <- 1
for(l in 1:6){
    colnames(MatchupTable[[l]])[1] <- "Sp"
}
Table2[Table2 == "-"] <- 0
Appendix1[Appendix1 == "-"] <- 0


#### Merge All Tables ####
#Table3_tmp <- do.call("rbind", MatchupTable)
s   <- 1 # choose species
spN <- nrow(Table2)
allSpN <- nrow(Appendix1)
l <- 1 # choose Life stage
order <- 0 # for ordering Table 3

# frame of Table3
Table3 <- as.data.frame.matrix(matrix(rep(NA), nrow = 8*6, ncol = 18))
colnames(Table3) <- c(
    "Order", "FocalSp", "LifeStage", "NumIndiv",
    Table2$Sp,
    "ConMatchups", "HeteroMatchups", "PercentCon",
    "Isolations", "PercentIso"
)

for(s in 1:8){
    for(l in 1:6){
        order <- order + 1

        # in a life stage l,
        ## Name of species s
        Sp_tmp         <- MatchupTable[[l]][s,1]
        ## Matchup freq between individuals of the species s and that of study species
        StudySp_tmp    <- MatchupTable[[l]][s,c(2:9)]
        ## Matchup freq between individuals of the species s and that of NOT study species
        OtherSp_tmp    <- MatchupTable[[l]][s,c(10:(allSpN + 1))]
        ## Sum up matchup freq with other species
        SumOtherSp_tmp <- rowSums(OtherSp_tmp, na.rm = TRUE)
        ## Isolated individual of the species s
        Isolation_tmp  <- MatchupTable[[l]][s,allSpN + 2]
        ## Number of focal individuals of the species s
        FocalN_tmp     <- Table2[s,(l+1)]

        # freq of conspecific matchups
        FreqCon_tmp <- StudySp_tmp[ ,s]
        # freq of heterospecific matchups
        FreqHet_tmp <- sum(StudySp_tmp[ ,-s], na.rm = TRUE) + SumOtherSp_tmp
        # Percent of conspecific matchups
        ConPCT_tmp  <- round(FreqCon_tmp/(FreqCon_tmp + FreqHet_tmp) * 100, 1)

        # Percent of isolated individuals
        IsoPCT_tmp  <- round(Isolation_tmp/FocalN_tmp * 100, 1)

        # into data frame
        Table3[l + (6 * (s - 1)), ] <- c(
            order, Sp_tmp, LifeStage[l], FocalN_tmp,
            t(StudySp_tmp), t(SumOtherSp_tmp),
            FreqCon_tmp, FreqHet_tmp, ConPCT_tmp,
            Isolation_tmp, IsoPCT_tmp
        )
    }
}


if(saveFiles){
  write.csv(Table3, "output/04allMatchupTable/00Table3_AllMatchupTable.csv", row.names = FALSE)
}
