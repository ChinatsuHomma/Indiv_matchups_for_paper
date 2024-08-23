##### Make Figure 5 #####
#### initial settings ####
rm(list=ls(all=TRUE)) # clear all objects
saveFiles <- TRUE     # Do you want save result file?

#### Read data ####
Table3 <- read.csv("output/04allMatchupTable/00Table3_AllMatchupTable.csv")
Table2 <- read.csv("output/01subjects/00Table2_sampleSizes.csv")

#### initial object ####
LifeStage <- c(
    "Seedlings", "Small-saplings", "Large-saplings",
    "Poles", "Juveniles", "Subadults"
)
LifeStageN <- length(LifeStage)

# for results
All <- as.data.frame.matrix(
    matrix(rep(NA), nrow = 7*7, ncol = 11)
)
colnames(All) <- c(
    "FocalNum", "OpponentNum", "NumSet",
    "FocalSp", "OpponentSp",
    LifeStage
)
SpN <- length(Table2$Sp[-length(Table2$Sp)])

Num <- expand.grid(1:SpN,1:SpN)

All$FocalNum    <- Num$Var2
All$OpponentNum <- Num$Var1
All$NumSet <- paste(All$FocalNum, All$OpponentNum, sep = "-")

Hetero <- All

i <- 1
for(i in 1:SpN){
    AllRow_tmp <- (1:SpN)+(i-1)*SpN
    Table3Row_tmp <- (1:LifeStageN)+(i-1)*LifeStageN

    TotalMatchups_tmp  <- Table3[Table3Row_tmp, (5 + SpN + 1)] + Table3[Table3Row_tmp, (5 + SpN + 2)]
    HeteroMatchups_tmp <- Table3[Table3Row_tmp, (5 + SpN + 2)]
    
    # the num of a pair of matchups divided by that of all matchups
    All[AllRow_tmp, 6:11]      <- t(Table3[Table3Row_tmp,5:(5 + SpN - 1)]/TotalMatchups_tmp * 100)
    All$FocalSp[AllRow_tmp]    <- rep(Table3$FocalSp[Table3Row_tmp[1]])
    All$OpponentSp[AllRow_tmp] <- colnames(Table3)[5:(5 + SpN - 1)]

    # the num of a pair of matchups divided by that of all matchups
    tmpHetero <- t(Table3[Table3Row_tmp,5:(5 + SpN - 1)]/HeteroMatchups_tmp * 100)
    tmpHetero[i, ] <- rep(NA)
    Hetero[AllRow_tmp, 6:11] <- tmpHetero
    Hetero$FocalSp[AllRow_tmp]    <- rep(Table3$FocalSp[Table3Row_tmp[1]])
    Hetero$OpponentSp[AllRow_tmp] <- colnames(Table3)[5:(5 + SpN - 1)]

}

# current ver. (only hetero from the object "All")
Current <- All[All$FocalSp != All$OpponentSp, ]

# only heterospecific matchups
Hetero <- Hetero[Hetero$FocalSp != Hetero$OpponentSp, ]

if(saveFiles){
    write.csv(Current, "output/05forFigure/00DataForFig4_01All-ConMatchups.csv", row.names = FALSE)
    write.csv(All,     "output/05forFigure/00DataForFig4_02AllMatchups.csv",     row.names = FALSE)
    write.csv(Hetero,  "output/05forFigure/00DataForFig4_03HeteroMatchups.csv",  row.names = FALSE)
}
