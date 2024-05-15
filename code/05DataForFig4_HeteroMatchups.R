##### Make Figure 5 #####
#### initial settings ####
rm(list=ls(all=TRUE)) # clear all objects
saveFiles <- TRUE     # Do you want save result file?

#### Read data ####
Table3 <- read.csv("output/04allMatchupTable/00Table3_AllMatchupTable.csv")

#### initial object ####
LifeStage <- c(
    "Seedlings", "Small-saplings", "Large-saplings",
    "Poles", "Juveniles", "Subadults"
)

# for results
Hetero <- as.data.frame.matrix(
    matrix(rep(NA), nrow = 8*8, ncol = 11)
)
colnames(Hetero) <- c(
    "FocalNum", "OpponentNum", "NumSet",
    "FocalSp", "OpponentSp",
    LifeStage
)

Num <- expand.grid(1:8,1:8)

Hetero$FocalNum    <- Num$Var2
Hetero$OpponentNum <- Num$Var1
Hetero$NumSet <- paste(Hetero$FocalNum, Hetero$OpponentNum, sep = "-")

i <- 1
for(i in 1:8){
    HeteroRow_tmp <- (1:8)+(i-1)*8
    Table3Row_tmp <- (1:6)+(i-1)*6
    TotalMatchups_tmp <- rowSums(Table3[Table3Row_tmp,5:13])
    Hetero[HeteroRow_tmp, 6:11]      <- t(Table3[Table3Row_tmp,5:12]/TotalMatchups_tmp * 100)
    Hetero$FocalSp[HeteroRow_tmp]    <- rep(Table3$FocalSp[Table3Row_tmp[1]])
    Hetero$OpponentSp[HeteroRow_tmp] <- colnames(Table3)[5:12]
}

# only Heterospecific matchups
Hetero <- Hetero[Hetero$FocalSp != Hetero$OpponentSp, ]

if(saveFiles){
    write.csv(Hetero, "output/05forFigure/00DataForFig4_01HeteroMatchups.csv", row.names = FALSE)
}
