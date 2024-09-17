##### Ranking of matchup frequently #####
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

#### subject species ####
SubSp  <- Table2$Sp
SubSpN <- length(SubSp) - 1

#### for results ####
Appendix3 <- as.data.frame.matrix(
    matrix(rep(NA), nrow = (length(LifeStage) * SubSpN), ncol = 10)
)
colnames(Appendix3) <- c("FocalSp", SubSp, "note")

Rank_tmp <- as.data.frame.matrix(
    matrix(rep(NA), nrow = nrow(Appendix1), ncol = (length(LifeStage) * 2))
)
colnames(Rank_tmp) <- c(
    paste("Rank", LifeStage, sep = ""),
    paste("Num",  LifeStage, sep = "")
)
rownames(Rank_tmp) <- Appendix1$Sp
Rank <- list()

#### make rank table ####
i <- 1
j <- 1
for(i in 1:SubSpN){ # choose species
    Rank[[i]] <- Rank_tmp

    for(j in 1:6){ # choose life stages
        Top_tmp <- data.frame()

        tmp <- MatchupTable[[j]][i, 1:(1 + nrow(Appendix1))]
        tmp <- tmp[-1]
        
        now_tmp  <- 10000
        rank_tmp <- 1
        next_tmp <- nrow(Appendix1)

        while(next_tmp > 0){
            now_tmp  <- max(tmp[tmp < now_tmp & tmp != 0], na.rm = TRUE)

            # fill data
            Rank[[i]][(which(tmp == now_tmp)), j]       <- rep(rank_tmp)
            Rank[[i]][(which(tmp == now_tmp)), (j + 6)] <- rep(now_tmp)

            rank_tmp <- rank_tmp + length(tmp[tmp == now_tmp])
            next_tmp <- length(tmp[tmp < now_tmp & tmp != 0])
        }
        
        Appendix3$FocalSp[6 * (i - 1) + j] <- SubSp[i]
        Top_tmp <- data.frame(
            "Sp"   = rownames(Rank[[i]])[which(Rank[[i]][j] <= 3)],
            "Rank" = Rank[[i]][which(Rank[[i]][j] <= 3), j],
            "Plus" = rep(NA)
        )
        Top_tmp$Plus <- c("+++", "++", "+")[Top_tmp$Rank]
        Top_tmp$Others[!(Top_tmp$Sp %in% SubSp)] <- Top_tmp$Sp[!(Top_tmp$Sp %in% SubSp)]
        Top_tmp$Sp[!(Top_tmp$Sp %in% SubSp)] <- "Other species"

        Top_tmpN <- nrow(Top_tmp)
        k <- 1
        for(k in 1:Top_tmpN){
            if(k == 1){
                Appendix3[(6 * (i - 1) + j), which(colnames(Appendix3) == Top_tmp$Sp[k])] <- Top_tmp$Plus[k]
            } else {
                if(!(Top_tmp$Sp[k] == Top_tmp$Sp[k-1] & Top_tmp$Rank[k] >= Top_tmp$Rank[k-1])){
                    Appendix3[(6 * (i - 1) + j), which(colnames(Appendix3) == Top_tmp$Sp[k])] <- Top_tmp$Plus[k]
                }
            }
        }
        if(sum(Top_tmp$Sp %in% "Other species") >= 1){
            if(length(unique(Top_tmp$Plus[Top_tmp$Sp == "Other species"])) > 2){
                Appendix3$note[(6 * (i - 1) + j)] <- paste(
                    na.omit(
                        Top_tmp$Others[Top_tmp$Plus == ifelse(any(Top_tmp$Plus[Top_tmp$Sp == "Other species"] %in% "+++"), "+++", "++")]
                    ), collapse = ", "
                )
            } else {
                Appendix3$note[(6 * (i - 1) + j)] <- paste(na.omit(Top_tmp$Others), collapse = ", ")
            }
            
        }
    }
}

if(saveFiles){
    write.csv(Appendix3, "output/04allMatchupTable/00Appendix3_Rank.csv", row.names = FALSE)
    for(i in 1:SubSpN){
        write.csv(
            Rank[[i]], 
            paste(
                "output/04allMatchupTable/00Appendix3",
                i,
                SubSp[i],                        
                "Rank.csv",
                sep = "_"
            )
        )
    }
}

