##### PCA with {vegan} #####
#### initial settings ####
rm(list=ls(all=TRUE)) # clear all objects
saveFiles <- TRUE     # Do you want save result file?
#saveFiles <- FALSE
scaling <- 1 # 1 = 1, 2 = 2, 0 = both

#### packages ####
library("vegan")

#### functions ####
source("code/fromBook/cleanplot.pca.R") # modified only about graphic params

#### Read data ####
All     <- read.csv("output/05forFigure/00DataForFig4_02AllMatchups.csv")
Hetero  <- read.csv("output/05forFigure/00DataForFig4_03HeteroMatchups.csv")

#### organize data ####
rownames(All) <- All$NumSet
All <- as.matrix(All[ , c(6:11)]) # for PCA
rownames(Hetero) <- Hetero$NumSet
Hetero <- as.matrix(Hetero[ , c(6:11)]) # for PCA

#### initial object ####
LifeStage <- c(
    "Seedlings", "Small-saplings", "Large-saplings",
    "Poles", "Juveniles", "Subadults"
)
Sp <- c(
    "Pterocarya", "Fagus", 
    "A. pictum", "Aesculus", "Ulmus", 
    "Cercidiphyllum ", "Quercus"
)

##### pairs #####
comb <- as.data.frame.matrix(t(combn(c(1:7), 2)))
seg  <- data.frame(
    "pair1" = paste(comb$V1, comb$V2, sep = "-"),
    "pair2" = paste(comb$V2, comb$V1, sep = "-")
)

#### principal component analysis ####
PCA_All <- rda(All, scale = FALSE) 

summary(PCA_All) # scaling = 2
summary(PCA_All, scale = 1) # scaling = 1

screeplot(PCA_All, bstick = TRUE, npcs = length(PCA_All$CA$eig), las = 2)

PCA_Hetero <- rda(Hetero, scale = FALSE) 

summary(PCA_Hetero) # scaling = 2
summary(PCA_Hetero, scale = 1) # scaling = 1

screeplot(PCA_Hetero, bstick = TRUE, npcs = length(PCA_Hetero$CA$eig), las = 2)

#### plot ####
# plot    
if(saveFiles){
    dev.off()
    tiff(
        filename = "output/06figures/Figure_4.tiff", 
        height = 1200, #switch((scaling + 1), 3600, 1800, 1800),
        width  = 3600, units = "px", res = 600, compression = "lzw"
    )
    size <- 0.5
} else {
    dev.new(
        height = 2.67, #switch((scaling + 1), 8, 4, 4),
        width  = 8
    )
    size <- 0.85
}

par(mfrow = c(1, 3), mar = c(3.1, 3.1, 0.2, 0.2))

i <- 1
for(i in 1:2){
    if(i == 1){
        PCA.tmp <- PCA_All
    } else {
        PCA.tmp <- PCA_Hetero
    }

    # from cleanplot.pca() to draw segments
    vec <- select.spe <- length(PCA.tmp$colsum)
    n <- nrow(PCA.tmp$CA$u)
    eig.val = PCA.tmp$CA$eig          # Eigenvalues of Y-hat
    Lambda = diag(eig.val)            # Diagonal matrix of eigenvalues
    Z.sc2 = PCA.tmp$CA$u * sqrt(n - 1)# Site scores, scaling=2
    Z.sc1 = Z.sc2 %*% Lambda ^ (0.5)  # Site scores, scaling=1
    U.sc1 = PCA.tmp$CA$v              # Species scores, scaling=1
    U.sc2 = U.sc1 %*% Lambda ^ (0.5)  # Species scores, scaling=2

    # For scaling=1
    sit.sc1 <- data.frame(Z.sc1)
    spe.sc1 <- U.sc1[vec,]
    sit.sc1$pair1 <- rownames(sit.sc1) # for merge
    sit.sc1$pair2 <- rownames(sit.sc1) # for merge

    # For scaling=2
    sit.sc2 <- data.frame(Z.sc2)
    spe.sc2 <- U.sc2[vec,]
    sit.sc2$pair1 <- rownames(sit.sc2) # for merge
    sit.sc2$pair2 <- rownames(sit.sc2) # for merge

    # for segments
    ## scaling 1 and pair 1
    seg.tmp <- merge(
        seg, sit.sc1[ , c(1,2,7)],
        all.x = TRUE, all.y = FALSE,
        by = "pair1"
    )
    colnames(seg.tmp)[c(3,4)] <- c("X_sca1_pair1", "Y_sca1_pair1")

    ## scaling 1 and pair 2
    seg.tmp <- merge(
        seg.tmp, sit.sc1[ , c(1,2,8)],
        all.x = TRUE, all.y = FALSE,
        by = "pair2"
    )
    colnames(seg.tmp)[c(5,6)] <- c("X_sca1_pair2", "Y_sca1_pair2")

    ## scaling 2 and pair 1
    seg.tmp <- merge(
        seg.tmp, sit.sc2[ , c(1,2,7)],
        all.x = TRUE, all.y = FALSE,
        by = "pair1"
    )
    colnames(seg.tmp)[c(7,8)] <- c("X_sca2_pair1", "Y_sca2_pair1")

    ## scaling 1 and pair 2
    seg.tmp <- merge(
        seg.tmp, sit.sc2[ , c(1,2,8)],
        all.x = TRUE, all.y = FALSE,
        by = "pair2"
    )
    colnames(seg.tmp)[c(9,10)] <- c("X_sca2_pair2", "Y_sca2_pair2")

    if(scaling != 2){
        # scaling 1
        ## draw nothing
        cleanplot.pca(
            PCA.tmp, scaling = 1, mar.percent = 0, 
            plot.sites  = FALSE, plot.spe  = FALSE, 
            label.sites = FALSE, label.spe = FALSE,
            cex = size#,
            #cex.lab = size
        )

        segments(
            seg.tmp$X_sca1_pair1, seg.tmp$Y_sca1_pair1,
            seg.tmp$X_sca1_pair2, seg.tmp$Y_sca1_pair2,
            col = "#00a0e9"
        )

        par(new = TRUE)
        cleanplot.pca(PCA.tmp, scaling = 1, mar.percent = 0, plot.sites = FALSE, cex = size)

        mtext(
            paste("PC", 1, " (", round(summary(PCA.tmp)$cont$importance[2,1] * 100, digits = 1),"%)", sep = ""),
            side = 1, line = 2, 
            cex  = size * ifelse(saveFiles, 1.25, 1)
        )
        mtext(
            paste("PC", 2, " (", round(summary(PCA.tmp)$cont$importance[2,2] * 100, digits = 1),"%)", sep = ""),
            side = 2, line = 2,
            cex  = size * ifelse(saveFiles, 1.25, 1)
        )

        mtext(
            switch(i, "a)", "c)"), 
            side = 3, 
            line = ifelse(saveFiles, -1, -1.25),
            adj = 0.025,
            cex = size * ifelse(saveFiles, 1.25, 1)
        )
    }

    if(i == 1){
        rect(
            -60, -60, 40, 40,
            lty = 2
        )

        # scaling 1 expanded
        ## draw nothing
        cleanplot.pca(
            PCA.tmp, scaling = 1, mar.percent = 0, 
            plot.sites  = FALSE, plot.spe  = FALSE, 
            label.sites = FALSE, label.spe = FALSE,
            xlim = c(-60,40), ylim = c(-60,40),
            cex = size#,
            #cex.lab = size
        )

        segments(
            seg.tmp$X_sca1_pair1, seg.tmp$Y_sca1_pair1,
            seg.tmp$X_sca1_pair2, seg.tmp$Y_sca1_pair2,
            col = "#00a0e9"
        )

        par(new = TRUE)
        cleanplot.pca(
            PCA.tmp, scaling = 1, mar.percent = 0, 
            plot.sites  = FALSE, label.spe = FALSE,
            plot.spe  = FALSE, 
            xlim = c(-60,40), ylim = c(-60,40),
            cex = size
        )

        mtext(
            paste("PC", 1, " (", round(summary(PCA.tmp)$cont$importance[2,1] * 100, digits = 1),"%)", sep = ""),
            side = 1, line = 2, 
            cex  = size * ifelse(saveFiles, 1.25, 1)
        )
        mtext(
            paste("PC", 2, " (", round(summary(PCA.tmp)$cont$importance[2,2] * 100, digits = 1),"%)", sep = ""),
            side = 2, line = 2,
            cex  = size * ifelse(saveFiles, 1.25, 1)
        )

        mtext(
            "b)", 
            side = 3, 
            line = ifelse(saveFiles, -1, -1.25),
            adj = 0.025,
            cex = size * ifelse(saveFiles, 1.25, 1)
        )
    }
}

if(saveFiles){
    dev.off()
}
