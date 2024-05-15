##### Make Figure 5 #####
#### initial settings ####
rm(list=ls(all=TRUE)) # clear all objects
saveFiles <- TRUE     # Do you want save result file?
#saveFiles <- FALSE

#### Read data ####
HeteroMatchup <- read.csv("output/05forFigure/00DataForFig4_01HeteroMatchups.csv")
rownames(HeteroMatchup) <- HeteroMatchup$NumSet
HeteroMatchup <- as.matrix(HeteroMatchup[ , c(6:11)]) # for PCA


#### initial object ####
LifeStage <- c(
    "Seedlings", "Small-saplings", "Large-saplings",
    "Poles", "Juveniles", "Subadults"
)
Sp <- c(
    "Acer spp.", "Pterocarya", "Fagus", 
    "A. pictum", "Aesculus", "Ulmus", 
    "Cercidiphyllum ", "Quercus"
)


#### principal component analysis ####
Result_PCA <- prcomp(HeteroMatchup, scale = FALSE) # as a variance-covariance matrix

summary(Result_PCA)
Result_PCA$rotation
Result_PCA$x

if(saveFiles){
    write.csv(summary(Result_PCA)[6], "output/06figures/PCAsummary/01PCAsummary.csv")
    write.csv(Result_PCA$rotation, "output/06figures/PCAsummary/02PCArotation.csv")
    write.csv(Result_PCA$x, "output/06figures/PCAsummary/03PCAdetails.csv")
}


# Objects for plot
EachPair <- data.frame(Result_PCA$x[ ,c(1,2)])
EachPair$pair1 <- rownames(EachPair)
EachPair$pair2 <- rownames(EachPair)

Segment <- as.data.frame.matrix(t(combn(c(1:8), 2)))
Segment$pair1 <- paste(Segment$V1,  Segment$V2, sep = "-")
Segment$pair2 <- paste(Segment$V2,  Segment$V1, sep = "-")
Segment$pair  <- paste(Segment$pair1, Segment$pair2, sep = "_")

Segment <- merge(
    Segment, EachPair[ ,c(1,2,3)],
    all.x = TRUE, all.y = FALSE,
    by = "pair1"
)
colnames(Segment)[colnames(Segment) == "PC1"] <- "x1"
colnames(Segment)[colnames(Segment) == "PC2"] <- "y1"

Segment <- merge(
    Segment, EachPair[ ,c(1,2,4)],
    all.x = TRUE, all.y = FALSE,
    by = "pair2"
)
colnames(Segment)[colnames(Segment) == "PC1"] <- "x2"
colnames(Segment)[colnames(Segment) == "PC2"] <- "y2"

# choose pair to draw
choose <- data.frame(
    "lty"   = rep(1),
    "pair1" = c("1-3", "1-4", "2-5", "2-6", "2-7", "2-8", "3-6", "7-8")
)
Segment <- merge(
    Segment, choose,
    all.x = TRUE, all.y = FALSE,
    by = "pair1"
)
Segment$lty[is.na(Segment$lty)] <- 0


#### Plot ####
dev.off()

if(saveFiles == 1){
    tiff(
        filename = "output/06figures/Figure_4.tiff", 
        height = 3600, width = 3600, units = "px", res = 600, compression = "lzw"
    )
} else {
    dev.new(width=25, height=25)
}

par(mai = c(0.8,0.8,0.8,0.8))
par(
    plt = c(0.15,0.95,0.15,0.95),
    fig = c(0,1,0,1),
    xpd = TRUE
)

plot(
    EachPair$PC1, EachPair$PC2,
    pch  = NA,
    xlim = c(-50,50),
    ylim = c(-40,40),
    xlab = paste("PC1 (", sprintf(summary(Result_PCA)[[6]][2,1]*100, fmt = "%#.1f"), " %)", sep = ""),
    ylab = paste("PC2 (", sprintf(summary(Result_PCA)[[6]][2,2]*100, fmt = "%#.1f"), " %)", sep = ""),
    asp = 1
)

segments(
    Segment$x1, Segment$y1,
    Segment$x2, Segment$y2,
    lty = Segment$lty,
    lwd = 1,
    col = "#20B2AA"
)

scale <- 30
i = 1
for(i in 1:6){
    arrows(
        0, 0, 
        Result_PCA$rotation[i,1] * scale,
        Result_PCA$rotation[i,2] * scale, 
        length = 0.1, 
        col = "red"
    )
}

text(
    EachPair$PC1, EachPair$PC2,
    rownames(EachPair),
    cex = 0.8
)

text(
    Result_PCA$rotation[ ,1] * scale + Result_PCA$rotation[ ,1] * scale / 5,
    Result_PCA$rotation[ ,2] * scale + Result_PCA$rotation[ ,2] * scale / 5, 
    LifeStage,
    cex = 0.8,
    col = "red"
)

par(font = 3) # Italic
legend(
    "topright",
    legend = Sp,
    pch = c("1", "2", "3", "4", "5", "6", "7", "8"),
    cex = 1
)
par(font = 1) # Normal font

if(saveFiles == 1){
  dev.off()
}
