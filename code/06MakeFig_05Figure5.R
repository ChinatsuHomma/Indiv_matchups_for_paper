##### Make Figure 5 #####
#### initial settings ####
rm(list=ls(all=TRUE)) # clear all objects
saveFiles <- TRUE     # Do you want save result file?
#saveFiles <- FALSE

#### Read data ####
Table3 <- read.csv("output/04allMatchupTable/00Table3_AllMatchupTable.csv", row.names = 1)

# Color palette
UC <- read.csv("data/universal_color.csv",row.names = 1)
UC[c(6,5), ] <- UC[c(5,6), ]


#### initial object ####
LifeStage <- c(
    "Seedlings", "Small-saplings", "Large-saplings",
    "Poles", "Juveniles", "Subadults"
)
Sp <- c(
    "Acer spp.", "Pterocarya", "Fagus", 
    "A. pictum", "Aesculus", "Ulmus", 
    "Cercidiphyllum", "Quercus"
)

# Objects for plot
mag  <- 1 # Magnification of figure
pch  <- c(0,1,2,3,5,6,7,8,10,14) # select markers
SpV  <- c(1:8)   # Species-specific vectors


#### organize the data ####
Iso <- data.frame(tapply(Table3$PercentIso, list(Table3$FocalSp, Table3$LifeStage), sum))
Iso <- Iso[c(2,6,5,1,3,8,4,7), c(4,5,2,3,1,6)]


#### make fig ####
if(saveFiles){
    tiff(
        filename = "output/06figures/Figure_5.tiff", 
        height = 1800*4/3.5, width = 1800, units = "px", res = 600, compression = "lzw"
    )
} else {
    dev.new(width=3.5, height=4)
}

# graphic parameters
par(
    xpd = TRUE,
    plt = c(0.2, 0.925, 0.3, 0.925),
    fig = c(0, 1, 0, 1)
)

max <- 100

matplot(
    t(Iso),
    type = "o",
    lty  = 1,
    pch  = pch,
    lwd  = 1,
    col  = rgb(UC[SpV, 4:6]),
    cex  = 0.8 * mag,
    ylim = c(0, max),
    xlab = "",
    ylab = "",
    xaxt = "n",
    yaxt = "n"
)

# y-axis
axis(side = 2, at = seq(0, 100, by = 20), labels = FALSE)
yN <- length(seq(0, max, by = 0.5))
i <- 1
for(i in 1:yN){
    text(
        0.45, (i-1) * 20, seq(0, 100, by = 20)[i], cex = 0.9, adj = 1
    )
}
mtext(side = 2, "% isolation", line = 2, cex = 0.8*mag)

# x-axis
axis(side = 1, at = 1:6, labels = FALSE)
i <- 1
for(i in 1:6){
    text(
        i+0.15, (par()$usr[3])-max/15, LifeStage[i], cex = 0.7*mag, srt = 90, pos = 2, offset = 0.5
    )
}

par(font = 3) # Italic
legend(
    "topright",
    legend = Sp[SpV],
    pch    = pch,
    bty    = "n",
    lty    = 1,
    cex    = 0.55*mag,
    pt.cex = 1,
    ncol   = 2,
    col    = rgb(UC[SpV, 4:6])
)
par(font = 1) # Not Italic

if(saveFiles){
    dev.off()
}
