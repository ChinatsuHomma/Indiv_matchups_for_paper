##### Make Figure 2 #####
#### initial settings ####
rm(list=ls(all=TRUE)) # clear all objects
saveFiles <- TRUE     # Do you want save result file?
#saveFiles <- FALSE

#### Read data ####
NumMatchupSp <- read.csv("output/05forFigure/00DataForFig2_01MatchupsForEachIndiv_Sp.csv", row.names = 1)
NumMatchupIndiv <- read.csv("output/05forFigure/00DataForFig2_02MatchupsForEachIndiv_Indiv.csv", row.names = 1)

# Color palette
UC <- read.csv("data/universal_color.csv",row.names = 1)
UC[c(6,5), ] <- UC[c(5,6), ]


#### initial object ####
LifeStage <- c(
    "Seedlings", "Small-saplings", "Large-saplings",
    "Poles", "Juveniles", "Subadults"
)
Sp <- c(
    "Pterocarya", "Fagus", 
    "Acer", "Aesculus", "Ulmus", 
    "Cercidiphyllum", "Quercus"
)
SpN <- length(Sp)

# Objects for plot
mag  <- 1 # Magnification of figure
pch  <- c(0,1,2,3,5,6,7,8,10,14) # select markers
SpV  <- c(1:SpN)   # Species-specific vectors
SpV2 <- SpV[Sp != "Cercidiphyllum"] # Species-specific vectors (except Cercidiphyllum)


#### make fig ####
if(saveFiles){
  tiff(
    filename = "output/06figures/Figure_2.tiff", 
    height = 2160, width = 3600, units = "px", res = 600, compression = "lzw"
  )
} else {
  dev.new(width = 25, height = 15)
}

mat <- matrix(c(1,2), 1,2, byrow = TRUE)

# Figure about matchup *species*
max <- round(max(NumMatchupSp, na.rm = TRUE) + 0.5, 0) # Y-axis height

# graphic parameters
par(
    xpd = TRUE,
    plt = c(0.2, 0.925, 0.3, 0.925),
    fig = c(0, 1, 0, 1)
)

# layout
layout(mat)

matplot(
    t(NumMatchupSp),
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
axis(side = 2, at = seq(0, max, by = 0.5), labels = FALSE)
yN <- length(seq(0, max, by = 0.5))
i <- 1
for(i in 1:yN){
  text(
    0.45, seq(0, max+0.5, by = 0.5)[i], sprintf(seq(0, max, by = 0.5)[i], fmt = "%#.1f"), cex = 0.8*mag, adj = 1
  )
}
mtext(side = 2, "Number of neighboring species per individual", line = 2, cex = 0.8*mag)

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
text(1.1, max - max/30, "a)", cex = 1*mag)



# Figure about matchup *individuals*
max <- round(max(NumMatchupIndiv[SpV2, ], na.rm = TRUE) + 0.5, 0) # Y-axis height

matplot(
    t(NumMatchupIndiv[SpV2, ]),
    type = "o",
    lty  = 1,
    pch  = pch[-6],
    lwd  = 1,
    col  = rgb(UC[SpV2, 4:6]),
    cex  = 0.8 * mag,
    ylim = c(0, max),
    xlab = "",
    ylab = "",
    xaxt = "n",
    yaxt = "n"
)

# y-axis
axis(side = 2, at = seq(0, max, by = 1), labels = FALSE)
i <- 1
yN <- length(seq(0, max, by = 0.5))
for(i in 1:yN){
  text(
    0.45, seq(0, max, by = 1)[i], sprintf(seq(0, max, by = 1)[i], fmt = "%#.1f"), cex = 0.8*mag, adj = 1
  )
}
mtext(side = 2, "Number of neighbouring stems per individual", line = 2, cex = 0.8*mag)

# x-axis
axis(side = 1, at = 1:6, labels = FALSE)
i <- 1
for(i in 1:6){
  text(
    i+0.3, (par()$usr[3])-max/15, LifeStage[i], cex = 0.7*mag, srt = 90, pos = 2, offset = 0.7
  )
}

if(FALSE){
  par(font = 3) # Italic
  legend(
        "topleft",
        #par()$usr[2],
        #par()$usr[4],
        legend = s[SpV2],
        pch    = pch,
        bty    = "n",
        lty    = 1,
        ncol   = 2,
        cex    = 1*mag,
        pt.cex = 1*mag,
        col    = rgb(UC[SpV2, 4:6])
    )
}

par(font = 1) # Not Italic
text(1.1, max - max/30, "b)", cex = 1*mag)

if(saveFiles){
  dev.off()
}
