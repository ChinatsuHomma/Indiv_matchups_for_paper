##### Make Figure 3 #####
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
alphabet <- c(
  "a)",  "b)",  "c)",  "d)",
  "e)",  "f)",  "g)",  "h)"
)

# Objects for plot
SpV <- c(1:8)   # Species-specific vectors
SpN <- 8


#### organize the data ####
Con <- data.frame(tapply(Table3$PercentCon, list(Table3$FocalSp, Table3$LifeStage), sum))
Con <- Con[c(2,6,5,1,3,8,4,7), c(4,5,2,3,1,6)]

#### make fig ####
dev.off()

if(saveFiles){
    tiff(
        filename = "output/06figures/Figure_3.tiff", 
        height = 2160, width = 3600, units = "px", res = 600, compression = "lzw"
    )
} else {
    dev.new(width=10, height=6)
}

max <- 100

mat_tmp <- c(9,1,1,2,2,3,3,4,4,9,1,1,2,2,3,3,4,4,9,5,5,6,6,7,7,8,8,9,5,5,6,6,7,7,8,8,9,9,9,9,9,9,9,9,9)
mat <- matrix(mat_tmp, 5, 9, byrow = TRUE)

# graphics parameters
par(
    xpd = TRUE,
    fig = c(0, 1, 0, 1)
)

# layout
layout(mat)

# plot by each species
sp      <- 1
plots   <- SpN + 1
dilute  <- 0.95 # value about color
hatched <- TRUE

for(sp in 1:plots){
    if(sp != plots){
        # x label
        xlabels <- switch(
            sp,
            FALSE, FALSE, FALSE, FALSE,
            LifeStage, LifeStage, LifeStage, LifeStage
        )
        # y label
        ylabels <- switch(
            sp,
            seq(0, 100, by = 20), FALSE, FALSE, FALSE,
            seq(0, 100, by = 20), FALSE, FALSE, FALSE
        )
        # margin
        plttmp <- switch(
            sp,
            c(0.1, 0.9, 0.1, 0.8), c(0.1, 0.9, 0.1, 0.8), c(0.1, 0.9, 0.1, 0.8), c(0.1, 0.9, 0.1, 0.8),
            c(0.1, 0.9, 0.2, 0.9), c(0.1, 0.9, 0.2, 0.9), c(0.1, 0.9, 0.2, 0.9), c(0.1, 0.9, 0.2, 0.9)
        )

        # Colors
        col1   <- UC[sp, 4:6]
        col2   <- col1
        col2$r <- ifelse(1 - (1 - col1[1]) * (1 - dilute) > 1.0, 1.0, 1 - (1 - col1[1]) * (1 - dilute))
        col2$g <- ifelse(1 - (1 - col1[2]) * (1 - dilute) > 1.0, 1.0, 1 - (1 - col1[2]) * (1 - dilute))
        col2$b <- ifelse(1 - (1 - col1[3]) * (1 - dilute) > 1.0, 1.0, 1 - (1 - col1[3]) * (1 - dilute))

        # adjustment
        par(plt = plttmp)

        # plot
        barplot(
            t(
                data.frame(
                    "cons" = c(t(Con[sp, ])),
                    "hete" = c(t(100 - Con[sp, ]))
                )
            ),
            las  = 1,
            yaxt = "n",
            col  = c(rgb(col1), rgb(col2))
        )

        # plot with diagonal lines
        if(hatched){
            par(new = TRUE)
            barplot(
                t(
                    data.frame(
                        "cons" = c(t(Con[sp, ])),
                        "hete" = c(t(100 - Con[sp, ]))
                    )
                ),
                density = c(100, 20),
                angle   = c(0  , 25),
                las     = 1,
                yaxt    = "n",
                col     = c(rgb(col1), rgb(col1))
            )
        }

        # x-axis
        axis(
            side   = 1,
            at     = seq(0.7, 6.7, by = 1.2),
            las    = 2,
            labels = xlabels
        )

        # y-axis
        axis(
            side   = 2,
            at     = seq(0, 100, by = 20),
            las    = 1,
            labels = ylabels
        )

        # panel title
        text(
            -0.3, 110,
            alphabet[sp],
            adj = 0
        )

        # species name
        par(font = 3) # Italic
        text(
            0.6, 110,
            Sp[sp],
            adj = 0
        )
        par(font = 1) # normal font

    } else {
        # y label
        # adjustment
        par(plt = c(0.1, 0.9, 0.1, 0.9))

        # plot nothing to write y label only
        plot(
            1, 1,
            type = "n",
            xaxt="n",yaxt="n",
            xlab=" ",ylab=" ",
            bty ="n",
            asp = 1
        )

        # y label
        mtext(
            side = 2,
            "% conspecific matchups",
            line = 2
        )
    }
}

if(saveFiles){
    dev.off()
}
