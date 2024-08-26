# Run All Processes
## Please set the working directory to the same level as the "code" and "data" folders

#### make folders ####
if(!dir.exists("data")){

    message(
        'The "data" folder does not exist.\n Check working directory or create "data" folder.\n Please create "data" folder at the same level as the "code" folders.\n All data from Zenodo (10.5281/zenodo.11198410) should be saved in the "data" folder.\n Please set the working directory to the same level as the "code" and "data" folders.\n'
    )

} else {

    message("#### Install packages if it needs ####\n")
    packages <- c(
        "vegan", "stringr", "car", "multcomp",
        "emmeans", "easystats", "dplyr", "ggplot2"
    )
    packages <- packages[!(packages %in% library()$results[ ,1])]
    if(length(packages) > 0){lapply(packages, install.packages)}

    message("#### Create folders ####\n")
    folder <- c(
        "output",
        "output/01subjects", "output/02matchupList", "output/03matchupTable",
        "output/04allMatchupTable", "output/05forFigure", "output/06figures",
        "output/06figures/PCAsummary","output/07GLM"
    )
    folderN <- length(folder)
    i <- 1
    for(i in 1:folderN){
        if(!dir.exists(folder[i])){
            dir.create(folder[i])
        }
    }

    #### Data preparation ####
    message("#### Data preparation ####\n")

    ##### seedlings #####
    message("for seedlings\n")
    source("code/01DataPrep_01Seedlings.R")

    ##### small-saplings #####
    message("for small saplings\n")
    source("code/01DataPrep_02SmallSaplings.R")

    ##### large saplings #####
    message("for large saplings\n")
    source("code/01DataPrep_03LargeSaplings.R")

    ##### poles #####
    message("for poles\n")
    source("code/01DataPrep_04Poles.R")

    ##### juveniles #####
    message("for juveniles\n")
    source("code/01DataPrep_05Juveniles.R")

    ##### subadults #####
    message("for subadults\n")
    source("code/01DataPrep_06Subadults.R")

    #### Table 2 ####
    message("Summarise in Table 2\n")
    # number of samples for each life stage
    source("code/02Table2_NumSamples.R")

    #### Table 3 ####
    # counting matchups
    message("#### Counting encounters ####\n")
    ## each life stage
    message("for seedlings\n")
    source("code/03Matchup_01Seedlings.R")
    
    message("for small saplings\n")
    source("code/03Matchup_02SmallSapling.R")

    message("for large saplings\n")
    source("code/03Matchup_03LargeSapling.R")

    message("for poles\n")
    source("code/03Matchup_04Poles.R")
    
    message("for juveniles\n")
    source("code/03Matchup_05Juveniles.R")
    
    message("for subadults\n")
    source("code/03Matchup_06Subadults.R")

    ## summarize
    message("Summarise in Table 3\n")
    source("code/04Table3_AllMatchupTable.R")

    message("Summarise in Appendix 3\n")
    source("code/04Appendix3_Rank.R")

    #### Make figures ####
    message("#### Drawing figures ####\n")
    ##### Organize data for make figures #####
    message("organizing data for Fig 2\n")
    source("code/05DataForFig2_MatchupsForEachIndiv.R")
    message("organizing data for Fig 4\n")
    source("code/05DataForFig4_HeteroMatchups.R")

    ##### Figure 2 #####
    message("Drawing Fig 2\n")
    source("code/06MakeFig_02Figure2.R")

    ##### Figure 3 #####
    message("Drawing Fig 3\n")
    source("code/06MakeFig_03Figure3.R")

    ##### Figure 4 #####
    message("Drawing Fig 4\n")
    source("code/06MakeFig_04Figure4.R")

    ##### Figure 5 #####
    message("Drawing Fig 5\n")
    source("code/06MakeFig_05Figure5.R")

    #### GLM ####
    message("#### GLM ####\n")
    message("preparation for GLM\n")
    source("code/07Analysis_00prepGLMv2.0.R")
    message("Building GLM\n")
    source("code/07Analysis_01GLMv2.0.R")
}
