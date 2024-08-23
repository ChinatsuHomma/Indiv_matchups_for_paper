# Run All Processes
## Please set the working directory to the same level as the "code" and "data" folders

#### make folders ####
if(!dir.exists("data")){

    message(
        'The "data" folder does not exist.\n Check working directory or create "data" folder.\n Please create "data" folder at the same level as the "code" folders.\n All data from Zenodo (10.5281/zenodo.11198410) should be saved in the "data" folder.\n Please set the working directory to the same level as the "code" and "data" folders.\n'
    )

} else {

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
    ##### seedlings #####
    source("code/01DataPrep_01Seedlings.R")

    ##### small-saplings #####
    source("code/01DataPrep_02SmallSaplings.R")

    ##### large saplings #####
    source("code/01DataPrep_03LargeSaplings.R")

    ##### poles #####
    source("code/01DataPrep_04Poles.R")

    ##### juveniles #####
    source("code/01DataPrep_05Juveniles.R")

    ##### subadults #####
    source("code/01DataPrep_06Subadults.R")

    #### Table 2 ####
    # number of samples for each life stage
    source("code/02Table2_NumSamples.R")

    #### Table 3 ####
    # counting matchups
    ## each life stage
    source("code/03Matchup_01Seedlings.R")
    source("code/03Matchup_02SmallSapling.R")
    source("code/03Matchup_03LargeSapling.R")
    source("code/03Matchup_04Poles.R")
    source("code/03Matchup_05Juveniles.R")
    source("code/03Matchup_06Subadults.R")

    ## summarize
    source("code/04Table3_AllMatchupTable.R")
    source("code/04Appendix3_Rank.R")

    #### Make figures ####
    ##### Organize data for make figures #####
    source("code/05DataForFig2_MatchupsForEachIndiv.R")
    source("code/05DataForFig4_HeteroMatchups.R")

    ##### Figure 2 #####
    source("code/06MakeFig_02Figure2.R")

    ##### Figure 3 #####
    source("code/06MakeFig_03Figure3.R")

    ##### Figure 4 #####
    source("code/06MakeFig_04Figure4.R")

    ##### Figure 5 #####
    source("code/06MakeFig_05Figure5.R")

    #### GLM ####
    source("code/07Analysis_00prepGLMv2.0.R")
    source("code/07Analysis_01GLMv2.0.R")
}