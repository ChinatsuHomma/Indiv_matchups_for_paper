##### Prepare for GLM #####
#### initial settings ####
rm(list=ls(all=TRUE)) # clear all objects
saveFiles <- TRUE     # Do you want save result file?
#saveFiles <- FALSE

#### Library ####
library("lme4")
library("car")

#### Read data ####
DataForGLM <- read.csv("output/07GLM/00dataForGLM.csv")

#### initial object ####
LifeStage <- c(
    "Seedlings", "SmallSaplings", "LargeSaplings",
    "Poles", "Juveniles", "Subadults"
)

#### data formatting ####
# character
DataForGLM$TrialAndLifeStage <- as.character(DataForGLM$TrialAndLifeStage)
DataForGLM$Focal_ID          <- as.character(DataForGLM$Focal_ID)

# factor
DataForGLM$LifeStage      <- as.factor(DataForGLM$LifeStage)
DataForGLM$Focal_Sp       <- as.factor(DataForGLM$Focal_Sp)
DataForGLM$BinConspecific <- as.factor(DataForGLM$BinConspecific)
DataForGLM$BinIsolation   <- as.factor(DataForGLM$BinIsolation)

DataForGLM$LifeStage <- factor(
    DataForGLM$LifeStage,
    levels = LifeStage
)

#### GLM about conspecific matchups ####
# Whether the proportion of conspecific matchups differed 
# between life stages and/or focal species
## Response variables: an individual experienced conspecific matchup (=1) or not (=0)
## dependent variables: Life stages and Species of an individual
ModelConspecific <- glm(
    BinConspecific ~ LifeStage + Focal_Sp,
    data   = DataForGLM,
    family = binomial
)
summary(ModelConspecific)

(AnovaConspecific <- Anova(ModelConspecific, type = 2, test.statistic = c("LR")))

if(saveFiles){
    write.csv(AnovaConspecific, "output/07GLM/01GLM_ConspecificMatchups.csv", row.names = FALSE)
}

#### GLM about isolation ####
# Whether the proportion of isolation differed 
# between life stages and/or focal species
## Response variables: an individual was isolated (=1) or not (=0)
## dependent variables: Life stages and Species of an individual
ModelIsolation <- glm(
    BinIsolation ~ LifeStage + Focal_Sp,
    data   = DataForGLM,
    family = binomial
)
summary(ModelIsolation)

(AnovaIsolation <- Anova(ModelIsolation, type = 2, test.statistic = c("LR")))

if(saveFiles){
    write.csv(AnovaIsolation, "output/07GLM/02GLM_Isolation.csv", row.names = FALSE)
}

