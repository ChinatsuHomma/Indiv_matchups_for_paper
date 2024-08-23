##### Prepare for GLM #####
#### initial settings ####
rm(list=ls(all=TRUE)) # clear all objects
saveFiles <- TRUE     # Do you want save result file?
#saveFiles <- FALSE

#### Library ####
library("car")
library("multcomp")
library("emmeans")
library("easystats")
library("dplyr")
library("ggplot2")

#### Read data ####
DataForGLM <- read.csv("output/07GLM/00dataForGLM.csv")

#### initial object ####
LifeStage <- c(
    "Seedlings", "SmallSaplings", "LargeSaplings",
    "Poles", "Juveniles", "Subadults"
)
Sp <- c("PR", "FC", "AP", "AT", "UL", "CJ", "QC")

#### data formatting ####
# character
DataForGLM$TrialAndLifeStage <- as.character(DataForGLM$TrialAndLifeStage)
DataForGLM$Focal_ID          <- as.character(DataForGLM$Focal_ID)

# factor
DataForGLM$LifeStage      <- as.factor(DataForGLM$LifeStage)
DataForGLM$Focal_Sp       <- as.factor(DataForGLM$Focal_Sp)
DataForGLM$BinIsolation   <- as.factor(DataForGLM$BinIsolation)
DataForGLM$Location       <- as.factor(DataForGLM$Location) # not used

DataForGLM$LifeStage <- factor(
    DataForGLM$LifeStage,
    levels = LifeStage
)
DataForGLM$Focal_Sp <- factor(
    DataForGLM$Focal_Sp,
    levels = Sp
)

Con <- droplevels(DataForGLM[DataForGLM$BinIsolation == 0, ])

#### GLM about conspecific matchups ####
# Whether the proportion of conspecific matchups differed 
# between life stages and/or focal species
## Response variables: the number of conspecific and heterospecific matchups
## dependent variables: Life stages and Species of an individual
ModelConspecific <- glm(
    cbind(Conspecific, Heterospecific) ~ LifeStage * Focal_Sp,
    data   = Con,
    family = binomial
)
summary(ModelConspecific)
#report(ModelConspecific)

(AnovaConspecific <- Anova(ModelConspecific, test = "LR", type = 2))

# multiple comparison
EmConspecific <- emmeans(
    ModelConspecific,
    ~ LifeStage * Focal_Sp,
    type = "response"
)

EmConLS <- pairs(EmConspecific, simple = "LifeStage")
EmConSp <- pairs(EmConspecific, simple = "Focal_Sp")

#cld(EmConLS, alpha = 0.001, decreasing = FALSE, Letters = LETTERS)
#cld(EmConSp, alpha = 0.001, decreasing = FALSE, Letters = LETTERS)

pwpp(
    EmConspecific,
    sort = FALSE,
    by = "LifeStage",
    plim = c(0, 0.1)
)+ ggplot2::theme_bw() + ggplot2::theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

pwpp(
    EmConspecific,
    sort = FALSE,
    by = "Focal_Sp"
)+ ggplot2::theme_bw() + ggplot2::theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

sink("output/07GLM/EmConspecificLifeStage.txt")
EmConLS
sink()
sink("output/07GLM/EmConspecificFocalSp.txt")
EmConSp
sink()

if(saveFiles){
    write.csv(AnovaConspecific, "output/07GLM/01GLM_ConspecificMatchups.csv")
    report_table(ModelConspecific) %>% write.csv("output/07GLM/ModelConspecific.csv")
}

#### GLM about isolation ####
# Whether the proportion of isolation differed 
# between life stages and/or focal species
## Response variables: an individual was isolated (=1) or not (=0)
## dependent variables: Life stages and Species of an individual
ModelIsolation <- glm(
    BinIsolation ~ LifeStage * Focal_Sp,
    data   = DataForGLM,
    family = binomial
)
summary(ModelIsolation)

(AnovaIsolation <- Anova(ModelIsolation, test = "LR", type = 2))

# multiple comparison
EmIsolation <- emmeans(
    ModelIsolation,
    ~ LifeStage * Focal_Sp,
    type = "response"
)

EmIsoLS <- pairs(EmIsolation, simple = "LifeStage")
EmIsoSp <- pairs(EmIsolation, simple = "Focal_Sp")

#cld(EmIsoLS, alpha = 0.001, decreasing = FALSE, Letters = LETTERS)
#cld(EmIsoSp, alpha = 0.001, decreasing = FALSE, Letters = LETTERS)

pwpp(
    EmIsolation,
    sort = FALSE,
    by = "LifeStage",
    plim = c(0, 0.1)
)+ ggplot2::theme_bw() + ggplot2::theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

pwpp(
    EmIsolation,
    sort = FALSE,
    by = "Focal_Sp"
)+ ggplot2::theme_bw() + ggplot2::theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

sink("output/07GLM/EmIsolationLifeStage.txt")
EmIsoLS
sink()
sink("output/07GLM/EmIsolationFocalSp.txt")
EmIsoSp
sink()

if(saveFiles){
    write.csv(AnovaIsolation, "output/07GLM/01GLM_IsolationMatchups.csv")
    report_table(ModelIsolation) %>% write.csv("output/07GLM/ModelIsolation.csv")
}
