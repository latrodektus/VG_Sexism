#####Import data and Check
##Read csv data table (File = "Data")
data = read.table(file.choose(),  na.strings =".", header=T, sep=",")

##Attach and check the headers of each column
attach(data)
names(data)

############################
####General housekeeping####


##Load the necessary libraries
library(plyr)
library(nlme)
library(MASS)
library(lattice)
library(ggplot2)
library(lme4)
library(effects)
library(QuantPsyc)

##Ensuring that certain columns are treated as categorical
GameOutcome <- factor(GameOutcome)
Treatment <- factor(Treatment)

##Add new columns
##Add Difference in Max skill
data$SkillDiff <- data$MaxSkillLevel - data$TreatmentMaxSkill

##Add Difference in Kills 
data$KillDiff <- data$Kills-data$TreatmentKills

##Add Difference in Deaths
data$DeathDiff <- data$Deaths-data$TreatmentDeaths

##Subset data by sex
fem = subset(data,Treatment=="Female")
mal = subset(data,Treatment=="Male")

##Subset data from 'data' that has less than 20 negative comments to test for significance without outliers.
dataNoOutliers = subset(data, data$Negative<20)

##Standardize positive statements (data) and negative statments (dataNoOutliers)
data$stdPos = scale(data$Positive, center = TRUE, scale = TRUE)
data$stdKills = scale(data$Kills, center = TRUE, scale = TRUE)
data$stdDeaths = scale(data$Deaths, center = TRUE, scale = TRUE)
data$stdSkill = scale(data$MaxSkillLevel, center = TRUE, scale = TRUE)

data$stdSkillDiff = scale(data$SkillDiff, center = TRUE, scale = TRUE)
data$stdKillDiff = scale(data$KillDiff, center = TRUE, scale = TRUE)
data$stdDeathDiff = scale(data$DeathDiff, center = TRUE, scale = TRUE)


dataNoOutliers$stdNeg = scale(dataNoOutliers$Negative, center = TRUE, scale = TRUE)
dataNoOutliers$stdKills = scale(dataNoOutliers$Kills, center = TRUE, scale = TRUE)
dataNoOutliers$stdDeaths = scale(dataNoOutliers$Deaths, center = TRUE, scale = TRUE)
dataNoOutliers$stdSkill = scale(dataNoOutliers$MaxSkillLevel, center = TRUE, scale = TRUE)

dataNoOutliers$stdSkillDiff = scale(dataNoOutliers$SkillDiff, center = TRUE, scale = TRUE)
dataNoOutliers$stdKillDiff = scale(dataNoOutliers$KillDiff, center = TRUE, scale = TRUE)
dataNoOutliers$stdDeathDiff = scale(dataNoOutliers$DeathDiff, center = TRUE, scale = TRUE)


#######################
####Checking counts####

##Checking counts of any variable (in this example, the number of male and female trials)
table(data$Treatment)
table(data$Game)
table(data$Sexism)


####################
####ModelTesting####

#########################
####Positive Comments####

##Test full model to examine the correlation between the number of Positive Comments and game outcome, treatment (condition), Kills, Skill level, and interactions of condition with Kills and Skill Level.
modelAGGPos<-glm(Positive ~ GameOutcome + Treatment + Kills + Deaths + MaxSkillLevel + Treatment*Kills + Treatment*Deaths + Treatment*MaxSkillLevel, (family = poisson(link="log")), data=data)
anova(modelAGGPos, test="Chisq")

##Use standarrdized model to get stadardized Betas
modelAGGPos.std<-glm(Positive ~ GameOutcome + Treatment + stdKills + stdDeaths + stdSkill+ Treatment*stdKills + Treatment*stdDeaths + Treatment*stdSkill, (family = poisson(link="log")), data=data)
summary(modelAGGPos.std)

#####
##Test full model to examine how relative performance to The Player affected positive comments with treatment as an interaction
modelAGGPosPerf<-glm(Positive ~ Treatment + SkillDiff + KillDiff + DeathDiff + Treatment*SkillDiff + Treatment*KillDiff + Treatment*DeathDiff, (family = poisson(link="log")), data=data)
anova(modelAGGPosPerf, test="Chisq")

##Use standarrdized model to get stadardized Betas
modelAGGPosPerf.std<-glm(Positive ~ Treatment + stdSkillDiff + stdKillDiff + stdDeathDiff + Treatment*stdSkillDiff + Treatment*stdKillDiff + Treatment*stdDeathDiff, (family = poisson(link="log")), data=data)
summary(modelAGGPosPerf.std)


##Plot the predicted lines for the model 
##Figure 1
modelAGGPos.eff <-effect("Treatment*MaxSkillLevel",modelAGGPos, se=TRUE)
plot(modelAGGPos.eff, rescale.axis=F, rug=FALSE, ylab="Number of Positive Comments",
       xlab="Maximum Skill Level Acheived", multiline= T)

##Figure 2
modelAGGPosPerf.eff <-effect("Treatment*SkillDiff",modelAGGPosPerf, se=TRUE)
plot(modelAGGPosPerf.eff, rescale.axis=F, rug=FALSE, ylab="Number of Positive Comments",
     xlab="Difference in Maximum Skill Level Acheived", multiline= T)


#########################
####Negative comments####


##Test full model to examine the correlation between the number of Negative Comments and game outcome, Treatment (i.e. condition), Kills, Skill level, and interactions of condition with Kills and Skill Level.
##Removing the two outliers
modelAGGNeg<-glm(Negative ~ GameOutcome + Treatment + Kills + Deaths + MaxSkillLevel+ Treatment*Kills + Treatment*Deaths + Treatment*MaxSkillLevel, (family = poisson(link="log")), data=dataNoOutliers)
anova(modelAGGNeg, test="Chisq")

##Use standarrdized model to get stadardized Betas
modelAGGNeg.std<-glm(Negative ~ GameOutcome + Treatment + stdKills + stdDeaths + stdSkill+ Treatment*stdKills + Treatment*stdDeaths + Treatment*stdSkill, (family = poisson(link="log")), data=dataNoOutliers)
summary(modelAGGNeg.std)


##Get the mean number of negative comments in the different treatments
ddply(dataNoOutliers, c("Treatment"), summarise,
      N    = length(Negative),
      mean = mean(Negative),
      sd   = sd(Negative),
      se   = sd / sqrt(N)
)

##Figure 3
modelAGGNeg.effDeath <-effect("Treatment*Deaths",modelAGGNeg, se=TRUE)
plot(modelAGGNeg.effDeath, rescale.axis=F, rug=FALSE, ylab="Number of Negative Comments",
     xlab="Maximum Skill Level Achieved", multiline= T)

modelAGGNeg.effKills <-effect("Treatment:Kills",modelAGGNeg, se=TRUE)
plot(modelAGGNeg.effKills, rescale.axis=F, rug=FALSE, ylab="Number of Negative Comments",
     xlab="Maximum Skill Level Achieved", multiline= T)


#####
##Test full model to examine how relative performance to The Player affected negative comments with treatment as an interaction
modelAGGNegPerf<-glm(Negative ~ Treatment + SkillDiff + KillDiff + DeathDiff + Treatment*SkillDiff + Treatment*KillDiff + Treatment*DeathDiff, (family = poisson(link="log")), data=dataNoOutliers)
anova(modelAGGNegPerf, test="Chisq")

##Use standarrdized model to get stadardized Betas
modelAGGNegPerf.std<-glm(Negative ~ Treatment + stdSkillDiff + stdKillDiff + stdDeathDiff + Treatment*stdSkillDiff + Treatment*stdKillDiff + Treatment*stdDeathDiff, (family = poisson(link="log")), data=dataNoOutliers)
summary(modelAGGNegPerf.std)


########################
####Neutral comments####

##Test full model to examine the correlation between the number of Neutral Comments and game outcome, Treatment (i.e. condition), Kills, Skill level, and interactions of condition with Kills and Skill Level.
##Removing the two outliers
modelAGGNeutral<-glm(Neutral ~ GameOutcome + Treatment + Kills + Deaths + MaxSkillLevel+ Treatment*Kills + Treatment*Deaths + Treatment*MaxSkillLevel, (family = poisson(link="log")), data=data)
anova(modelAGGNeutral, test="Chisq")

##Use standarrdized model to get stadardized Betas
modelAGGNeutral.std<-glm(Neutral ~ GameOutcome + Treatment + stdKills + stdDeaths + stdSkill+ Treatment*stdKills + Treatment*stdDeaths + Treatment*stdSkill, (family = poisson(link="log")), data=data)
summary(modelAGGNeutral.std)

#####
##Test full model to examine how relative performance to The Player affected negative comments with treatment as an interaction
modelAGGNeutralPerf<-glm(Neutral ~ Treatment + SkillDiff + KillDiff + DeathDiff + Treatment*SkillDiff + Treatment*KillDiff + Treatment*DeathDiff, (family = poisson(link="log")), data=data)
anova(modelAGGNeutralPerf, test="Chisq")

##Use standarrdized model to get stadardized Betas
modelAGGNeutralPerf.std<-glm(Neutral ~ Treatment + stdSkillDiff + stdKillDiff + stdDeathDiff + Treatment*stdSkillDiff + Treatment*stdKillDiff + Treatment*stdDeathDiff, (family = poisson(link="log")), data=data)
summary(modelAGGNeutralPerf.std)

#####################################################################
####Testing for sexist responses -- Although only 11 sexist cases####

table(fem$Sexism)

Sexism=subset(data, Treatment=="Female")
modelSexism<-glm(Sexism ~ SkillDiff + KillDiff + DeathDiff, (family = binomial(link="logit")), data=Sexism)
anova(modelSexism, test="Chisq")

