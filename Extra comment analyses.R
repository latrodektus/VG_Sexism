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

#Mitch
library(MCMCglmm)
library(mvabund)


##Ensuring that certain columns are treated as categorical
GameOutcome <- factor(GameOutcome)
Treatment <- factor(Treatment)

# all models include max skill and Deaths, so get rid of NAs as they would be removed in the models
data = data[!is.na(data$MaxSkillLevel),]
data = data[!is.na(data$Deaths),]

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
modelAGGPos<-glm(Positive ~ GameOutcome + Treatment + Kills + Deaths + MaxSkillLevel + Treatment*Kills + Treatment*Deaths + Treatment*MaxSkillLevel, family = poisson, data=data)
anova(modelAGGPos, test="Chisq")

# fit nb glm
modelAGGPos.nb<-glm.nb(Positive ~ GameOutcome + Treatment + Kills + Deaths + MaxSkillLevel + Treatment*Kills + Treatment*Deaths + Treatment*MaxSkillLevel, data=data)
anova(modelAGGPos.nb)
summary(modelAGGPos.nb)

par(mfrow=c(1,2))
plot(modelAGGPos, which=1, main="Pos - Poisson")
plot(modelAGGPos.nb, which=1, main="Pos - neg bin")

# fit neg bin GLMs with different theta estimation
modelAGGPos.poismv = manyglm(Positive ~ GameOutcome + Treatment + Kills + Deaths + MaxSkillLevel + Treatment*Kills + Treatment*Deaths + Treatment*MaxSkillLevel, data=data, family="poisson")
anova(modelAGGPos.poismv, nBoot=1000)

modelAGGPos1.nbmv = manyglm(Positive ~ GameOutcome + Treatment + Kills + Deaths + MaxSkillLevel + Treatment*Kills + Treatment*Deaths + Treatment*MaxSkillLevel, data=data, theta.method="PHI")
anova(modelAGGPos.nbmv, nBoot=1000)

modelAGGPos2.nbmv = manyglm(Positive ~ GameOutcome + Treatment + Kills + Deaths + MaxSkillLevel + Treatment*Kills + Treatment*Deaths + Treatment*MaxSkillLevel, data=data, theta.method="ML")
anova(modelAGGPos.nbmv, nBoot=1000)

plot(modelAGGPos.poismv)
plot(modelAGGPos1.nbmv)
plot(modelAGGPos2.nbmv)

##Positive Comments
# Bayesian estimation (using MCMC)

# MCMCglmm fits an "over-dispersed" Poisson, which includes an 
# additional, per-observation random-effect.  
modelAGGPos.mcmc <- MCMCglmm(Positive ~ GameOutcome + Treatment + stdKills + stdDeaths + stdSkill + Treatment*stdKills + Treatment*stdDeaths + Treatment*stdSkill, 
                             family="poisson", data=data, nitt=100000, burnin=10000, thin=10)
summary(modelAGGPos.mcmc)
HPDinterval(modelAGGPos.mcmc$Sol, prob=0.9)
# plot the posteriors, with marginal densities
plot(modelAGGPos.mcmc)

##Negative Comments

##Removing the two outliers
modelAGGNeg<-glm(Negative ~ GameOutcome + Treatment + Kills + Deaths + MaxSkillLevel+ Treatment*Kills + Treatment*Deaths + Treatment*MaxSkillLevel, (family = poisson(link="log")), data=dataNoOutliers)
anova(modelAGGNeg, test="Chisq")

# Bayesian estimation - mcmc
modelAGGNeg.mcmc<-MCMCglmm(fixed=Negative ~ GameOutcome + Treatment + stdKills + stdDeaths + stdSkill+ Treatment*stdKills + Treatment*stdDeaths + Treatment*stdSkill, random=NULL,
                           family="poisson", data=dataNoOutliers, nitt=100000, burnin=10000, thin=10)
summary(modelAGGNeg.mcmc)
HPDinterval(modelAGGNeg.mcmc$Sol, prob=0.9)
# plot the posteriors, with marginal densities
plot(modelAGGNeg.mcmc)

