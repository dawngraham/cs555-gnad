# CS555 Term Project - Dawn Graham

#+eval=FALSE

library(vtable)
library(pROC)

# Session > Set Working Directory > To Source File Location 
campaigns <- read.csv("data/campaigns_clean_subset.csv")

sumtable(campaigns, add.median=TRUE, digits=2)
sumtable(campaigns, add.median=TRUE, digits=2, out='return')

campaigns$pcs_local <- factor(campaigns$pcs_local)
campaigns$had_social_elites <- factor(campaigns$had_social_elites)
campaigns$had_campaigner_violence <- factor(campaigns$had_campaigner_violence)
campaigns$had_repressive_violence <- factor(campaigns$had_repressive_violence)
campaigns$success_above_avg <- factor(campaigns$success_above_avg)

attach(campaigns)

str(campaigns)

# success_goal
maxModel <- lm(success_goal ~ . - success_total - success_survival - success_growth - success_above_avg, data=campaigns)
m.success_goal <- step(maxModel, direction="backward")
summary(m.success_goal)
confint(m.success_goal)

plot(m.success_goal, which=2, main="Campaign Success: Goals")

# success_survival
maxModel <- lm(success_survival ~ . - success_goal - success_total - success_growth - success_above_avg, data=campaigns)
m.success_survival <- step(maxModel, direction="backward")
summary(m.success_survival)
confint(m.success_survival)

plot(m.success_survival, which=2, main="Campaign Success: Survival")

# success_growth
maxModel <- lm(success_growth ~ . - success_goal - success_survival - success_total - success_above_avg, data=campaigns)
m.success_growth <- step(maxModel, direction="backward")
summary(m.success_growth)
confint(m.success_growth)

plot(m.success_growth, which=2, main="Campaign Success: Growth")

# success_total
maxModel <- lm(success_total ~ . - success_goal - success_survival - success_growth - success_above_avg, data=campaigns)
m.success_total <- step(maxModel, direction="backward")
summary(m.success_total)
confint(m.success_total)

plot(m.success_total, which=2, main="Campaign Success: Total")

boxplot(success_total~pcs_local, main="Total Success Score by Local Goals", xlab="Local Goals and/or Opponents", ylab="Total Success Score", las=1)

# success_above_avg
maxModel <- glm(success_above_avg ~ . - success_goal - success_survival - success_total - success_growth, data=campaigns, family="binomial")
m.success_above_avg <- step(maxModel, direction="backward")
summary(m.success_above_avg)

exp(cbind(OR = coef(m.success_above_avg), confint.default(m.success_above_avg)))

par(pty="s")
probsuccess <- predict(m.success_above_avg, type=c("response"))
roc(success_above_avg ~ probsuccess, plot=TRUE, las=1, main="Above Average Success: ROC Curve (AUC=0.6562)")
