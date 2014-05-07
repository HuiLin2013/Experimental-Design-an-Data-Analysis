cow <- read.table("cow.txt", header=TRUE)
print(cow)

#Perform a fixed effects analysis
cow$id = factor(cow$id)
cow$per = factor (cow$per)
cow$order = factor (cow$order)
cowlm = lm(milk~treatment+per+id, data = cow)
summary(cowlm)
anova(cowlm)

#Perform a mixed effects analysis
library(lme4)
cowlmer = lmer(milk~treatment+order+per+(1|id), data = cow, REML=FALSE)
summary(cowlmer)
#To retrieve p-value by refitting the model
cowlmer1 = lmer(milk~order+per+(1|id), data = cow, REML=FALSE)
anova(cowlmer1, cowlmer)

#Study the commands
attach(cow)
t.test(milk[treatment=="A"], milk[treatment=="B"], paired=TRUE)


