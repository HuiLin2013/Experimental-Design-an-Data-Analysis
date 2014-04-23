###Exercise 4 Assignment 4
##boxplot of the three samples
dogs <- read.table("dogs.txt", header=TRUE)
print(dogs)
iso <- dogs[,1]
hal <- dogs[,2]
cyc <- dogs[,3]
boxplot(iso, hal, cyc, names=c("isofluorane","halothane","cyclopropane"),main="Boxplot of epinephrine concentrations in dogs")

##QQ-plots of the three samples
qqnorm(iso, main="QQ-plot of isofluorane concentration")
qqline(iso)
qqnorm(hal, main="QQ-plot of halothane concentration")
qqline(hal)
qqnorm(cyc, main="QQ-plot of cyclopropane concentration")
qqline(cyc)

##one-way analysis of variance
#create a data-frame for dogs under the 1-way ANOVA case
dogsframe <- data.frame(concentration=as.vector(as.matrix(dogs)), drug=factor(rep(1:3,each=10)))
dogsaov=lm(concentration~drug,data=dogsframe)
anova(dogsaov)
summary(dogsaov)
confint(dogsaov)

##Kruskal-Wallis test
#reuse data-frame for dogs created under the 1-way ANOVA case
attach(dogsframe)
kruskal.test(concentration, drug)

##Compare 1-way ANOVA and Kruskal-Wallis
qqnorm(dogsaov$residuals, main="QQ-plot of dogsaov$residuals")
qqline(dogsaov$residuals)
