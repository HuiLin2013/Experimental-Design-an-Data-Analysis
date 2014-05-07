bread <- read.table("bread.txt", header=TRUE)
print(bread)

#Ramdomization in R
I=3; J=2; N=18
rbind(rep(1:I, each=N*J), rep(1:J, N*I), sample(1:(N*I*J)))

#Boxplots of hoursversus the two actors
boxplot(bread$hours~bread$environment, xlab="environment", ylab="hours", main="Boxplot of hours versus environment")
boxplot(bread$hours~bread$humidity,  xlab="humidity", ylab="hours", main="Boxplot of hours versus humidity")

#Interaction plots between two factors
interaction.plot(bread$environment, bread$humidity, bread$hours)
interaction.plot(bread$humidity, bread$environment, bread$hours)

#Perform two-way ANOVA
breadaov=lm(hours~environment+humidity+environment*humidity, data=bread)
anova(breadaov)
summary(breadaov)
confint(breadaov)

#QQ plot of residuals and Plot of fitted values versus residuals
qqnorm(residuals(breadaov), main="Normal Q-Q Plot of the residuals")
qqline(residuals(breadaov))
plot(fitted(breadaov),residuals(breadaov), xlab="Fitted", ylab="Residuals", main="Plot of fitted values versus resuduals")