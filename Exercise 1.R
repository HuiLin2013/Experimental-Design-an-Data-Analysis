fruitfly = read.table("fruitflies.txt",header=TRUE)
#ex 1.1 code
fruitfly$loglongevity = log(fruitfly$longevity)

#ex 1.2 code
boxplot(fruitfly$loglongevity~fruitfly$activity,main="boxplot of loglongevity\n given activity level")

#ex1.3 code
fruitflylm = lm(loglongevity~activity,data=fruitfly)
print(anova(fruitflylm))
normsample=rnorm(75)
par(mfrow=c(1,2))
qqnorm(residuals(fruitflylm),main="Residuals QQ-plot")
qqnorm(normsample,main="Normal sample QQ-plot")

#ex1.4 code
print(summary(fruitflylm))

#ex1.5 code
attach(fruitfly)

contrasts(activity)=contr.sum
fruitflyaov=lm(loglongevity~thorax+activity)
print(anova(fruitflyaov))

#ex1.6 code
print(summary(fruitflyaov))

print(mean(thorax))
print(min(thorax))

#ex1.7 code
par(mfrow=c(1,1))
plot(loglongevity~thorax,pch=as.character(activity))
abline(lm(loglongevity~thorax, data=fruitfly[fruitfly$activity=="high",]))
abline(lm(loglongevity~thorax, data=fruitfly[fruitfly$activity=="low",]))
abline(lm(loglongevity~thorax, data=fruitfly[fruitfly$activity=="isolated",]))

#ex1.9 code
par(mfrow=c(1,2))
qqnorm(residuals(fruitflyaov))
plot(fitted(fruitflyaov),residuals(fruitflyaov))

#ex1.10 code
fruitflyraw=lm(longevity~thorax+activity)
print(anova(fruitflyraw))
qqnorm(residuals(fruitflyraw))
plot(fitted(fruitflyraw),residuals(fruitflyraw))
