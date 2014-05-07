search = read.table("search.txt",header=TRUE)
print(search)

search$skill=as.factor(search$skill)
search$interface=as.factor(search$interface)

inter_model = lm(time~interface+skill, data=search)
print(anova(inter_model))

print("data summary")
print(summary(inter_model))

# par(mfrow=c(1,2))
# qqnorm(residuals(inter_model),main="Residuals QQ-plot")
# qqnorm(rnorm(15), main="Normal sample QQ-plot")
par(mfrow=c(1,1))
plot(fitted(inter_model),residuals(inter_model))
