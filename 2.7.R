search = read.table("search.txt",header=TRUE)[,-c(2)]
print(search)
search$interface = as.factor(search$interface)

inter_model = lm(time~interface, data=search)
print(anova(inter_model))

par(mfrow=c(1,2))
qqnorm(residuals(inter_model),main="Residuals QQ-plot")
qqnorm(rnorm(15), main="Normal sample QQ-plot")
