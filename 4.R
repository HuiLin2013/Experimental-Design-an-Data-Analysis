datar = read.table("expensescrime.txt",header=TRUE)[,-c(1)]
#print(data)bad, crime, lawyers, employ, pop
plot(datar)

# print(summary(lm(expend~bad,data=datar)))
# print(summary(lm(expend~crime,data=datar)))
# print(summary(lm(expend~lawyers,data=datar)))
# print(summary(lm(expend~employ,data=datar)))
# print(summary(lm(expend~pop,data=datar)))


# print(summary(lm(expend~employ+bad,data=datar)))
# print(summary(lm(expend~employ+crime,data=datar)))
# print(summary(lm(expend~employ+lawyers,data=datar)))
# print(summary(lm(expend~employ+pop,data=datar)))

# print(summary(lm(expend~employ+lawyers+bad,data=datar)))
# print(summary(lm(expend~employ+lawyers+crime,data=datar)))
# print(summary(lm(expend~employ+lawyers+pop,data=datar)))

#print(summary(lm(expend~employ+lawyers+bad+pop+crime,data=datar)))

# print(summary(lm(expend~employ+lawyers+bad+pop,data=datar)))

# print(summary(lm(expend~employ+lawyers+bad,data=datar)))

# print(summary(lm(expend~employ+lawyers,data=datar)))

# resultm=lm(expend~employ+lawyers,data=datar)
# par(mfrow=c(1,2))
# qqnorm(residuals(resultm))
# 
# print(round(cor(datar[,2:6]),2))
# qqline(residuals(resultm))
# plot(fitted(resultm),residuals(resultm))

result=lm(expend~employ,data=data_red)
print(round(cooks.distance(result),2))

data_red = datar[-c(5),]
result=lm(expend~employ,data=data_red)
print(summary(result))
par(mfrow=c(1,2))

qqnorm(residuals(result))
plot(fitted(result),residuals(result))
