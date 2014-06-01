AIDS2002NYOri <- read.table("AIDS_modified_final2.txt", header=TRUE)
#print(AIDS2002NYOri)

AIDS2002NYOri$ageGroup=as.factor(AIDS2002NYOri$ageGroup)
AIDS2002NYOri$country=as.factor(AIDS2002NYOri$country)
AIDS2002NYOri$sex=as.factor(AIDS2002NYOri$sex)

attach(AIDS2002NYOri)
#par(mfrow=c(1,2))

# interaction.plot(country,ageGroup,cases)
# interaction.plot(ageGroup,country,cases)

# interaction.plot(country,sex,cases)
# interaction.plot(sex,country,cases)
#  
# interaction.plot(country,race,cases)
# interaction.plot(race,country,cases)
 
# interaction.plot(ageGroup,sex,cases)
# interaction.plot(sex,ageGroup,cases)
 
# interaction.plot(ageGroup,race,cases)
# interaction.plot(race,ageGroup,cases)
 
# interaction.plot(sex,race,cases)
# interaction.plot(race,sex,cases)

#make boxplots of the output for each grou

par(mfrow=c(2,2))
plot(cases~country+ageGroup+sex+race, data=AIDS2002NYOri)


 results = lm(cases~country + ageGroup + sex + race, data=AIDS2002NYOri) 
 print(anova(results)) 
# 
par(mfrow=c(1,2))
qqnorm(results$res, main="QQ plot - data residuals") 
qqnorm(rnorm(120),  main="QQ plot - normal sample") 
# 
# par(mfrow=c(1,1))
# plot(results$fitted,results$res,xlab="Fitted",ylab="Residuals")
# 
# # par(mfrow=c(1,2))
# plot(AIDS2002NYOri)


AIDS2002NYOri$rankcases=rank(AIDS2002NYOri$cases)
# print(AIDS2002NYOri)
#
#regular Multi-ANOVA
# results = lm(cases~country + ageGroup + sex + race, data=AIDS2002NYOri) 
#ranked data MANOVA
results = lm(rankcases~country + ageGroup + sex + race, data=AIDS2002NYOri) 
print(anova(results)) 
# 
# par(mfrow=c(1,2))
# qqnorm(results$res, main="QQ plot - data residuals") 
# qqnorm(rnorm(120),  main="QQ plot - normal sample") 

par(mfrow=c(1,1))
plot(results$fitted,results$res,xlab="Fitted",ylab="Residuals")