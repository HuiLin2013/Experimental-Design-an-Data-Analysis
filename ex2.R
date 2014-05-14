airpo = read.table("airpollution.txt", header=TRUE)

#Scatter plot of the candidate explanatory variables each other and against the response variable
pairs(airpo)

#Step-up method - Step 1
summary(lm(oxidant~day, data=airpo))
summary(lm(oxidant~wind, data=airpo))
summary(lm(oxidant~temperature, data=airpo))
summary(lm(oxidant~humidity, data=airpo))
summary(lm(oxidant~insolation, data=airpo))
#Step-up method - Step 2
summary(lm(oxidant~wind+day, data=airpo))
summary(lm(oxidant~wind+temperature, data=airpo))
summary(lm(oxidant~wind+humidity, data=airpo))
summary(lm(oxidant~wind+insolation, data=airpo))
#Step-up method - Step 3
summary(lm(oxidant~wind+temperature+day, data=airpo))
summary(lm(oxidant~wind+temperature+humidity, data=airpo))
summary(lm(oxidant~wind+temperature+insolation, data=airpo))
#Step-up method - Step 4
summary(lm(oxidant~wind+temperature+day+humidity, data=airpo))
summary(lm(oxidant~wind+temperature+day+insolation, data=airpo))
#Recall the chosen one during the Step 3 of Step-up method
summary(lm(oxidant~wind+temperature+humidity, data=airpo))

#Step-down method - Step 1
summary(lm(oxidant~day+wind+temperature+humidity+insolation, data=airpo))
#Step-down method - Step 2
summary(lm(oxidant~wind+temperature+humidity+insolation, data=airpo))
#Step-down method - Step 3
summary(lm(oxidant~wind+temperature+humidity, data=airpo))

#Investigate the normality of the residuals of the chosen model
model=lm(oxidant~wind+temperature+humidity, data=airpo)
qqnorm(residuals(model), main="Normal Q-Q Plot of the residuals")
qqline(residuals(model))
plot(fitted(model), residuals(model), xlab="Fitted", ylab="Residuals", main="Plot of fitted values versus residuals")
