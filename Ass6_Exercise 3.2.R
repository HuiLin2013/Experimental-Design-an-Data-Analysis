africa = read.table("africa.txt",header=TRUE)

#ex 3.3
par(mfrow=c(1,1))
plot(africa)

africalm=glm(miltcoup~oligarchy+pollib+parties+pctvote+popn+size+numelec+numregim,family=poisson,data=africa)
print(summary(africalm))


par(mfrow=c(1,3))
plot(fitted(africalm),residuals(africalm))
plot(log(fitted(africalm)),residuals(africalm))
plot(fitted(africalm),residuals(africalm,type="response"))

#ex3.4
africalm=glm(miltcoup~oligarchy+pollib+parties+pctvote+popn+size+numregim,family=poisson,data=africa)
print(summary(africalm))

africalm=glm(miltcoup~oligarchy+pollib+parties+pctvote+popn+size,family=poisson,data=africa)
print(summary(africalm))

africalm=glm(miltcoup~oligarchy+pollib+parties+pctvote+popn,family=poisson,data=africa)
print(summary(africalm))

africalm=glm(miltcoup~oligarchy+pollib+parties+pctvote,family=poisson,data=africa)
print(summary(africalm))

africalm=glm(miltcoup~oligarchy+pollib+parties,family=poisson,data=africa)
print(summary(africalm))

#ex 3.5
plot(fitted(africalm),residuals(africalm))
plot(log(fitted(africalm)),residuals(africalm))
plot(fitted(africalm),residuals(africalm,type="response"))
