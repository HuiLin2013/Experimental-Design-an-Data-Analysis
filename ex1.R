#Create data.frame for nauseatable
nausea <- c(rep(0,times=100), rep(1,times=52),
            rep(0, times=32), rep(1, times=35),
            rep(0, times=48), rep(1, times=37))
medicin <- c(rep("Chlorpromazine", times=152),
             rep("Pentobarbital(100mg)", times=67),
             rep("Pentobarbital(150mg)", times=85))
nausea.frame <- data.frame(nausea,medicin)
nausea.frame

#The outcome of xtabs
xtabs(~nausea.frame$medicin+nausea.frame$naus)

#Permutation test
mystat <- function(x) sum(residuals(x)^2)
B=1000
tstar=numeric(B)
for (i in 1:B)
{
  treatstar = sample(factor(nausea.frame$medicin))
  tstar[i] = mystat(lm(nausea.frame$naus~nausea.frame$medicin))  
}
myt=mystat(lm(nausea.frame$naus~nausea.frame$medicin))
hist(tstar)
print(myt)
pl=sum(tstar<myt)/B
pr=sum(tstar>myt)/B
print(pl)

#chisq.test
chisq.test(xtabs(~nausea.frame$medicin+nausea.frame$naus))