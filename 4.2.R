rundata = read.table(file='run.txt',header=TRUE)
#lemospeed = 60/rundata[rundata[,'drink']=='lemo',1:2]
energyspeed = 60/rundata[rundata[,'drink']=='energy',1:2]

#choose the mean as the test statistic for checking the difference
statfunc=function(x,y){mean(x-y)}

#t=statfunc(lemospeed[,1],lemospeed[,2])
t=statfunc(energyspeed[,1],energyspeed[,2])
B=2500
tstar=numeric(B)
for(i in 1:B)
{
  #lemospeedstar=t(apply(cbind(lemospeed[,1],lemospeed[,2]),1,sample))
  energyspeedstar=t(apply(cbind(energyspeed[,1],energyspeed[,2]),1,sample))
  #tstar[i]=statfunc(lemospeedstar[,1],lemospeedstar[,2])
  tstar[i]=statfunc(energyspeedstar[,1],energyspeedstar[,2])
}

pl=sum(tstar<t)/B
pr=sum(tstar>t)/B
p=2*min(pl,pr)
print("the p value:");print(p)
par(mfrow=c(1,1))

#hist(tstar,prob=T,ylim=c(0,2.5),main="histogram of tstar for lemodata speed\n and line through the data T value")
hist(tstar,prob=T,ylim=c(0,3.5),main="histogram of tstar for energydata speed\n and line through the data T value")
axis(1,t,expression(paste("T") ) )
abline(v=t,col='red',lwd=2)
