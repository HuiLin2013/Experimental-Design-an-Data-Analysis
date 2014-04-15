data = scan(file='dataexp.txt')
#t-statistic from data
t=mean(data) - median(data)

#t-statistic from surrogate data sampled from the standard exponential distribution
B=2000
tstar=numeric(B)
n=length(data)

for(i in 1:B)
{
  xstar=rexp(n,1)
  tstar[i]=mean(xstar)-median(xstar)
}

#calculate p
pl=sum(tstar<t)/B
pr=sum(tstar>t)/B
p=2*min(pl,pr)
print("the p value")
print(p)
print(t)

par(mfrow=c(1,2))

#plot the data to see  how the distribution looks like
hist(data,prob=T,ylim=c(0,0.7))
x=seq(0,max(data),length=1000)
lines(x,dexp(x),type="l",col="blue",lwd=2)

#plot the tstar distribution and the actual t value as a line in the graph
hist(tstar,prob=T,ylim=c(0,2.8),main="histogram of tstar and line through\n the data T value")
axis(1,t,expression(paste("T") ) )
abline(v=t,col='red',lwd=2)

