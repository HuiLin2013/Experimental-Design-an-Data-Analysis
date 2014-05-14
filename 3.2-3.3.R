genal2 = read.table("genal2.txt",header=TRUE)
genal2frame=data.frame()
init=0.005
k=1;



for (i in 1:length(genal2)) 
{
  for(j in 1:3)
  {
    genal2frame[k,1]=genal2[j,i]
    genal2frame[k,2]=init
    k=k+1
  }
  init=init+0.005
}

col_headings = c('y','mut')
names(genal2frame)=col_headings
print(genal2frame)

boxplot(genal2frame$y~genal2frame$mut)

genal2frame$mut2=genal2frame$mut^2
print(genal2frame[1:6,])
result = lm(y~mut+mut2,data=genal2frame)
print(summary(result))

par(mfrow=c(1,2))
qqnorm(residuals(result))
plot(fitted(result),residuals(result))
