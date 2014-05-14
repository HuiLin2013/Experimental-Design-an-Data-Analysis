mut=seq(0.004,0.08,length=1000)

y = array( dim=c(1000,1) ) 
i=1
min=0.266919
for(j in mut)
{
  y[i]=0.60538 + -23.78772*j + 417.96154*(j^2)
  if(round(y[i],6) == min){
    print("optimal mutation: ");print(j)
  }
  i=i+1
  
}

plot(mut,y,xlim=c(0.004,0.08))
print(min(y))
abline(a=0,b=min(y))
