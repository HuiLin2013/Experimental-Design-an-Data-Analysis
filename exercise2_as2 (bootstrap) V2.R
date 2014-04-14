#Load data sample from two prescribed text files that are stored under the same folder
data79 <- scan("light1879.txt")
data82 <- scan("light1882.txt")

#Exercise 2.1
#Make histograms and Q-Q Plot for data79 and data82
hist(data79, xlab="Speed of light", main="Histogram of light1879")
hist(data82, xlab="Speed of light", main="Histogram of light1882")
qqnorm(data79, main="Normal Q-Q Plot of light1879")
qqline(data79)
qqnorm(data82, main="Normal Q-Q Plot of light1882")
qqline(data82)

#Exercise 2.2 ~ Exercise 2.5
#Apply the bootstrap confidence interval at a 95% confidence level

#Calcualte the mean for data79 and data82
mean79 <- mean(data79)
mean82 <- mean(data82)

#Calculate the median for data79 and data82
median79 <- median(data79)
median82 <- median(data82)

#Function to return the value of Tstar
Tstar <- function(data, type){
  B=1000
  Tstar=numeric(B)
  if(type == "mean")
  {
    for(i in 1:B)
    {
      Xstar=sample(data, replace=TRUE)
      Tstar[i]=mean(Xstar)
    }
  }
  else if(type == "median")
  {
    for(i in 1:B)
    {
      Xstar=sample(data, replace=TRUE)
      Tstar[i]=median(Xstar)
    }
  }
  return(Tstar)  
}

#Calculate 95% CI (Bootstrap) for population mean of light1879
Tstar_mean79 <- Tstar(data79, "mean")
Tstar25_mean79 <- quantile(Tstar_mean79, 0.025)
Tstar975_mean79 <- quantile(Tstar_mean79, 0.975)
CI_mean79 <- c(2*mean79-Tstar975_mean79, 2*mean79-Tstar25_mean79)
print(CI_mean79)

#Calculate 95% CI (Bootstrap) for population mean of light1882
Tstar_mean82 <- Tstar(data82, "mean")
Tstar25_mean82 <- quantile(Tstar_mean82, 0.025)
Tstar975_mean82 <- quantile(Tstar_mean82, 0.975)
CI_mean82 <- c(2*mean82-Tstar975_mean82, 2*mean82-Tstar25_mean82)
print(CI_mean82)

#Calculate 95% CI (Bootstrap) for population median of light1879
Tstar_median79 <- Tstar(data79, "median")
Tstar25_median79 <- quantile(Tstar_median79, 0.025)
Tstar975_median79 <- quantile(Tstar_median79, 0.975)
CI_median79 <- c(2*median79-Tstar975_median79, 2*median79-Tstar25_median79)
print(CI_median79)

#Calculate 95% CI (Bootstrap) for population median of light1882
Tstar_median82 <- Tstar(data82, "median")
Tstar25_median82 <- quantile(Tstar_median82, 0.025)
Tstar975_median82 <- quantile(Tstar_median82, 0.975)
CI_median82 <- c(2*median82-Tstar975_median82, 2*median82-Tstar25_median82)
print(CI_median82)