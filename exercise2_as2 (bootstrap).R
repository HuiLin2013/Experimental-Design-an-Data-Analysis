#Load data sample from two prescribed text files that are stored under the same folder
data79 <- scan("light1879.txt")
data82 <- scan("light1882.txt")

#Exercise 2.1
#Make histograms for data79 and data82
hist(data79, xlab="Speed of light (1879 experiments)", main="Histogram of 1879 experiments")
hist(data82, xlab="Speed of light (1882 experiments)", main="Histogram of 1882 experiments")

#Exercise 2.2 ~ Exercise 2.5
#Apply the bootstrap confidence interval at a 95% confidence level

#Calcualte the mean for data79 and data82
mean79 <- mean(data79)
mean82 <- mean(data82)

#Calculate the median for data79 and data82
median79 <- median(data79)
median82 <- median(data82)

#Function to return the value of Tstar_mean
Tstar_mean <- function(data, B){
  Tstar_mean=numeric(B)
  for(i in 1:B)
  {
    Xstar=sample(data, replace=TRUE)
    Tstar_mean[i]=mean(Xstar)
  }
  return(Tstar_mean)  
}

#Function to return the value of Tstar_median
Tstar_median <- function(data, B){
  Tstar_median=numeric(B)
  for(i in 1:B)
  {
    Xstar=sample(data, replace=TRUE)
    Tstar_median[i]=median(Xstar)
  }
  return(Tstar_median)  
}

#Calculate 95% CI (Bootstrap) for population mean of light1879
Tstar_mean79 <- Tstar_mean(data79, 1000)
mean79_CILB <- quantile(Tstar_mean79, 0.025)
print(mean79_CILB)
mean79_CIUB <- quantile(Tstar_mean79, 0.975)
print(mean79_CIUB)

#Calculate 95% CI (Bootstrap) for population mean of light1882
Tstar_mean82 <- Tstar_mean(data82, 1000)
mean82_CILB <- quantile(Tstar_mean82, 0.025)
print(mean82_CILB)
mean82_CIUB <- quantile(Tstar_mean82, 0.975)
print(mean82_CIUB)

#Calculate 95% CI (Bootstrap) for population median of light1879
Tstar_median79 <- Tstar_median(data79, 1000)
median79_CILB <- quantile(Tstar_median79, 0.025)
print(median79_CILB)
median79_CIUB <- quantile(Tstar_median79, 0.975)
print(median79_CIUB)

#Calculate 95% CI (Bootstrap) for population median of light1882
Tstar_median82 <- Tstar_median(data82, 1000)
median82_CILB <- quantile(Tstar_median82, 0.025)
print(median82_CILB)
median82_CIUB <- quantile(Tstar_median82, 0.975)
print(median82_CIUB)