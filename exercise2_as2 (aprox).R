#Load data sample from two prescribed text files that are stored under same folder
data79 <- scan("light1879.txt")
data82 <- scan("light1882.txt")

#Exercise 2.1
#Make histograms for data79 and data82
hist(data79, xlab="Speed of light (1879 experiments)", main="Histogram of 1879 experiments")
hist(data82, xlab="Speed of light (1882 experiments)", main="Histogram of 1882 experiments")

#Exercise 2.2 ~ Exercise 2.5
#Implement 95% CI(Confidence Interval) for subquestions 2-5 of Excercise 2
#Formula for 95% CI of population mean or median: m +- z(1-alpha/2)*sigma/sqrt(n)  
#m = value of mean or median    
#alpha = 1-0.95 (due to 95% CI)
#sigma = standard deviation of mean or median value   
#n = sample size

#Calcualte the mean for data79 and data82
mean79 <- mean(data79)
mean82 <- mean(data82)

#Calculate the median for data79 and data82
median79 <- median(data79)
median82 <- median(data82)

#Calculate the standard deviation for data79 and data82
sd79 <- sd(data79)
sd82 <- sd(data82)

#Value of sample size
n79=100
n82=23

#Calculate the value of z(1-alpha/2) by using the qnorm function
qnormValue <- qnorm(1-(1-0.95)/2)

#Calculate 95% CI for population mean of light1879
mean79_CILB <- mean79 - qnormValue*sd79/sqrt(n79)
print(mean79_CILB)
mean79_CIUB <- mean79 + qnormValue*sd79/sqrt(n79)
print(mean79_CIUB)

#Calculate 95% CI for population mean of light1882
mean82_CILB <- mean82 - qnormValue*sd82/sqrt(n82)
print(mean82_CILB)
mean82_CIUB <- mean82 + qnormValue*sd82/sqrt(n82)
print(mean82_CIUB)

#Calculate 95% CI for population median of light1879
median79_CILB <- median79 - qnormValue*sd79/sqrt(n79)
print(median79_CILB)
median79_CIUB <- median79 + qnormValue*sd79/sqrt(n79)
print(median79_CIUB)

#Calculate 95% CI for population median of light1882
median82_CILB <- median82 - qnormValue*sd82/sqrt(n82)
print(median82_CILB)
median82_CIUB <- median82 + qnormValue*sd82/sqrt(n82)
print(median82_CIUB)

