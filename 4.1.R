 to task 4.2
rundata = read.table(file='run.txt',header=TRUE)
lemotime = rundata[rundata[,'drink']=='lemo',1:2]
lemospeed = 60/lemotime
energytime = rundata[rundata[,'drink']=='energy',1:2]
energyspeed = 60/energytime

par(mfrow=c(2,3))

#boxplot for lemo before and after drink speed, histogram and Q-Q plot of differences
boxplot(lemospeed[,1],lemospeed[,2],main="Lemodrink speed data\n boxplot",names=c("no lemodrink","with lemodrink"))
hist(lemospeed[,1]-lemospeed[,2],main="Energy speed data\n difference histogram")
qqnorm(lemospeed[,1]-lemospeed[,2],main="Lemodrink speeds difference\n Q-Q plot")

boxplot(energyspeed[,1],energyspeed[,2],main="Energydrink speed\n data boxplot",names=c("no energydrink","with energydrink"))
hist(energyspeed[,1]-energyspeed[,2],main="Energy speed data\n difference histogram")
qqnorm(energyspeed[,1]-energyspeed[,2],main="Energydrink speed difference\n Q-Q plot")