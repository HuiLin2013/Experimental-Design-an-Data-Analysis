search = read.table("search.txt",header=TRUE)
print(search[,1])
I=3; B=5; N=1
for (i in 1:B) print(sample(1:(N*I)))

attach(search)
par(mfrow=c(1,2))
boxplot(time~skill, xlab="skill lvl", ylab="time", main="Boxplot of time\n depending on skill lvl")
boxplot(time~interface, xlab="interface type", ylab="time", main="Boxplot of time\n depending on interface type")

interaction.plot(skill,interface,time)
interaction.plot(interface,skill,time)
