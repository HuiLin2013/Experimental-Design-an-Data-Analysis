#get the data columns relevant for task 1
peruvians = read.table("peruvians.txt",header=TRUE)[,-c(5,6,7)]

#make scatter plot of attribute pairs using pairs plot
pairs(peruvians, pch=21,main="Pairs plots of peruvians", col="blue")
attach(peruvians)
#test migration for normality
qqnorm(migration, main="Q-Q plot migration")
#test correlation, replace age with the other parameters
print(cor.test(age,migration,method="spearman"))