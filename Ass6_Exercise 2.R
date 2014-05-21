psi = read.table("psi.txt", header=TRUE)
psiaov = lm(passed~psi+gpa+psi*gpa, data = psi)
anova(psiaov)
qqnorm(residuals(psiaov), main="Normal Q-Q Plot of the residuals")
qqline(residuals(psiaov))
plot(fitted(psiaov), residuals(psiaov), xlab="Fitted", ylab="Residuals", main="Plot of fitted values versus residuals")

##Logistic regression
psiglm = glm(passed~psi+gpa, data = psi, family=binomial)
psiglm
summary(psiglm)
exp(coef(psiglm))
exp(confint.default(psiglm))
dp1 <- psi[psi$passed == 1, ]
dp2 <- dp1[dp1$psi == 1, ]
dp3 <- dp2[round(dp2$gpa, digits = 0) == 3, ]
dp3
psiglmppre = predict.glm(psiglm, dp3, type="response", se.fit=TRUE)
psiglmppre

df1 <- psi[psi$passed == 0, ]
df2 <- df1[df1$psi == 1, ]
df3 <- df2[round(df2$gpa, digits = 0) == 3, ]
df3
psiglmfpre = predict.glm(psiglm, df3, type="response", se.fit=TRUE)
psiglmfpre

##fisher.test
x=matrix(c(3, 15, 8, 6), 2, 2)
x
fisher.test(x)