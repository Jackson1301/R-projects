#1
attdata <- read.csv("C:/Users/jacks/RData/attend.csv",fileEncoding="UTF-8-BOM",header=FALSE,na.strings = ".")
colnames(attdata) <- c("attend","termGPA", "priGPA", "ACT", "final", "atndrte", "hwrte", "frosh", "soph", "skipped", "stndfnl")
att_lm1 <- lm(final ~ atndrte + hwrte + priGPA + frosh + I(priGPA^2) + ACT, data=attdata)
summary(att_lm1)

#2
tp<-coefficients(att_lm1)["priGPA"]/(-2*coefficients(att_lm1)["I(priGPA^2)"])
tp

pctgttp  <- ifelse(tp > 0, sum(attdata$priGPA>tp)/length(attdata$priGPA), "Negative Turning Point")
pctgttp

samps <- att_lm1$coefficients[4] + 2 * att_lm1$coefficients[6] * mean(attdata$priGPA)
samps

#3
library(car)
ftest_1 <- linearHypothesis(att_lm1,c("atndrte=0","frosh=0"))
ftest_1

#4
library(MASS)
bc <- boxcox(att_lm1,lambda = seq(-3,3, 0.05))

lllam0 <- bc$y[which(bc$x==0)] # This prints the log-likelihood value for lambda = 0 (ln(y) model)
lllam0

lllam1 <- bc$y[which(bc$x==1)] # This prints the log-likelihood value for lambda = 1 (y, level(y) model)
lllam1

#5
library(lmtest)
bptest(att_lm1)

#6