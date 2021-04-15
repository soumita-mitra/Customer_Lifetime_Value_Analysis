list.of.packages <- c("boot", "car","QuantPsyc","lmtest","sandwich","vars","nortest","MASS","caTools")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, repos="http://cran.rstudio.com/")

library(boot) 
library(car)
library(QuantPsyc)
library(lmtest)
library(sandwich)
library(vars)
library(nortest)
library(MASS)
library(caTools)
library(dplyr)
library(ggplot2)

dg <- read.csv(file.choose())
str(dg)


dg$Number.of.Open.Complaints <- as.factor(dg$Number.of.Open.Complaints)
dg$Number.of.Policies <- as.factor(dg$Number.of.Policies)

str(dg)

summary(dg)

colnames(dg)[which(names(dg)=="Customer.Lifetime.Value")]="clv"

str(dg)

quantile(dg$clv, seq(0,1,.05))
boxplot(dg$clv, xlab = "Customer Lifetime Value")

dg1 <- dg[dg$clv <9000,]
boxplot(dg1$clv, xlab = "Customer Lifetime Value")

quantile(dg1$Monthly.Premium.Auto, seq(0,1,.05))
boxplot(dg1$Monthly.Premium.Auto)

dg2 <- dg1[dg1$Monthly.Premium.Auto<140,]
boxplot(dg2$Monthly.Premium.Auto)

dg3 <- filter(dg2, Income>0)

summary(dg3)

dg4 = dg3[, !(colnames(dg) %in% c("Customer","State","Effective.To.Date"))]

hist(dg4$clv, xlab="Customer Lifetime Value", col = "blue", main = "Histogram")

boxplot(dg4$Income, outlier = T, xlab="Income")
boxplot(dg4$Monthly.Premium.Auto, outlier = T, xlab="Monthly.Premium.Auto")


ggplot(dg4, aes(Income))+
  geom_density()

densityPlot(dg4$Monthly.Premium.Auto, xlab = 'Monthly Premium Auto')

ggplot(dg4, aes(EmploymentStatus))+
  geom_bar()

ggplot(dg4, aes(Education))+
  geom_bar()

ggplot(dg4, aes(Marital.Status, clv))+
  geom_bar(stat = "identity")

ggplot(dg4, aes(Gender, clv))+
  geom_bar(stat = "identity")

ggplot(dg4, aes(Number.of.Open.Complaints))+
  geom_bar()

ggplot(dg4, aes(Number.of.Policies))+
  geom_bar()


  
sample = sample.split(dg4$clv,SplitRatio = 0.70)
train4 =subset(dg4,sample ==TRUE)
str(train4)
dim(train4)

test4=subset(dg4, sample==FALSE)
str(test4)
dim(test4)

L0 <- lm(clv~. ,data = train4)
summary(L0)

L1 <- lm(clv~Response+	Coverage+	Education+	EmploymentStatus+	Gender+	Income+	Location.Code+	Marital.Status+	Monthly.Premium.Auto+	Months.Since.Last.Claim+	Months.Since.Policy.Inception+	Number.of.Open.Complaints+	Number.of.Policies +	Renew.Offer.Type+	Sales.Channel+	Total.Claim.Amount+	Vehicle.Class+	Vehicle.Size, data=train4)
summary(L1)

L2 <- lm(clv~Response+	Coverage+	Education+	EmploymentStatus+	Gender+	Income+	Location.Code+	Marital.Status+	Monthly.Premium.Auto+	Months.Since.Last.Claim+	Months.Since.Policy.Inception+	Number.of.Open.Complaints+	Number.of.Policies +	Renew.Offer.Type+	Sales.Channel+	Total.Claim.Amount, data=train4)
summary(L2)

L3 <- lm(clv~Response+	Coverage+	Education+	EmploymentStatus+	Gender+	Income+	Location.Code+	Marital.Status+	Monthly.Premium.Auto+	Months.Since.Last.Claim+	Months.Since.Policy.Inception+	Number.of.Open.Complaints+	Number.of.Policies +	Renew.Offer.Type, data=train4)
summary(L3)

L4 <- lm(clv~Response+	Coverage+	Education+	EmploymentStatus+	Gender+	Income+	Location.Code+	Marital.Status+	Monthly.Premium.Auto+	Months.Since.Last.Claim+	Number.of.Open.Complaints+	Number.of.Policies, data=train4)
summary(L4)

L5 <- lm(clv~Response+	I(Coverage == "Premium")+	I(Education == "High School or Below")+ I(Education == "Master")+	EmploymentStatus+	Gender+	Income+	Marital.Status+	Monthly.Premium.Auto+	Number.of.Open.Complaints+	Number.of.Policies, data=train4)
summary(L5)

L6 <- lm(clv~Response+	I(Coverage == "Premium")+	I(Education == "High School or Below")+ I(Education == "Master")+	I(EmploymentStatus == "Employed")+ I(EmploymentStatus == "Retired")+	Gender+	Income+	Marital.Status+	Monthly.Premium.Auto+	Number.of.Open.Complaints+	Number.of.Policies, data=train4)
summary(L6)

par(mfrow=c(2,2))
plot(L6)

resids1 <- L6$residuals
qqnorm(resids1)
ad.test(resids1)

bptest(L6)

durbinWatsonTest(L6)

vif(L6)

#fitted model
f1 <- lm(clv~Response+	I(Coverage == "Premium")+	I(Education == "High School or Below")+ I(Education == "Master")+	I(EmploymentStatus == "Employed")+ I(EmploymentStatus == "Retired")+	Gender+	Income+	Marital.Status+	Monthly.Premium.Auto+	Number.of.Open.Complaints+	Number.of.Policies, data=test4)
summary(f1)

#removing response, coverage, I(Education == "Master"), I(MaritaStatus == "Married")
f1 <- lm(clv~I(Education == "High School or Below")+	I(EmploymentStatus == "Employed")+ I(EmploymentStatus == "Retired")+	Gender+	Income+	I(Marital.Status == "Single")+	Monthly.Premium.Auto+	Number.of.Open.Complaints+	Number.of.Policies, data=test4)
summary(f1)

par(mfrow=c(2,2))
plot(f1)

resids1 <- f1$residuals
qqnorm(resids1)
ad.test(resids1)

bptest(f1)

durbinWatsonTest(f1)

vif(f1)


test4$prediction <- fitted(f1)
#write.csv(ori_data,"mape.csv")

#Calculating MAPE
attach(test4)
(sum((abs(clv-prediction))/clv))/nrow(test4)

p <- ggplot(test4, aes(clv, prediction)) + geom_point()
p + geom_abline()









