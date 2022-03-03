#code for the article @ https://medium.com/me/stats/post/cce073d50d8

library(readxl)
library(dplyr)
library(caTools)

data<-read_excel("c:/users/serhat/desktop/cse5023/presentation/antro_data.xls",col_names=FALSE)
col_names<-c("age","gender","height","BW","UEL","UEC","WC","LEL","LEC","bmi")
colnames(data)<-col_names
head(data)
attach(data)
data<- data %>% filter(gender==1)

set.seed(245702)
sample_size=floor(2/3*nrow(data))
pick<-sample(nrow(data),sample_size)
train<-data[pick,]
test<-data[-pick,]



par(mfrow=c(1,1))
hist(train$BW,xlab="Body Weight",breaks=10)
hist(train$UEL,xlab="Upper Extremity Length",breaks=10)
hist(train$UEC,xlab="Upper Extremity Circumference",breaks=10)
hist(train$WC,xlab="Waist Circumference",breaks=10)
hist(train$LEL,xlab="Lower Extremity Length",breaks=10)
hist(train$LEC,xlab="Lower Extremity Circumference",breaks=10)

shapiro.test(train$BW)
shapiro.test(train$UEL)
shapiro.test(train$UEC)
shapiro.test(train$WC)
shapiro.test(train$LEL)
shapiro.test(train$LEC)

data<-data[,-c(1,2,3,10)]
head(data)
cor(data)
pairs(data)

#simple regression
simple_regression_UEL<-lm(train$BW~train$UEL)
summary(simple_regression_UEL)
plot(data$UEL,data$BW,xlab="Upper Extremity Length",ylab="Body Weight",pch=19,col="red")
abline(lm(data$BW~data$UEL),col="blue")

simple_regression_UEC<-lm(train$BW~train$UEC)
summary(simple_regression_UEC)
plot(data$UEC,data$BW,xlab="Upper Extremity Circumference",ylab="Body Weight",pch=19,col="red")
abline(lm(data$BW~data$UEC),col="blue")

simple_regression_WC<-lm(train$BW~train$WC)
summary(simple_regression_WC)
plot(data$WC,data$BW,xlab="Waist Circumference",ylab="Body Weight",pch=19,col="red")
abline(lm(data$BW~data$WC),col="blue")

simple_regression_LEL<-lm(train$BW~train$LEL)
summary(simple_regression_LEL)
plot(data$LEL,data$BW,xlab="Lower Extremity Length",ylab="Body Weight",pch=19,col="red")
abline(lm(data$BW~data$LEL),col="blue")

simple_regression_LEC<-lm(train$BW~train$LEC)
summary(simple_regression_LEC)
plot(data$LEC,data$BW,xlab="Lower Extremity Circumference",ylab="Body Weight",pch=19,col="red")
abline(lm(data$BW~data$LEC),col="blue")

regmodel<-lm(train$BW~train$UEL+train$UEC+train$WC+train$LEL+train$LEC)
summary(regmodel)
confint(regmodel)

#multicollinearity test
library(car)
vif(regmodel)

#model selection
library(olsrr)
ols_step_all_possible(regmodel)

ols_step_all_possible(regmodel,print_plot=FALSE)$aic
ols_step_all_possible(regmodel,print_plot=FALSE)$sbc

which.min(ols_step_all_possible(regmodel,print_plot=FALSE)$aic)
which.min(ols_step_all_possible(regmodel,print_plot=FALSE)$sbc)

ols_step_both_p(regmodel)

ols_step_forward_p(regmodel)

ols_step_backward_p(regmodel)

#normal distribution of residuals

regmodel2<-lm(train$BW~train$WC+train$LEC)
summary(regmodel2)
vif(regmodel2)

hist(regmodel2$residuals)
shapiro.test(regmodel2$residuals)

library(lmtest)
bptest(regmodel2)

plot(regmodel2,which=2)
plot(regmodel2)

train2<-train[-c(57,38,47),]
regmodel3<-lm(train2$BW~train2$UEC+train2$UEL+train2$WC+train2$LEL+train2$LEC)
summary(regmodel3)
plot(regmodel3)

#Testing new variables
test<-test[,-c(1,2,3,10)]
test$prediction<-regmodel2$coefficients[1]+regmodel2$coefficients[2]*test$WC+regmodel2$coefficients[3]*test$LEC

test$bad_prediction<-regmodel$coefficients[1]+regmodel$coefficients[2]*test$UEL+regmodel$coefficients[3]*test$UEC+
  regmodel$coefficients[4]*test$WC+regmodel$coefficients[5]*test$LEL+regmodel$coefficients[6]*test$LEC
head(test)

library(Metrics)
mae(test$BW,test$prediction)
mae(test$BW,test$bad_prediction)
mse(test$BW,test$prediction)
mse(test$BW,test$bad_prediction)
rmse(test$BW,test$prediction)
rmse(test$BW,test$bad_prediction)
