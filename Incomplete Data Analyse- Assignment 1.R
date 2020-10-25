#Question 3 (a)
require(MASS)
n<-500
#simulating the data
Sigma<-matrix(c(1,0,0,0,1,0,0,0,1),nrow=3,byrow=T)
set.seed(1)
#generate Z1 Z2 Z3 follow independent standard normal (that is, mean 0 and variance 1) distribution
Z<-mvrnorm(n=n,mu=c(0,0,0),Sigma=Sigma)
Z1<-Z[,1];Z2<-Z[,2];Z3<-Z[,3]
Y1=1+Z1
Y2=5+2*Z1+Z2
#a=2 b=0
r_mar=2*(Y1-1)+Z3
#Y2 is missing when r_mar<0
ind_mar <- which(r_mar<0)
Y2_MAR_obs <-Y2[-ind_mar]
Y2_MAR_mis <-Y2[ind_mar]
#plotting the marginal distribution
plot(density(Y2), lwd = 2, col = "blue", xlab =expression(Y[2]),main = "MAR", ylim =c(0, 0.3))
lines(density(Y2_MAR_obs), lwd = 2, col = "red")
legend(8, 0.3, legend =c("Complete data", "Observed data"),col =c("blue", "red"), lty =c(1,1), lwd =c(2,2), bty ="n")
#(b)
Y2_afmar <-ifelse(r_mar<0, NA, Y2)
data <-data.frame(Y1=Y1, Y2=Y2_afmar)
fit1 <-lm(Y2_afmar~Y1, data=data)
fit1$coefficient
set.seed(1)
predicted_sri1 <-predict(fit1, newdata = data)+ rnorm(500, 0,summary(fit1)$sigma)
Y2_sri1 <-ifelse(is.na(data$Y2), predicted_sri1, data$Y2)
plot(density(Y2), lwd = 2, col = "blue", xlab =expression(Y[2]),main = "MAR", ylim =c(0, 0.3))
lines(density(Y2_sri1), lwd = 2, col = "red")
legend(8, 0.3, legend =c("Original Complete data", "Imputation Complete data"),col =c("blue", "red"), lty =c(1,1), lwd =c(2,2), bty ="n")
#(c)
#a=0 b=2
r_mnar=2*(Y2-5)+Z3
#Y2 is missing when r_mnar<0
ind_mnar <-which(r_mnar<0)
Y2_MNAR_obs <- Y2[-ind_mnar]
plot(density(Y2), lwd = 2, col = "blue", xlab =expression(Y[2]), main = "MNAR",ylim =c(0, 0.3))
lines(density(Y2_MNAR_obs), lwd = 2, col = "red")
legend(8, 0.3, legend =c("Complete data", "Observed data"), col =c("blue","red"), lty =c(1, 1), lwd =c(2, 2), bty = "n")
#(d)
Y2_afmnar <-ifelse(r_mnar<0, NA, Y2)
data <-data.frame(Y1=Y1, Y2=Y2_afmnar)
fit2 <-lm(Y2_afmnar~Y1, data=data)
fit2$coefficient
predicted_sri2 <-predict(fit2, newdata = data)+ rnorm(nrow(data), 0,sigma(fit2))
Y2_sri2 <-ifelse(is.na(data$Y2), predicted_sri2, data$Y2)
plot(density(Y2), lwd = 2, col = "blue", xlab =expression(Y[2]),main = "MNAR", ylim =c(0, 0.3))
lines(density(Y2_sri2), lwd = 2, col = "red")
legend(8, 0.3, legend =c("Original Complete data", "Imputation Complete data"),col =c("blue", "red"), lty =c(1,1), lwd =c(2,2), bty ="n")
#Question 4 (a)
recovtime_mean=mean(databp$recovtime, na.rm = TRUE)
recovtime_mean
recovtime_sd=sd(databp$recovtime, na.rm = TRUE)
recovtime_se=recovtime_sd/sqrt(22)
recovtime_se
cor(databp$recovtime, databp$logdose, use = "complete")
cor(databp$recovtime, databp$bloodp, use = "complete")
#(b)
# creating a vector with recovtime values after mean imputation
recovtime_mi <-ifelse(is.na(databp$recovtime), recovtime_mean, databp$recovtime)
mean(recovtime_mi)
# standard deviation of the completed variable
sd(recovtime_mi)
se_recovtime_mi=sd(recovtime_mi)/sqrt(25)
se_recovtime_mi
cor(recovtime_mi, databp$logdose)
cor(recovtime_mi, databp$bloodp)
#(c)
fit <-lm(recovtime~logdose+bloodp, data = databp)
summary(fit)
fit$coefficients
predicted_ri <-predict(fit, newdata = databp)
# creating the completed recovtime variable
recovtime_ri <-ifelse(is.na(databp$recovtime), predicted_ri, databp$recovtime)
# std deviation for the completed data
sd(recovtime_ri)
se_ri=sd(recovtime_ri)/sqrt(25)
se_ri
# correlations between the recovery time and the dose and between the recovery time and blood pressure
cor(recovtime_ri, databp$logdose)
cor(recovtime_ri, databp$bloodp)
#(d)
set.seed(1)
predicted_sri <-predict(fit, newdata = databp)+ rnorm(nrow(databp), 0,sigma(fit))
# completed recovery time variable
recovtime_sri <-ifelse(is.na(databp$recovtime), predicted_sri, databp$recovtime)
mean(recovtime_sri)
# std deviation for the completed data
sd(recovtime_sri)
se_sri=sd(recovtime_sri)/sqrt(25)
se_sri
# correlations between the recovery time and the dose and between the recovery time and blood pressure
cor(recovtime_sri, databp$logdose)
cor(recovtime_sri, databp$bloodp)
#(e)
# predictive mean matching (those with observed recovery time)
predicted_ri <-predict(fit, newdata = databp)
predicted_ri_ <- predicted_ri
predicted_ri_ [4] <- NA
donor4 <- which.min(abs(predicted_ri_ - predicted_ri[4]))
predicted_ri_ [10] <- NA
donor10 <- which.min(abs(predicted_ri_ - predicted_ri[10]))
predicted_ri_ [22] <- NA
donor22 <- which.min(abs(predicted_ri_ - predicted_ri[22]))
donor4
donor10
donor22
donor4 <- 5
donor10 <- 2
donor22 <- 15
newdata <- c(databp$recovtime[1:3], 13, databp$recovtime[5:9], 10, databp$recovtime[11:21], 39, databp$recovtime[23:25])
mean(newdata)
se=sd(newdata)/sqrt(25)
se
cor(newdata,databp$logdose)
cor(newdata, databp$bloodp)
