# Andreas Put & Li Quan                             #
# Homework 2: Statistische Modellen & Data-analyse  #
# May 23, 2011                                      #
#####################################################

library(MASS)
library(car)

autos<-read.table("autos.txt",header=T,sep=",",na.strings="?")
autos$symboling <- factor(autos$symboling,ordered=T)
autos$num.of.doors <- factor(autos$num.of.doors,ordered=T,levels=c("two","four"))
autos$num.of.cylinders <- factor(autos$num.of.cylinders,ordered=T,levels=c("two","three","four","five","six","eight","twelve"))

#exploratory analysis
#library(rggobi)## for interactive data exploration with ggobi, type ggobi(autos)
summary(autos)
autos<-na.omit(autos) #just remove the instances with some missing attribute value
summary(autos)

autosNum <- autos[sapply(autos,is.numeric)] #consider only numerical attributes
summary(autosNum)

library(corrplot)
autosNum.cor <- cor(autosNum)
corrplot::corrplot(autosNum.cor) #avoids collision with corrplot of pls library

autosWorking <- autosNum         #this will be our main working dataset
autosWorking$drive.wheels <- autos$drive.wheels
par(mfrow=c(5,3))
plot(price~.,autosWorking)
par(mfrow=c(1,1))

#data transformation is needed for price
shapiro.test(autosWorking$price)
#we can perform logarithmic transformation on price
autosLog <- autosWorking
autosLog$price <- log10(autosWorking$price)
#lets use the more general boxcox transformation for better results
library(FitAR)
autosWorking.lm <- lm(price~.,autosWorking); summary(autosWorking.lm)
boxcox(autosWorking.lm)
lambda <- -0.4
autosWorking$price <- bxcx(autosWorking$price, lambda, InverseQ = F, type = "BoxCox")
shapiro.test(autosWorking$price)

par(mfrow=c(1,3))
qqnorm(autos$price,main="before transformation")
qqline(autos$price)
qqnorm(autosLog$price,main="after log transformation")
qqline(autosLog$price)
qqnorm(autosWorking$price,main="after BoxCox transformation")
qqline(autosWorking$price)

autosNum$price <- autosWorking$price

#now make again a plot of response variable versus regressors 
par(mfrow=c(5,3))
plot(price~.,autosWorking)

par(mfrow=c(1,2))
boxplot(price~drive.wheels,autos)
boxplot(price~drive.wheels,autosWorking)
par(mfrow=c(1,1))


#PCA
library(pls)
autosNum.pca <- prcomp(autosNum[1:14],scale=T,center=T)
plot(autosNum.pca)
summary(autosNum.pca)
biplot(autosNum.pca)
#qqnorm(autosNum.pca$x[,1]);qqline(autosNum.pca$x[,1])
#scatterplot(autosNum.pca$x[,1], autosNum.pca$x[,2])
npc <- 4

#PCR
autosNum.pcr <- lm(autos$price~autosNum.pca$x[,1]+autosNum.pca$x[,2]+autosNum.pca$x[,3]+autosNum.pca$x[,4])
summary(autosNum.pcr)
plot(autos$price,autosNum.pcr$fitted.values,main="PCR",xlab = 'measured',ylab = 'predicted')
lines(c(1:40000),c(1:40000),col='red')

#PCR With Box-Cox

boxcox(autos$price~autosNum.pca$x[,1]+autosNum.pca$x[,2]+autosNum.pca$x[,3]+autosNum.pca$x[,4])
lambda <- -0.4
autosNum.bcprice <-   bxcx(autosNum$price, lambda, InverseQ = FALSE, type = "BoxCox")
shapiro.test(autosNum.bcprice)
autosNum.bcpcr <- lm(autosNum.bcprice~autosNum.pca$x[,1]+autosNum.pca$x[,2]+autosNum.pca$x[,3]+autosNum.pca$x[,4])
summary(autosNum.bcpcr)
autosNum.rbcprice <- bxcx(autosNum.bcpcr$fitted.values, lambda,InverseQ= TRUE, type = "BoxCox")
plot(autosNum$price,autosNum.rbcprice,main="PCR_Box-Cox",xlab = 'measured',ylab = 'predicted')
lines(c(1:40000),c(1:40000),col='red')

#PLSR
par(mfrow=c(2,1))
autosNum.plsr <- plsr(price~.-price, data = autosNum, ncomp=npc, validation="CV",scale=T)
summary(autosNum.plsr)
plot(autosNum$price,autosNum.pcr$fitted.values,main="PCR",xlab = 'measured',ylab = 'predicted')
lines(c(1:40000),c(1:40000),col='red')

plot(autosNum$price,autosNum.plsr$fitted.values[,1,1],main="PLSR",xlab = 'measured',ylab = 'predicted')#autosNum.plsr,main="PLS")
lines(c(1:40000),c(1:40000),col='red')
par(mfrow=c(1,1))

# linear model using all attributes
autosWorking.lm <- lm(price~.,autosWorking)
summary(autosNum.lm)
plot(autosNum.lm)

#variable selection by AIC in stepwise algorithm
autosNum.aic <- stepAIC(lm(price~1,autosWorking),
                        list(upper=~normalized.losses+wheel.base+length+width+height+
                                curb.weight+engine.size+bore+stroke+compression.ratio+
                                horsepower+peak.rpm+city.mpg+highway.mpg+drive.wheels,
                             lower=~1),
                        direction="both")
formula <- price ~ curb.weight + horsepower + length + normalized.losses + 
                    width + compression.ratio + city.mpg
autosNum.lm2 <- lm(formula, autosWorking)
plot(autosNum.lm2)


#aov
autosWorking.aov <- aov(price ~ drive.wheels, autosWorking)
summary(autosWorking.aov)
qqnorm(autosWorking$price[autosWorking$drive.wheels == "4wd"])
qqline(autosWorking$price[autosWorking$drive.wheels == "4wd"])

qqnorm(autosWorking$price[autosWorking$drive.wheels == "fwd"])
qqnorm(autosWorking$price[autosWorking$drive.wheels == "rwd"])
