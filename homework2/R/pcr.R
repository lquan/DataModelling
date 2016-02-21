library(pls)
library(MASS)
library(boot)
library(DAAG)
library(car)
#library(TeachingDemos)
library(FitAR)

autos<-read.table("autos.txt",header=T,sep=",",na.strings="?");
autos$symboling <- as.ordered(autos$symboling)
autos <- na.omit(autos)

autos2 <- autos[sapply(autos,is.numeric)]



autos2.pca <- prcomp(autos2[1:14],scale=TRUE,center=TRUE)
plot(autos2.pca)
summary(autos2.pca)
autos2.pcr <- lm(autos2$price~autos2.pca$x[,1]+autos2.pca$x[,2]+autos2.pca$x[,3]+autos2.pca$x[,4])
autos2.pcr
summary(autos2.pcr)
#summary(autos2.pca)

#autos2.pcr2 <- pcr(price~normalized.losses+wheel.base+length+width+height+curb.weight+engine.size+bore+stroke+compression.ratio+horsepower+peak.rpm+city.mpg+highway.mpg, ncomp=4, autos2, validation = "CV",scale=TRUE,center=TRUE)
#width+length+horsepower+height+city.mpg+highway.mpg
#plot(autos2.pcr)
#lines(c(1:40000),c(1:40000),col="red")

#met boxcox
shapiro.test(autos2$price)
boxcox(autos2$price~autos2.pca$x[,1]+autos2.pca$x[,2]+autos2.pca$x[,3]+autos2.pca$x[,4])
lambda <- -0.4
autos2.bcprice <-   bxcx(autos2$price, lambda, InverseQ = FALSE, type = "BoxCox")
shapiro.test(autos2.bcprice)
autos2.bcpcr <- lm(autos2.bcprice~autos2.pca$x[,1]+autos2.pca$x[,2]+autos2.pca$x[,3]+autos2.pca$x[,4])
autos2.bcpcr
summary(autos2.bcpcr)

par(mfrow=c(2,1))
autos2.rbcprice <- bxcx(autos2.bcpcr$fitted.values, lambda,InverseQ= TRUE, type = "BoxCox")
plot(autos2$price,autos2.rbcprice,main="PCR",xlab = 'measured',ylab = 'predicted')
lines(c(1:40000),c(1:40000),col='red')

autos2.plsr <- plsr(autos2.bcprice~.-price, data = autos2, ncomp=4, validation="CV",scale=T)
autos2.pbcprice <-   bxcx(autos2.plsr$fitted.values[,1,1], lambda, InverseQ = TRUE, type = "BoxCox")
plot(autos2$price,autos2.pbcprice,main="PLSR",xlab = 'measured',ylab = 'predicted')#autosNum.plsr,main="PLS")
lines(c(1:40000),c(1:40000),col='red')
autos2.plsr
summary(autos2.plsr)

summary(autos2$price)
summary(autos2.rbcprice)
summary(autos2.pbcprice)

sqrt(sum((autos2$price-autos2.plsr$fitted.values[,1,1])^2))/159


par(mfrow=c(1,1))
#manuele PCR
sqrt(sum((autos2.pcr$residuals)^2))/159
#automatisch met pcr commando
sqrt(sum((autos2.pcr2$residuals)^2))/159
#manuele met boxcox transfo
sqrt(sum((autos2.price-autos2$price)^2))/159


plot(autos2$price,autos2.pcr$fitted.values)
lines(c(1:40000),c(1:40000),col='red')

