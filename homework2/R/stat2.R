#Andreas Put & Li Quan - May 23, 2011
#homework 2
#library(FactoMineR)
library(pls)
library(MASS)
library(boot)
library(DAAG)

autos<-read.table("autos.txt",header=T,sep=",",na.strings="?");
autos$symboling <- as.ordered(autos$symboling)
autos <- na.omit(autos)

autos2 <- autos[sapply(autos,is.numeric)]
#nobs <- nrow(autos2)
#idx <- 1:nobs
#trainingidx <- sample(idx, trunc(length(idx)/3))
#autos2.training <- autos2[-trainingidx,]
#autos2.validation <- autos2[trainingidx,]
#biplot(autos2)

autos3 <- autos2
for (i in c(4,7,11,15)) {
  autos3[,i] <- log(autos3[,i])
  print(i)
}
autos3 <- autos3[,-10]


autos2.pca <- prcomp(autos2,scale=TRUE,center=TRUE)
plot(autos2.pca)

#summary(autos2.pca)

autos2.pcr <- pcr(price~normalized.losses+wheel.base+length+width+height+curb.weight+engine.size+bore+stroke+compression.ratio+horsepower+peak.rpm+city.mpg+highway.mpg, ncomp=4, autos2, validation = "CV",scale=TRUE,center=TRUE)
#width+length+horsepower+height+city.mpg+highway.mpg
plot(autos2.pcr)
lines(c(1:40000),c(1:40000),col="red")

crossval(autos2.pcr,autos2,3)
print(sum(abs(residuals(autos2.pcr)))/nobs)
#qqnorm(autos2.pca)

#linear<-lm(price~normalized.losses+wheel.base+length+width+height+curb.weight+engine.size+bore+stroke+compression.ratio+horsepower+peak.rpm+city.mpg+highway.mpg,autos2)
#plot(linear)
autos2.initial <- lm(price~1,autos2)
autos2.aic <- stepAIC(autos2.initial,list(upper=~normalized.losses+wheel.base+length+width+height+curb.weight+engine.size+bore+stroke+compression.ratio+horsepower+peak.rpm+city.mpg+highway.mpg,lower=~1),direction="both")

cvtemp <- CVlm(df=autos2, lm(price~curb.weight+width+engine.size+peak.rpm+length+stroke+bore+wheel.base,data = autos2), m=3) # 3 fold cross-validation

linear<-lm(price~curb.weight+width+engine.size+peak.rpm+length+stroke+bore+wheel.base,data = autos2.training)



autos2.pls <- plsr(price~normalized.losses+wheel.base+length+width+height+curb.weight+engine.size+bore+stroke+compression.ratio+horsepower+peak.rpm+city.mpg+highway.mpg, ncomp=4, autos2, validation = "CV",scale=TRUE,center=TRUE)
biplot(autos2.pls)

for (i in 1:14) {
  hist(autos2[,i])
  readline('CR to continue ')
  print(i)
  
  }


