#1
PGA <- read.csv('pga.csv')

#2

PGA_numeric <- PGA[,sapply(PGA,is.numeric)]
par(mfrow=c(2,3))
hist(PGA_numeric$Age)
hist(PGA_numeric$AverageDrive)
hist(PGA_numeric$DrivingAccuracy)
hist(PGA_numeric$GreensonRegulation)
hist(PGA_numeric$AverageNumofPutts)
hist(PGA_numeric$SavePercent)
hist(PGA_numeric$MoneyRank)
hist(PGA_numeric$NumEvents)
     

pairs(PGA_numeric,pch = 20)




#3
model1 <- lm(AverageWinnings~Age+AverageDrive+DrivingAccuracy+GreensonRegulation+AverageNumofPutts+SavePercent+
               NumEvents,data = PGA)

#4
summary(model1)$coef
               
#5
summary(model1)

#6


confint(model1,level=0.95)

#7 and 8

observations_for_pred=data.frame(Age=c(35,42), AverageDrive=c(287,295),
                                 DrivingAccuracy=c(64,69), GreensonRegulation=c(64.9,67.7),
                                 AverageNumofPutts = c(1.778,1.80),SavePercent = c(48,54),NumEvents = c(26,30))


predict(model1,observations_for_pred,interval="prediction", level=0.95, type="response")
predict(model1,observations_for_pred,interval="confidence", level=0.95, type="response")

#9

pga_unit_normal=as.data.frame(apply(PGA[,c("AverageWinnings","Age","AverageDrive","DrivingAccuracy","GreensonRegulation","AverageNumofPutts","SavePercent",
                                             "NumEvents")],2,function(x){(x-mean(x))/sd(x)}))

model1_unit_normal <- lm(AverageWinnings~Age+AverageDrive+DrivingAccuracy+GreensonRegulation+AverageNumofPutts+SavePercent+
                           NumEvents,data = pga_unit_normal)


summary(model1_unit_normal)


