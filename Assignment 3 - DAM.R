#1
tombstone <- read.csv("tombstone.csv")
names(tombstone) <- c("city","x", "y")

model1 <- lm(formula = y ~ x, data = tombstone)

#2

summary(model1)$r.square


#3.1. Perform hypothesis tests for each coefficient, obtain p values, interpret the results, make a conclusion (i.e. reject or not reject) and explain why.  Note: please explain what the null hypothesis is.

summary(model1)$coef

#3.2. Compute interval estimation for coefficients, interpret the meanings of these quantities.

confint(model1,level=0.95)

#3.3. Make prediction of the Recession Rate at the 1st, 2nd, 3rd, .., 99th percentiles of the SO2, and obtain their prediction intervals (two types of the intervals).
newx <- quantile(tombstone$x, c(seq(0.01, 0.99, by=0.01)))
pred1 <- predict(model1, newdata=data.frame(x=newx),type="response", interval = "prediction")

head(pred1)

pred2 <- predict(model1, newdata=data.frame(x=newx),type="response", interval = "confidence")

head(pred2)



#3.4. Plot data points, the regression line, the predictions and its intervals (two types) and show that the interval is wider on both sides and narrow in the center.


plot(tombstone$x,tombstone$y,pch=20)
abline(model1,col="blue")
points(newx,pred1[,1],pch=1,col="red")
lines(newx,pred1[,2],col="red",lty=2)
lines(newx,pred1[,3],col="red",lty=2)
lines(newx,pred2[,2],col="red",lty=3)
lines(newx,pred2[,3],col="red",lty=3)

#3.53.5.  Making prediction at what range of values of SO2 would be considered extrapolation?  Is it OK to do extrapolation in this case?
max(tombstone$x)
#do not make prediction above covariate of 323


##PART 2

#1
bus <- read.csv("bus.csv")
names(bus) <- c("y","x")
model1 <- lm(formula = y ~ x, data = bus)

#2

summary(model1)$r.square


#3.1. Perform hypothesis tests for each coefficient, obtain p values, interpret the results, make a conclusion (i.e. reject or not reject) and explain why.  Note: please explain what the null hypothesis is.

summary(model1)$coef

#3.2. Compute interval estimation for coefficients, interpret the meanings of these quantities.

confint(model1,level=0.95)

#3.3. Make prediction of the Recession Rate at the 1st, 2nd, 3rd, .., 99th percentiles of the SO2, and obtain their prediction intervals (two types of the intervals).
newx <- quantile(bus$x, c(seq(0.01, 0.99, by=0.01)))
pred1 <- predict(model1, newdata=data.frame(x=newx),type="response", interval = "prediction")

head(pred1)

pred2 <- predict(model1, newdata=data.frame(x=newx),type="response", interval = "confidence")

head(pred2)



#3.4. Plot data points, the regression line, the predictions and its intervals (two types) and show that the interval is wider on both sides and narrow in the center.


plot(bus$x,bus$y,pch=20)
abline(model1,col="blue")
points(newx,pred1[,1],pch=1,col="red")
lines(newx,pred1[,2],col="red",lty=2)
lines(newx,pred1[,3],col="red",lty=2)
lines(newx,pred2[,2],col="red",lty=3)
lines(newx,pred2[,3],col="red",lty=3)

#3.53.5.  Making prediction at what range of values of SO2 would be considered extrapolation?  Is it OK to do extrapolation in this case?
max(bus$x)











