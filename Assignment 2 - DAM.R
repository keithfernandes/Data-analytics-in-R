tombstone <- read.csv("tombstone.csv")
names(tombstone) <- c("city","x", "y")

plot(tombstone$x,tombstone$y)


model1 <- lm(formula = tombstone$y ~ tombstone$x, data = tombstone)
model1$coefficients
model1$fitted.values
sum(model1$fitted.values)
sum(tombstone$y)

mean(model1$fitted.values)
mean(tombstone$y)

model1$residuals
sum(model1$residuals)


summary(model1)


tombstone_new <- tombstone
tombstone_new$x <- tombstone_new$x+1
model2 <- lm(tombstone_new$y ~ tombstone_new$x,data = tombstone_new)

par(mfrow=c(1,2))
plot(tombstone_new$x,model2$fitted.values)

plot(tombstone$x,model1$fitted.values)


tombstone$x


tombstone[which.max(tombstone$y),]

tombstone[16,1]


mean(tombstone$x)
mean(tombstone$y)
plot(mean(tombstone$x), mean(tombstone$y))
abline(model1)


model1 <- lm(formula = tombstone$y ~ tombstone$x, data = tombstone)
summary(model1)$coeff


summary(model1)$coeff



#Part 2

#1
 bus <- read.csv("bus.csv")
 

 names(bus) <- c("y","x")
 plot(bus$x,bus$y)



 model1 <- lm(formula = bus$y ~ bus$x, data = bus)
 summary(model1)$coeff


 
 model1$fitted.values
 sum(model1$fitted.values)
 sum(bus$y)

 mean(model1$fitted.values)
 mean(bus$y)


 model1$residuals
 sum(model1$residuals)
 
 summary(model1)$coeff
 
 
 
bus_new <- bus
 bus_new$x <- bus_new$x+1
 model2 <- lm(bus_new$y ~ bus_new$x,data = bus_new)
 
 par(mfrow=c(1,2))
 plot(bus_new$x,model2$fitted.values)
 
 plot(bus$x,model1$fitted.values)
 
 
 bus$x
 
 
 mean(bus$x)
 mean(bus$y)
 plot(mean(bus$x), mean(bus$y))
 abline(model1)
 


 
 


