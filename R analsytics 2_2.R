chicago_ds=read.csv("data_chicago.csv")
names(chicago_ds)

plot(chicago_ds$x,chicago_ds$y)
model2=lm(chicago_ds$y~chicago_ds$x)
summary(model2)
abline(model2)

plot(y,model2$fitted.values)
abline(a=0,b=1)

hist(model2$residuals)

plot(model2$residuals,pch=20)
points(model2$residuals,type="h")
abline(h=0)

