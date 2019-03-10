chicago <- read.csv('data_chicago.csv')

head(chicago)

model1 <- lm(chicago$y~chicago$x,data = chicago)

pred <- predict(model1,newdata=data.frame(x=newx),interval = c("prediction"),level = 0.95, type="response")

confint(model1,level=0.95)
summary(model1)$r.square

newx <- range()
pred <- predict(model1,newdata=data.frame(x=10),interval = c("prediction"),level = 0.95, type="response")