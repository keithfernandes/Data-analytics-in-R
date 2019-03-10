
#Question 1


car <- read.csv('car_r.csv')
head(car)

#Question 2
colnames(car)
ncol(car)

#Question 3
nrow(car)
nrow(car[car$brand == "Ford",])



#Question 4
sapply(car[sapply(car,function(x)is.numeric(x))],function(x) mean(x,na.rm = TRUE))
sapply(car[sapply(car,function(x)is.numeric(x))],function(x) sd(x,na.rm = TRUE))


#Question 5
car_measures_ds <- car[sapply(car,function(x)is.numeric(x))]
col_names <- names(car_measures_ds)
for(i in 1:length(col_names))
 hist(car_measures_ds[,i],main = c("Histogram of",col_names[i]),xlab = col_names[i])

#Question 6

sapply(car,function(x) any(is.na(x)))
#proportion 
sum(is.na(car))/(nrow(car)*ncol(car))

#Question 7
relative_car_speed <- car$speed_car+car$speed_air
head(relative_car_speed)
mean(relative_car_speed)

#Question 8
nrow(car[car$height < 5,])
nrow(car[car$mileage < 40000,])
car <- na.omit(car)
car_new <- subset(car_drop_na,car_drop_na$mileage > 40000 | car_drop_na$height > 5)


#Question 9
car_ford <- car_new[car_new$brand =="Ford",]
car_gm <- car_new[car_new$brand =="GM",]
car_toyota <- car_new[car_new$brand =="Toyota",]







