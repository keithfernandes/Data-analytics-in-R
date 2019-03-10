df <- InsectSprays
dt <- as.data.frame.table(df)

#1
nrow(df); ncol(df)

#2
mean(df$count)



#3

length(unique(df$spray))

#4
table(df$spray)

#5
by(df$count,df$spray,mean)

#6
boxplot(count~spray, data = df, xlab = "types of spray", ylab = "insect count", main = "InsectSpraysData")


#7 no they are not different

y1 <- df$count[df$spray == "A"]
y2 <- df$count[df$spray == "B"]
t.test(y1,y2)


#8 no they are not

t.test(y1,y2,var.equal = T)

#9 based on anova test below, spray type statistically significant to determine coount of insects

ft <- aov(df$count ~df$spray)

summary(ft)


#10 yes (no need to state the reason)
ft2 <- aov(sqrt(df$count) ~df$spray)

summary(ft2)





