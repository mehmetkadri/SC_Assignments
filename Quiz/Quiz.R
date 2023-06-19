
library(ggplot2)



###1

men <- c(1,8,14,13,20,24,16,7,6,19)
women <- c(5,7,22,21,23,28,16,6,12,23)


#a
#histograms
hist(men,col="blue",breaks = 8)
hist(women,col = "red",breaks = 8)
#boxplots
boxplot(men)
boxplot(women)
#density
d.highway <- density(men)
plot(d.highway, xlab = "Men",main = "Histogram Plot for Men",col="blue")
d.city <- density(women)
plot(d.city,xlab = "Women" ,main = "Density Plot for Women",col="red")
## This men data is distrubuted normally but women data is not.

#b
#µ0 = men
#µ1 = women
#H0 µ0 = µ1
#H1 µ0 != µ

#c
#These two sample are independent from each other so we will do independent two sample t-test.

#d
#Checking normality for each data

#H0: Data is Normally Distributed
#H1: Data is not Normally Distributed    alpha=0.05

#If p ≤ 0.05: then the null hypothesis can be rejected 
#(i.e. the variable is NOT normally distributed).
#If p > 0.05: then the null hypothesis cannot be rejected 
#(i.e. the variable MAY BE normally distributed).

shapiro.test(men)
shapiro.test(women)

# So p is greater than 0.05 these two samples are normally distributed.

var.test(men,women)

#e
t.test(men,women,alternative = "two.sided",mu=0,paired = FALSE,var.equal = TRUE)

#Conclusion: So, we can not reject H0 and we can not say their means are not different.

#g
#µ0 = men
#µ1 = women
#H0 µ0 >= µ1
#H1 µ0 < µ1

#h
t.test(men,women,alternative = "greater",mu=0,paired = FALSE,var.equal = TRUE)
#Conclusion: So, we can  reject H0 and we can say being a women means 
#the person is more likely to have more body fat.


t.score <-  qt(p=0.05, df=188,lower.tail=F)
#j
#Confidence interval 

t.score <-  qt(p=0.05, df=length(men)-1,lower.tail=F)

men.mean <- mean(men)
men.sd <- sd(men)
men.se <- men.sd/sqrt(length(men))
error <- t.score * men.se
lower.bound <- men.mean  - error
upper.bound <- men.mean  + error
print(c(lower.bound,upper.bound))


t.score <-  qt(p=0.05, df=length(women)-1,lower.tail=F)

women.mean <- mean(women)
women.sd <- sd(women)
women.se <- women.sd/sqrt(length(women))
error <- t.score * women.se
lower.bound <- women.mean  - error
upper.bound <- women.mean  + error
print(c(lower.bound,upper.bound))

###2
start <- c(5,7,22,21,23,28,16,6,12,23)
after <- c(1,8,14,13,20,24,16,7,6,19)


#a
#µ0 = start
#µ1 = after
#H0: µ0 <= µ1
#H1: µ0 > µ1

#b
#Our individuals are the exactly the same person so we can say that these tow sample are dependent.

#c
#Checking normality for each data

#H0: Data is Normally Distributed
#H1: Data is not Normally Distributed    alpha=0.05

#If p ≤ 0.05: then the null hypothesis can be rejected 
#(i.e. the variable is NOT normally distributed).
#If p > 0.05: then the null hypothesis cannot be rejected 
#(i.e. the variable MAY BE normally distributed).
shapiro.test(start)
shapiro.test(after)

t.test(start,after, mu =0 ,paired = TRUE, alternative = "less",conf.level = 0.95)
#Conclusion We can not reject H0 and we can say that doing sports decreases the amount of body fat. 

##3
 
group1 <- c(1,3,6,5,7,2,1,2,3,5)
group2 <- c(9,7,7,4,3,6,5,4,6,7)
group3 <- c(6,8,7,7,9,5,7,8,6,7)
#a
#H0: µ0 = µ1 = µ2
#H1: µ0 != µ1 != µ2

#b
groups <- c(group1,group2,group3)
freq  <- as.factor(c(rep("once",10),rep("three",10),rep("five",10)))
data <- data.frame(groups,freq)

bartlett.test(groups~freq,data = data)

model <- aov(groups~freq,data = data)
summary(model)

TukeyHSD(model)
