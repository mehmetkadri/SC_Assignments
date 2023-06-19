first_data <- read.csv("first_data.csv")
second_data <- read.csv("second_data.csv")



str(first_data)

first_data[which(first_data$Sex=='0'), ]$Pregnancy=2
first_data[which(first_data$Sex=='1'& is.na(first_data$Pregnancy)), ]$Pregnancy=0

first_data[which(is.na(first_data$alcohol_consumption_per_day)), ]$alcohol_consumption_per_day=0


first_data$Blood_Pressure_Abnormality <- as.factor(first_data$Blood_Pressure_Abnormality)
first_data$Sex <- as.factor(first_data$Sex)
first_data$Smoking <- as.factor(first_data$Smoking)
first_data$Level_of_Stress <- as.factor(first_data$Level_of_Stress)
first_data$Chronic_kidney_disease <- as.factor(first_data$Chronic_kidney_disease)
first_data$Adrenal_and_thyroid_disorders <- as.factor(first_data$Adrenal_and_thyroid_disorders)
first_data$Pregnancy <- as.factor(first_data$Pregnancy)

str(first_data)

with(first_data, plot(Age, Level_of_Hemoglobin))

library(ggplot2)
qplot(x=Age, y=Level_of_Hemoglobin, data=first_data)



str(second_data)

second_data$Gender <- as.factor(second_data$Gender)
second_data$Treatment <- as.factor(second_data$Treatment)

str(second_data)
sum(is.na(first_data))
missmap(first_data)

first_data.cor <- cor(first_data[,c(3,4,5,9,10,11)])
corrplot(first_data.cor, method="circle")



sample10 <- c()

#take 1,000 random samples of size n=30

n = 1000
for (i in 1:n){
  sample10[i] = mean(sample(first_data$BMI, 10, replace=TRUE))
}
mean(sample10)


sd(sample10)

hist(sample10, col ='steelblue', xlab='BMI', main='Sample size = 10')




sample50 <- c()

#take 1,000 random samples of size n=30

for (i in 1:n){
  sample50[i] = mean(sample(first_data$BMI, 50, replace=TRUE))
}
mean(sample50)


sd(sample50)

hist(sample50, col ='steelblue', xlab='BMI', main='Sample size = 50')




level_of_hemoglobin_mean <- mean(first_data$Level_of_Hemoglobin)
level_of_hemoglobin_sd <- sd(first_data$Level_of_Hemoglobin)
level_of_hemoglobin_se <- level_of_hemoglobin_sd/sqrt(length(first_data))
t.score <-  qt(p=0.05, df=1999,lower.tail=F)

error <- t.score * level_of_hemoglobin_se
lower.bound <- level_of_hemoglobin_mean  - error
upper.bound <- level_of_hemoglobin_mean  + error
print(c(lower.bound,upper.bound))

BMI_mean <- mean(first_data$BMI)
BMI_sd <- sd(first_data$BMI)
BMI_se <- BMI_sd/sqrt(length(first_data))
t.score <-  qt(p=0.05, df=1999,lower.tail=F)

error <- t.score * BMI_se
lower.bound <- BMI_mean  - error
upper.bound <- BMI_mean  + error
print(c(lower.bound,upper.bound))


#MLP 


first_data_for_mlp <- first_data[,c(3,4,5,9,10,11)]
mlp <- lm(Level_of_Hemoglobin~. ,data = first_data_for_mlp)
summary(mlp)
plot(mlp)

mlp$coefficients
to_be_predicted_mlp<- data.frame(44,26,27456,30213,400)
colnames(to_be_predicted_mlp) <- c("Age","BMI","Physical_activity","salt_content_in_the_diet","alcohol_consumption_per_day")
predicted_value_mlp <- predict(mlp,newdata = as.data.frame(to_be_predicted))

reduce_mlp <- step(mlp)
summary(reduce_mlp)


to_be_predicted_reduced_mlp <- data.frame(64,36)
colnames(to_be_predicted_reduced_mlp) <- c("Age","BMI")
predicted_valuereduce_mlp <- predict(reduce_mlp,newdata = as.data.frame(to_be_predicted))

reduce_mlp$coefficients

first_data$LOH_belov_average <- 0
for(i in 1:length(first_data)){
  if(first_data[i,]$Level_of_Hemoglobin<mean(first_data$Level_of_Hemoglobin)){
    first_data[i,]$LOH_belov_average = 1
  }
}

fisher_table <- with(first_data , table(LOH_belov_average,Smoking))

fisher_exact_test <- fisher.test(fisher_table)

hist(first_data$Level_of_Hemoglobin , )
t.test(Level_of_Hemoglobin~Smoking,data=first_data)

#########################################################
not_Smoking <- data.frame(first_data[which(first_data$Smoking=='0'), ]$Level_of_Hemoglobin)
colnames(not_Smoking) <- "Level_of_Hemoglobin of Not Smoking Patients"
smoking <- data.frame(first_data[which(first_data$Smoking=='1'), ]$Level_of_Hemoglobin)
colnames(smoking) <- "Level_of_Hemoglobin of Smoking Patients"

sd(not_Smoking$`Level_of_Hemoglobin of Not Smoking Patients`)
sd(smoking$`Level_of_Hemoglobin of Smoking Patients`)

var.test(not_Smoking$`Level_of_Hemoglobin of Not Smoking Patients`, smoking$`Level_of_Hemoglobin of Smoking Patients`)

shapiro.test(log(not_Smoking$`Level_of_Hemoglobin of Not Smoking Patients`))
shapiro.test(log(smoking$`Level_of_Hemoglobin of Smoking Patients`))

wilcox.test(Level_of_Hemoglobin~Smoking,data=first_data)
#########################################################
#ANOVA-TUKEY

first_data_for_anova <- first_data[,c(11,12)]

bartlett.test(alcohol_consumption_per_day~Level_of_Stress,data=first_data_for_anova)

anova_model <- aov(alcohol_consumption_per_day~Level_of_Stress,data=first_data_for_anova)

TukeyHSD(anova_model)
