setwd("C:/Users/samom/Desktop/Year 2/Semester 2/MS4034 Data Analysis/R Project")

dat <- read.csv(file="MS4034data.csv", header = TRUE)
set.seed(19236719)
sampledata <- sample(1:nrow(dat), 320)
mydata <- dat[sampledata, ]
mydata <- subset(mydata, select = -X)

library(tidyverse)
library(epiDisplay)

Z_0.975 <- qnorm(0.975)
n <- dim(mydata)[1]

#Summary statistics for pregnant
summary(mydata$pregnant)

mu_pregnant <- mean(mydata$pregnant)
sd_pregnant <- sd(mydata$pregnant)
#CI_L refers to the lower end of the confidence interval. CI_U refers to the upper
CI_L_pregnant <- mu_pregnant - Z_0.975*sd_pregnant/sqrt(n)
CI_U_pregnant <- mu_pregnant + Z_0.975*sd_pregnant/sqrt(n)
c(CI_L_pregnant, CI_U_pregnant)

mydata %>%
  count(pregnant) %>%
  mutate(prop = prop.table(n)) %>%
  ggplot(aes(x=pregnant, y=prop)) + geom_bar(stat="identity") +
  xlab("Number of times pregnant") + ylab("Proportion of women") +
  ggtitle("Pregnancy in the Sample")

ggplot(mydata, aes(y=pregnant)) + geom_boxplot() +
  ggtitle("Pregnancy in the Sample")

#glucose
summary(mydata$glucose)

mu_glucose <- mean(mydata$glucose)
sd_glucose <- sd(mydata$glucose)
CI_L_glucose <- mu_glucose - Z_0.975*sd_glucose/sqrt(n)
CI_U_glucose <- mu_glucose + Z_0.975*sd_glucose/sqrt(n)
c(CI_L_glucose, CI_U_glucose)

ggplot(mydata, aes(x=glucose)) + geom_area(stat = "bin", binwidth = 10) +
xlab("Plasma Glucose Concentration (glucose tolerance test)") + ylab("Number of people")+
  ggtitle("Plasma Glucose Concentration")

ggplot(mydata, aes(y=glucose)) + geom_boxplot()+
  ggtitle("Plasma Glucose Concentration")


#pressure
summary(mydata$pressure)

mu_press <- mean(mydata$pressure)
sd_press <- sd(mydata$pressure)
CI_L_pressure <- mu_press - Z_0.975*sd_press/sqrt(n)
CI_U_pressure <- mu_press + Z_0.975*sd_press/sqrt(n)
c(CI_L_pressure, CI_U_pressure)

ggplot(mydata, aes(x=pressure)) + geom_area(stat = "bin", binwidth = 6) +
  xlab("Diastolic Blood Pressure (mm Hg)") + ylab("Number of people") +
  ggtitle("Blood Pressure")

ggplot(mydata, aes(y=pressure)) + geom_boxplot() +
  ggtitle("Blood Pressure")


#triceps
summary(mydata$triceps)

mu_triceps <- mean(mydata$triceps )
sd_triceps <- sd(mydata$triceps )
CI_L_triceps  <- mu_triceps - Z_0.975*sd_triceps/sqrt(n)
CI_U_triceps  <- mu_triceps + Z_0.975*sd_triceps/sqrt(n)
c(CI_L_triceps , CI_U_triceps )

ggplot(mydata, aes(x=triceps )) + geom_area(stat = "bin", binwidth = 5) +
  xlab("Triceps skin fold thickness (mm)") + ylab("Number of people") +
  ggtitle("Triceps skin fold thickness")


ggplot(mydata, aes(y=triceps)) + geom_boxplot()+
  ggtitle("Triceps skin fold thickness")

#insulin
summary(mydata$insulin)

mu_insulin <- mean(mydata$insulin )
sd_insulin <- sd(mydata$insulin )
CI_L_insulin  <- mu_insulin - Z_0.975*sd_insulin/sqrt(n)
CI_U_insulin  <- mu_insulin + Z_0.975*sd_insulin/sqrt(n)
c(CI_L_insulin , CI_U_insulin )

ggplot(mydata, aes(x=insulin )) + geom_area(stat = "bin", binwidth = 20) +
  xlab("2-Hour serum insulin (mu U/ml)") + ylab("Number of people")+
  ggtitle("Insulin")

ggplot(mydata, aes(y=insulin)) + geom_boxplot()+
  ggtitle("Insulin")

#mass
summary(mydata$mass)

mu_mass <- mean(mydata$mass )
sd_mass <- sd(mydata$mass )
CI_L_mass  <- mu_mass - Z_0.975*sd_mass/sqrt(n)
CI_U_mass  <- mu_mass + Z_0.975*sd_mass/sqrt(n)
c(CI_L_mass , CI_U_mass )

ggplot(mydata, aes(x=mass )) + geom_area(stat = "bin", binwidth = 3) +
  xlab("Body mass index (weight in kg/height in m^2)") + ylab("Number of people") +
  ggtitle("BMI")

ggplot(mydata, aes(y=mass)) + geom_boxplot() +
  ggtitle("BMI")

#age
summary(mydata$age)

mu_age <- mean(mydata$age )
sd_age <- sd(mydata$age )
CI_L_age  <- mu_age - Z_0.975*sd_age/sqrt(n)
CI_U_age  <- mu_age + Z_0.975*sd_age/sqrt(n)
c(CI_L_age , CI_U_age )

ggplot(mydata, aes(x=age)) + geom_bar(width = 0.8) +
  xlab("Age (years)") + ylab("Number of people") +
  coord_cartesian(xlim=c(20, 85)) +
  scale_x_continuous(breaks = c(0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80))+
  ggtitle("Ages of people sampled")

ggplot(mydata, aes(y=age)) + geom_boxplot()+
  ggtitle("Ages of people sampled")

#diabetes
tab1(mydata$diabetes, sort.group = "alphabetic")

mydata %>%
  count(diabetes) %>%
  mutate(prop = prop.table(n)) %>%
  ggplot(aes(x=diabetes, y=prop)) + geom_bar(stat="identity") +
  xlab("Result of Test for Diabetes") + ylab("Proportion of sample") +
  ggtitle("Diabetes")

#Risk
summary(mydata$Risk)

mu_risk <- mean(mydata$Risk)
sd_risk <- sd(mydata$Risk )
CI_L_risk  <- mu_risk - Z_0.975*sd_risk/sqrt(n)
CI_U_risk  <- mu_risk + Z_0.975*sd_risk/sqrt(n)
c(CI_L_risk , CI_U_risk )

ggplot(mydata, aes(x=Risk)) + geom_area(stat = "bin", binwidth = 20) +
  xlab("Risk Score") + ylab("Number of people") +
  ggtitle("Risk")

ggplot(mydata, aes(y=Risk)) + geom_boxplot()+
  ggtitle("Risk")

#part (b)
#boxplots for glucose, pressure, insulin in diabetics and nondiabetics
ggplot(mydata, aes(x=diabetes, y=glucose)) + geom_boxplot() +
  ggtitle("Glucose in Diabetics versus Non-Diabetics")
ggplot(mydata, aes(x=diabetes, y=pressure)) + geom_boxplot() +
  ggtitle("Blood Pressure in Diabetics versus Non-Diabetics")
ggplot(mydata, aes(x=diabetes, y=insulin)) + geom_boxplot() +
  ggtitle("Insulin measured in Diabetics versus Non-Diabetics")

diabeticData <- filter(mydata, diabetes == "pos")
nondiabeticData <- filter(mydata, diabetes == "neg")

#summary statistics
mean(diabeticData$glucose)
mean(diabeticData$pressure)
mean(diabeticData$insulin)
sd(diabeticData$glucose)
sd(diabeticData$pressure)
sd(diabeticData$insulin)
summary(diabeticData$glucose)
summary(diabeticData$pressure)
summary(diabeticData$insulin)

mean(nondiabeticData$glucose)
mean(nondiabeticData$pressure)
mean(nondiabeticData$insulin)
sd(nondiabeticData$glucose)
sd(nondiabeticData$pressure)
sd(nondiabeticData$insulin)
summary(nondiabeticData$glucose)
summary(nondiabeticData$pressure)
summary(nondiabeticData$insulin)


shapiro.test(diabeticData$pressure)
shapiro.test(nondiabeticData$pressure)
library(car)
leveneTest(pressure ~ diabetes, mydata, center = mean)

#graphs for above variables in diabetics and nondiabetics
#glucose
ggplot(mydata, aes(x=glucose)) + geom_area(stat = "bin", binwidth = 5, fill = "skyblue") +
  facet_grid(rows = vars(diabetes)) +
  xlab("Plasma Glucose Concentration") + ylab("Number of people")

#pressure
ggplot(mydata, aes(x=pressure)) + geom_area(stat = "bin", binwidth = 4, fill = "skyblue") +
  xlab("Diastolic blood pressure") + ylab("Number of people") +
  facet_grid(rows = vars(diabetes))

#insulin
ggplot(mydata, aes(x=insulin)) + geom_area(stat = "bin", binwidth = 25, fill = "skyblue") +
  xlab("Insulin") + ylab("Number of people") +
  facet_grid(rows = vars(diabetes))

leveneTest(insulin ~ diabetes, mydata, center = median)


#4c

shapiro.test(diabeticData$glucose)
shapiro.test(nondiabeticData$glucose)
#based on tests, data not normal in either group -> mann-Whitney U Test


leveneTest(glucose ~ diabetes, mydata, center = median)
# p = 0.01389, reject the null hypothesis. The variances are not equal.

wilcox.test(diabeticData$glucose, nondiabeticData$glucose, paired = FALSE, var.equal = FALSE,conf.int = TRUE, alternative = "two.sided")
# p < 0.05, reject null hypothesis, the medians are different

#Question required that I test for a difference in means even though data not normal
t.test(mydata$glucose ~ mydata$diabetes,alternative = "two.sided", var.equal = FALSE )
#p-value < 0.05, reject null hypothesis, means not equal

ggplot(mydata, aes(x=glucose, fill=diabetes)) +
  geom_density(alpha=.25)


#4d
#recoding diabetes variable
mydata$diabCode[mydata$diabetes=="pos"] <- "1"
mydata$diabCode[mydata$diabetes=="neg"] <- "0"
mydata$diabCode <- as.numeric(mydata$diabCode)

#correlation matrices
round(cor(subset(mydata, select = -diabetes), method = "pearson"),3)
round(cor(subset(mydata, select = -diabetes), method = "spearman"),3)

pairs(subset(mydata, select = -diabetes))
pairs(subset(mydata, select = c(pressure, mass, diabCode, Risk)))


#start of modelling
model1 <- lm(Risk ~ pregnant + glucose + pressure + triceps + insulin + mass + age + diabCode, data = mydata)
summary(model1)
plot(model1)
hist(model1$residuals)
Anova(model1)


myhist <- hist(model1$residuals)
multiplier <- myhist$counts / myhist$density
mydensity <- density(model1$residuals)
mydensity$y <- mydensity$y * multiplier[1]
plot(myhist)
lines(mydensity)

#pressure and mass seem to have the most effect, with diabetes and triceps close behind
#pregnancy seems to have little effect. Age and insulin only have slight effects
#the residuals seem slightly positively skewed -> try a transformation

model2 <- lm(Risk ~ I(pregnant^0.7) + glucose + I(pressure^2.5) + triceps + insulin + I(mass^1.8) + age + diabCode, data = mydata)
summary(model2)
plot(model2)

myhist <- hist(model2$residuals)
multiplier <- myhist$counts / myhist$density
mydensity <- density(model2$residuals)
mydensity$y <- mydensity$y * multiplier[1]
plot(myhist)
lines(mydensity)


#model 3. Removing variables that seem insignificant and removing outliers using IQR of Risk 
mydata.noOutliers <- mydata[mydata$Risk < median(mydata$Risk) + 1.5*IQR(mydata$Risk) & 
                   mydata$Risk > median(mydata$Risk) - 1.5*IQR(mydata$Risk), ]

model3 <- lm(Risk ~ I(pressure^2) + I(mass^1.5) + diabCode, data = mydata.noOutliers)
summary(model3)
plot(model3)

shapiro.test(model3$residuals)

myhist <- hist(model3$residuals)
multiplier <- myhist$counts / myhist$density
mydensity <- density(model3$residuals)
mydensity$y <- mydensity$y * multiplier[1]
plot(myhist)
lines(mydensity)
