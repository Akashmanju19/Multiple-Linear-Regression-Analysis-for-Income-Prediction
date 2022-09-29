
#load the file
Incomecsv = read.csv("C:\\Users\\akash\\Desktop\\CLass Notes\\Stastics\\STATS CA 1\\IncomeData.csv")



#load the data in to data frame
Incomedata = data.frame(Incomecsv)
Incomedata

#correlation plot
install.packages("psych")
library(psych)
attach(Incomedata)
pairs.panels (Incomedata,
              method = "pearson", # correlation method
              hist.col = "#00AFBB",
              density = TRUE,  # show density plots
              ellipses = TRUE) # show correlation ellipse

#correlation plot
install.packages("psych")
library(psych)
attach(Incomedata)
pairs.panels (Incomedata,
              method = "spearman", # correlation method
              hist.col = "#00AFBB",
              density = TRUE,  # show density plots
              ellipses = TRUE) # show correlation ellipse


#conversion to categorical variables 
Incomedata$edcat <- as.factor(Incomedata$edcat)
Incomedata$default <- as.factor(Incomedata$default)
Incomedata$jobsat <- as.factor(Incomedata$jobsat)
Incomedata$homeown <- as.factor(Incomedata$homeown)
str(Incomedata)
Incomedata <- Incomedata[-c(3647,4261), ]

summary(Incomedata)


#check for simple linear regression
simplelin1 <- lm(income~carvalue, data = Incomedata)
simplelin2 <- lm(income~othdebt, data = Incomedata)
simplelin3 <- lm(income~creddebt, data = Incomedata)
simplelin4 <- lm(income~default, data = Incomedata)
simplelin5 <- lm(income~yrsempl, data = Incomedata)
simplelin6 <- lm(income~yrsed, data = Incomedata)





summary(simplelin1)
summary(simplelin2)
summary(simplelin3)
summary(simplelin4)
summary(simplelin5)
summary(simplelin6)

#model building processs 
model102 <- lm(income~carvalue*creddebt*othdebt+
                 carvalue+creddebt+othdebt+default,data=Incomedata)
summary(model102)

model103 <- lm(log(income)~carvalue*creddebt*othdebt+log(carvalue)+creddebt+othdebt+default,data=Incomedata)
summary(model103)

Incomedata <- Incomedata[-c(3647,4261), ]
model103 <- lm(log(income)~carvalue*creddebt*othdebt+log(carvalue)+creddebt+othdebt+default,data=Incomedata)
summary(model103)

model104<-update(model103,~.-carvalue:creddebt:othdebt)
summary (model104)

model105<-update(model104,~.-creddebt:othdebt)
summary (model105)

model106<-update(model105,~.-carvalue:creddebt)
summary (model106)

model107<-update(model106,~.-carvalue)
summary (model107)

model7<-update(model107,~.-othdebt:carvalue)
summary (model7)


#remove outliers
Incomedata <- Incomedata[-c(3258,1691), ]
Incomedata <- Incomedata[-c(1690,2245), ]
#model108<-update(model107,~.-othdebt)
#summary (model108)

library(car)
library(leaps)

BestModels<-regsubsets(income~log(carvalue)+creddebt+othdebt+default,data=Incomedata, nbest = 2, method="exhaustive")
summary(BestModels)
par(mfrow=c(1,1))
plot(BestModels,scale = "adjr2")
plot(BestModels,scale = "bic")


par(mfrow=c(2,2))
plot(model7)

hist(model107$residuals)


library(car)
ncvTest(model7)



#Checking for Independence of Errors
library(car)
durbinWatsonTest(model7)
#vif test
vif(model103)


library(performance)
library(see)
library(patchwork)
check_model(model7)

