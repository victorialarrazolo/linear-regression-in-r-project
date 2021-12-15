#Lesson 1 Hands On 
#Install Packages
install.packages("car")
install.packages("caret")
install.packages("gvlma")
install.packages("predictmeans")
install.packages("e1071")
install.packages("lmtest")

#Install Libraries
library("car")
library("caret")
library("gvlma")
library("predictmeans")
library("e1071")
library("lmtest")

#Load in data 
library(readr)
heights <- read_csv("~/Downloads/heights.csv")
View(heights)

# Do our heights change throughout the day due to the pull of gravity?

#Testing for Linearity 
scatter.smooth(x=heights$AM_Height, y=heights$PM_Height, main="Heights in the morning and Heights at night")
#There is a linear relationship between peoples hights at night and in the morning 

#Testing for homoscedasticity
lmHeights = lm(AM_Height~PM_Height, data = heights)

par(mfrow=c(2,2))
plot(lmHeights)

lmtest::bptest(lmHeights)

car::ncvTest(lmHeights)

#The P value was not significant meaning that there is homoscedacity

gvlma(lmHeights)

#Testing for outliers 
CookD(lmHeights, group=NULL, plot = TRUE, idn=3, newwd=TRUE)

lev= hat(model.matrix(lmHeights))
plot(lev)

heights[lev>.2,]

#Testing for outliers in  y space
car::outlierTest(lmHeights)
#Testing for outliers in x and y space 
summary(influence.measures(lmHeights))

heightsNoO <- heights[c(1,2,5,6,7,8,9,10,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,38,39,40,41),]
lmHeightsNoO = lm(PM_Height~AM_Height, data=heightsNoO)

summary(lmHeights)

summary(lmHeightsNoO)



