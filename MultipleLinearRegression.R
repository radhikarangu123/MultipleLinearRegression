#########################1st example#############################
#Prepare a prediction model for profit of 50_startups data.
startups<-read.csv(file.choose())
View(startups)
summary(startups)
attach(startups)
pairs(startups)


model1<-lm(Profit~.,data = startups[,-4])
summary(model1)

#adm & mspend are insign

# Multicollinearity check
# Model based on only R.D Spend 
model1.RD<-lm(Profit~R.D.Spend,data=startups)
summary(model1.RD) # R.DSpend became significant

#p-value: < 2.2e-16
#Multiple R-squared:  0.9465,	Adjusted R-squared:  0.9454
cor(startups[,-4])

# Model based on only  administration
model1.admin<-lm(Profit~Administration,data = startups)
summary(model1.admin)# administration became insignificant
#p-value: 0.1622 > 0.05
#Multiple R-squared:  0.04029,	Adjusted R-squared:  0.02029 
plot(Administration)

#model based on only Marketing.Spend
model1.marketing<-lm(Profit~.,data = startups[,-c(2,4)])
summary(model1.marketing)

#Multiple R-squared:  0.9505,	Adjusted R-squared:  0.9483 


installed.packages("car")
library(car)
vif(model1.marketing)
#vif(model1.marketing)
#R.D.Spend Marketing.Spend 
#2.103206        2.103206 

#conclusion there is no colliniarity problem with this model.
# no need to go for trasformations beacuse Multiple R-squared:  0.9505>0.85

#################2nd example################################################

#Predict Price of the computer
computerdata<-read.csv(file.choose())
View(computerdata)
library(car)
library(VIF)
summary(computerdata)
colnames(computerdata)
attach(computerdata)
str(computerdata)

cd_new<-ifelse(cd == "no",0,1)
multi_new<-ifelse(multi == "no",0,1)
premium_new<-ifelse(premium == "no",0,1)
computerdata_new <-cbind.data.frame(X,price,speed,hd,ram,screen,cd_new,multi_new,premium_new,ads,trend)
View(computerdata_new)


#Scatter plot to find the co-relation b/w  variables 
windows()
pairs(computerdata_new)

# Correlation Coefficient matrix - Strength & Direction of Correlation
cor(computerdata_new)

### Partial Correlation matrix - Pure Correlation  b/n the varibles
library(corpcor)
cor2pcor(cor(computerdata_new))

# The Linear Model of interest with all the columns
model.comp_data1 <- lm(price~.,data=computerdata_new)
summary(model.comp_data1)
#Residual standard error: 274 on 6248 degrees of freedom
#Multiple R-squared:  0.7778,	Adjusted R-squared:  0.7775 
#F-statistic:  2187 on 10 and 6248 DF,  p-value: < 2.2e-16

#Vif and AVplot
vif(model.comp_data1)
windows()
avPlots(model.comp_data1,id.n=2,id.cex=0.7)

#Model 2
model.comp_data2 <- lm(price ~ speed + hd + ram + screen + ads + trend + cd_new + multi_new + premium_new, data = computerdata_new)
summary(model.comp_data2)
#Residual standard error: 275.3 on 6249 degrees of freedom
#Multiple R-squared:  0.7756,	Adjusted R-squared:  0.7752 
#F-statistic:  2399 on 9 and 6249 DF,  p-value: < 2.2e-16

#VIF and AVPlot
vif(model.comp_data2)
windows()
avPlots(model.comp_data2,id.n=2,id.cex=0.7)



#VIF and AVPlot
#plot(model.comp_data3)
vif(model.comp_data2)
windows()
avPlots(model.comp_data2,id.n=2,id.cex=0.7)

#conclusion:model.comp_data2
#Multiple R-squared:  0.7756,	Adjusted R-squared:  0.7752 



######################3rd example##############################

#Consider only the below columns and prepare a prediction model for predicting Price.

#Corolla<-Corolla[c("Price","Age_08_04","KM","HP","cc","Doors","Gears","Quarterly_Tax","Weight")]

Corolla <-corolla[c("Price","Age_08_04","KM","HP","cc","Doors","Gears","Quarterly_Tax","Weight")]
View(Corolla)
attach(corolla)
corolla_model1<-lm(Price~.,data=Corolla)
summary(corolla_model1)
#Residual standard error: 1342 on 1427 degrees of freedom
#Multiple R-squared:  0.8638,	Adjusted R-squared:  0.863 
#F-statistic:  1131 on 8 and 1427 DF,  p-value: < 2.2e-16
library(corpcor)
cor2pcor(cor(Corolla))
# Multicollinearity check
# Model based on only cc 
model1_cc<-lm(Price~cc)
summary(model1_cc) 
#Residual standard error: 3599 on 1434 degrees of freedom
#Multiple R-squared:  0.01597,	Adjusted R-squared:  0.01529 
model2_doors<-lm(Price~cc+Doors)
summary(model2_doors)
#Residual standard error: 3543 on 1433 degrees of freedom
#Multiple R-squared:  0.04688,	Adjusted R-squared:  0.04555 
#F-statistic: 35.24 on 2 and 1433 DF,  p-value: 1.15e-15
install.packages("pysch")
library(pysch)
pairs.panels(Corolla)
influence.measures(model2_doors)
install.packages("cars")
library(cars)
influencePlot(model2_doors,id.n=5)
model33<-lm(Price~.,data=Corolla[-81,])
summary(model33)
modelfinal <- lm(Price~.,data=Corolla[,-5])
summary(modelfinal)
vif(model33)
#cc and doors are correlated with each other however insignificant when compared with o/p variable. so delete both the column
modelfinall <- lm(Price~Age_08_04+KM+HP+Gears+Quarterly_Tax+Weight,data=Corolla)
summary(modelfinall)
#Residual standard error: 1342 on 1429 degrees of freedom
#Multiple R-squared:  0.8636,	Adjusted R-squared:  0.863 
#F-statistic:  1508 on 6 and 1429 DF,  p-value: < 2.2e-16

