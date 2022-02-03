#  x28.txt
#
#  Discussion:
#
#    The death rate is to be represented as a function of other variables.
#
#    There are 60 rows of data.  The data includes:
#
#      I,   the index;
#      A1,  the average annual precipitation;
#      A2,  the average January temperature;
#      A3,  the average July temperature;
#      A4,  the size of the population older than 65;
#      A5,  the number of members per household;
#      A6,  the number of years of schooling for persons over 22;
#      A7,  the number of households with fully equipped kitchens;
#      A8,  the population per square mile; 
#      A9,  the size of the nonwhite population;
#      A10, the number of office workers;
#      A11, the number of families with an income less than $3000;
#      A12, the hydrocarbon pollution index;
#      A13, the nitric oxide pollution index;
#      A14, the sulfur dioxide pollution index;
#      A15, the degree of atmospheric moisture.
#      B,   the death rate.
#
#    We seek a model of the form:
#
#      B =  A1 *  X1 +  A2 *  X2 +  A3 *  X3 +  A4 *  X4 +  A5 *  X5
#        +  A6 *  X6 +  A7 *  X7 +  A8 *  X8 +  A9 *  X9 + A10 * X10
#        + A11 * X11 + A12 * X12 + A13 * X13 + A14 * X14 + A15 * X15
#
#  Modified:
#
#    08 November 2010
#
#  Reference:
#
#    Richard Gunst, Robert Mason,
#    Regression Analysis and Its Applications: a data-oriented approach,
#    Dekker, 1980, pages 370-371.
#    ISBN: 0824769937.
#
#    Gary McDonald, Richard Schwing,
#    Instabilities of regression estimates relating air pollution to mortality,
#    Technometrics,
#    Volume 15, Number 3, pages 463-482, 1973.
#
#    Helmut Spaeth,
#    Mathematical Algorithms for Linear Regression,
#    Academic Press, 1991,
#    ISBN 0-12-656460-4.
#
#17 columns
#60 rows
#Index
#A1 average annual precipitation in inches
#A2 average January temperature in degrees Fahrenheit
#A3 average July temperature in degrees Fahrenheit
#A4 percent of 1960 SMSA population 65 years old or older
#A5 household size, 1960
#A6 schooling for persons over 22
#A7 household with full kitchens
#A8 population per square mile in urbanized areas
#A9 percent nonwhite population
#A10 percent office workers
#A11 poor families (annual income under $3000)
#A12 relative pollution potential of hydrocarbons
#A13 relative pollution potential of oxides of Nitrogen
#A14 relative pollution of Sulfur Dioxide
#A15 percent relative humidity, annual average at 1pm.
#B death rate

XB = read.table('D:/Studies/NJiT/Sem_2/Regression_Analysis/Project/x28.txt')
XB

str(XB) # Compact structure of Data
summary(XB)

library(Hmisc)
describe(XB)

library(pastecs)
stat.desc(XB)
options(digits=3)
StatAn <- pd.DataFrame(stat.desc(XB, basic=F))
StatAn

XB.cor = cor(XB)# using Pearson

XB.cor

library(corrplot)
dev.off()
corrplot(XB.cor)

palette = colorRampPalette(c("cyan", "#3296fa", "#003294")) (20)
heatmap(x = XB.cor, col = palette, symm = TRUE)

pairs(~V1+V2+V3+V4+V5+V6+V7+V8+V9+V10+V11+V12+V13+V14+V15+V16, data = XB)

library(car)
scatterplotMatrix(~ V1+V2+V3+V4+V5+V6+V7+V8+V9+V10+V11+V12+V13+V14+V15+V16, data = XB)

set.seed(125)
library(caTools)

data_split = sample.split(XB, SplitRatio = 0.8)
train <- subset(XB, data_split == TRUE)
test <-subset(XB, data_split == FALSE)
summary(train)

V16 = train$V16
V1 <- train$V1
V2 = train[,2]
V3 <- train[,3]
V4 = train$V4
V5 <- train$V5
V6 = train[,6]
V7 <- train[,7]
V8 = train$V8
V9 <- train$V9
V10 = train[,10]
V11 <- train[,11]
V12 = train$V12
V13 <- train$V13
V14 = train[,14]
V15 <- train[,15]

k=1  # test Variability is larger than 0;  therefore 5th and 6th columns not considered
while (k<16){
  print(var(train[,k]))
  k = k+1
}

Y_og <- test[nrow(test)]   # Original Values
Y_og

Y_calc <- test[nrow(test)]*0 # Initialization For calculated values
Y_calc

regBase = lm(V16 ~ 1)
regBase

#__________(1)_______FULL MODEL: All Predictors___________________________________________


regFull = lm(V16 ~ V1+V2+V3+V4+V7+V8+V9+V10+V11+V12+V13+V14+V15, data = train)
regFull

# VIF factor: No perfect multicollinearity
vif(modeL)

regFull = lm(V16 ~ V1+V2+V3+V7+V8+V10+V14+V15, data = train)
regFull

Y_calcF1 <- Y_calc  # Initialization For calculated values
Y_calc1 <- Y_calcF1  # Initialization For calculated values
modeL <- regFull

summary(modeL)
anova(modeL)
attributes(modeL)
residuals(modeL)
sum(residuals(modeL))
mean(residuals(modeL))
 

# homoscedasticity
par(mfrow=c(length(test),length(test)))
gvlma::gvlma(modeL, alphalevel = 0.05)
dev.off()
plot(modeL)

# TEST DATA
coefficients(modeL)

pairs(~V1+V2+V3+V7+V8+V10+V14+V15, data = train)

library(car)
scatterplotMatrix(~ V1+V2+V3+V7+V8+V10+V14+V15, data = train)

# CODE for getting calculated values of the test model and later comparing them with the Original values

j = 1
while (j<=nrow(test)) {
  i = 1
  Y_calc1[j,1] <- coef(modeL)[i]
  
  while (i<(length(coef(modeL))-1)) {
    Y_calc1[j,1] = Y_calc1[j,1] + test[j,i]*coef(modeL)[i+1]
    i=i+1
  }
  j=j+1
  
}
   #   Checking Accuracy
library(forecast)
Acc_Y_calc1 = accuracy(Y_calc1[,1], Y_og[,1])
CompTab = Y_og
CompTab[,2] = Y_calc1[,1]
CompTab[,3] = Y_og[,1] - Y_calc1[,1]
CompTab[,4] = CompTab[,3]*CompTab[,3]
library(dplyr)
CompTab <- rename(CompTab, Y_og = V16, Y_calcl1 = V2, error = V3, Sq.error = V4)
CompTab
Y_calcF1 <- Y_calc1
Acc_Y_calcF1 <- Acc_Y_calc1
Acc_Y_calcF1

#_________(2)________Reduced Model: p-value/F-test-based backward selection ______________________________________

# F-test-based backward selection using rms::fastbw()

library(rms) # rms: root mean sqaure; ols: ordinary least squares
ols.full <- ols(V16 ~ V1+V2+V3+V4+V7+V8+V9+V10+V11+V12+V13+V14+V15, data = train)
regPval = fastbw(ols.full, rule = "p", sls = 0.5)
regPval

Y_calc2 <- Y_calc  # Initialization For calculated values
Y_calc1 <- Y_calc2  # Initialization For calculated values
modeL = regPval

summary(modeL)
#anova(modeL)
attributes(modeL)
residuals(modeL)
sum(residuals(modeL))
mean(residuals(modeL))

#par(mfrow=c(length(test),length(test)))
#gvlma::gvlma(modeL, alphalevel = 0.05)
#dev.off()
#plot(modeL)

# TEST DATA
coefficients(modeL)
# Graph plotted on the basis of Coefficients
pairs(~V1+V2+V3+V7+V8+V9+V14, data = train)

library(car)
scatterplotMatrix(~ V1+V2+V3+V7+V8+V9+V14, data = train)

# VIF factor: No perfect multicollinearity
vif(modeL)

# CODE for getting calculated values of the test model and later comparing them with the Original values

j = 1
while (j<=nrow(test)) {
  i = 1
  Y_calc1[j,1] <- coef(modeL)[i]
  
  while (i<(length(coef(modeL))-1)) {
    Y_calc1[j,1] = Y_calc1[j,1] + test[j,i]*coef(modeL)[i+1]
    i=i+1
  }
  j=j+1
  
}

library(forecast)
Acc_Y_calc1 = accuracy(Y_calc1[,1], Y_og[,1])
CompTab = Y_og
CompTab[,2] = Y_calc1[,1]
CompTab[,3] = Y_og[,1] - Y_calc1[,1]
CompTab[,4] = CompTab[,3]*CompTab[,3]
library(dplyr)
CompTab <- rename(CompTab, Y_og = V16, Y_calcl1 = V2, error = V3, Sq.error = V4)
CompTab
Y_calc2 <- Y_calc1
Acc_Y_calc2 <- Acc_Y_calc1
Acc_Y_calc2


#_______________(3)_________Reduced Model: AIC both direction________________________________________

regAICboth <- step(regFull, scope = list(upper=regFull, lower=~1), direction = "both",  k = 2, trace = 1)
regAICboth
Y_calc3 <- Y_calc  # Initialization For calculated values
Y_calc1 <- Y_calc3  # Initialization For calculated values
modeL = regAICboth

summary(modeL)
anova(modeL)
attributes(modeL)
residuals(modeL)
sum(residuals(modeL))
mean(residuals(modeL))
par(mfrow=c(length(test),length(test)))
gvlma::gvlma(modeL, alphalevel = 0.05)
dev.off()
plot(modeL)

# TEST DATA
coefficients(modeL)

# Graph plotted on the basis of Coefficients
pairs(~V1+V2+V7+V8+V14, data = train)

library(car)
scatterplotMatrix(~ V1+V2+V7+V8+V14, data = train)

# VIF factor: No perfect multicollinearity
#vif(modeL) 

# CODE for getting calculated values of the test model and later comparing them with the Original values
j = 1
while (j<=nrow(test)) {
  i = 1
  Y_calc1[j,1] <- coef(modeL)[i]
  
  while (i<(length(coef(modeL))-1)) {
    Y_calc1[j,1] = Y_calc1[j,1] + test[j,i]*coef(modeL)[i+1]
    i=i+1
  }
  j=j+1
  
}
library(forecast)
Acc_Y_calc1 = accuracy(Y_calc1[,1], Y_og[,1])
CompTab = Y_og
CompTab[,2] = Y_calc1[,1]
CompTab[,3] = Y_og[,1] - Y_calc1[,1]
CompTab[,4] = CompTab[,3]*CompTab[,3]
library(dplyr)
CompTab <- rename(CompTab, Y_og = V16, Y_calcl1 = V2, error = V3, Sq.error = V4)
CompTab
Y_calc3 <- Y_calc1
Acc_Y_calc3 <- Acc_Y_calc1
Acc_Y_calc3


#_________________(4)_______Reduced Model 4: # AIC Forward ______________________________________

regAICfwd = step(regBase, scope = list(upper=regFull, lower=~1), direction = "forward",  k = 2, trace = 1)
regAICfwd

Y_calc4 <- Y_calc  # Initialization For calculated values
Y_calc1 <- Y_calc4  # Initialization For calculated values
modeL = regAICfwd

summary(modeL)
anova(modeL)
attributes(modeL)
#residuals(modeL)
mean(residuals(modeL))

par(mfrow=c(length(test),length(test)))
gvlma::gvlma(modeL, alphalevel = 0.05)
dev.off()
plot(modeL)

# TEST DATA
coefficients(modeL)

# Graph plotted on the basis of Coefficients
pairs(~V1+V2+V7+V8+V14, data = train)

library(car)
scatterplotMatrix(~ V1+V2+V7+V8+V14, data = train)

# VIF factor: No perfect multicollinearity
#vif(modeL)

# CODE for getting calculated values of the test model and later comparing them with the Original values
j = 1
while (j<=nrow(test)) {
  i = 1
  Y_calc1[j,1] <- coef(modeL)[i]
  
  while (i<(length(coef(modeL))-1)) {
    Y_calc1[j,1] = Y_calc1[j,1] + test[j,i]*coef(modeL)[i+1]
    i=i+1
  }
  j=j+1
  
}

library(forecast)
Acc_Y_calc1 = accuracy(Y_calc1[,1], Y_og[,1])
CompTab = Y_og
CompTab[,2] = Y_calc1[,1]
CompTab[,3] = Y_og[,1] - Y_calc1[,1]
CompTab[,4] = CompTab[,3]*CompTab[,3]
library(dplyr)
CompTab <- rename(CompTab, Y_og = V16, Y_calcl1 = V2, error = V3, Sq.error = V4)
CompTab
Y_calc4 <- Y_calc1
Acc_Y_calc4 <- Acc_Y_calc1
Acc_Y_calc4


#_________________(5)_______Reduced Model 5: # BIC Forward ______________________________________

regBICfwd = step(regBase, scope = list(upper=regFull, lower=~1), direction = "forward",  k = log(length(test)),  trace = 1)
regBICfwd

Y_calc5 <- Y_calc  # Initialization For calculated values
Y_calc1 <- Y_calc5  # Initialization For calculated values
modeL = regBICfwd

summary(modeL)
anova(modeL)
attributes(modeL)
#residuals(modeL)
mean(residuals(modeL))

par(mfrow=c(length(test),length(test)))
gvlma::gvlma(modeL, alphalevel = 0.05)
dev.off()
plot(modeL)

# TEST DATA
coefficients(modeL)

# Graph plotted on the basis of Coefficients
pairs(~V1+V2+V7+V14, data = train)

library(car)
scatterplotMatrix(~ V1+V2+V7+V14, data = train)

# VIF factor: No perfect multicollinearity
#vif(modeL)

# CODE for getting calculated values of the test model and later comparing them with the Original values
j = 1
while (j<=nrow(test)) {
  i = 1
  Y_calc1[j,1] <- coef(modeL)[i]
  
  while (i<(length(coef(modeL))-1)) {
    Y_calc1[j,1] = Y_calc1[j,1] + test[j,i]*coef(modeL)[i+1]
    i=i+1
  }
  j=j+1
  
}

library(forecast)
Acc_Y_calc1 = accuracy(Y_calc1[,1], Y_og[,1])
CompTab = Y_og
CompTab[,2] = Y_calc1[,1]
CompTab[,3] = Y_og[,1] - Y_calc1[,1]
CompTab[,4] = CompTab[,3]*CompTab[,3]
library(dplyr)
CompTab <- rename(CompTab, Y_og = V16, Y_calcl1 = V2, error = V3, Sq.error = V4)
CompTab
Y_calc5 <- Y_calc1
Acc_Y_calc5 <- Acc_Y_calc1
Acc_Y_calc5



#_________________(6)_______Reduced Model 6: # BIC Both ______________________________________

regBICboth = step(regFull, scope = list(upper=regFull, lower=~1), direction = "both",  k = log(length(test)),  trace = 1)
regBICboth

Y_calc6 <- Y_calc  # Initialization For calculated values
Y_calc1 <- Y_calc6  # Initialization For calculated values
modeL = regBICboth

summary(modeL)
anova(modeL)
attributes(modeL)
#residuals(modeL)
mean(residuals(modeL))

par(mfrow=c(length(test),length(test)))
gvlma::gvlma(modeL, alphalevel = 0.05)
dev.off()
plot(modeL)

# TEST DATA
coefficients(modeL)

# Graph plotted on the basis of Coefficients
pairs(~V1+V2+V7+V14, data = train)

library(car)
scatterplotMatrix(~ V1+V2+V7+V14, data = train)

# VIF factor: No perfect multicollinearity
#vif(modeL)

# CODE for getting calculated values of the test model and later comparing them with the Original values
j = 1
while (j<=nrow(test)) {
  i = 1
  Y_calc1[j,1] <- coef(modeL)[i]
  
  while (i<(length(coef(modeL))-1)) {
    Y_calc1[j,1] = Y_calc1[j,1] + test[j,i]*coef(modeL)[i+1]
    i=i+1
  }
  j=j+1
  
}

library(forecast)
Acc_Y_calc1 = accuracy(Y_calc1[,1], Y_og[,1])
CompTab = Y_og
CompTab[,2] = Y_calc1[,1]
CompTab[,3] = Y_og[,1] - Y_calc1[,1]
CompTab[,4] = CompTab[,3]*CompTab[,3]
library(dplyr)
CompTab <- rename(CompTab, Y_og = V16, Y_calcl1 = V2, error = V3, Sq.error = V4)
CompTab
Y_calc6 <- Y_calc1
Acc_Y_calc6 <- Acc_Y_calc1
Acc_Y_calc6


#_________________#_______Comparing all the Models ______________________________________
anova(regFull, regAICboth, regAICfwd, regBICfwd, regBICboth)
AIC(regFull, regAICboth, regAICfwd, regBICfwd, regBICboth)
BIC(regFull, regAICboth, regAICfwd, regBICfwd, regBICboth)
#CompModel<-as.data.frame(matrix(nrow=4,ncol=6))
#colnames(CompModel)<-c("regFull","regAICboth","regAICfwd","regBICfwd","regBICboth")
#
ModelNames <- c("regFull","regAICboth","regAICfwd","regBICfwd","regBICboth","regPval")
ModelAcc <- c(Acc_Y_calc1, Acc_Y_calc3, Acc_Y_calc4, Acc_Y_calc5, Acc_Y_calc6, Acc_Y_calc2)
AdjRsq <- c(summary(regFull)$adj.r.squared, summary(regAICboth)$adj.r.squared, summary(regAICfwd)$adj.r.squared, summary(regBICfwd)$adj.r.squared, summary(regBICboth)$adj.r.squared, NA)
Rsq <- c(summary(regFull)$r.squared, summary(regAICboth)$r.squared, summary(regAICfwd)$r.squared, summary(regBICfwd)$r.squared, summary(regBICboth)$r.squared, NA)
MeanErr <- c(Acc_Y_calc1[,1], Acc_Y_calc3[,1], Acc_Y_calc4[,1], Acc_Y_calc5[,1], Acc_Y_calc6[,1], Acc_Y_calc2[,1])
RootMeanSqErr <- c(Acc_Y_calc1[,2], Acc_Y_calc3[,2], Acc_Y_calc4[,2], Acc_Y_calc5[,2], Acc_Y_calc6[,2], Acc_Y_calc2[,2])
MeanAbsErr <- c(Acc_Y_calc1[,3], Acc_Y_calc3[,3], Acc_Y_calc4[,3], Acc_Y_calc5[,3], Acc_Y_calc6[,3], Acc_Y_calc2[,3])
MeanPercentErr <- c(Acc_Y_calc1[,4], Acc_Y_calc3[,4], Acc_Y_calc4[,4], Acc_Y_calc5[,4], Acc_Y_calc6[,4], Acc_Y_calc2[,4])
MeanAbsPercentErr <- c(Acc_Y_calc1[,5], Acc_Y_calc3[,5], Acc_Y_calc4[,5], Acc_Y_calc5[,5], Acc_Y_calc6[,5], Acc_Y_calc2[,5])
Coeff <- c(coefficients(regFull), coefficients(regAICboth), coefficients(regAICfwd),coefficients(regBICfwd), coefficients(regBICboth))
Coeff
CompareModel <- data.frame(ModelNames, Rsq, AdjRsq, MeanErr, MeanAbsErr, MeanPercentErr, MeanAbsPercentErr)
CompareModel[1:6,]

broom::glance(regFull)
broom::glance(regAICboth)
broom::glance(regAICfwd)
broom::glance(regBICfwd)
broom::glance(regBICboth)
broom::glance(regPval)
