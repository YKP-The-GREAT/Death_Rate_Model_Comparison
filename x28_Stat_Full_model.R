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
summary(XB)
describe(XB)
library(pastecs)
stat.desc(XB)
XB.cor = cor(XB)# using Pearson
XB.cor
#install.packages("corrplot")
library(corrplot)

corrplot(XB.cor)

B = XB$V17
A1 <- XB$V2
A2 = XB[,3]
A3 <- XB[,4]
A4 = XB$V5
A5 <- XB$V6
A6 = XB[,7]
A7 <- XB[,8]
A8 = XB$V9
A9 <- XB$V10
A10 = XB[,11]
A11 <- XB[,12]
A12 = XB$V13
A13 <- XB$V14
A14 = XB[,15]
A15 <- XB[,16]

regFull = lm(B ~ A1+A2+A3+A4+A5+A6+A7+A8+A9+A10+A11+A12+A13+A14+A15)
summary(regFull)
anova(regFull)
