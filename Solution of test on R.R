# TEST
# Task 1
data(mtcars) # Loading data
head(mtcars, 6) # Print the first 6 rows

# Checking object type
typeof(mtcars) #type of the object from the point of view of R
class(mtcars) #type of the object from the point of view of OOP
mode(mtcars) #type of the object from the point of view of Becker, Chambers & Wilks (1988)

# mtcars is already dataframe, but the way making it a dataframe is shown below 
datainit <- data.frame(mtcars)
 
# Cheking missing values
is.na(datainit) 
# As all are false in the output, there are no missing values, otherwise we would use
# na.omit() function, which returns the object with listwise deletion of missing values.
# I would use Amelia II, Mice, and mitools for advanced data manipulation concerning missing values.

# Task 2 
# Selecting only numeric columns
datanum <- datainit[sapply(datainit,is.numeric)]
head(datanum,5)

# Task 3
#mean for columns and rows respectively
apply(datanum,2,mean)
apply(datanum,1,mean)

#median for columns and rows respectively
apply(datanum,2,median)
apply(datanum,1,median)

#standard deviation for columns and rows respectively
apply(datanum,2,sd)
apply(datanum,1,sd)

# Task 4
# boxploting all the variables
boxplot(datanum)

# whether there are outliers or not depends on how we define the outliers.
# If we define them as Q1 - 1.5*IQR, Q3 + 1.5*IQR, then graph on the right shows
# that there is an outlier for the variable "hp" (for others cannot tell before calculations).
# Moreover, there can be local outliers as well, which we cannot discover just looking 
#at graph or using the definition given above.

# Task 5
library(dplyr)
datanew <- filter_all(datanum,all_vars((quantile((.),0.25)-1.5*IQR((.)))<(.) & (quantile((.),0.75)+1.5*IQR((.)))>(.)))
datanew

# Task 6
rowdiff <- nrow(datanum)-nrow(datanew)
rowdiff

# Task 7
medmean <- function(x) all(mean(x)<median(x))
datamhm <select_if(datanew, medmean)
datamhm
# Task 8
# Creating correlation matrix, rounding up resulting values
# to 2 decimal points and drawing a plot based on it.
cormat <- round(cor(datanum),2)
heatmap(x = cormat)

# Task 9
# Creating a table that will report standard deviation of mpg per value of am
# rounded up to 2 decimal points.
round(aggregate(datanum$mpg, by=list(datanum$am), FUN=sd),2)

# Task 10
# Normality tests
library(nortest)
# Creating Group 1 and Group 0
dt1 <- filter(datanum, am==1)
dt0 <- filter(datanum, am==0)

# Pearson's chi-squared normality test
pear1 <- pearson.test(dt1$mpg)
pear0 <- pearson.test(dt0$mpg)

# p-values rounded to 3 decimal points
round(pear1$p.value,3)
round(pear0$p.value,3)

## Shapiro-Wilk's normality test
shap1 <- shapiro.test(dt1$mpg) 
shap0 <- shapiro.test(dt0$mpg)

# p-values rounded to 3 decimal points
round(shap1$p.value,3)
round(shap0$p.value,3)

# As for the both groups p-values of Pearson and Shapiro-Wilk's normality test are greater >0.05, 
# the groups don't have normal distribution. One of the test would be enough too and there are many other normality tests.
# Our conclusion of non-normality of both groups is substantiated by the Q-Q plots presented belwo too.
qqnorm(dt1$mpg)
qqline(dt1$mpg, col = 2)
qqnorm(dt0$mpg)
qqline(dt0$mpg, col = 2)

# Task 11
# Levene Test for Equality of Variances
library(Rcmdr)

y <- c(dt0$mpg, dt1$mpg)
group <- as.factor(c(rep(1, length(dt0$mpg)), rep(2, length(dt1$mpg))))

leveneTest(y, group)
qf(.95,1,30)

# We are testing the hypothesis that the group variances are equal. 
# We fail to reject the null hypothesis at the 0.05 significance level 
# since the value of the Levene test statistic(4.17) is less than the critical value F (4.18). 
# We conclude that there is insufficient evidence to claim that the variances are not equal.
# Thus, there is no need to stabilize the data.

# Task 12
# two-sample t-test and Wilcox test
t.test(dt0$mpg, dt1$mpg)
wilcox.test(dt0$mpg, dt1$mpg, exact=FALSE)
# Due to non-normality of our groups, besides t-test, 
# we perform Wilcox test to ensure our results are not being biased 
# due to assumption violations (the assumption of no statistically significant
# variances between groups is statisfied).

# Task 13
# Using linear model to explain variance in mpg with the rest of available variables.
library(knitr)
library(printr)
head(mtcars,6)
modela <- lm(mpg ~ cyl+disp+hp+drat+wt+qsec+factor(vs)+factor(am)+gear+carb, data = mtcars)
# as vc and am variables are categorical, we use factor() function to make the lm model 
# to interact with them in a corresponding way.
summary(modela)
anova(modela)
# The initial model shows that 80.66% (Adjusted R-squared) of the mpg variance is explained by the independent variables.
# According to Anova table only three variables have statistically significant 
#effect on mpg (p<0.05, variables are cyl, disp, wt).

# In case of multivariate linear regression we must pay attention to multicollinearity.
# We use variance inflation factor (VIF) to check it.
kable(vif(modela),align = 'c')
# As results show, we have variables with values greater than 10, which is sign of multicollinearity.
# Thus, we must keep only one of the variables with greater than 10 VIF values.
#We will take the necessary steps in the next task to improve the model.

# Task 14
# Improving the model
# To improve the model we can use stepwise selection method, add interaction and squared terms,
# eliminate outliers.

#stepwise selection method
library(MASS)
modelb <- lm(mpg ~ cyl+disp+hp+drat+wt+qsec+factor(vs)+factor(am)+gear+carb, data = mtcars)
modelc <- stepAIC(modelb, direction="both", trace=FALSE)
summary (modelc)
coefficients(modelc)
anova(modelc)
# The results of new model are better. Adj. R-squared is 83.36% (we have an increase of variance explained).
# Moreover, the number of variables are reduced to three: wt, qsec, am.

# Fitting the model to the above three variables.
modeld <- lm(mpg ~ wt+qsec+factor(am), data = mtcars)
summary(modeld)
coefficients(modeld)
anova(modeld)

# Checking the multicollinearity for new model and see that there are no signs of it.
kable(vif(modeld),align = 'c')

# Checking normality of residuals
qqPlot(modeld, main="Normal Q-Q plot")
# The plot shows that errors' distribution is close to normal, which is good.

# Eliminating rows containing outliers.
# We created datafrane "datanew" in Task5, which eliminated outliers. We will use it here. We will fit the model to final three variables from the model above.
modele <- lm(mpg ~ cwt+qsec+factor(am), data = datanew)
summary(modele)
# As results show, Adjusted R-squared for new model is 76.82%, which is worse that the previous model's result.
# This can be conditioned by the fact that we have small number of observations (32) and eliminating outliers
# decreases the number of the observations to 26, increasing the variance.
# Thus, elimination of outliers was not useful.

# Adding interaction and squared terms
# To add interaction terms can be done by the example provided below or adding variables (cyl+ hp + cyl:hp or cyl*hp)
#modelf <- lm(mpg ~ (cyl+disp+hp+drat)^2+wt+qsec+gear+carb+factor(vs)+factor(am), data = mtcars)
# To add square terms we use function poly(x,2), for example poly (cyl,2)
# As adding interaction and squared terms, did not provide any improvement, the results are not presented below.
#modelg <- stepAIC(modelb, direction="both", trace=FALSE)
#summary (modelf)
#kable(vif(modelf),align = 'c')
#coefficients(modelg)
#anova(modelg)

#Thus, our best model is modeld with adjusted R-squared of 83.36%:
modeld <- lm(mpg ~ wt+qsec+factor(am), data = mtcars)
summary(modeld)
coefficients(modeld)
anova(modeld)

# Task 15
# Creating function
# Function takes vector of variables of having any possible length and outputs variables' name, coefficient, 
# and whether it is statistically significant or not.

myfunction <- function(datalist){
  forma <- eval(paste("mpg ~",paste(paste(datalist[c(seq(length.out=length(datalist)))], sep=''), collapse=" + ")))
  fitfunc <- lm(forma, data=mtcars)
  outfunc <- summary(fitfunc)[4]
  return(outfunc)
}

# Calling function
data1 <- c("carb", "am", "wt","vs","gear")
print(myfunction(data1))

