library(tidyverse)
library(MASS)
library(ISLR)

#Reproducing the table in ISRS 5.29 from the bodydata 

bodydata <- read.table('D:/coursework/week3/body.dat.txt', header = TRUE,  sep = '	',  stringsAsFactors = FALSE)
view(bodydata)

bodydata <-  bodydata |> rename("Height" = X174.0, "Weight" = X65.6)

ggplot(bodydata, aes(x = Height, y = Weight)) + geom_point(color = "lightblue")

lm.fit <- lm(Weight ~ Height, data = bodydata)

summary(lm.fit)


#-------------------------------------------------------------------------

# Lab 3.6.3 

library(MASS)
library(ISLR)

view(Boston)

lm.fit <- lm(medv ~ lstat + age, data = Boston) #lm(y ∼ x1 + x2 + x3)
summary(lm.fit)

lm.fit <- lm(medv ~ ., data = Boston)
summary(lm.fit)
str(Boston)
install.packages('car')
library(car)

vif(lm.fit) # vif for all the variables

lm.fit1 <- lm(medv ~ . - age, data = Boston) # for all the variables except the age

vif(lm.fit1)
summary(lm.fit1)


lm.fit1 <- update(lm.fit, ~ . - age) # Another way to do the model using every variable except the age using a model you did before with all the variables 

#-------------------------------------------------------------------------


# Lab 3.6.4 

#The syntax lstat:age tells R to include an interaction term between lstat and age.

# lstat * age simultaneously includes lstat, age, and the interaction term lstat×age as predictors

summary(lm(medv ~ lstat * age, data = Boston))

#-------------------------------------------------------------------

# Lab 3.6.5 

lm.fit2 <- lm(medv ~ lstat + I(lstat^2))
summary(lm.fit2)

#anova() function to further quantify the extent to which the quadratic fit is superior to the linear fit

lm.fit <- lm(medv ~ lstat)
anova(lm.fit, lm.fit2) # compares the two models: Linear and quadertic ==> prints the Analysis of Variance Table 

#The null hypothesis is that the two models fit the data equally well, and the alternative hypothesis is that the full model is superior

lm.fit3 <- lm(medv ~ lstat + I(lstat^2) + I(lstat^3)) # I created this out of curiosity to see how the ^3 would do comapring to them  

par(mfrow = c(2, 2))
plot(lm.fit2)
plot(lm.fit)
plot(lm.fit3)


lm.fit5 <- lm(medv ~ poly(lstat , 5))

plot(lm.fit5)
summary(lm.fit5)
summary(lm(medv ~ log(rm), data = Boston))

#-------------------------------------------------------------------

# Lab 3.6.6 

head(Carseats)
view(Carseats)

lm.fit <- lm(Sales ~ . + Income:Advertising + Price:Age, data = Carseats)
summary(lm.fit)
attach(Carseats) # makes the columns names avaliable to use without needing to mention the dataset everytime

contrasts(ShelveLoc) # Shows the coding in R for qualitative variables 


#-------------------------------------------------------------------

# Exercises 6.1

babyweights <- read.table('D:/coursework/week3/babyweights.txt', header = TRUE,  sep = '	',  stringsAsFactors = FALSE)
view(babyweights)

# Question a) 

#Avg weight of the baby = -8.94 * smoke + 123.05

# Question b) 

# Slope (-8.94) ==> means that if the mother is a smoker, there will be a decrease of 8.94 in the baby's weight 

123.05 - 8.94
# the baby weight for a smoker = 114.11
# the baby weight for a non-smoker = 123.05 


# Question c) 
# Yes, because the p-value for smoke is less than 5% ==> then the variable is statistically sig 
# This will lead to the fact that there is a realtionship between smoking and average weight for the baby


#Replication part from actual data

lm.fit1 <- lm(bwt ~ smoke, data = babyweights)
summary(lm.fit1)

#-------------------------------------------------------------------

# Exercises 6.2 

# Question a) 

# Avg weight of the baby = -1.93 * parity  + 120.07 

# Question b) 

# Slope (-1.93) ==> means that if the baby is the first born, there will be a decrease of 1.93 in the average baby's weight


120.07 - 1.93
# the baby weight if the baby is first-born = 118.14
# the baby weight if the baby isn't first-born = 120.07 

# Question c) 
# Yes, because the p-value for smoke is greater than 5% ==> then the variable isn't statistically sig 
# This will lead to the fact that there isn't a clear realtionship between parity and average weight for the baby


#Replication part from actual data

lm.fit2 <- lm(bwt ~ parity, data = babyweights)
summary(lm.fit2)
#-------------------------------------------------------------------


# Exercises 6.3 

# Question a) 

# Avg weight of the baby = 0.44 * gestation - 3.33 * parity - 0.01 * age + 1.15 * height + 0.05 weight - 8.4 * smoke - 80.41 


# Question b) 

# Slope for gestation (0.44) ==> if the length of the pregnancy increased by a day, there will be 0.44 ounces added to the average weight 
# Slope for age (-0.01) ==> if the mother's age increased by 1 year, there will be 0.01 ounces decreament in the avg weight

#Question c)

# This might be explained by the collinearity among the variables. Also, each new variable added to the model would have an effect on the other variables


#Question d)

0.44 * 284 - 3.33 * 0 - 0.01 * 27 + 1.15 * 62 + 0.05 * 100 - 8.4 * 0 - 80.41 
# predicted value :  120.58
# Acutal avg weight : 120 

# The residual = 120 - 120.58 = -0.58 


#Question E)

# R^2 = 1- variance  of residuals / variance of the birth weights 

1- 249.28/332.57

# R^2 = 0.2504435

# Adj R^2 = 1-  variance  of residuals / variance of the birth weights * (n-1) / n-k-1
# n ==> number of obs ==> 1236 
# K ==> number of independent variables (predictors) ==> 6

1- (249.28/332.57 * (1236-1)/ (1236 - 6 -1) )

# Adj R^2 = 0.2467842


#Replication part from actual data

lm.fit3 <- lm(bwt ~ ., data = babyweights)
summary(lm.fit3)


