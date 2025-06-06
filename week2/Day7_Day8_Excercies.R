library(tidyverse)
library(dplyr)
magents <- read.csv("D:/coursework/week2/magnets.csv")
View(magents)


# Chapter 9 Section 1
#Question 1 
avg_change <- magents |> mutate(change_avg = mean(change))
view(avg_change)

#Question 2
str(magents)

# The type of data in active is characters. The answers to the question is that active is factor not numeric.
# It's two different levels either 1 or 2. 

avg_change_for_active =  mean(magents$change[1:29])
avg_change_for_nonactive =  mean(magents$change[30:50])

view(avg_change_for_active)
view(avg_change_for_nonactive)


# Question 4
std_for_active = sd(magents$change[1:29])
std_for_non_active = sd(magents$change[30:50])

print(std_for_active)
print(std_for_non_active)

# Question 5
#box_plot for active: 

boxplot(magents$change[1:29])

boxplot(magents$change[30:50]) #I see there are three outliers


# Chapter 10 Section 1 

#Question 1: 

#Normal (3,2) ==> 3 is the mean & 2 is the variance 

mu <- 3 
variance <- 2
std <- sqrt(2)

x_bar <- rep(0, 10^3)
x_mid <- rep(0, 10^3)

for(i in 1:10^3){
    x <- rnorm(100, mu, std)
    x_bar[i] <- mean(x)
    x_mid[i] <- median(x)
}

x_bar
x_mid

mean(x_bar) # mean of the means
mean(x_mid) # mean of the medians

var(x_bar) #variance for x_bar
var(x_mid) # variance for x_mid 

# The variance of x_bar is going to be around 0.2, but the variance of x_mid is going to be around 0.3 

#Question 2: 

min <- 0.5 
max <- 5 

x_bar <- rep(0, 10^3)
x_mid <- rep(0, 10^3)

for(i in 1:10^3){
    x <- runif(100, min, max)
    x_bar[i] <- mean(x)
    x_mid[i] <- median(x)
}

x_bar
x_mid

mean(x_bar) # mean of the means
mean(x_mid) # mean of the medians

var(x_bar) #variance for x_bar
var(x_mid) # variance for x_mid 


# Chapter 10 Section 2 
library(tidyverse)
population_data<- read.csv("D:/coursework/week2/pop2.csv")
sample_population_data<- read.csv("D:/coursework/week2/ex2.csv")
view(population_data)
view(sample_population_data)


#Question 1 
people_from_sample_with_high_bp = count(sample_population_data, group == "HIGH")
ans <- people_from_sample_with_high_bp / 150
print(ans)

# Another sol: 
mean (sample_population_data$group == "HIGH")


#Question 2:

mean (population_data$group == "HIGH")


#Question 3:

p_hat <- rep(0, 10^3)

for(i in 1: 10^3){
    y <- sample(population_data$group, 150) # we are getting 150 as a sample size to simulate the sample that we have 
    p_hat[i] = (y == "HIGH")   

    view(y)
}
mean(p_hat)

# Question 4

var(p_hat)

# Question 5

p <- mean (population_data$group == "HIGH")

variance <- p * (1-p) / 150
print(variance)

# Chapter 2 Section 2 from Intro to Stat with Randomization and Simulation

# Question a) 

Dead_from_control_group = 30/34

Dead_from_treatment_group = 25/69


# Question b) part 1)

#H0 ==> the null hypothesis is that the treatment is not effective 
#HA ==> the alternative hypothesis is that the treament is effective

# Question b) part 2)

# 28 alive, 75 dead, 69 recieved the treatment, 34 control group, the null hypothesis, the percentage of the alive in the grou


# Question C)

41/99 #==> this suggests that the treatment is effective 





# Chapter 2 Section 6 from Intro to Stat with Randomization and Simulation: 

# Question 1 

#The hypotheses are: 
# H0: Yawing next to someone doesn't affect the person  p_treament = p_control 
# HA: (There is an association between being near a yawner and yawning) p_treament does not equal to p_control 

# Question 2 

p_hat_treatment = 10/34
p_hat_control = 4/16

my_diff = p_hat_treatment - p_hat_control
print(my_diff)
# So our difference is 0.04411765

# Question 3: 

# some notes from graph are: 
#The null distribution is approximately normally distributed around the zero (0.0)

#Our observation is 0.04411765, so I located this on the x-axis 

# The p-value = p(p_hat_treatment - p_hat_control >= 0.044) + p(p_hat_treatment - p_hat_control <= -0.044)
# This is because my hypothesis is double-sided 

#This p-value will be around the zero, and since the largest bars (large proportion) of the distribution lies between -0.1 and +0.1

#let's guess that 4,000 to 6,000 lies in this distribution 

#p-value = (4000 to 6000) / 10,000 = 0.4 to 0.6  

#So, I would say that the p-value would be large than 5% anyway, which would be enough to reject the null hypothesis. 


# Chapter 3 Section 1 from Intro to Stat with Randomization and Simulation

#Question 1:

#The answer is false because of the central limit theorem conditions: n >= 30, np >= 10 & n(1-p) >= 10



#Chapter 9 Section 2 from Introduction to Statistical Thinking

# Question 1 
# the goal we are trying to do replication to test the statistic mentioned in the question 
mu <- 3.5 
sig1 <- 3 
sig2 <- 1.5 
stat_test <- rep(0, 10^3)

for(i in 1:10^3){
    x1 <- rnorm(29,mu, sig1)
    x2 <- rnorm(21, mu, sig2)
    x1_bar <- mean(x1)
    x1_variance <- var(x1)
    x2_bar <- mean(x2)
    x2_variance <- var(x2)
    stat_test[i] <- (x1_bar - x2_bar) / sqrt((x1_variance/29) + (x2_variance/21))
}
quantile(stat_test, c(0.025, 0.975), na.rm = TRUE)

#The answer would be an interval between the two numbers [-2.018366, 2.011073] This is just an example because everytime the code would
# run, the number would be different, depending on the samples that would be generated 


#Question 2 

