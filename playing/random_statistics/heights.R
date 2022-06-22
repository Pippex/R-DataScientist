library(tidyr)
library(dslabs)
library(purrr)
library(dplyr)
library(NHANES)


data("heights")

heights <- tibble(heights)

heights <- group_by(heights, sex)

resumen <- heights %>%
          summarize(std = sd(height), mean = mean(height))

resumen

###I was seeking for a large variation in means measured by standard deviation,
###but there isn't
###I don't know a lot about statistics yet, so I don't know what to do with this xD
###Future employers, I promise that I was studying a lot about English
###When I'm going looking for job will be a lot better than this
###For now, I only can say that I'm 15

data("NHANES")

dat <- NHANES

plot(dat$Age, dat$Height)

plot(dat$Height, dat$Weight)

plot(dat$Education, dat$HHIncomeMid)

plot(dat$Race1, dat$HHIncomeMid)

plot(dat$Age1stBaby, dat$HHIncomeMid)

plot(dat$Race1, dat$Diabetes)

plot(dat$MaritalStatus, dat$BMI)

ga_grouped <- dat %>%
                  group_by(Gender, Age)

ga_summarize <- ga_grouped %>%
                    summarize(mean = mean(Height, na.rm=TRUE), sd = sd(Height, na.rm=TRUE))

plot(ga_summarize)

ga_summarize

female <- ga_summarize %>%
          filter(Gender == "female")

male <- ga_summarize %>%
        filter(Gender == "male")


difference <- male$mean - female$mean

plot(difference)

#Difference is the difference between male an female heights. Now I'm gonna try to 
#prove with statistics a significant correlation between age, gender and height
#To do this, my null hypothesis is that there's no correlation between this three 
#factors, so, I'm gonna use the central limit theorem 
#If you do a subtraction in the mean of two samples took from the same population
#The result will be near to 0. By the CLT (Central Limit Theorem) you can predict that
#The distribution of a lot of operations like this is a normal distribution.
#You can also calculate the standard deviation of this, so, you can calculate the
#(I remember that standard deviation of a operation like this is called standard error)
#Returning to the main idea, you can calculate the probability of a given result
#So, I'm gonna use the standard deviation of the complete population by ages
#My confidence level is .05 or 5 percent


standard_error <- female$sd * sqrt(2)

standard_error

difference_from_mean <- difference/standard_error

difference_from_mean

#OK, all the differences from the mean (0) measured by standard error are less than
#2, so, I can't do anything with my current knowledge, I'm gonna learn more about 
#Statistics