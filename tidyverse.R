library("dslabs")
library("dplyr")


data(co2)

typeof(co2)

co2

data("ChickWeight")

head(ChickWeight)

ChickWeight
typeof(ChickWeight)


data("BOD")
BOD


data("EuStockMarkets")

EuStockMarkets
head(EuStockMarkets)


data(DNase)
head(DNase)


data("Formaldehyde")
head(Formaldehyde)


data(Orange)
Orange


data(murders)


murders$rate <- with(murders, total/population*100000)

plot(murders$population, murders$total)

plot(murders$region, murders$total)

data(murders)

murders <- mutate(murders, rate = total/population*100000)

murders

murders$rate <= 0.71


filter(murders, rate <= 0.71)

new_data <- select(murders, state, region, rate)
new_data


murders <- mutate(murders, rank = rank(-rate))

murders

select(murders, state, population) %>% head()

select(murders, state, abb) %>% head()

filter(murders, rank <= 10)

no_south <- filter(murders, region != "South")
nrow(no_south)

filter(murders, state %in% c("New York", "Vermont"))

murders_n_w <- filter(murders, region %in% c("West", "Northeast"))

murders_n_w


boxplot(rate~region, murders)
plot(murders$region, murders$rate)


my_states <- filter(murders_n_w, rate <= 1)


data(murders)


my_states <- murders%>%
             mutate(rate = total/population * 100000, rank = rank(-rate))%>%
             filter(region %in% c("Northeast", "West") & rate <= 1) %>% 
             select(state, rate, rank)

my_states


#####################################


library(dplyr)
library(dslabs)
data(heights)

male <- heights %>%
        filter(sex=="Male") %>%
        summarize(mean = mean(height), standard_deviation = sd(height), 
                  min = min(height), median = median(height), max = max(height))

male

male %>% pull(standard_deviation)


data(murders)


descriptive <- murders %>%
               group_by(region) %>%
               summarize(rate = sum(total)/sum(population)*100000,
                         min = min(total/population*10^5), median = median(total/population*10^5), max = max(total/population*10^5))

descriptive


murders %>% arrange(total)

murders %>% arrange(state)

murders %>% top_n(10)


library(NHANES)
data("NHANES")

nhanes <- NHANES

nhanes

interesting_females <- nhanes %>% filter(Gender == "female", AgeDecade == " 20-29")

top_n(interesting_females, 10)

ref <- interesting_females %>%
       summarize(sd = sd(BPSysAve, na.rm=TRUE), mean = mean(BPSysAve, na.rm=TRUE))

ref

ref_avg <- ref %>% pull(mean)

ref

ref_avg

min(interesting_females$BPSysAve, na.rm=TRUE)

max(interesting_females$BPSysAve, na.rm=TRUE)

females <- nhanes %>%
           filter(Gender == "female")


females <- group_by(females, AgeDecade)

ref_all <- females %>%
           summarize(mean = mean(BPSysAve, na.rm=TRUE), std = sd(BPSysAve, na.rm=TRUE))

ref_all


males <- nhanes %>%
         filter(Gender == "male")

males <- males %>%
         group_by(AgeDecade)


rem_all <- males %>%
           summarize(mean = mean(BPSysAve, na.rm=TRUE), std = sd(BPSysAve, na.rm=TRUE))


rea_all <- group_by(nhanes, AgeDecade, Gender) %>%
           summarize(mean = mean(BPSysAve, na.rm=TRUE), std = sd(BPSysAve, na.rm=TRUE))

rer_male<- nhanes %>%
           filter(Gender == "male") %>%
           group_by(Race1) %>%
           summarize(mean = mean(BPSysAve, na.rm=TRUE), std = sd(BPSysAve, na.rm=TRUE))

arrange(rer_male, mean)
class(pull(rer_male, mean))

murders
as_tibble(murders)
murders[1,2]

df_function <- tibble(func = c(mean, median, sd))

tab_1 <- filter(murders, region == "South")
tab_2 <- mutate(tab_1, rate = total/population * 10^5)
tab_2

south_median <- with(tab_1, sum(total)/sum(population)*10^5)
south_median

south_states_median <- filter(murders, region == "South") %>%
                       with(total/population*10^5) %>%
                       median()
south_states_median

filter(murders, region == "South") %>%
  with(total/population*10^5)

quantile(murders$population, seq(0,1,.01))

