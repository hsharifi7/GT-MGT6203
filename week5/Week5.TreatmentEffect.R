

# the tidyverse package includes the dplyr, ggplot2, readr, tidyr, tibble, and purrr packages

if (!require(tidyverse)) install.packages("tidyverse")
library(tidyverse)  
if (!require(psych)) install.packages("psych")
library(psych)
if (!require(Ecdat)) install.packages("Ecdat")
library(Ecdat)

mydata <- dplyr::filter(Ecdat::Star, classk=="small.class"|classk=="regular") 
# only analyze small and regular size classes
str(mydata)

mydata <- mydata %>%
  mutate(totalscore = tmathssk + treadssk) %>%
  mutate(small = ifelse(classk=="small.class",1,0)) %>%
  mutate(boy = ifelse(sex=="boy",1,0)) %>%
  mutate(whiteother = ifelse(race=="white"|race=="other",1,0)) %>%
  mutate(freelunch = ifelse(freelunk=="yes",1,0)) %>%
  mutate(schoolj = factor(schidkn))

str(mydata)

# get summary stats for small= 0 and small = 1
describeBy(mydata, mydata$small) # describeBy is from the psych package in R

# summary stats across all data
describe(mydata)


# Please run a linear regression to understand for the case of small= 0

reg_0 <- lm(totalscore ~  1, data = dplyr::filter(mydata,small == 0))
summary(reg_0)

# Please run a linear regression to understand for the case of small= 1

reg_1 <- lm(totalscore ~  1, data = dplyr::filter(mydata,small == 1))
summary(reg_1)

# Please run a linear regression using all the data using dummy variable for small
reg_all <- lm(totalscore ~  small, data = mydata)
summary(reg_all)

# Add Teacher Experience to the model 
reg_2 <- lm(totalscore ~  small + totexpk, data = mydata)
summary(reg_2)

# Add School fixed effects to the model reg-all
reg_3 <- lm(totalscore ~  small + schoolj, data = mydata)
summary(reg_3)

# Add School fixed effects to the model reg_2
reg_4 <- lm(totalscore ~  small + totexpk + schoolj, data = mydata)
summary(reg_4)

# check if small is randomly assigned
reg_5 <- lm(small ~ boy + whiteother + totexpk + freelunch, data = mydata)
summary(reg_5)


