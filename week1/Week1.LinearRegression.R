# R code used in Module 1 Linear Regression

# calling libraries

if (!require(Ecdat)) install.packages("Ecdat")
library(Ecdat)

if (!require(ggplot2)) install.packages("ggplot2")
library(ggplot2)

if (!require(ISLR)) install.packages("ISLR")
library(ISLR)

if (!require(GGally)) install.packages("GGally")
library(GGally)

if (!require(car)) install.packages("car")
library(car)


# use Housing dataset in the Ecdat package in R

?Housing
summary(Housing)
str(Housing)
View(head(Housing,10))

#table(housing[1:15,],caption='Housing Prices')
h1 <- data.frame(Housing$price, Housing$lotsize, Housing$bedrooms, Housing$bathrms)
View(head(h1,15))

# some useful statistics



pp <- Housing$price
pricesd <- sd(pp)
mean(Housing$price)
median(Housing$price)

lot <- Housing$lotsize
lotsd <- sd(lot)
mean(Housing$lotsize)
median(Housing$lotsize)

res <- cor(h1)
round(res,2)

#res

# Plot Histogram of House Prices

ggplot(data=Housing, aes(Housing$price)) + 
                 geom_histogram(breaks=seq(25000, 190000, by =10000), 
                 col="red", 
                 fill="green", 
                 alpha = .2) + 
  labs(title="Histogram for price") +
  labs(x="price", y="Count")


# Plot Histogram of Lotsize

ggplot(data=Housing, aes(Housing$lotsize)) + 
geom_histogram(breaks=seq(0, 17000, by =1000), 
               col="red", 
               fill="green", 
               alpha = .2) + 
              labs(title="Histogram for lotsize") +
                labs(x="lotsize", y="Count")

# Using ggpairs to get Correlation matrix along with with scatter plots

summary(h1)
ggpairs(h1, 
        upper = list(continuous = wrap("cor", size = 9))) 

# Scatter Plot of price (y) against lotsize (x), including the linear regression line

ggplot(Housing, aes(x=lotsize, y=price)) + geom_point() +
  scale_colour_hue(l=50) + # Use a slightly darker palette than normal
  geom_smooth(method=lm,   # Add linear regression lines
              se=FALSE,    # Don't add shaded confidence region
              fullrange=TRUE) # Extend regression lines

# simple linear regression model with lotsize as predictor

a.lm <- lm(formula = price ~ lotsize , data = Housing)

summary(a.lm) # what does this command do?
anova(a.lm)   # what does this command do?


# create dataframes called new, new2, and new3  

new = data.frame(lotsize=3000)
predict(a.lm, new, interval = "predict")

# 
new2 = data.frame(lotsize=5150)
predict(a.lm, new2, interval = "predict")

new3 = data.frame(lotsize=7300)
predict(a.lm, new3, interval = "predict")



# simple linear regression model with bedroom as predictor

b.lm <- lm(formula = price ~ bedrooms, data = Housing)

# Scatter Plot of price (y) against bedrooms (x), including the linear regression line

ggplot(Housing, aes(x=bedrooms, y=price)) + geom_point() +
  scale_colour_hue(l=50) + # Use a slightly darker palette than normal
  geom_smooth(method=lm,   # Add linear regression lines
              se=FALSE,    # Don't add shaded confidence region
              fullrange=TRUE) # Extend regression lines


# Multiple Regression - with two predictors lotsize and bedrooms

ab.lm <- lm(formula = price ~ lotsize + bedrooms, data = Housing)

summary(ab.lm) 
anova(ab.lm)   


# making a prediction (interpolation). First create a dataframe with values for the predictors. Then use the predict function.

newdata <- data.frame(lotsize=3000, bedrooms=2)

predict(ab.lm, newdata, interval = "predict")

