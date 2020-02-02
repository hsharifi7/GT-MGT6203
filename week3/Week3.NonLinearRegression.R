# R code for Module 4

# R code for Module 4

if (!require(ggplot2)) install.packages("ggplot2")
library(ggplot2) 

if (!require(Ecdat)) install.packages("Ecdat")
library(Ecdat) 

if (!require(ISLR)) install.packages("ISLR")
library(ISLR)

if (!require(GGally)) install.packages("GGally")
library(GGally)

if (!require(car)) install.packages("car")
library(car)

if (!require(tidyverse)) install.packages("tidyverse")
library(tidyverse)

#   read top 100 cities in the us

getwd()

# An Example of a Nonlinear Relationship US city Population and Rank

CitPop <- read_csv("Cities.csv", col_types = list(
  Rank = col_integer(),
  CityState = col_character(),
  Population2010 = col_integer(),
  Population2012 = col_integer(),
  Growth = col_integer()))

str(CitPop)
View(head(CitPop,10))

ggplot(CitPop, aes(x=Rank, y=Population2012)) + geom_point() + labs(x = "Rank") + labs(y = "Population 2012") +
  scale_colour_hue(l=50) + # Use a slightly darker palette than normal
  geom_smooth(method=lm, size = 1.5) +   # Add linear regression lines  
  theme(axis.text.x = element_text(size=15), axis.text.y = element_text(size=15),
        axis.title=element_text(size=20,face="bold")) +
      scale_y_continuous(breaks=seq(0,10000000,500000))

#   
# we use the Housing dataset in the Ecdata Package in R

?Housing
summary(Housing)

# Create a dataframe h1 which has the four columns from Housing - price, lotsize, bedrooms, and bathrms 
h1 <- data.frame(Housing$price, Housing$lotsize, Housing$bedrooms, Housing$bathrms)

View(head(h1,15))  # in case you want to view the first 15 records in h1

#create natural log of the variables price and lotsize and also the square of lotsize and add these new variables to h1

h1 <- h1 %>%
  mutate(Ln_price = log(Housing.price)) %>%
  mutate(Ln_lotsize = log(Housing.lotsize)) %>%
  mutate(lot_square = Housing.lotsize*Housing.lotsize) 
         
View(head(h1,15)) # in case you want to view the first 15 records in h1
         
#
# Model A:  price = b0 + b1*lotsize  Linear-linear model

a.lm <- lm(formula = price ~ lotsize , data = Housing)
summary(a.lm)

# Model A: Scatter Plot with regression line 

ggplot(Housing, aes(x=lotsize, y=price)) + geom_point() + labs(x = "lotsize") + labs(y = "price") +
  scale_colour_hue(l=50) + # Use a slightly darker palette than normal
  geom_smooth(method=lm, size = 1.5) +   # Add linear regression lines  
        theme(axis.text.x = element_text(size=15), axis.text.y = element_text(size=15),
        axis.title=element_text(size=20,face="bold"))

# Model A:  Diagnostics Plots
par(mfrow = c(2, 2))  # Split the plotting panel into a 2 x 2 grid
plot(a.lm) # Plot # plots the four diagnostics plots


# Model B:  price = b0 + b1*log(lotsize)  Linear-Log Model

b.lm <- lm(formula = Housing.price ~ Ln_lotsize , data = h1)
summary(b.lm)

# Model B: Scatter Plot with regression line 
ggplot(h1, aes(x=Ln_lotsize, y=Housing.price)) + geom_point() + labs(x = "Ln(lotsize)") + labs(y = "price") +
  scale_colour_hue(l=50) + # Use a slightly darker palette than normal
  geom_smooth(method=lm,   # Add linear regression lines
              se=TRUE,    # Don't add shaded confidence region
              fullrange=TRUE) + # Extend regression lines
        theme(axis.text.x = element_text(size=15), axis.text.y = element_text(size=15),
        axis.title=element_text(size=20,face="bold"))

# Model B:  Diagnostics Plots
par(mfrow = c(2, 2))  # Split the plotting panel into a 2 x 2 grid
plot(b.lm)


# Model C:  log(price) = b0 + b1*lotsize  Log-Linear Model

c.lm <- lm(formula = Ln_price ~ Housing.lotsize , data = h1)
summary(c.lm)

# Model C: Scatter Plot with regression line 
ggplot(h1, aes(x=Housing.lotsize, y=Ln_price)) + geom_point() + geom_point() + labs(x = "lotsize") + labs(y = "Ln(price)") +
  scale_colour_hue(l=50) + # Use a slightly darker palette than normal
  geom_smooth(method=lm,   # Add linear regression lines
              se=TRUE,    # Don't add shaded confidence region
              fullrange=TRUE) + # Extend regression lines
      theme(axis.text.x = element_text(size=15), axis.text.y = element_text(size=15),
        axis.title=element_text(size=20,face="bold"))

# Model C:  Diagnostics Plots
par(mfrow = c(2, 2))  # Split the plotting panel into a 2 x 2 grid
plot(c.lm)


# Model D:  log(price) = b0 + b1*log(lotsize)  Log-Log Model

d.lm <- lm(formula = Ln_price ~ Ln_lotsize , data = h1)
summary(d.lm)

# Model D: Scatter Plot with regression line 
ggplot(h1, aes(x=Ln_lotsize, y=Ln_price)) + geom_point() +
  scale_colour_hue(l=50) + # Use a slightly darker palette than normal
  geom_smooth(method=lm,   # Add linear regression lines
              se=TRUE,    # Don't add shaded confidence region
              fullrange=TRUE) + # Extend regression lines
  theme(axis.text.x = element_text(size=15), axis.text.y = element_text(size=15),
      axis.title=element_text(size=20,face="bold"))

# Model D:  Diagnostics Plots
par(mfrow = c(2, 2))  # Split the plotting panel into a 2 x 2 grid
plot(d.lm) # Plot # plots the four diagnostics plots

# Model E:  price = b0 + b1*lotsize + b2*lotsize2  Polynomial (Quadratic) Model

e.lm <- lm(formula = Housing.price ~ Housing.lotsize + lot_square, data = h1)
summary(e.lm)


