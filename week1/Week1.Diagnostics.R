# R code used in Module 2 Regression Diagnostics

# calling libraries

if (!require(Ecdat)) install.packages("Ecdat")
library(Ecdat)

if (!require(ggplot2)) install.packages("ggplot2")
library(ggplot2)

if (!require(ISLR)) install.packages("ISLR")
library(ISLR)

if (!require(GGally)) install.packages("GGAlly")
library(GGally)

if (!require(car)) install.packages("car")
library(car)

if (!require(scatterplot3d)) install.packages("scatterplot3d")
library(scatterplot3d)

if (!require(tidyverse)) install.packages("tidyverse")
library(tidyverse)

# do a ggplot histogram plot of price with binsize 500

ggplot(data=Housing, aes(Housing$price)) + 
  geom_histogram(breaks=seq(1000, 200000, by =500), 
                 col="red", 
                 fill="green", 
                 alpha = .2) + 
  labs(title="Histogram for lotsize") +
  labs(x="price", y="Count")


# do a ggplot histogram plot of price with binsize = 10000

ggplot(data=Housing, aes(Housing$price)) + 
  geom_histogram(breaks=seq(25000, 300000, by =10000), 
                 col="red", 
                 fill="green", 
                 alpha = .2) + 
  labs(title="Histogram for price") +
  labs(x="price", y="Count")


# do a ggplot histogram plot of lotsize with binsize = 50000

ggplot(data=Housing, aes(Housing$price)) + 
  geom_histogram(breaks=seq(25000, 300000, by =50000), 
                 col="red", 
                 fill="green", 
                 alpha = .2) + 
  labs(title="Histogram for price") +
  labs(x="price", y="Count")

# boxplot 
ggplot(data=Housing, mapping = aes(x = factor(bedrooms), y = price)) + 
    geom_boxplot() + geom_jitter(width = 0.1)


# scatterplot
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) +
  geom_smooth(mapping = aes(x = displ, y = hwy)) + 
  theme(axis.text.x = element_text(size=40), axis.text.y = element_text(size=40), 
        axis.text=element_text(size=40), axis.title=element_text(size=24,face="bold"))

#
# example 5 from scatterplot3d.f

data(trees)
s3d <- scatterplot3d(trees, type="h", highlight.3d=TRUE,
                     angle=55, scale.y=0.7, pch=16, main="scatterplot3d - 5")
# Now adding some points to the "scatterplot3d"
s3d$points3d(seq(10,20,2), seq(85,60,-5), seq(60,10,-10),
             col="blue", type="h", pch=16)
# Now adding a regression plane to the "scatterplot3d"
attach(trees)
my.lm <- lm(Volume ~ Girth + Height)
s3d$plane3d(my.lm, lty.box = "solid")


# correlation matrix
h1 <- data.frame(Housing$price, Housing$lotsize, Housing$bedrooms, Housing$bathrms)

ggpairs(h1, 
        upper = list(continuous = wrap("cor", size = 9))) 
#


# Anscombe's Quartet

View(anscombe)  # view the Anscombe dataset 

# run all four regression models

r1 <- lm(y1 ~ x1, data = anscombe)
summary(r1)

r2 <- lm(y2 ~ x2, data = anscombe)
summary(r2)

r3 <- lm(y3 ~ x3, data = anscombe)
summary(r3)

r4 <- lm(y4 ~ x4, data = anscombe)
summary(r4)

# plot all four Anscombe models 

ggplot(anscombe, aes(x=x1, y=y1)) + geom_point((aes(size=3))) +
  scale_colour_hue(l=50) + # Use a slightly darker palette than normal
  geom_smooth(method=lm,   # Add linear regression lines
              se=TRUE,    #  add shaded confidence region
              fullrange=TRUE)  + 
  theme(axis.text.x = element_text(size=40), axis.text.y = element_text(size=40), 
        axis.title=element_text(size=24,face="bold"))


ggplot(anscombe, aes(x=x2, y=y2)) + geom_point((aes(size=3))) +
  scale_colour_hue(l=50) + # Use a slightly darker palette than normal
  geom_smooth(method=lm,   # Add linear regression lines
              se=TRUE,    #  add shaded confidence region
              fullrange=TRUE) + 
  theme(axis.text.x = element_text(size=40), axis.text.y = element_text(size=40), 
         axis.title=element_text(size=24,face="bold"))

ggplot(anscombe, aes(x=x3, y=y3)) + geom_point((aes(size=3))) +
  scale_colour_hue(l=50) + # Use a slightly darker palette than normal
  geom_smooth(method=lm,   # Add linear regression lines
              se=TRUE,    #  add shaded confidence region
              fullrange=TRUE) + 
  theme(axis.text.x = element_text(size=40), axis.text.y = element_text(size=40), 
         axis.title=element_text(size=24,face="bold"))

ggplot(anscombe, aes(x=x4, y=y4)) + geom_point(aes(size=3)) +
  scale_colour_hue(l=50) + # Use a slightly darker palette than normal
  geom_smooth(method=lm,   # Add linear regression lines
              se=TRUE,    #  add shaded confidence region
              fullrange=TRUE) + 
  theme(axis.text.x = element_text(size=40), axis.text.y = element_text(size=40), 
         axis.title=element_text(size=24,face="bold"))
#
# plot the 4 diagnostics plots for a linear model of Price vs. lotsize 
#

a.lm <- lm(formula = price ~ lotsize , data = Housing)

plot.new()
plot(a.lm)

par(mfrow = c(2, 2))  # Split the plotting panel into a 2 x 2 grid
plot(a.lm)  # Plot # plots the four diagnostics plots
 
# plot residual against fitted (predicted) price
plot.new()
a.res <- resid(a.lm)
a.pred <- fitted(a.lm)

plot.new()

plot(a.pred, a.res, main = "Residuals vs. predicted price", 
     xlab = "Predicted Price", ylab = "Residuals")

# ggplot residual against fitted (predicted) price

# I didn't want to mess aroung with the built-in dataset Housing. So copy it to a new dataframe df

df <-  Housing  %>% modelr::add_predictions(a.lm) %>% modelr::add_residuals(a.lm)


ggplot(df,aes(x=pred, y=resid)) + geom_point() + labs(x = "Predicted Price") + labs(y = "Residuals") +
  scale_colour_hue(l=50) + # Use a slightly darker palette than normal
    theme(axis.text.x = element_text(size=15), axis.text.y = element_text(size=15),
         axis.title=element_text(size=20,face="bold"))


# histogram of residuals is useful 
abline(0,0)

hist(a.res, breaks="FD", xlab="Residuals", 
     main="Histogram of residuals")


#  Two Regression to illustrate multicollinearity

Reg1 <- lm(formula = mpg ~ cylinders, data = Auto)
summary(Reg1)
Reg2 <-  lm(formula = mpg ~ cylinders + displacement + weight, data = Auto)
summary(Reg2)

# print Variance Inflation factors
vif(Reg2)
#
h1 <- data.frame(Auto$cylinders, Auto$displacement, Auto$weight)
View(head(h1,10))

ggpairs(h1, 
        upper = list(continuous = wrap("cor", size = 9))) 


