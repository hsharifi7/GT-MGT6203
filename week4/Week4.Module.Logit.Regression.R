
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
if (!require(scatterplot3d)) install.packages("scatterplot3d")
library(scatterplot3d)
if (!require(tidyverse)) install.packages("tidyverse")
library(tidyverse)

if (!require(ggExtra))install.packages("ggExtra")
library("ggExtra")

if (!require(ROCR)) install.packages("ROCR")
library("ROCR")

getwd()
setwd("C:/Users/sn2/Dropbox (GaTech)/Computer/Desktop/Daytime MBA 6203 Fall 2018/September 24-26 Logistic Regression")
 
logit_grade <- read_csv("GradesR.csv", col_types = list(
  Student = col_integer(),
  Grade = col_integer(),
  Hours = col_double()))

View(head(logit_grade,10))

# boxplot of Hours vs. Grade  (Need to use Grade as a factor)
 ggplot(data=logit_grade, aes(x=factor(Grade), y = Hours, fill=factor(Grade))) + 
  geom_boxplot() +
  ggtitle("BoxPlot for Hours of Studying vs. Grade") + theme(plot.title = element_text(size = 40, face = "bold")) +
  labs(x="Grade", y="Hours") + 
  theme(axis.text.x = element_text(size=40), axis.text.y = element_text(size=40), 
  axis.title=element_text(size=24,face="bold"))


# Scatter Plot + Linear Regression line of Grade vs. Hours 
 
ggplot(logit_grade, aes(x=Hours, y=Grade)) + geom_point() +
  scale_colour_hue(l=50) + 
  theme(axis.text.x = element_text(size=40), axis.text.y = element_text(size=40), 
  axis.title=element_text(size=24,face="bold")) + 
  geom_smooth(method=lm,   # Add linear regression lines
              se=TRUE,    # Don't add shaded confidence region
              fullrange=TRUE) 

# Using Linear Regression to Model Binary Outcomes

a.lm <- lm(formula = Grade ~ Hours, data = logit_grade)
summary(a.lm)
anova(a.lm)


# Scatter Plot + Linear Regression line of Grade vs. Hours + Logistic Model Curve

ggplot(logit_grade, aes(x=Hours, y=Grade)) + geom_point() + 
  theme(axis.text.x = element_text(size=25), axis.text.y = element_text(size=25), 
  axis.title=element_text(size=24,face="bold")) + 
# add logit curve
  stat_smooth(method="glm", method.args=list(family="binomial"), se=FALSE) +
#add the regression line
    geom_smooth(method=lm,  color="red", # Add linear regression lines
              se=FALSE,    # Don't add shaded confidence region
              fullrange=TRUE) 
  

ol <- read_csv("oddslogodds.csv", col_types = list(
  p = col_double(),
  odds = col_double(),
  logodds = col_double()))

View(ol)
#
# Plot Probability, Odds, and Log of Odds
#
ggplot(ol, aes(x=p)) +
  geom_line(aes(y=odds), colour ="red") +
  geom_line(aes(y=logodds), colour ="blue") +
  ggtitle("Odds (Red) and log(odds) (Blue) vs. p") + theme(plot.title = element_text(size = 24, face = "bold")) +
  labs(x="p", y="Odds & log(odds)") +
  theme(axis.text.x = element_text(size=40), axis.text.y = element_text(size=40), 
  axis.title=element_text(size=24,face="bold"))

# plot of log odds vs odds

ggplot(ol, aes(x=odds)) +
  geom_line(aes(y=logodds), colour ="Black") + 
  ggtitle("log(odds) vs. Odds") + theme(plot.title = element_text(size = 40, face = "bold")) +
  labs(x="odds", y="log(odds") +
  theme(axis.text.x = element_text(size=40), axis.text.y = element_text(size=40), 
        axis.text=element_text(size=40), axis.title=element_text(size=24,face="bold"))
# 
#
??Default

str(ISLR::Default)
 
df <- ISLR::Default
df<- df %>%
  mutate(dft = ifelse(default=="Yes",1,0)) %>%
  mutate(stdt = ifelse(student=="Yes",1,0)) 

# Scatterplot of Income vs. Balance (Default in Blue)

ggplot(data = df) + 
  geom_point(mapping = aes(x = balance, y = income, color = default)) + 
  theme(axis.text.x = element_text(size=25), axis.text.y = element_text(size=25), 
  axis.title=element_text(size=24,face="bold"))

# BoxPlot for Balance vs. Default Status
ggplot(data=df, aes(x=default, y = balance, fill=default)) + 
  geom_boxplot() +
  ggtitle("BoxPlot for Balance vs. Default Status") + theme(plot.title = element_text(size = 40, face = "bold")) +
#  labs(title="BoxPlot for Balance vs. Default Status") +
  labs(x="Default", y="Balance") + 
  theme(axis.text.x = element_text(size=40), axis.text.y = element_text(size=40), 
  axis.title=element_text(size=24,face="bold"))


#  Run different logit models

# Model1 has no predictor variables
Model1 <- glm(dft ~ 1 , data = df, family = "binomial")
summary(Model1)

# Get the count of defaulters and non-defaulters in the dataframe df 
# using the group_by function

df %>% 
  group_by(default) %>%
  summarise(n=n())

# Model 2: logit(p) = b0 + b1*stdt (single 0/1 predictor variable)
Model2 <- glm(dft ~ stdt , data = df, family = "binomial")
summary(Model2)

# Model 3: logit(p) = b0 + b1*balance (single continuous predictor variable)
Model3 <- glm(dft ~ balance, data = df, family = "binomial")
summary(Model3)

# plot default rate for the entire population of students and non-students
ggplot(df, aes(x=balance, y=dft)) + geom_point() + 
  theme(axis.text.x = element_text(size=40), axis.text.y = element_text(size=40), 
        axis.text=element_text(size=40), axis.title=element_text(size=24,face="bold")) + 
  # add logit curve
  stat_smooth(method="glm", method.args=list(family="binomial"), se=FALSE) 

# Model 4: logit(p) = b0 + b1*balance + b2*income + b3*stdt
Model4 <- glm(dft ~ balance + income + stdt, data = df, family = "binomial")
summary(Model4)

# boxplot
ggplot(data=df, aes(x=student, y = balance, fill=student)) + 
  geom_boxplot() +
  ggtitle("BoxPlot for Balance vs. Student Status") + theme(plot.title = element_text(size = 40, face = "bold")) +
  #  labs(title="BoxPlot for Balance vs. Student Status") +
  labs(x="Student", y="Balance") + 
  theme(axis.text.x = element_text(size=40), axis.text.y = element_text(size=40), 
        axis.text=element_text(size=40), axis.title=element_text(size=24,face="bold"))


# make predictions using Model 4

df <-  df %>% 
  mutate(pred_prob_model4 = predict(Model4, newdata = ., type = "response")) %>% 
  mutate(pred_outcome_model4 = ifelse(pred_prob_model4 >= 0.5,1,0))
# we are using 0.5 as cutoff for predicting Y=1. 
View(df)

# plot default rates for students and non-students
ggplot(data=df, aes(x=balance, y=pred_prob_model4, group=student, colour=student)) +
  geom_line() +
  geom_hline(aes(yintercept=0.058), colour="blue", linetype="dashed")+
  geom_hline(aes(yintercept=0.015), colour="#990000", linetype="dashed") + 
  labs(x="Balance", y="Default Rate") + 
  theme(axis.text.x = element_text(size=40), axis.text.y = element_text(size=40), 
  axis.title=element_text(size=24,face="bold"))

## two-way Cross Tab table of Actual outcome and predicted Outcome 
xtabs(~dft + pred_outcome_model4, data = df)
#pred_outcome_model4
#dft    0    1
# 0   9627   40
# 1   228   105

#Same thing can be computed with tally
tally(group_by(df,dft,pred_outcome_model4))

#ROC Curve
pred <- prediction(df$pred_prob_model4,df$dft) # create a prediction object in R
class(pred)

perf <- performance(pred, "tpr", "fpr") # tpr and fpr are true and false positive rates
plot(perf, colorize=T)

# calculate Area Under the Curve for this Logit Model
auc.perf <-  performance(pred, measure = "auc")
auc.perf@y.values

# Make predictions (using model 4) for original dataset with 0.9 as cutoff 

df <- df%>% 
  mutate(pred_outcome_0.90 = ifelse(pred_prob_model4 >= 0.90,1,0))

xtabs(~dft + pred_outcome_0.90, data = df)
# pred_outcome_0.90
# dft    0    1
# 0 9665    2
# 1  323   10

#Same thing can be computed with tally
tally(group_by(df,dft,pred_outcome_0.90))
# dft pred_outcome_0.90     n
# <dbl>             <dbl> <int>
#   1  0                 0     9665
# 2  0                 1.00     2
# 3  1.00              0      323
# 4  1.00              1.00    10



