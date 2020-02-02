# Load data
setwd("C:/Users/kxu/OneDrive/Georgia Tech/2019_Fall/MGT6203/Week_8_Factor_Investing")
library(stargazer)

data <- read.csv("contraFactorExample.csv")

# Run factor models
factor1 <- lm(Contra.rf ~ Mkt_rf, data = data)
factor3 <- lm(Contra.rf ~ Mkt_rf+SMB+HML, data = data)
factor4 <- lm(Contra.rf ~ Mkt_rf+SMB+HML+Mom, data = data)
factor6 <- lm(Contra.rf ~ Mkt_rf+SMB+HML+Mom+BAB+QMJ, data = data)
stargazer(factor1, factor3, factor4, factor6, align = TRUE, type = "html", out = "factorModelFull.html")
