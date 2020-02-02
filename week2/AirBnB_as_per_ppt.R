

#After installing, we need to call the package to make it available to R
library(readr)
library(stargazer)
library(knitr)
library(dplyr)
#library(GGally)
#library("psych")
library(ggplot2)
library(stringr)
library("ggExtra")
library(psych)

getwd()
setwd("C:/Users/sn2/Dropbox (GaTech)/Computer/Desktop/Summer Evening MBA/May 23 Classs")
# this is the directory (folder) where I store the dataset used below

load("./la_listing_full.RData")

la_listing <- la_listing_full  %>% 
                select(price,number_of_reviews,beds,bathrooms,accommodates,reviews_per_month, property_type, room_type,review_scores_rating) %>% 
                rename(Reviews = number_of_reviews) %>% 
                rename(Beds = beds) %>% 
                rename(Baths = bathrooms) %>% 
                rename(Capacity = accommodates) %>% 
                rename(Monthly_Reviews = reviews_per_month) %>% 
                rename(Property_Type = property_type) %>% 
                rename(Room_Type = room_type) %>% 
                rename(Price = price) %>% 
                rename(Rating = review_scores_rating)


la_listing <-  la_listing %>% 
                mutate(Price = str_replace(Price, "[$]", "")) %>% 
                mutate(Price = str_replace(Price, "[,]", "")) %>% 
                mutate(Price = as.numeric(Price)) %>% 
                mutate(Room_Type = factor(Room_Type, levels = c("Shared room", "Private room", "Entire home/apt"))) %>% 
                mutate(Capacity_Sqr = Capacity * Capacity) %>% 
                mutate(Beds_Sqr = Beds * Beds) %>% 
                mutate(Baths_Sqr = Baths * Baths) %>% 
                mutate(ln_Price = log(1+Price)) %>% 
                mutate(ln_Beds = log(1+Beds)) %>%
                mutate(ln_Baths = log(1+Baths)) %>% 
                mutate(ln_Capacity = log(1+Capacity)) %>% 
                mutate(ln_Rating = log(1+Rating)) %>% 
                mutate(Shared_ind = ifelse(Room_Type == "Shared room",1,0)) %>% 
                mutate(House_ind = ifelse(Room_Type == "Entire home/apt",1,0)) %>% 
                mutate(Private_ind = ifelse(Room_Type == "Private room",1,0)) %>% 
                mutate(Capacity_x_Shared_ind = Shared_ind * Capacity) %>% 
                mutate(H_Cap = House_ind * Capacity) %>% 
                mutate(P_Cap = Private_ind * Capacity) %>% 
                mutate(ln_Capacity_x_Shared_ind = Shared_ind * ln_Capacity) %>% 
                mutate(ln_Capacity_x_House_ind = House_ind * ln_Capacity) %>% 
                mutate(ln_Capacity_x_Private_ind = Private_ind * ln_Capacity)


la_listing <- la_listing %>% 
              dplyr::filter(Price < 1000 , !is.na(Beds), !is.na(Baths), !is.na(Price), !is.na(Rating)) %>% 
                dplyr::filter(Capacity < 9) %>% 
                mutate(ln_Reviews = log(1+Reviews)) %>% 
                mutate(ln_Monthly_Reviews = log(1+Monthly_Reviews))

   



#We can examine if the number of people a listing can accomodate is related to price. 

lm0 <- lm(Price ~ Capacity, data = la_listing)
summary(lm0)
stargazer(lm0, type = "text")


ggplot(data = la_listing, aes(x = Capacity, y = Price)) + geom_point(aes(size=3)) +
scale_colour_hue(l=50) + # Use a slightly darker palette than normal
geom_smooth(method=lm,   # Add linear regression lines
           se=TRUE,    #  add shaded confidence region
           fullrange=TRUE) +
theme(axis.text.x = element_text(size=40), axis.text.y = element_text(size=40), 
        axis.title=element_text(size=24,face="bold"))




#The moderating effect of type of room. Lets model that.

lm1 <- lm(Price ~ Private_ind + House_ind, data = la_listing)
summary(lm1)
stargazer(lm1, type = "text")


#Regression with Capacity and Dummy Variables for type of room:

lm2 <- lm(Price ~ Capacity + Private_ind + House_ind, data = la_listing)
summary(lm2)
stargazer(lm2, type = "text")




#Regression with Capacity,Dummy Variables and interaction between the two:
lm3 <- lm(Price ~ Capacity+Private_ind + House_ind+P_Cap+H_Cap, data = la_listing)
summary(lm3)
stargazer(lm3,type = "text")

