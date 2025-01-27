library(readr)
library(dplyr)
library(lattice)
library(stringr)
library(car)

list_reviews<- read_csv("./data/list_reviews.csv")


#Anova test to check the relationship between price and neighbourhood/room_type

#Null Hypothesis - there is no meaningful relationship between price and the neighbourhood and room_type

#H0 = mu0 = mu01 = mu02
#H1 = Not all means are equal
list_reviews_aov<-aov(list_reviews$price~factor(list_reviews$neighbourhood_name))
summary(list_reviews_aov)


hist_list_reviews<- list_reviews %>%
  mutate(neighbourhood_name_short = str_extract(neighbourhood_name, "^[^,]+")) 
  
# Variance in mean within group and between group 
histogram(~price | neighbourhood_name_short, data = subset(hist_list_reviews, room_type == "Entire home/apt"),
          xlab = "Neighbourhood", ylab = "Price", main = "Room Type")

histogram(~price | neighbourhood_name_short, data = subset(hist_list_reviews, room_type == "Private room"),
          xlab = "Neighbourhood", ylab = "Price", main = "Room Type")


histogram(~price | neighbourhood_name_short, data = subset(hist_list_reviews, room_type == "Shared room"),
          xlab = "Neighbourhood", ylab = "Price", main = "Room Type")

list_reviews_aov2 <- aov(hist_list_reviews$price~factor(hist_list_reviews$neighbourhood_name_short) * 
                     factor(hist_list_reviews$room_type)) 
summary(list_reviews_aov2) 



### Price Prediction using Multiple Regression

hist_list_reviews$host_response_time<- as.factor(hist_list_reviews$host_response_time)
hist_list_reviews$host_response_rate<- parse_number(hist_list_reviews$host_response_rate)
hist_list_reviews$host_acceptance_rate<- parse_number(hist_list_reviews$host_acceptance_rate)
hist_list_reviews$host_is_superhost<- as.factor(hist_list_reviews$host_is_superhost)
hist_list_reviews$room_type<- as.factor(hist_list_reviews$room_type)
hist_list_reviews$neighbourhood_name_short<- as.factor(hist_list_reviews$neighbourhood_name_short)

lm_model<- lm(price~ host_response_time + host_response_rate + host_acceptance_rate + host_is_superhost +
                room_type + accommodates + availability_30 + number_of_reviews_l30d + review_scores_accuracy + 
                review_scores_rating + review_scores_cleanliness + review_scores_communication + review_scores_checkin + 
                review_scores_location + num_amenities + premium_amenities_count + neighbourhood_name_short, 
              data = hist_list_reviews)
summary(lm_model)
vif(lm_model)

#GVIF > 10 equals (or GVIF^(1/(2*Df)) > 2) means multicolinearity issues therefore remove 
#review_scores_rating and neighbourhood_name_short from model, review_scores_accuracy, review_scores_cleanliness

lm_model_refined<- lm(price~ host_response_time + host_response_rate + host_acceptance_rate + host_is_superhost +
                        room_type + accommodates + availability_30 + number_of_reviews_l30d+ 
                        review_scores_communication + review_scores_checkin + review_scores_location + num_amenities + 
                        premium_amenities_count, 
                      data = hist_list_reviews)
summary(lm_model_refined)

plot(lm_model_refined, which = 1)
par(mfrow=c(2,2))
plot(lm_model_refined)
plot(lm_model_refined, which = 3)
plot(lm_model_refined, which = 4)


library(lmtest)
#Breusch-Pagan Test (Heteroscedasticity)
bptest(lm_model_refined)
dwtest(lm_model_refined)

#There is heteroskedasticity since there seems to be a pattern where the variance seem to increase
# the predictions become less accurate the greater the fitted values here and the Q-Q plot does not follow the line
#Also heteroskedasticity because Breusch-Pagan Test has p-value <= 0.05
#model robustness could be increased by getting rid of outliers


#get rid of outliers
Q1 <- quantile(hist_list_reviews$price, 0.25, na.rm = TRUE)
Q3 <- quantile(hist_list_reviews$price, 0.75, na.rm = TRUE)
IQR_value <- IQR(hist_list_reviews$price, na.rm = TRUE)
outliers <- hist_list_reviews$price < (Q1 - 1.5 * IQR_value) | hist_list_reviews$price > (Q3 + 1.5 * IQR_value)
summary(outliers)

data_clean <- hist_list_reviews[!outliers, ]

lm_model_clean<-lm(price~ host_response_time + host_response_rate + host_acceptance_rate + host_is_superhost +
                   room_type + accommodates + availability_30 + number_of_reviews_l30d+ 
                   review_scores_communication + review_scores_checkin + review_scores_location + num_amenities + 
                   premium_amenities_count, 
                 data = data_clean)

summary(lm_model_clean)
plot(lm_model_clean$residuals)
standard_res <- rstandard(lm)

bptest(lm_model_clean)
