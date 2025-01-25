library(dplyr)
library(readr)

calendar<- read_csv("./data/calendar.csv")
listings<- read_csv("./data/listings.csv")
reviews<- read_csv("./data/reviews.csv")

#### Calendar Data Set ----
summary(calendar)
sum(is.na(calendar$available))
sum(is.na(calendar$price))
sum(is.na(calendar$minimum_nights))
sum(is.na(calendar$maximum_nights))

#### Data is clean 

#### Listings ----
summary(listings)

#### Data is clean ---

#### Reviews ----

summary(reviews)


#### Data merge and save
cal_list<- left_join(calendar, listings, by=c('listing_id'= 'id'))
write_csv(cal_list, file = './data/cal_list.csv')

list_reviews<- left_join(listings, reviews, by=c('id' = 'listing_id'))
write_csv(list_reviews, file = './data/list_reviews.csv')

cal_reviews<- left_join(calendar, reviews, by = 'listing_id')
write_csv(cal_reviews, file='./data/cal_reviews.csv')

full_data <- left_join(calendar, listings, by = c('listing_id' = 'id')) %>%
  left_join(reviews, by = 'listing_id')
write_csv(full_data, file='./data/full_data.csv')
