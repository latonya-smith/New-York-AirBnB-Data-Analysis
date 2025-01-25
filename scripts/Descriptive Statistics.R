library(readr)
library(dplyr)
library(ggplot2)
library(maps)
library(leaflet)
library(vcd)

cal_list<- read_csv("./data/cal_list.csv")
list_reviews<- read_csv("./data/list_reviews.csv")
cal_reviews<- read_csv("./data/cal_reviews.csv")

### Central Tendency

## Price
cal_list<- cal_list %>%
  mutate(price.x = parse_number(price.x))  ### Add this to the cleaning and merges code

mean<- cal_list %>%
  group_by(listing_id) %>%
  summarise(mean_price = mean(price.x, na.rm=TRUE))

ggplot(mean %>%
         filter(mean_price <= 1000), 
       aes(x= mean_price)) +
  geom_histogram(binwidth = 100, fill="lightgreen", color="black", alpha=0.7) +
  labs(title = "Distribution of AirBnb Prices",
       x ="Mean Price Per Night",
       y = "Count") +
  scale_x_continuous(breaks=seq(0,1000, by=100)) +
  theme_minimal() 


#Minimum and Maximum Nights

nights<- cal_list %>%
  group_by(listing_id) %>%
  summarise(mean_min_nights = mean(minimum_nights.x, na.rm=TRUE), mean_max_nights = mean(maximum_nights.x))


ggplot(nights %>%
         filter(mean_min_nights < 50), aes(x=mean_min_nights)) +
  geom_histogram(binwidth = 1, fill="skyblue", color="black", alpha=0.7) +
  labs(title="Distribution of Minimum NIghts", x="Minimum Nights", y="Frequency") +
  theme_minimal()

ggplot(nights, aes(x=mean_max_nights)) +
  geom_histogram(binwidth = 1, fill="blue", color="yellow", alpha=0.7) +
  scale_x_log10() +
  labs(title="Distribution of Maximum Nights", x="Log to base 10 Maximum Nights", y="Frequency") +
  theme_classic()

#Distribution of Host Listings
ggplot(list_reviews %>%
         filter(host_total_listings_count <=100), aes(x=host_total_listings_count)) +
  geom_histogram(binwidth=1, fill="lightyellow", color="green", alpha=0.7) +
  labs(title="Distribution of Amount of Listings per Host", x="Listing Count", y= "Number of Hosts") +
  theme_classic()


#Map showing mean price
leaflet(cal_list) %>%
  addTiles() %>%
  addMarkers(~longitude, ~latitude,
             popup= ~paste("Mean Price: $", price.x))


#Table of number of listings per neighbourhood
list_per_neigh<- list_reviews %>%
  group_by(neighbourhood_cleansed) %>%
  summarise(listings_count = n()) %>%
  arrange(desc(listings_count)) 

ggplot(list_per_neigh, aes(x=listings_count, y= neighbourhood_cleansed)) +
  geom_bar(stat="identity") +
  labs(title="Distribution of Amount of Listings per Neighbourhood", x="Listing Count", y= "Neighbourhood") +
  theme_classic()

#Which neighbourhood had the most expensive prices
expensive_neighbourhood<- list_reviews %>%
  group_by(neighbourhood_cleansed, room_type) %>%
  summarise(mean_price = mean(parse_number(price), na.rm=TRUE))

ggplot(expensive_neighbourhood, aes(x= mean_price, y=neighbourhood_cleansed, fill = room_type))+
  geom_bar(stat="identity") +
  labs(title="Amount of Listings per Neighbourhood", x="Listing Count", y= "Neighbourhood") +
  theme_classic()


