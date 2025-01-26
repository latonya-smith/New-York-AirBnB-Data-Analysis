library(readr)
library(dplyr)
library(ggplot2)
library(sf)
library(leaflet)
library(stringr)

#### Exploratory Data Analysis

### Geographical Visualization --- fix this
listings_feats<- read_csv("./data/listings_feats.csv")

map <- leaflet(listings_feats) %>%
  addTiles() %>%
  addCircleMarkers(~longitude, ~latitude, color = ~neighbourhood_name, 
                   radius = 5, stroke = FALSE, fillOpacity = 0.8, 
                   popup = ~paste("Neighborhood:", neighbourhood_name)) %>%
  addLegend("bottomright", pal = colorFactor("Set1", domain = listings_feats$neighbourhood_name),
            values = listings_feats$neighbourhood_name, title = "Neighborhoods", opacity = 1)
map

#----

listings_sf<- st_as_sf(listings_feats, coords = c("latitude", "longitude"), crs = 4326)

# Plot the geographical map using ggplot2
ggplot(data = listings_sf) +
  geom_sf(aes(color = neighbourhood), size = 2) +  # Plot points with neighborhood-based colors
  labs(title = "Geographical Distribution of Listings",
       x = "Longitude", y = "Latitude") +
  theme_minimal() +
  theme(legend.position = "bottom")



#### Price Distribution

#Over neighbourhood and room type

listings_feats %>%
  group_by(neighbourhood_cleansed, room_type) %>%
  summarise(avg_price= mean(price, na.rm = TRUE)) %>%
  ggplot(aes(x=neighbourhood_cleansed, y=avg_price, fill=room_type)) +
  geom_bar(stat = "identity") +
  labs(title= "Distribution of Airbnb Prices", x = "Neighbourhood", y= "Average Price")+
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1), # Rotate the x-axis labels
    axis.title.x = element_text(size = 12), # Adjust the size of x-axis title
    axis.title.y = element_text(size = 12)  # Adjust the size of y-axis title
  ) +
  scale_fill_manual(values = c("Entire home/apt" = "skyblue", "Private room" = "green4", "Shared room" = "red4"))

#Over superhost/not superhost 



listings_feats %>%
  filter(!is.na(host_is_superhost)) %>% # Remove rows with NA in superhost status
  mutate(neighbourhood_name = str_extract(neighbourhood_name, "^[^,]+")) %>% # Keep everything before the first comma
  group_by(neighbourhood_name, host_is_superhost) %>%
  summarise(avg_price = mean(price, na.rm = TRUE)) %>%
  ggplot(aes(x = host_is_superhost, y = avg_price, fill = host_is_superhost)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Distribution of Airbnb Prices", 
    x = "Superhost Status", 
    y = "Average Price"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1), # Rotate the x-axis labels
    axis.title.x = element_text(size = 12), # Adjust the size of x-axis title
    axis.title.y = element_text(size = 12), # Adjust the size of y-axis title
    strip.text = element_text(size = 10)    # Adjust the size of facet labels
  ) +
  scale_fill_manual(values = c("FALSE" = "black", "TRUE" = "yellow")) +
  facet_wrap(~ neighbourhood_name, scales = "free_y")


####  Room Type Analysis

listings_feats %>%
  group_by(neighbourhood_name, room_type) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  group_by(neighbourhood_name) %>%
  mutate(total_count = sum(count),
         percentage = (count / total_count) * 100) %>%
  ggplot(aes(x = neighbourhood_name, y = percentage, fill = room_type)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Room Type Distribution Across Neighborhoods",
    x = "Neighborhood",
    y = "Percentage",
    fill = "Room Type"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = c("Entire home/apt" = "darkblue", "Private room" = "green", "Shared room" = "yellow"))


ggplot(listings_feats %>%
         filter(!is.na(premium_amenities_count) & !is.na(relative_price)),  # Filter out NA values from premium_amenities_count
       aes(x = interaction(room_type, relative_price), y = premium_amenities_count, fill = relative_price)) +
  geom_boxplot()+
  labs(
    title = "Distribution of Premium Count by Room Type and Relative Price",
    x = "Room Type and Relative Price",
    y = "Premium Count",
    fill = "Relative Price"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


####  Host Analysis

#Most Active Hosts
listings_feats %>%
  group_by(host_id, host_name, room_type) %>%
  summarise(
    avg_price = mean(price, na.rm = TRUE),
    total_listings = max(host_total_listings_count, na.rm = TRUE)
  ) %>%
  filter(!is.na(total_listings) & total_listings <= 20 & avg_price <=500) %>%
  ggplot(aes(x = total_listings, y = avg_price, color = room_type)) +
  geom_point(size = 1.5) +  # Points with different shapes and sizes
  geom_text(aes(label = host_name), vjust = -1, size = 2, check_overlap = TRUE) +
  labs(
    title = "Scatter Plot of Average Price vs Host Total Listings",
    x = "Total Listings per Host",
    y = "Average Price",
    color = "Room Type",
    shape = "Room Type"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#Does experience of hosts reflect in the ratings?
listings_feats%>%
  group_by(host_id, host_name) %>%
  summarise(
    avg_rating = mean(review_scores_rating),
    total_listings = max(host_total_listings_count, na.rm = TRUE))%>%
  filter(!is.na(total_listings) & total_listings <= 20) %>%
  ggplot(aes(x=total_listings, y= avg_rating)) +
  geom_point(size=1.5, aes(color=avg_rating)) +
  labs(
    title = "Host Listings vs Average Review Score",
    x = "Total Listings per Host",
    y = "Average Review Score"
  ) +
  theme_minimal()

#Does experience of hosts reflect in occupancy per month and ratings?
cal_list %>%
  group_by(host_id, host_name) %>%
  summarise(
    avg_occupancy=mean(unavailable_days, na.rm=TRUE),
    avg_rating = mean(review_scores_rating, na.rm = TRUE),
    total_listings = max(host_total_listings_count, na.rm = TRUE)
  ) %>%
  filter(!is.na(total_listings) & total_listings <= 20) %>%
  ggplot(aes(x=total_listings, y= avg_occupancy)) +
  geom_point(size=1.5, aes(color=avg_rating)) + 
  labs(
    title="Host Listings vs Average Occupancy",
    x= "Total Listings per Host",
    y= "Average Occupancy"
  ) +
  theme_minimal()
