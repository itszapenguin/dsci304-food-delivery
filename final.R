###############################################################
# DSCI 304 Final Visualizations
# Ethan Hsu
###############################################################

#Clear Workspace
cat("\014")
rm(list=ls())
set.seed(18552)

#Libraries
library(stargazer)
library(haven)
library(margins)
library(ggeffects)
library(tidyr)
library(dplyr)
library(ggplot2)
library(foreign)
library(dotwhisker)
library(corrplot)
library(reshape2)
library(waffle)
library(sf)
library(sp)
library(maps)


#Setup
setwd("~/Developer/dsci304/final")
nyc<-read.csv("nyc_order.csv")

#Data Cleaning
nyc$restaurant_name[nyc$restaurant_name == 'Empanada Mama (closed)'] <- "Empanada Mama"
nyc$restaurant_name[nyc$restaurant_name == 'Chipotle Mexican Grill $1.99 Delivery'] <- "Chipotle Mexican Grill"
nyc$restaurant_name[nyc$restaurant_name == 'Dirty Bird To Go (archived)'] <- "Dirty Bird To Go"
nyc$restaurant_name[nyc$restaurant_name == 'CafÌ© China'] <- "Cafe China"

nyc <- nyc %>%
  mutate(restaurant_name = case_when(
    startsWith(restaurant_name, "Joe's Shanghai") ~ "Joe's Shanghai",
    startsWith(restaurant_name, "Big Wong Restaurant") ~ "Big Wong Restaurant",
    TRUE ~ restaurant_name
  ))


###GENERAL###

#Horizontal Bar Graph of Most Ordered/Popular Restaurants by Order
top_restaurant_counts <- nyc %>% 
  group_by(restaurant_name, cuisine_type) %>% 
  summarise(count = n(), .groups = 'drop')

top_restaurant_counts <- top_n(top_restaurant_counts,10,count)

ggplot(top_restaurant_counts, aes(x=factor(restaurant_name), y = count, fill = cuisine_type))+
  geom_bar(stat="identity", width=0.7) +
  coord_flip() + 
  theme_minimal() + 
  theme(plot.title = element_text(face="bold"), plot.subtitle = element_text(face="italic")) + 
  labs(title = "Shake Shack is NYC's Favorite Takeaway Restaurant ", subtitle = "(Data collected on FoodHub)", x = "Restaurant", y = "Number of Times Ordered" ) +
  scale_fill_manual(values=c("Italian" = "#ffbe0b", "Chinese" = "#ff006e", "American" = "#3a86ff", "Japanese" = "#8338ec"), name = "Cuisines")


#Vertical Bar Graph of Shake Shack compared to other American Restaurants
top_american_restaurant_counts <- nyc %>%
  filter(cuisine_type == "American") %>%
  group_by(restaurant_name, cuisine_type) %>%
  summarise(count = n(), .groups = 'drop')

top_american_restaurant_counts <- top_n(top_american_restaurant_counts,8,count)

top_american_restaurant_counts <- top_american_restaurant_counts %>%
  mutate(fill_color = ifelse(restaurant_name == "Shake Shack", "#3a86ff", "darkgrey"))

ggplot(top_american_restaurant_counts, aes(x = factor(restaurant_name), y = count, fill = fill_color)) +
  geom_bar(stat = "identity", width = 0.6) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"), plot.subtitle = element_text(face = "italic")) +
  labs(title = "Shake Shack Clears Other NYC American Restaurants in Number of Orders", subtitle = "(Data collected on FoodHub)", x = "American Restaurants", y = "Number of Times Ordered") +
  scale_fill_manual(values = c("#3a86ff", "darkgrey")) +
  guides(fill = FALSE)


#Graph of Top On-Average Revenue Per Order Restaurants on the App
average_order_costs <- nyc %>%
  group_by(restaurant_name, cuisine_type) %>%
  summarise(
    generated_revenue = sum(cost_of_the_order, na.rm = TRUE),
    total_orders = n(),
    average_cost = generated_revenue / total_orders,
    .groups = 'drop')

highest_orders <- average_order_costs[which.max(average_order_costs$total_orders),]
average_order_costs2 <- average_order_costs[average_order_costs$restaurant_name != highest_orders$restaurant_name,]
second_highest_orders <- average_order_costs2[which.max(average_order_costs2$total_orders),]

highest_cost <- average_order_costs[which.max(average_order_costs$average_cost),]
lowest_cost <- average_order_costs[which.min(average_order_costs$average_cost),]

ggplot(average_order_costs, aes(x = average_cost, y = total_orders, color = cuisine_type)) +
  geom_point() +
  geom_label(data = highest_orders, aes(label = restaurant_name), nudge_x = 0, nudge_y = 10, color = "black") +
  geom_segment(data = highest_orders, aes(xend = average_cost, yend = total_orders + 6), x = highest_orders$average_cost, y = highest_orders$total_orders, color = "black") +
  geom_label(data = second_highest_orders, aes(label = restaurant_name), nudge_x = 0, nudge_y = 10, color = "black") +
  geom_segment(data = second_highest_orders, aes(xend = average_cost, yend = total_orders + 6), x = second_highest_orders$average_cost, y = second_highest_orders$total_orders, color = "black") +
  geom_label(data = highest_cost, aes(label = restaurant_name), nudge_x = -5, nudge_y = 15, color = "black") +
  geom_segment(data = highest_cost, aes(xend = average_cost - 5, yend = total_orders + 11), x = highest_cost$average_cost, y = highest_cost$total_orders, color = "black") +
  geom_label(data = lowest_cost, aes(label = restaurant_name), nudge_x = 2, nudge_y = 20, color = "black") +
  geom_segment(data = lowest_cost, aes(xend = average_cost + 2, yend = total_orders + 16), x = lowest_cost$average_cost, y = lowest_cost$total_orders, color = "black") +
  scale_x_continuous(breaks = seq(0, 50, by = 10)) +
  theme_minimal() +
  theme(plot.title = element_text(face="bold"), plot.subtitle = element_text(face="italic")) + 
  labs(x = "Average Cost per Order (USD)", y = "Number of Orders", title = "Most Food Orders in NYC are around $15-20", subtitle = "(Data collected on FoodHub)") +
  scale_color_manual(name = "Cuisine Types", values = c("#3a86ff", "#ff006e", "#0D9276", "#FBA834", "#FCDC2A", "#A0153E", "#FF204E", "#EABE6C", "#6420AA", "#747264", "#4E4FEB", "#A5DD9B", "#FF6868", "#561C24"))


#Cost of the Order and Time Fufillment
time_to_average_order_costs <- nyc %>%
  group_by(restaurant_name, cuisine_type, delivery_time, food_preparation_time) %>%
  summarise(
    generated_revenue = sum(cost_of_the_order, na.rm = TRUE),
    total_orders = n(),
    average_cost = generated_revenue / total_orders,
    .groups = 'drop'
  )

time_to_average_order_costs$total_time = time_to_average_order_costs$food_preparation_time + time_to_average_order_costs$delivery_time

ggplot(time_to_average_order_costs, aes(x = average_cost, y = total_time, color = cuisine_type)) +
  geom_point() +
  theme_minimal() +
  theme(plot.title = element_text(face="bold"), plot.subtitle = element_text(face="italic")) + 
  labs(x = "Average Cost per Order (USD)", y = "Time to Fulfill Order", title = "No Clear Relationship between the Cost of the Order and the Time of Fufillment", subtitle = "(Data collected on FoodHub)") +
  scale_color_manual(name = "Cuisine Types", values = c("#3a86ff", "#ff006e", "#0D9276", "#FBA834", "#FCDC2A", "#A0153E", "#FF204E", "#EABE6C", "#6420AA", "#747264", "#4E4FEB", "#A5DD9B", "#FF6868", "#561C24"))


#Heat map of correlation of all variables
nyc_cor <- subset(nyc, select=c(cuisine_type, cost_of_the_order, day_of_the_week, food_preparation_time, delivery_time))

nyc_cor$day_of_the_week <- as.numeric(factor(nyc_cor$day_of_the_week))
nyc_cor$cuisine_type <- as.numeric(factor(nyc_cor$cuisine_type))

names(nyc_cor) <- c("Cuisine Type", "Cost of the Order", "Is a Weekday", "Food Preparation Time", "Delivery Time")

nyc_cor_mat <- round(cor(nyc_cor, use = "complete.obs"), 2)
melt_nyc_cor <- melt(nyc_cor_mat)

get_upper_tri <- function(cormat) {
  cormat[lower.tri(cormat)] <- NA
  return(cormat)
}

nyc_cor_mat_upper <- get_upper_tri(nyc_cor_mat)
melt_nyc_cor_upper <- melt(nyc_cor_mat_upper, na.rm = TRUE)

ggplot(data = melt_nyc_cor_upper, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "#66CCE6", high = "#6666E6", mid = "#6699E6", limit = c(-1, 1), name = "Correlation") +
  labs(title = "Correlation Heatmap of NYC Order Data", subtitle = "Including Cuisine Type, Cost, Day of the Week, Preparation and Delivery Time") +
  theme_minimal() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  geom_text(aes(label = value), color = "black", size = 4) +
  coord_flip()

###CUISINE TYPE ANALYSIS###

#Map of Cuisine and Number of Restaurants of that Cuisine
world_cuisine_data <- subset(nyc, select=c(restaurant_name, cuisine_type))

world_cuisine_data$region <- NA
cuisine_country_map <- c(
  "Chinese" = "China", "American" = "USA", "French" = "France",
  "Indian" = "India", "Italian" = "Italy", "Japanese" = "Japan",
  "Korean" = "South Korea", "Mediterranean" = "Greece", "Mexican" = "Mexico",
  "Middle Eastern" = "Lebanon", "Southern" = "USA", "Spanish" = "Spain",
  "Thai" = "Thailand", "Vietnamese" = "Vietnam")


world_cuisine_data$region <- cuisine_country_map[world_cuisine_data$cuisine_type]

world_cuisine_data <- world_cuisine_data %>% distinct()

world_cuisine_data <- world_cuisine_data %>% 
  group_by(region) %>% 
  summarise(count = n(), .groups = 'drop')

world_map <- map_data("world")

world_cuisine_map <- merge(world_map, world_cuisine_data, by.x = "region", by.y = "region", all.x = TRUE)
world_cuisine_map$count[is.na(world_cuisine_map$count)] <- 0

# Plot the map
ggplot(data=world_cuisine_map) +
  geom_polygon(aes(x=long, y=lat, group=group, fill=count)) +
  scale_fill_gradient(low = "darkgrey", high = "darkred") +
  labs(fill = "Number of Restaurants", title = "Cuisines Offered on FoodHub") +
  theme_minimal() +
  coord_fixed(1.2) + 
  guides(fill=FALSE)


ggplot(data=world_map)+
  geom_polygon(aes(x=long, y=lat, group=group, fill=region))+
  coord_fixed(1.2)+
  guides(fill=FALSE)


#Waffle Chart of Cuisine Order Count
cuisine_data <- subset(nyc, select=c(cuisine_type))

cuisine_counts <- cuisine_data %>%
  count(cuisine_type) %>%
  mutate(tiles = ceiling(n / 10))

cuisine_data <- subset(cuisine_counts, select=c(cuisine_type, tiles))
cuisine_data <- arrange(cuisine_data, desc(tiles))

waffle(cuisine_data, 
       colors=c("#3a86ff", "#A0153E", "#FCDC2A", "#ff006e", "#FBA834", "#6420AA", "#EABE6C", 
                "#747264", "#0D9276", "#FF204E", "#4E4FEB", "#A5DD9B", "#FF6868", "#561C24"), 
       rows=14, size=0.5, title="Types of Cuisine Ordered on FoodHub \n(1 tile represents 10 orders)", keep = FALSE, flip = TRUE)

###PREP AND FUFILLMENT TIME ANALYSIS####

#Pie Chart of Weekend vs Weekday
weekday_weekend <- subset(nyc, select=c(restaurant_name, day_of_the_week, food_preparation_time, delivery_time))

weekday_weekend$total_time <- weekday_weekend$delivery_time + weekday_weekend$food_preparation_time

weekday_weekend_count <- weekday_weekend %>%
  count(day_of_the_week = day_of_the_week) %>%
  mutate(percentage = n / sum(n) * 100)

ggplot(weekday_weekend_count, aes(x = "", y = n, fill = day_of_the_week)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar(theta = "y") + 
  geom_text(aes(label = paste0(round(percentage, 1), "%")), position = position_stack(vjust = 0.5)) +
  labs(fill = "Day Type", title = "Food Delivery is Much More Popular on Weekends", subtitle = "(Data collected on FoodHub)") +
  theme_void() +
  theme(plot.title = element_text(face="bold"), plot.subtitle = element_text(face="italic")) + 
  scale_fill_manual(values = c("Weekday" = "#3a86ff", "Weekend" = "#FF6868"), name="Day of the Week")

#Predict Time Model, Box Plot; Weekend vs Weekday











###REDACTED PLOTS###

#Fastest and Slowest Restaurants

#Waffle Chart of All Restaurants and their Cuisine Types offered on the App

average_order_costs <- nyc %>%
  group_by(restaurant_name, cuisine_type) %>%
  summarise(
    generated_revenue = sum(cost_of_the_order, na.rm = TRUE),
    total_orders = n(),
    average_cost = generated_revenue / total_orders,
    .groups = 'drop')

highest_orders <- average_order_costs[which.max(average_order_costs$total_orders),]
highest_cost <- average_order_costs[which.max(average_order_costs$average_cost),]
lowest_cost <- average_order_costs[which.min(average_order_costs$average_cost),]


ggplot(average_order_costs, aes(x = total_orders, y = average_cost, color = cuisine_type)) +
  geom_point() +
  geom_label(data = highest_orders, aes(label = restaurant_name), nudge_x = 15, nudge_y = 1, color = "black") +
  geom_label(data = highest_cost, aes(label = restaurant_name), nudge_x = 15, nudge_y = 1, color = "black") +
  geom_label(data = lowest_cost, aes(label = restaurant_name), nudge_x = 15, nudge_y = 1, color = "black") +
  geom_segment(data = highest_orders, aes(xend = total_orders + 14.9, yend = average_cost + 0.5), x = highest_orders$total_orders, y = highest_orders$average_cost, color = "black") +
  geom_segment(data = highest_cost, aes(xend = total_orders + 14.9, yend = average_cost + 0.5), x = highest_cost$total_orders, y = highest_cost$average_cost, color = "black") +
  geom_segment(data = lowest_cost, aes(xend = total_orders + 14.9, yend = average_cost + 0.5), x = lowest_cost$total_orders, y = lowest_cost$average_cost, color = "black") +
  theme_minimal() +
  labs(x = "Number of Orders", y = "Average Cost per Order (USD)", title = "Most Food Orders in NYC are around $15-20", subtitle = "(Data collected on FoodHub)") +
  scale_color_manual(name = "Cuisine Types", values = c("#3a86ff", "#ff006e", "#0D9276", "#FBA834", "#FCDC2A", "#A0153E", "#FF204E", "#EABE6C", "#6420AA", "#747264", "#4E4FEB", "#A5DD9B", "#FF6868", "#561C24"))



world_map <- st_read("~/Developer/dsci304/final/world-map")


world_cuisine_map <- full_join(world_map, world_cuisine_data, by = "CNTRY_NAME")
world_cuisine_map$count[is.na(world_cuisine_map$count)] <- 0



ggplot(world_cuisine_map) +
  geom_sf(aes(fill = count), color = "black") + 
  coord_sf() +  
  theme_minimal() + 
  ggtitle("Cuisines Offered on FoodHub") +
  labs(fill = "Number of Restaurants") +
  scale_fill_distiller(palette = "Greens", direction = 1, na.value = "white")

ggplot(world_cuisine_map)+
  geom_sf(aes(fill=count), color="black")+
  coord_sf()+
  theme(axis.line=element_blank(),axis.text.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.background=element_blank(),panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),plot.background=element_blank())+
  ggtitle("Cuisines Offered on FoodHub")+
  labs(fill="Number of Restaurants")+
  scale_fill_distiller(palette="Greens", direction=1)

