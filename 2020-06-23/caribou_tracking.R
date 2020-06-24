# Caribou Locations
# TidyTuesday: 2020-06-23
# Author: Ryan Martin

library(tidyverse)
library(gganimate)
library(ggmap)
library(RColorBrewer)
library(magick)

# Read in dataset
locations <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-23/locations.csv')

# Get rid of the hours, minutes and seconds to the timestamp
locations$timestamp <- as.Date(strftime(locations$timestamp, format="%Y-%m-%d"))

# Fine the range for the longitude and latitude in the dataset
long <- c(min(locations$longitude), max(locations$longitude))
lat <- c(min(locations$latitude), max(locations$latitude))

# Get the region from ggmap
map_animals <- ggmap(get_stamenmap(bbox = make_bbox(lat=lat, lon=long), crop = F, zoom = 7, maptype = 'terrain')) 

# Add geom_path for each animal, coloured by the study_site 
map_animals <- map_animals +
  geom_path(data = locations, aes(x = longitude, y = latitude, color = study_site, group = interaction(animal_id, study_site))) +
  theme(legend.position = 'none') +
  scale_color_brewer(palette = 'Dark2') +
  labs(x = 'Longitude',
       y = 'Latitude',
       title = 'Movement of each caribou during tracking period \n Date: {frame_time}') +
  transition_time(timestamp) +
  shadow_mark(past = T, alpha = 0.1)

# Render gif for map with animal tracking
map_animals_anim <- animate(map_animals, renderer = gifski_renderer())

# Function to determine the cumulative number of unique caribou tracked
# over the entire timespan in the dataset
# Applied to each study_location independently
number_unique_animals <- function(x) {
  # List of dates for the study_site
  dates <- sort(unique(x$timestamp))
  # Get name of site currently on
  site <- unique(x$study_site)
  # For loop to collect all unique animals at each time point
  animals <- list()
  for (i in seq_along(dates)) {
    animals[[i]] <- unique(x[x$timestamp == dates[i], ]$animal_id)
  }
  names(animals) <- dates

  num_animals <- list()
  unique_animals <- c()
  for (i in seq_along(animals)) {
    potential <- animals[[i]]
    # For each time point, compare to the previous animals identified
    new <- setdiff(potential, unique_animals)
    # If new animals identified, add to the unique_animals vector
    unique_animals <- c(unique_animals, new)
    # Save the length of the unique_animals, this is the number of animals identified up to that date
    num_animals[[i]] <- length(unique_animals)
  }
  names(num_animals) <- dates
  # Collapse list in dataframe
  num_animals <- plyr::ldply(num_animals)
  # Add a column with the study_site on each row
  num_animals$study_site <- site
  # Rename columns
  colnames(num_animals) <- c('Date', 'count', 'study_site')
  # Reformat Date column from string to Date variable
  num_animals$Date <- as.Date(num_animals$Date)
  # Return the data.frame with the cumulative number of unique animals 
  return(num_animals)
  
}


total_animals <- locations %>%
  # Split in a separate dataframe for each study site
  split(., f = .$study_site) %>%
  # Calculate cumulative unique animals
  map_dfr(number_unique_animals) %>% 
  # Pivot to wide format to be able to have same time point for each study_site
  pivot_wider(names_from = study_site, values_from = count) %>%
  # Arrange by date
  arrange(Date)

# If no animals at a study_site on the earliest overal time point, input a 0
total_animals[1,] <- ifelse(is.na(total_animals[1,]), 0, total_animals[1,])

# Function to fill in NAs with value from the previous time point
total_animals <- data.table::setnafill(total_animals, type = "locf")

# Pivot back to long format for plotting
total_animals <- total_animals %>%
  pivot_longer(cols = 2:ncol(.), values_to = 'count', names_to = 'study_site')

# Bar graph of cumulative sum of unique animals tracked
site_count <- ggplot(total_animals, aes(x = study_site, y = count, fill = study_site)) +
  scale_fill_brewer(palette = 'Dark2') +
  geom_col(position = "identity") +
  ggpubr::theme_pubr() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = 'none') +
  labs(x = 'Study Site',
       y = 'Total Number of Unique Caribou',
       title = 'Total Number of unique caribou tracked at each study site \n Date: {frame_time}', 
       caption = 'Data Source: B.C. Ministry of Environment & Climate Change \n Map: ggmap \n Author: @ryan4martin')+
  ylim(0, 100) +
  transition_time(Date) 

# Animate the bar graph
site_count_anim <- animate(site_count, renderer = gifski_renderer())

# Use magick library to combine the two gifs
map_animals_mgif <- image_read(map_animals_anim)
site_count_mgif <- image_read(site_count_anim)

# Combine first frame of each gif
combined_gif <- image_append(c(map_animals_mgif[1], site_count_mgif[1]))
# Iterate through each frame, combine them and add to growing gif
for(i in 2:100){
  combined <- image_append(c(map_animals_mgif[i], site_count_mgif[i]))
  combined_gif <- c(combined_gif, combined)
}

# Save gif
anim_save("caribou_tracking.gif", combined_gif)
