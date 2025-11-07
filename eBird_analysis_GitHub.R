rm(list=ls())
library(ggplot2)
library(dplyr)
library(geosphere)
library(jtools)
library(gridExtra)
library(stringr)

#Directory for output plots
plot_dir <- "/path/"

data_dir <- "/path/"

#Read in hirundine ebird data
all_hirs <- read.csv(paste(data_dir, "Ebird_hirundine_200_data.csv", sep=""))
head(all_hirs)

#Read in radar station locations
all_stats <- read.csv(paste(data_dir, "radar_station_locations.csv", sep=""))
head(all_stats)


#Read in non-hirundine ebird data
all_non_hirs <- read.csv(paste(data_dir, "Ebird_nonhirundine_200_data.csv", sep=""))
head(all_non_hirs)

###
#Read in radar data
radar_processed_data0 <- read.csv(paste(data_dir, "Full_processed_radar_wind_data.csv", sep=""))
radar_processed_data <- radar_processed_data0[!is.na(radar_processed_data0$bearing_first_last),] #subset - 1 row per flock trajectory
head(radar_processed_data)
###

# Function to calculate nearest point and its distance for each row in df1
find_nearest <- function(lat1, lon1, lat2, lon2, id2) {
  # Calculate Haversine distances between (lat1, lon1) and all points in df2
  distances <- distHaversine(c(lon1, lat1), cbind(lon2, lat2))
  
  # Find the index of the minimum distance
  min_index <- which.min(distances)
  
  # Return the nearest ID and the corresponding distance
  return(c(id2[min_index], distances[min_index]))
}

# Find the distances of ebird observations to radar stations
all_hirs_new <- all_hirs %>%
  rowwise() %>%
  mutate(
    nearest_station = find_nearest(LATITUDE, LONGITUDE, all_stats$LATITUDE, all_stats$LONGITUDE,
                              all_stats$Station.Code)[1],
    distance_to_station = as.numeric(find_nearest(LATITUDE, LONGITUDE, all_stats$LATITUDE, all_stats$LONGITUDE,
                                       all_stats$Station.Code)[2])
  )

all_non_hirs_new <- all_non_hirs %>%
  rowwise() %>%
  mutate(
    nearest_station = find_nearest(LATITUDE, LONGITUDE, all_stats$LATITUDE, all_stats$LONGITUDE,
                                   all_stats$Station.Code)[1],
    distance_to_station = as.numeric(find_nearest(LATITUDE, LONGITUDE, all_stats$LATITUDE, all_stats$LONGITUDE,
                                                  all_stats$Station.Code)[2])
  )

ggplot(all_hirs_new, aes(distance_to_station))+
  geom_histogram()+
  xlab("Distance in metres to nearest RADAR station (m)")+
  ylab("Frequency")+
  theme_nice()+
  scale_x_log10(labels = scales::comma)


################

#Subset for observations within range of radar stations
radar_station_range <- 150000

all_hirs_new_station <- all_hirs_new[all_hirs_new$distance_to_station<radar_station_range,]
all_non_hirs_new_station <- all_non_hirs_new[all_non_hirs_new$distance_to_station<radar_station_range,]

#Number of observations
summary(as.factor(all_hirs_new_station$COMMON.NAME))
summary(as.factor(all_non_hirs_new_station$COMMON.NAME))

############ MAKE FIGURE

all_hirs_new_station$COMMON.NAME_sentencecase <- str_to_sentence(all_hirs_new_station$COMMON.NAME)

date_species <- ggplot(all_hirs_new_station, 
                       aes(julian_date, fill=COMMON.NAME_sentencecase))+
  geom_histogram(col="black")+
  xlab("Julian date")+
  ylab("Frequency")+
  scale_fill_manual(name=NULL,
    values=viridis::viridis(5, option = "D"),
    guide = guide_legend(nrow = 5))+
  theme_nice()+
  ylim(0, 1000)+
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.text = element_text(size = 12),
        plot.tag = element_text(color = "black", size = 14)
  )+  labs(tag = "B")
date_species

all_non_hirs_new_station$COMMON.NAME_sentencecase <- str_to_sentence(all_non_hirs_new_station$COMMON.NAME)

date_species_nonhirs <- ggplot(all_non_hirs_new_station, 
                               aes(julian_date, fill = COMMON.NAME_sentencecase)) +
  geom_histogram(col="black") +
  xlab("Julian date") +
  ylab("Frequency") +
  scale_fill_manual(
    name = NULL,
    values = viridis::viridis(4, option = "F")[1:3],
    labels = c(
      "Common   \ngrackle",
      "European   \nstarling",
      "Red-winged   \nblackbird"
    ),
    guide = guide_legend(nrow = 3)
  ) +
  theme_nice() +
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    legend.text = element_text(size = 12),
    plot.tag = element_text(color = "black", size = 14)
  ) +
  labs(tag = "C")

date_species_nonhirs

date_radar <- ggplot(radar_processed_data, aes(julian_date))+
  geom_histogram(fill="lightskyblue2", col="black")+
  xlab("Julian date")+
  ylab("Frequency")+
  theme_nice()+
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.text = element_text(size = 12),
        plot.tag = element_text(color = "black", size = 14)
        )+
  labs(tag = "A")
date_radar


ebird_out <- grid.arrange(
  date_radar,
  arrangeGrob(date_species, date_species_nonhirs, nrow=2),
  ncol = 2,
  widths = c(1, 1.2)
)
ebird_out

#ggsave(filename = paste0(plot_dir, "Figure2.png"),plot = ebird_out,width = 10,height = 5.5,dpi = 300)



######## FIND DATE WHEN OBSERVATIONS DOMINATED BY TREE SWALLOWS


all_hirs_new_station$tree_swallow_YN <- ifelse(all_hirs_new_station$COMMON.NAME=="Tree Swallow", 1, 0)

date_props <- all_hirs_new_station %>%
  filter(julian_date >180 & julian_date < 300 
  ) %>%
  group_by(julian_date) %>%
  summarise(prop_tree_swallow = sum(tree_swallow_YN)/n(),
            n = n(),
            .groups = 'drop')
as.data.frame(date_props)

plot(prop_tree_swallow~julian_date, data=date_props, type='l',
     xlab="Julian date", ylab="Proportion of tree swallows")

min(date_props$julian_date[date_props$prop_tree_swallow>0.95])
#Date after which tree swallows are more than 95% of observations on any given day
