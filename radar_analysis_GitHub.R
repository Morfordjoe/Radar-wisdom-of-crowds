rm(list=ls())
library(ggplot2)
library(circular)
library(dplyr)
library(tidyr)
library(jtools)
library(gridExtra)
library(png)
library(grid)
library(mgcv)
library(cowplot)




dir <- "/path/" #Directory with dataset
plot_dir <- "/path/" #Directory for output pots

#Loading data
radar_processed_data0 <- read.csv(paste(dir, "Full_processed_radar_wind_data.csv", sep=""))
radar_processed_data <- radar_processed_data0[!is.na(radar_processed_data0$bearing_first_last),]
head(radar_processed_data)


radar_processed_data$bearing_of_interest <- radar_processed_data$bearing_first_last 
#bearing for analysis is between centroids of first and last flock detection
radar_processed_data$bearing_of_interest_air_frame <- radar_processed_data$bearing_first_last_air_frame
#bearing in air frame of reference(wind-corrected)
radar_processed_data$group_size_of_interest <- radar_processed_data$mean_group_size
#group size for analysis - mean group size over track

############# ROBUSTNESS CHECK: CUTTING DATA - JUST TREE SWALLOWS
#radar_processed_data <- radar_processed_data[which(radar_processed_data$julian_date>260),]
# > Day 260 for >95% Tree Swallow cut-off
################

############# ROBUSTNESS CHECK: SUBSTITUTING IN THE FIRST BEARING READING OF EACH GROUP RATHER THAN FIRST TO LAST
#radar_processed_data$bearing_of_interest <- radar_processed_data$bearing ############ 
#radar_processed_data$bearing_of_interest_air_frame <- radar_processed_data$bearing_air_frame ############ 
################

############# ROBUSTNESS CHECK: SUBSTITUTING IN THE MAX GROUP SIZE RATHER THAN MEAN GROUP SIZE
#radar_processed_data$group_size_of_interest <- radar_processed_data$max_group_size ############ 
################


#Dates represented in dataset
hist(radar_processed_data$julian_date, xlab="Julian date", main=NA)

#Flock sizes distribution
group_size <- ggplot(radar_processed_data, aes(group_size_of_interest))+
  geom_histogram(fill="lightskyblue2")+
  scale_x_log10(labels = scales::comma)+
  xlab("Flock size")+
  ylab("Frequency")+
  theme_nice()+
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 11) )
group_size

#Assign flock sizes to quantiles
group_size_quantiles <- quantile(radar_processed_data$group_size_of_interest)
radar_processed_data$group_size_quant <- as.factor(ifelse(radar_processed_data$group_size_of_interest<group_size_quantiles[2], 1,
                                                          ifelse(radar_processed_data$group_size_of_interest<group_size_quantiles[3], 2, 
                                                                 ifelse(radar_processed_data$group_size_of_interest<group_size_quantiles[4], 3,
                                                                        4))))

#Roosts on map
all_roosts <- ggplot(radar_processed_data) +
  geom_point(aes(x = aea_lon, y = aea_lat, color = cluster_id)) + 
  geom_point(aes(x = cluster_center_lon_aea, y = cluster_center_lat_aea, 
                 color = cluster_id)) + 
  theme_nice()+
  theme(legend.position = "none")+
  xlab(NULL)+
  ylab(NULL)

roost_clusters <- ggplot(radar_processed_data) +
  geom_point(aes(x = cluster_center_lon_aea, y = cluster_center_lat_aea, 
                 color = cluster_id)) + 
  theme_nice()+
  theme(legend.position = "none")+
  xlab(NULL)+
  ylab(NULL)

grid.arrange(all_roosts, roost_clusters, ncol=2)


#Mean direction of flock departures
mean_dir <- as.numeric(180*mean.circular(pi*radar_processed_data$bearing_of_interest/180, na.rm=T)/pi)
mean_dir
sd_dir <- as.numeric(180*sd.circular(pi*radar_processed_data$bearing_of_interest/180, na.rm=T)/pi)
sd_dir

#Distribution of departure directions
all_dirs_plot <- ggplot(radar_processed_data, aes(x = bearing_of_interest)) +
  geom_histogram(fill = "grey", color = NA) + 
  coord_polar(start = pi) +  # Rotate the plot so 0 degrees is at the top (North)
  geom_vline(xintercept = c(mean_dir, mean_dir-sd_dir, mean_dir+sd_dir-360), 
             color = c("black", "black", "black"), 
             linetype = c("solid", "dashed", "dashed"), linewidth = c(0.75))+
  theme_minimal()+
  scale_x_continuous(limits = c(-180, 180), breaks=seq(-90, 180, by=90), labels=NULL)+
  scale_y_continuous(labels=NULL, breaks=NULL) +  
  xlab("Departure directions\nGround reference frame")+ ylab("")+ 
  theme(panel.grid.major = element_line(color = "grey55", size = 0.15),
        panel.grid.minor = element_line(color = "grey55", size = 0.15))
all_dirs_plot


######### WIND - flock movements / wind correlations


u_wind_plot <- ggplot(data=radar_processed_data, aes(u_wind_value, u_dist_moved_first_last))+
  geom_point(size=0.2, color=alpha("black", 0.2))+
  theme_nice()+
  xlab("u wind (m/s)")+
  ylab("u distance moved (m)")
u_wind_plot
cor.test(radar_processed_data$u_dist_moved_first_last, radar_processed_data$u_wind_value,
         method="spearman")

v_wind_plot <- ggplot(data=radar_processed_data, aes(v_wind_value, v_dist_moved_first_last))+
  geom_point(size=0.2, color=alpha("black", 0.2))+
  theme_nice()+
  xlab("v wind (m/s)")+
  ylab("v distance moved (m)")
v_wind_plot
cor.test(radar_processed_data$v_dist_moved_first_last, radar_processed_data$v_wind_value,
         method="spearman")


#Correlations abolished in air frame of reference
u_wind_af_plot <- ggplot(data=radar_processed_data, aes(u_wind_value, u_dist_moved_first_last_air_frame))+
  geom_point(size=0.2, color=alpha("red", 0.2))+
  theme_nice()+
  xlab("u wind (m/s)")+
  ylab("u distance moved in air r.f. (m)")
u_wind_af_plot
cor.test(radar_processed_data$u_dist_moved_first_last_air_frame, radar_processed_data$u_wind_value,
         method="spearman")


v_wind_af_plot <- ggplot(data=radar_processed_data, 
                         aes(v_wind_value, v_dist_moved_first_last_air_frame))+
  geom_point(size=0.2, color=alpha("red", 0.2))+
  theme_nice()+
  xlab("v wind (m/s)")+
  ylab("v distance moved in air r.f. (m)")
v_wind_af_plot
cor.test(radar_processed_data$v_dist_moved_first_last_air_frame, radar_processed_data$v_wind_value,
         method="spearman")


################# DIRECTIONAL DISTRIBUTION IN AIR RF

mean_dir_af <- as.numeric(
  180*mean.circular(pi*radar_processed_data$bearing_of_interest_air_frame/180, na.rm=T)/pi)
sd_dir_af <- as.numeric(
  180*sd.circular(pi*radar_processed_data$bearing_of_interest_air_frame/180, na.rm=T)/pi)
all_dir_af_plot <- ggplot(radar_processed_data, aes(x = bearing_of_interest_air_frame)) +
  geom_histogram(fill = "burlywood2", color = NA) + 
  coord_polar(start = pi) +  # Rotate the plot so 0 degrees is at the top (North)
  theme_minimal()+
  geom_vline(xintercept = c(mean_dir_af, mean_dir_af-sd_dir_af, mean_dir_af+sd_dir_af-360), 
             color = c("darkorange3", "darkorange3", "darkorange3"), 
             linetype = c("solid", "dashed", "dashed"), linewidth = c(0.75))+
  scale_x_continuous(limits = c(-180, 180), breaks=seq(-90, 180, by=90), labels=NULL)+
  scale_y_continuous(labels=NULL, breaks=NULL) +  
  xlab("Departure directions\nAir reference frame")+ ylab("")+
  theme(panel.grid.major = element_line(color = "grey55", size = 0.15),
        panel.grid.minor = element_line(color = "grey55", size = 0.15))
all_dir_af_plot

mean_dir_af
sd_dir_af


############ ROLLING MEAN DIRECTION - by date in season

rolling_circular_stats_unique_vals <- function(circular_data, numeric_data, window_size) {
  unique_ns <- unique(numeric_data)[order(unique(numeric_data))]
  n <- length(unique_ns)
  circular_means <- rep(NA, n)
  circular_sds <- rep(NA, n)
  
  for (i in 1:n) {
    neighbor_circular_data <- circular_data[which(numeric_data >= unique_ns[i] - window_size &
                                                    numeric_data <= unique_ns[i] + window_size)]
    
    circular_means[i] <- 360*mean.circular(2*pi*neighbor_circular_data/360, na.rm=T)/(2*pi)
    circular_sds[i] <- 360*sd.circular(2*pi*neighbor_circular_data/360, na.rm=T)/(2*pi)
    
  }
  circular_means <- ifelse(circular_means<0, 360+circular_means, circular_means)
  return(list(numeric_out = unique_ns, mean = circular_means, sd = circular_sds))
}

data_1_99_percentiles <- quantile(radar_processed_data$julian_date, c(0.01, 0.99))
data_1_99_percentiles

#In ground reference frame
rolling_stats_by_date_gf <- as.data.frame(
  rolling_circular_stats_unique_vals(radar_processed_data$bearing_of_interest[radar_processed_data$julian_date>data_1_99_percentiles[1]
                                                                             & radar_processed_data$julian_date<data_1_99_percentiles[2]], 
                                     radar_processed_data$julian_date[radar_processed_data$julian_date>data_1_99_percentiles[1]
                                                                      & radar_processed_data$julian_date<data_1_99_percentiles[2]], 
                                     window_size = 14))

date_bearing_gf <- ggplot(data=rolling_stats_by_date_gf, aes(numeric_out, mean))+
  geom_line(color="black")+
  theme_nice()+
  xlab("Julian date")+
  ylab("Bearing")
date_bearing_gf

#In air reference frame
rolling_stats_by_date_af <- as.data.frame(
  rolling_circular_stats_unique_vals(radar_processed_data$bearing_of_interest_air_frame[radar_processed_data$julian_date>data_1_99_percentiles[1]
                                                                                       & radar_processed_data$julian_date<data_1_99_percentiles[2]],
                                     radar_processed_data$julian_date[radar_processed_data$julian_date>data_1_99_percentiles[1]
                                                                      & radar_processed_data$julian_date<data_1_99_percentiles[2]], 
                                     window_size = 14))
date_bearing_af<- ggplot(data=rolling_stats_by_date_af, aes(numeric_out, mean))+
  geom_line(color="red")+
  theme_nice()+
  xlab("Julian date")+
  ylab("Bearing - air r.f.")
date_bearing_af

radar_processed_data$mean_dir_for_day_af <- NA
for (i in 1:length(rolling_stats_by_date_af$numeric_out)){
  radar_processed_data$mean_dir_for_day_af[which(radar_processed_data$julian_date==
                                                rolling_stats_by_date_af$numeric_out[i])] <- 
    rolling_stats_by_date_af$mean[i]
}

radar_processed_data$mean_dir_for_day_gf <- NA
for (i in 1:length(rolling_stats_by_date_gf$numeric_out)){
  radar_processed_data$mean_dir_for_day_gf[which(radar_processed_data$julian_date==
                                                        rolling_stats_by_date_gf$numeric_out[i])] <- 
    rolling_stats_by_date_gf$mean[i]
}


############# DEFLECTION CALCULATIONS

#Population mean directions - the baselines
global_mean_ref_af <- 180*mean.circular(pi*radar_processed_data$bearing_of_interest_air_frame/180, na.rm=T)/pi
global_mean_ref_gf <- 180*mean.circular(pi*radar_processed_data$bearing_of_interest/180, na.rm=T)/pi


deflection_0_180_func <- function(var){
  out <- ifelse(var > 180,
         -360 + (var),
         ifelse(var < (-180),
                360 + var,
                var))
  return(out)
}

#Deflection corrected for season in air RF
radar_processed_data$deflection_af <- deflection_0_180_func(radar_processed_data$mean_dir_for_day_af - radar_processed_data$bearing_of_interest_air_frame)
#Deflection uncorrected for season in air RF
radar_processed_data$deflection_globalm_af <- deflection_0_180_func(global_mean_ref_af - radar_processed_data$bearing_of_interest_air_frame)

#Deflection corrected for season in ground RF
radar_processed_data$deflection_gf <- deflection_0_180_func(radar_processed_data$mean_dir_for_day_gf - radar_processed_data$bearing_of_interest)
#Deflection corrected for season in air RF
radar_processed_data$deflection_globalm_gf <- deflection_0_180_func(global_mean_ref_gf - radar_processed_data$bearing_of_interest)


############# COLLECTIVE INTELLIGENCE

#Correlations - error and group size
cor.test(abs(radar_processed_data$deflection_globalm_af), 
         log10(radar_processed_data$group_size_of_interest), method="spearman")
cor.test(abs(radar_processed_data$deflection_globalm_gf), 
         log10(radar_processed_data$group_size_of_interest), method="spearman")

#Sample size
length(which(!is.na(radar_processed_data$deflection_globalm_af)))
length(which(!is.na(radar_processed_data$deflection_globalm_gf)))
length(which(!is.na(log10(radar_processed_data$group_size_of_interest))))

#Seasonally corrected results
cor.test(abs(radar_processed_data$deflection_af), 
         log10(radar_processed_data$group_size_of_interest), method="spearman")
cor.test(abs(radar_processed_data$deflection_gf), 
         log10(radar_processed_data$group_size_of_interest), method="spearman")

length(which(!is.na(radar_processed_data$deflection_af)))
length(which(!is.na(radar_processed_data$deflection_gf)))



############### COLLECTIVE INTELLIGENCE GAMS


gam_model_globalm_gf <- gam(abs(deflection_globalm_gf) ~ s(log(group_size_of_interest)), 
                 data = radar_processed_data, method = "REML")
summary(gam_model_globalm_gf)
plot(gam_model_globalm_gf, shade = TRUE, rug = TRUE)

gam_model_gf_correctedseason <- gam(abs(deflection_gf) ~ s(log(group_size_of_interest)), 
                            data = radar_processed_data, method = "REML")
summary(gam_model_gf_correctedseason)
plot(gam_model_gf_correctedseason, shade = TRUE, rug = TRUE)

gam_model_globalm_af <- gam(abs(deflection_globalm_af) ~ s(log(group_size_of_interest)), 
                 data = radar_processed_data, method = "REML")
summary(gam_model_globalm_af)
plot(gam_model_globalm_af, shade = TRUE, rug = TRUE)

gam_model_af_correctedseason <- gam(abs(deflection_af) ~ s(log(group_size_of_interest)), 
                    data = radar_processed_data, method = "REML")
summary(gam_model_af_correctedseason)
plot(gam_model_af_correctedseason, shade = TRUE, rug = TRUE)

flock_size_99_percentiles <- quantile(radar_processed_data$group_size_of_interest, c(0.01, 0.99)) #for plotting window

###### PLOT GAMS

group_seq <- data.frame(group_size_of_interest = seq(quantile(radar_processed_data$group_size_of_interest, 0.01),
                                              quantile(radar_processed_data$group_size_of_interest, 0.99),
                                              length.out = 200))

# Add log10-transformed group size (as expected by the model)
group_seq$log10_group_size <- log10(group_seq$group_size_of_interest)

# Predict using the model (with standard errors)
predictions_gf <- predict(gam_model_globalm_gf, newdata = group_seq, se.fit = TRUE)
predictions_af <- predict(gam_model_globalm_af, newdata = group_seq, se.fit = TRUE)

############## ALTERNATIVE: PLOT SEASONALLY CORRECTED GAMS:
#predictions_gf <- predict(gam_model_gf_correctedseason, newdata = group_seq, se.fit = TRUE)
#predictions_af <- predict(gam_model_af_correctedseason, newdata = group_seq, se.fit = TRUE)
#######################################

# Add predictions to the data frame
group_seq$fit_gf <- predictions_gf$fit
group_seq$se_gf <- predictions_gf$se.fit
group_seq$upper_gf <- group_seq$fit_gf + 2 * group_seq$se_gf
group_seq$lower_gf <- group_seq$fit_gf - 2 * group_seq$se_gf

group_seq$fit_af <- predictions_af$fit
group_seq$se_af <- predictions_af$se.fit
group_seq$upper_af <- group_seq$fit_af + 2 * group_seq$se_af
group_seq$lower_af <- group_seq$fit_af - 2 * group_seq$se_af

group_seq_long <- group_seq %>%
  pivot_longer(
    cols = c(fit_af, lower_af, upper_af, fit_gf, lower_gf, upper_gf),
    names_to = c(".value", "type"),
    names_pattern = "(fit|lower|upper)_(af|gf)"
  )

group_seq_long$type <- factor(group_seq_long$type, levels = c("gf", "af"))

gam_out_CI <- ggplot(group_seq_long, aes(x = group_size_of_interest, y = fit, color = type, fill = type)) +
  geom_line(linewidth = 1) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, color = NA) +
  coord_cartesian(
    xlim = flock_size_99_percentiles,
    ylim = c(48, 73)) +
  scale_y_continuous(breaks = seq(0, 90, by=10)) +
  scale_x_continuous(labels = scales::comma) +
  scale_color_manual(name = NULL,
                     values = c("gf" = "black", "af" = "darkorange3"),
                     labels = c("Ground reference frame", "Air reference frame"),
                     guide = guide_legend(nrow = 1)
  ) +
  scale_fill_manual(name = NULL,
                    values = c("gf" = "black", "af" = "darkorange3"),
                    labels = c("Ground reference frame", "Air reference frame")) +
  ylab("Absolute directional error (°)") +
  xlab("Flock size") +
  theme_nice()+
  theme(
    #legend.position = c(0.96, 0.96),  # x and y coordinates (0 = left/bottom, 1 = right/top)
    legend.position = "bottom",  
    legend.justification = c("right", "top"),  # anchor point of legend box
    legend.background = element_rect(fill = alpha("white", 0.7), color = NA),
    legend.text = element_text(size = 12),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 11))


gam_out_CI

####################### CIRCULAR PLOTS CI GF - QUANTILES

q1_mean_gf <- 180*mean.circular(pi*radar_processed_data$bearing_of_interest[radar_processed_data$group_size_quant==1]/180,
                                na.rm=T)/pi
q1_sd_gf <- 180*sd.circular(pi*radar_processed_data$bearing_of_interest[radar_processed_data$group_size_quant==1]/180,
                            na.rm=T)/pi
q2_mean_gf <- 180*mean.circular(pi*radar_processed_data$bearing_of_interest[radar_processed_data$group_size_quant==2]/180,
                                na.rm=T)/pi
q2_sd_gf <- 180*sd.circular(pi*radar_processed_data$bearing_of_interest[radar_processed_data$group_size_quant==2]/180,
                            na.rm=T)/pi
q3_mean_gf <- 180*mean.circular(pi*radar_processed_data$bearing_of_interest[radar_processed_data$group_size_quant==3]/180,
                                na.rm=T)/pi
q3_sd_gf <- 180*sd.circular(pi*radar_processed_data$bearing_of_interest[radar_processed_data$group_size_quant==3]/180,
                            na.rm=T)/pi
q4_mean_gf <- 180*mean.circular(pi*radar_processed_data$bearing_of_interest[radar_processed_data$group_size_quant==4]/180,
                                na.rm=T)/pi
q4_sd_gf <- 180*sd.circular(pi*radar_processed_data$bearing_of_interest[radar_processed_data$group_size_quant==4]/180,
                            na.rm=T)/pi

q1_sd_gf
q4_sd_gf

q1_gf <- ggplot(radar_processed_data, 
                aes(x = bearing_of_interest, fill=group_size_quant)) +
  geom_histogram(color = NA, position = "identity") + 
  coord_polar(start = pi) +  # Rotate the plot so 0 degrees is at the top (North)
  theme_minimal()+
  geom_vline(xintercept = c(q1_mean_gf, q1_mean_gf-q1_sd_gf, q1_mean_gf+q1_sd_gf-360), 
             color = c("black"), 
             linetype = c("solid", "dashed", "dashed"), linewidth = c(0.75))+
  theme(legend.position = "none")+
  scale_y_continuous(labels=NULL, breaks=NULL) +  
  scale_x_continuous(limits = c(-180, 180), breaks=seq(-90, 180, by=90), labels=NULL)+
  xlab("1st quartile: < 1500 birds\nGround reference frame")+
  ylab(NULL)+
  scale_fill_manual(values = c("1" = "grey",  # Assign colors for each factor level
                               "2" = "transparent",
                               "3" = "transparent",
                               "4" = "transparent"))+
  theme(panel.grid.major = element_line(color = "grey55", size = 0.15),
        panel.grid.minor = element_line(color = "grey55", size = 0.15))


q2_gf <- ggplot(radar_processed_data, 
                aes(x = bearing_of_interest, fill=group_size_quant)) +
  geom_histogram(color = NA, position = "identity") + 
  coord_polar(start = pi) +  # Rotate the plot so 0 degrees is at the top (North)
  geom_vline(xintercept = c(q2_mean_gf, q2_mean_gf-q2_sd_gf, q2_mean_gf+q2_sd_gf-360), 
             color = c("black"), 
             linetype = c("solid", "dashed", "dashed"), linewidth = c(0.75))+
  theme_minimal()+
  theme(legend.position = "none")+
  scale_y_continuous(labels=NULL, breaks=NULL) +  
  scale_x_continuous(limits = c(-180, 180), breaks=seq(-90, 180, by=90), labels=NULL)+
  xlab("2nd quartile: 1500 - 3300 birds\nGround reference frame")+
  ylab(NULL)+
  scale_fill_manual(values = c("1" = "transparent",  # Assign colors for each factor level
                               "2" = "grey",
                               "3" = "transparent",
                               "4" = "transparent"))+
  theme(panel.grid.major = element_line(color = "grey55", size = 0.15),
        panel.grid.minor = element_line(color = "grey55", size = 0.15))

q3_gf <- ggplot(radar_processed_data, 
                aes(x = bearing_of_interest, fill=group_size_quant)) +
  geom_histogram(color = NA, position = "identity") + 
  coord_polar(start = pi) +  # Rotate the plot so 0 degrees is at the top (North)
  theme_minimal()+
  geom_vline(xintercept = c(q3_mean_gf, q3_mean_gf-q3_sd_gf, q3_mean_gf+q3_sd_gf-360), 
             color = c("black"), 
             linetype = c("solid", "dashed", "dashed"), linewidth = c(0.75))+
  theme(legend.position = "none")+
  scale_y_continuous(labels=NULL, breaks=NULL) +  
  scale_x_continuous(limits = c(-180, 180), breaks=seq(-90, 180, by=90), labels=NULL)+
  xlab("3rd quartile: 3300 - 6600 birds\nGround reference frame")+
  ylab(NULL)+
  scale_fill_manual(values = c("1" = "transparent",  # Assign colors for each factor level
                               "2" = "transparent",
                               "3" = "grey",
                               "4" = "transparent"))+
  theme(panel.grid.major = element_line(color = "grey55", size = 0.15),
        panel.grid.minor = element_line(color = "grey55", size = 0.15))

q4_gf <- ggplot(radar_processed_data, 
                aes(x = bearing_of_interest, fill=group_size_quant)) +
  geom_histogram(color = NA, position = "identity") + 
  coord_polar(start = pi) +  # Rotate the plot so 0 degrees is at the top (North)
  theme_minimal()+
  geom_vline(xintercept = c(q4_mean_gf, q4_mean_gf-q4_sd_gf, q4_mean_gf+q4_sd_gf-360), 
             color = c("black"), 
             linetype = c("solid", "dashed", "dashed"), linewidth = c(0.75))+
  theme(legend.position = "none")+
  scale_y_continuous(labels=NULL, breaks=NULL) +  
  scale_x_continuous(limits = c(-180, 180), breaks=seq(-90, 180, by=90), labels=NULL)+
  xlab("4th quartile: > 6600 birds\nGround reference frame")+
  ylab(NULL)+
  scale_fill_manual(values = c("1" = "transparent",  # Assign colors for each factor level
                               "2" = "transparent",
                               "3" = "transparent",
                               "4" = "grey"))+
  theme(panel.grid.major = element_line(color = "grey55", size = 0.15),
        panel.grid.minor = element_line(color = "grey55", size = 0.15))


grid.arrange(q1_gf, q2_gf, q3_gf, q4_gf, ncol=4)

####################### CIRCULAR PLOTS CI AF - QUANTILES


q1_mean_af <- 180*mean.circular(pi*radar_processed_data$bearing_of_interest_air_frame[radar_processed_data$group_size_quant==1]/180,
                             na.rm=T)/pi
q1_sd_af <- 180*sd.circular(pi*radar_processed_data$bearing_of_interest_air_frame[radar_processed_data$group_size_quant==1]/180,
                         na.rm=T)/pi
q2_mean_af <- 180*mean.circular(pi*radar_processed_data$bearing_of_interest_air_frame[radar_processed_data$group_size_quant==2]/180,
                             na.rm=T)/pi
q2_sd_af <- 180*sd.circular(pi*radar_processed_data$bearing_of_interest_air_frame[radar_processed_data$group_size_quant==2]/180,
                         na.rm=T)/pi
q3_mean_af <- 180*mean.circular(pi*radar_processed_data$bearing_of_interest_air_frame[radar_processed_data$group_size_quant==3]/180,
                             na.rm=T)/pi
q3_sd_af <- 180*sd.circular(pi*radar_processed_data$bearing_of_interest_air_frame[radar_processed_data$group_size_quant==3]/180,
                         na.rm=T)/pi
q4_mean_af <- 180*mean.circular(pi*radar_processed_data$bearing_of_interest_air_frame[radar_processed_data$group_size_quant==4]/180,
                             na.rm=T)/pi
q4_sd_af <- 180*sd.circular(pi*radar_processed_data$bearing_of_interest_air_frame[radar_processed_data$group_size_quant==4]/180,
                         na.rm=T)/pi

q1_sd_af
q4_sd_af

q1_af <- ggplot(radar_processed_data, 
                aes(x = bearing_of_interest_air_frame, fill=group_size_quant)) +
  geom_histogram(color = NA, position = "identity") + 
  coord_polar(start = pi) +  # Rotate the plot so 0 degrees is at the top (North)
  theme_minimal()+
  geom_vline(xintercept = c(q1_mean_af, q1_mean_af-q1_sd_af, q1_mean_af+q1_sd_af-360), 
             color = c("darkorange3"), 
             linetype = c("solid", "dashed", "dashed"), linewidth = c(0.75))+
  theme(legend.position = "none")+
  scale_y_continuous(labels=NULL, breaks=NULL) +  
  scale_x_continuous(limits = c(-180, 180), breaks=seq(-90, 180, by=90), labels=NULL)+
  xlab("1st quartile: < 1500 birds\nAir reference frame")+
  ylab(NULL)+
  scale_fill_manual(values = c("1" = "burlywood2",  # Assign colors for each factor level
                               "2" = "transparent",
                               "3" = "transparent",
                               "4" = "transparent"))+
  theme(panel.grid.major = element_line(color = "grey55", size = 0.15),
        panel.grid.minor = element_line(color = "grey55", size = 0.15))

q2_af <- ggplot(radar_processed_data, 
             aes(x = bearing_of_interest_air_frame, fill=group_size_quant)) +
  geom_histogram(color = NA, position = "identity") + 
  coord_polar(start = pi) +  # Rotate the plot so 0 degrees is at the top (North)
  geom_vline(xintercept = c(q2_mean_af, q2_mean_af-q2_sd_af, q2_mean_af+q2_sd_af-360), 
             color = c("darkorange3"), 
             linetype = c("solid", "dashed", "dashed"), linewidth = c(0.75))+
  theme_minimal()+
  theme(legend.position = "none")+
  scale_y_continuous(labels=NULL, breaks=NULL) +  
  scale_x_continuous(limits = c(-180, 180), breaks=seq(-90, 180, by=90), labels=NULL)+
  xlab("2nd quartile: 1500 - 3300 birds\nAir reference frame")+
  ylab(NULL)+
  scale_fill_manual(values = c("1" = "transparent",  # Assign colors for each factor level
                               "2" = "burlywood2",
                               "3" = "transparent",
                               "4" = "transparent"))+
  theme(panel.grid.major = element_line(color = "grey55", size = 0.15),
        panel.grid.minor = element_line(color = "grey55", size = 0.15))

q3_af <- ggplot(radar_processed_data, 
             aes(x = bearing_of_interest_air_frame, fill=group_size_quant)) +
  geom_histogram(color = NA, position = "identity") + 
  coord_polar(start = pi) +  # Rotate the plot so 0 degrees is at the top (North)
  theme_minimal()+
  geom_vline(xintercept = c(q3_mean_af, q3_mean_af-q3_sd_af, q3_mean_af+q3_sd_af-360), 
             color = c("darkorange3"), 
             linetype = c("solid", "dashed", "dashed"), linewidth = c(0.75))+
  theme(legend.position = "none")+
  scale_y_continuous(labels=NULL, breaks=NULL) +  
  scale_x_continuous(limits = c(-180, 180), breaks=seq(-90, 180, by=90), labels=NULL)+
  xlab("3rd quartile: 3300 - 6600 birds\nAir reference frame")+
  ylab(NULL)+
  scale_fill_manual(values = c("1" = "transparent",  # Assign colors for each factor level
                               "2" = "transparent",
                               "3" = "burlywood2",
                               "4" = "transparent"))+
  theme(panel.grid.major = element_line(color = "grey55", size = 0.15),
        panel.grid.minor = element_line(color = "grey55", size = 0.15))

q4_af <- ggplot(radar_processed_data, 
             aes(x = bearing_of_interest_air_frame, fill=group_size_quant)) +
  geom_histogram(color = NA, position = "identity") + 
  coord_polar(start = pi) +  # Rotate the plot so 0 degrees is at the top (North)
  theme_minimal()+
  geom_vline(xintercept = c(q4_mean_af, q4_mean_af-q4_sd_af, q4_mean_af+q4_sd_af-360), 
             color = c("darkorange3"), 
             linetype = c("solid", "dashed", "dashed"), linewidth = c(0.75))+
  theme(legend.position = "none")+
  scale_y_continuous(labels=NULL, breaks=NULL) +  
  scale_x_continuous(limits = c(-180, 180), breaks=seq(-90, 180, by=90), labels=NULL)+
  xlab("4th quartile: > 6600 birds\nAir reference frame")+
  ylab(NULL)+
  scale_fill_manual(values = c("1" = "transparent",  # Assign colors for each factor level
                               "2" = "transparent",
                               "3" = "transparent",
                               "4" = "burlywood2"))+
  theme(panel.grid.major = element_line(color = "grey55", size = 0.15),
        panel.grid.minor = element_line(color = "grey55", size = 0.15))


grid.arrange(q1_af, q2_af, q3_af, q4_af, ncol=4)


################ BOOTSTRAPPING:
#########TO EXAMINE THE DIFFERENCES IN THE GROUND REFERENCE FRAME VS AIR REFERENCE FRAME CORRELATIONS

set.seed(123) #Set seed outcome that replicates

###########Set number of bootstraps
#n_boot_cor <- 1000
n_boot_cor <- 5
#########


daily_defl_gf_cor_boots <- c()
daily_defl_af_cor_boots <- c()

global_defl_gf_cor_boots <- c()
global_defl_af_cor_boots <- c()

for (tc in 1:n_boot_cor){
  
  print(tc)
  resampled_data <- radar_processed_data[sample(c(1:nrow(radar_processed_data)), 
                                                nrow(radar_processed_data), replace=T),]
  
  #######
  
  data_1_99_percentiles_boot <- quantile(resampled_data$julian_date, c(0.01, 0.99))

  rolling_stats_by_date_gf_boot <- as.data.frame(
    rolling_circular_stats_unique_vals(resampled_data$bearing_of_interest[resampled_data$julian_date>data_1_99_percentiles_boot[1]
                                                                               & resampled_data$julian_date<data_1_99_percentiles_boot[2]], 
                                       resampled_data$julian_date[resampled_data$julian_date>data_1_99_percentiles_boot[1]
                                                                        & resampled_data$julian_date<data_1_99_percentiles_boot[2]], 
                                       window_size = 14))
  
  
  rolling_stats_by_date_af_boot <- as.data.frame(
    rolling_circular_stats_unique_vals(resampled_data$bearing_of_interest_air_frame[resampled_data$julian_date>data_1_99_percentiles_boot[1]
                                                                                         & resampled_data$julian_date<data_1_99_percentiles_boot[2]],
                                       resampled_data$julian_date[resampled_data$julian_date>data_1_99_percentiles_boot[1]
                                                                        & resampled_data$julian_date<data_1_99_percentiles_boot[2]], 
                                       window_size = 14))

  resampled_data$mean_dir_for_day_af_boot <- NA
  for (i in 1:length(rolling_stats_by_date_af_boot$numeric_out)){
    resampled_data$mean_dir_for_day_af_boot[which(resampled_data$julian_date==
                                                    rolling_stats_by_date_af_boot$numeric_out[i])] <- 
      rolling_stats_by_date_af_boot$mean[i]
  }
  
  resampled_data$mean_dir_for_day_gf_boot <- NA
  for (i in 1:length(rolling_stats_by_date_gf_boot$numeric_out)){
    resampled_data$mean_dir_for_day_gf_boot[which(resampled_data$julian_date==
                                                     rolling_stats_by_date_gf_boot$numeric_out[i])] <- 
      rolling_stats_by_date_gf_boot$mean[i]
  }
  
  ########
  
  global_mean_ref_af_boot <- 180*mean.circular(pi*resampled_data$bearing_of_interest_air_frame/180, na.rm=T)/pi
  global_mean_ref_gf_boot <- 180*mean.circular(pi*resampled_data$bearing_of_interest/180, na.rm=T)/pi
  
  resampled_data$deflection_af_boot <- deflection_0_180_func(resampled_data$mean_dir_for_day_af_boot - resampled_data$bearing_of_interest_air_frame)
  resampled_data$deflection_globalm_af_boot <- deflection_0_180_func(global_mean_ref_af_boot - resampled_data$bearing_of_interest_air_frame)
  
  resampled_data$deflection_gf_boot <- deflection_0_180_func(resampled_data$mean_dir_for_day_gf_boot - resampled_data$bearing_of_interest)
  resampled_data$deflection_globalm_gf_boot <- deflection_0_180_func(global_mean_ref_gf_boot - resampled_data$bearing_of_interest)
  
  
  #######
  
  daily_defl_gf_cor_boots[tc] <- cor.test(abs(resampled_data$deflection_gf_boot), 
                             log10(resampled_data$group_size_of_interest), method="spearman")$estimate
  daily_defl_af_cor_boots[tc] <- cor.test(abs(resampled_data$deflection_af_boot), 
                                          log10(resampled_data$group_size_of_interest), method="spearman")$estimate
  global_defl_gf_cor_boots[tc] <- cor.test(abs(resampled_data$deflection_globalm_gf_boot), 
                              log10(resampled_data$group_size_of_interest), method="spearman")$estimate
  global_defl_af_cor_boots[tc] <- cor.test(abs(resampled_data$deflection_globalm_af_boot), 
                                        log10(resampled_data$group_size_of_interest), method="spearman")$estimate
}

hist(daily_defl_gf_cor_boots - daily_defl_af_cor_boots)
2*length(which(c(daily_defl_gf_cor_boots - daily_defl_af_cor_boots) < 0))/n_boot_cor 
#p-value on difference between air-frame and ground-frame correlations coefficients
#Multiplied by 2 for two-tailed test

hist(global_defl_gf_cor_boots - global_defl_af_cor_boots)
2*length(which(c(global_defl_gf_cor_boots - global_defl_af_cor_boots) < 0))/n_boot_cor
#p-value on difference between air-frame and ground-frame correlations coefficients
#Multiplied by 2 for two-tailed test

#################### Correlation effect size and confidence intervals plots

daily_ll <- c(quantile(daily_defl_gf_cor_boots, 0.025), quantile(daily_defl_af_cor_boots, 0.025))
daily_ests <- c(cor.test(abs(radar_processed_data$deflection_gf), 
         log10(radar_processed_data$group_size_of_interest), method="spearman")$estimate,
  cor.test(abs(radar_processed_data$deflection_af), 
         log10(radar_processed_data$group_size_of_interest), method="spearman")$estimate)
daily_ul <- c(quantile(daily_defl_gf_cor_boots, 0.975), quantile(daily_defl_af_cor_boots, 0.975))
daily_df <- data.frame(type=c("Ground reference frame", "Air reference frame"), daily_ests, daily_ll, daily_ul)
daily_df

global_ll <- c(quantile(global_defl_gf_cor_boots, 0.025), quantile(global_defl_af_cor_boots, 0.025))
global_ests <- c(cor.test(abs(radar_processed_data$deflection_globalm_gf), 
                          log10(radar_processed_data$group_size_of_interest), method="spearman")$estimate,
                 cor.test(abs(radar_processed_data$deflection_globalm_af), 
                          log10(radar_processed_data$group_size_of_interest), method="spearman")$estimate)
global_ul <- c(quantile(global_defl_gf_cor_boots, 0.975), quantile(global_defl_af_cor_boots, 0.975))
global_df <- data.frame(type=c("Ground reference frame", "Air reference frame"), global_ests, global_ll, global_ul)
global_df

daily_ests_fig <-  ggplot(daily_df, aes(x = daily_ests, y = type, color=type))+
  geom_vline(xintercept=0)+
  scale_y_discrete(limits=c("Air reference frame", "Ground reference frame"))+
  geom_pointrange(aes(xmin = daily_ll, xmax = daily_ul)) + 
  labs(x = "Estimate", y = "Type") +
  scale_color_manual(values=c("red", "black"))+
  theme_nice()+
  theme(legend.position = "none")+
  ylab("")+
  xlim(-0.2, 0.025)
daily_ests_fig


global_ests_fig <-  ggplot(global_df, aes(x = global_ests, y = type, color=type)) +
  geom_vline(xintercept=0)+
  scale_y_discrete(limits=c("Air reference frame", "Ground reference frame"))+
  geom_pointrange(aes(xmin = global_ll, xmax = global_ul)) + 
  labs(x = "Estimate", y = "Type") +
  scale_color_manual(values=c("red", "black"))+
  theme_nice()+
  theme(legend.position = "none")+
  ylab("")+
  xlim(-0.2, 0.025)
global_ests_fig


################### ALTERNATIVE EXPLANATION CHECK

#Is within track circ standard deviation smaller for larger groups? NO
within_track_sd <- ggplot(data=radar_processed_data, aes(group_size_of_interest, bearing_sd))+
  geom_point(size=0.2, color=alpha("black", 0.2))+
  theme_nice()+
  scale_x_log10()+
  geom_smooth(color="black", method = "loess")

cor.test(radar_processed_data$bearing_sd, radar_processed_data$group_size_of_interest, method="spearman")

#Is within track mean absolute deflection smaller for larger groups? NO
check2 <- ggplot(data=radar_processed_data, aes(group_size_of_interest, within_track_MAD))+
  geom_point(size=0.2, color=alpha("black", 0.2))+
  theme_nice()+
  scale_x_log10()+
  geom_smooth(color="black", method = "loess")
cor.test(radar_processed_data$within_track_MAD, radar_processed_data$group_size_of_interest, method="spearman")


################ CHANGE BY YEAR

hist(radar_processed_data$year)

av_flock_sizes <- radar_processed_data %>%
  group_by(year) %>%
  summarise(# Log transform group_size_of_interest and calculate the mean and standard error of the log-transformed data
    log_mean = mean(log(group_size_of_interest), na.rm = TRUE),
    n = n(),

    # Calculate the geometric mean
    geometric_mean = exp(log_mean),
    log_se = sd(log(group_size_of_interest), na.rm = TRUE) / sqrt(n()),
    
    # Calculate the 95% CI in the log scale and exponentiate to return to original scale
    lower_ci_geometric_mean = exp(log_mean - 1.96 * log_se),
    upper_ci_geometric_mean = exp(log_mean + 1.96 * log_se))

#Changes in flock sizes through time
group_size_change <- ggplot(av_flock_sizes, aes(x = year, y = geometric_mean)) +
  geom_errorbar(aes(ymin = lower_ci_geometric_mean, ymax = upper_ci_geometric_mean), width = 0.2, color = "black") +  # Plot the 95% CI as error bars
  geom_point(color = "black", size = 3) +  # Plot the geometric mean as points
  labs(
    x = "Year",
    y = "Average flock size"
  ) +
  theme_nice()
group_size_change

cor.test(av_flock_sizes$geometric_mean, av_flock_sizes$year, method="spearman")
length(av_flock_sizes$geometric_mean)

################ CHANGE BY YEAR - GAMs

gam_year_groupsize <- gam(log(group_size_of_interest) ~ s(year), 
                          data = radar_processed_data, method = "REML")
summary(gam_year_groupsize)
plot(gam_year_groupsize, shade = TRUE, rug = TRUE)


################ PREDICT YEAR-ON-YEAR CHANGES IN ERROR DUE TO CHANGING FLOCK SIZES
radar_processed_data$predicted_error_from_flocksize_gf <- predict(gam_model_globalm_gf) #predicted error given flocksize
radar_processed_data$predicted_error_from_flocksize_af <- predict(gam_model_globalm_af) #predicted error given flocksize

#Model predicted error by year
gam_year_error_expected <- gam(abs(predicted_error_from_flocksize_gf) ~ s(year), 
                      data = radar_processed_data, method = "REML")
summary(gam_year_error_expected)
max(predict(gam_year_error_expected)) - min(predict(gam_year_error_expected)) #Total range of expected values

gam_year_error_af_expected <- gam(abs(predicted_error_from_flocksize_af) ~ s(year), 
                               data = radar_processed_data, method = "REML")
summary(gam_year_error_af_expected)
max(predict(gam_year_error_af_expected)) - min(predict(gam_year_error_af_expected))


########### PLOT CHANGING GROUP SIZES OVER TIME
year_seq <- data.frame(
  year = seq(min(radar_processed_data$year),
             max(radar_processed_data$year), length.out= 1 + (length(unique(radar_processed_data$year)) - 1)*10)
)
pred_groupsize <- predict(gam_year_groupsize, newdata = year_seq, se.fit = TRUE)
year_seq <- year_seq %>%
  mutate(
    fit_groupsize = pred_groupsize$fit,
    se_groupsize = pred_groupsize$se.fit,
    upper_groupsize = fit_groupsize + 2 * se_groupsize,
    lower_groupsize = fit_groupsize - 2 * se_groupsize,
  )


year_flocksize_p <- ggplot() +
  geom_errorbar(data=av_flock_sizes, aes(x = year, y = log(geometric_mean),
                                         ymin = log(lower_ci_geometric_mean), 
                                         ymax = log(upper_ci_geometric_mean)), width = 0.2, color = "darkgrey") +  # Plot the 95% CI as error bars
  geom_point(data=av_flock_sizes, aes(x = year, y = log(geometric_mean)), color = "darkgrey", size = 1.5) +  # Plot the geometric mean as points
  geom_line(data=year_seq, aes(x = year, y = fit_groupsize), color = "steelblue", linewidth = 1) +
  geom_ribbon(data=year_seq, aes(x = year, ymin = lower_groupsize, ymax = upper_groupsize), 
              fill = "steelblue", alpha = 0.3) +
  ylab("Flock size") +  
  xlab("Year") +
  scale_y_continuous(
    limits=c(log(1850), log(5000)),
    breaks = log(seq(2000, 5000, by=1000)),            # breaks at logged values of the original breaks
    labels = seq(2000, 5000, by=1000)                   # label as original numbers
  ) +
  theme_nice() +
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 13),
    plot.tag = element_text(color = "black", size = 14)
  )
year_flocksize_p



######################## FIGS FOR MANUSCRIPT



#####  FIG1

purple_martin_map <- readPNG("/Users/joemorford/Desktop/RADAR/Writing and figures/eBird fig/Purple martins eBird range breed-winter map.png")
purple_martin_map_grob <- rasterGrob(purple_martin_map, interpolate = TRUE)

radar_traces_png <- readPNG("/Users/joemorford/Desktop/RADAR/Writing and figures/Radar traces fig/Radar_traces5.png")
radar_traces_grob <- rasterGrob(radar_traces_png, interpolate = TRUE)


top_part2 <- grid.arrange(
  arrangeGrob(radar_traces_grob, arrangeGrob(all_dirs_plot, all_dir_af_plot, ncol=2), nrow=2),
  purple_martin_map_grob,
  ncol = 2,
  widths = c(1, 0.95)  # Left side twice as wide as right side
)


gam_out_CI_with_hist <- ggdraw() +
  draw_plot(gam_out_CI) +
  draw_plot(group_size, x = 0.34, y = 0.64, width = 0.64, height = 0.345)

quartile_plots <- arrangeGrob(q1_gf, q4_gf, 
                              q1_af, q4_af, nrow=2)

bottom_part2 <- arrangeGrob(gam_out_CI_with_hist, quartile_plots, ncol = 2)
big_fig1_out <- grid.arrange(top_part2, bottom_part2, nrow=2,
                          heights=c(1,0.9))
big_fig1_out

#png(file = paste0(plot_dir, "Figure1.png"),width = 10 * 300,  height = 11 * 300,  res = 300)
grid.newpage()
grid.draw(arrangeGrob(top_part2, bottom_part2, nrow=2,
                      heights=c(1,0.9)))
grid.text("A", x = unit(0.02, "npc"), y = unit(0.98, "npc"), gp = gpar(fontsize = 14))
grid.text("B", x = unit(0.02, "npc"), y = unit(0.73, "npc"), gp = gpar(fontsize = 14))
grid.text("C", x = unit(0.515, "npc"), y = unit(0.98, "npc"), gp = gpar(fontsize = 14))
grid.text("D", x = unit(0.02, "npc"), y = unit(0.465, "npc"), gp = gpar(fontsize = 14))
grid.text("E", x = unit(0.515, "npc"), y = unit(0.465, "npc"), gp = gpar(fontsize = 14))

dev.off() 


#####  FIG3

img_cycle <- readPNG("Fig3A")
img_cycle_grob <- rasterGrob(img_cycle, interpolate = TRUE)

figallee_nolabel <- grid.arrange(img_cycle_grob, year_flocksize_p, ncol=2)
figallee_nolabel

#png(file = paste0(plot_dir, "Figure3.png"),width = 9 * 300,  height = 3.5 * 300,  res = 300)
grid.newpage()
grid.draw(arrangeGrob(img_cycle_grob, year_flocksize_p, ncol=2))
grid.text("A", x = unit(0.02, "npc"), y = unit(0.97, "npc"), gp = gpar(fontsize = 14))
grid.text("B", x = unit(0.49, "npc"), y = unit(0.97, "npc"), gp = gpar(fontsize = 14))
dev.off() 

#############
