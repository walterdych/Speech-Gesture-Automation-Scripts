# Load required libraries
library(rstudioapi)    
library(ggplot2)
library(plotly)
library(matlab)

# Author: Walter Dych
# Questions?: wdych@udel.edu

############################################ Load In Files and Directories

parentfolder <- (dirname(rstudioapi::getSourceEditorContext()$path)) #Get the parent folder of the source file
TS <- read.csv(paste0(dirname(parentfolder), "/DATA_PROCESSED/Benedicte_MT_p.csv")) # Read in the Motion Tracking data from the DATA_PROCESSED folder
data_processed <- paste0(dirname(parentfolder), "/DATA_PROCESSED/") # Set directory path for Processed Data

############################################ Calculate Peak Speed Timing

TS$peak_speed <- ave(TS$speed, TS$gesture_ID, FUN = max)        # Calculate the peak speed for each gesture    
TS$peak_speed[is.na(TS$gesture_ID)] <-NA                        # Set peak speed to NA for rows where gesture_ID is NA          
TS$peak_speed <- ifelse(TS$peak_speed == TS$speed, TS$speed, NA)#Set peak speed to the value of speed if the peak speed is equal to speed, else set to NA  

TS$timing_peak_speed <- ifelse(!is.na(TS$peak_speed), TS$time_ms, NA) # Set timing_peak_speed to time_ms if peak_speed is not NA, else set to NA

############################################ Find Peak Envelope Timing

TS$peak_env <- ave(TS$env, TS$gesture_ID, FUN = max)        # Calculate the peak env for each gesture            
TS$peak_env[is.na(TS$gesture_ID)] <-NA                      # Set peak env to NA for rows where gesture_ID is NA      
TS$peak_env <- ifelse(TS$peak_env == TS$env, TS$time_ms, NA)# Set peak_env to the value of time_ms if the peak_env is equal to env, else set to NA

TS$timing_peak_env <- ifelse(!is.na(TS$peak_env), TS$time_ms, NA) # Set timing_peak_env to time_ms if peak_env is not NA, else set to NA


asynchrony <- TS$timing_peak_speed[!is.na(TS$timing_peak_speed)]-TS$timing_peak_env[!is.na(TS$timing_peak_env)] # Calculate asynchrony as the difference between timing_peak_speed and timing_peak_env for non-NA values

g_type <- TS$gesture_t[!is.na(TS$peak_env)] # Get the gesture type for non-NA values of peak_env

peak_speed <- TS$peak_speed[!is.na(TS$peak_speed)] # Get peak_speed for non-NA values of peak_speed

peak_env <- TS$peak_env[!is.na(TS$peak_env)] # Get peak_env for non-NA values of peak_env

t_dat <- cbind.data.frame(asynchrony, g_type, peak_speed, peak_env) # Bind asynchrony, g_type, peak_speed, and peak_env into a single data frame

# Calculate the mean asynchrony for each gesture type
mbeat <- mean(t_dat$asynchrony[t_dat$g_type=="Beat"])
mbeaticonic <- mean(t_dat$asynchrony[t_dat$g_type=="Beat-Iconic"])
miconic <- mean(t_dat$asynchrony[t_dat$g_type=="Iconic"])

# Create a density plot of asynchrony colored by gesture type, with vertical lines at the mean asynchrony for each gesture type
p_time <- ggplot(t_dat, aes(x= asynchrony, color = g_type)) + geom_density() +
  geom_vline(xintercept =mbeat, color = "red") +  geom_vline(xintercept =miconic, color = "violet")+ geom_vline(xintercept =mbeaticonic, color = "darkgreen") + xlim(-800,800) +
  theme_bw()
ggplotly(p_time)

# Write the processed data to a CSV file in the DATA_PROCESSED folder
write.csv(TS, paste0(data_processed, "Benedicte_Peak_Timings_GT.csv"))
