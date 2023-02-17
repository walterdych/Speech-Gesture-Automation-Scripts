library(rstudioapi) 
library(binhf)
library(tidyverse)
library(plotly)

#FOLDER LOCATIONS
parentfolder <- (dirname(rstudioapi::getSourceEditorContext()$path))  #what is the current folder
TS <- read.csv(paste0(dirname(parentfolder), "/DATA_PROCESSED/Deborah_MT_Apex.csv"))      #Load the processed data
data_processed <- paste0(dirname(parentfolder), "/DATA_PROCESSED/") 
#TIMING OF PEAK SPEED
TS$peak_speed <- ave(TS$speed, TS$gesture_ID, FUN = max)            #for every gesture determine the maximum observed speed
TS$peak_speed[is.na(TS$gesture_ID)] <-NA                            #only keep actual gesture event peak speed, not NA events
TS$peak_speed <- ifelse(TS$peak_speed == TS$speed, TS$speed, NA)  #keep only one observation of peak speed observation peak speed
#we also want the timings for those peaks in speed
TS$timing_peak_speed <- ifelse(!is.na(TS$peak_speed), TS$time_ms, NA)

#Clean File
TS$gesture_t[TS$gesture_t==""] <- NA
TS <- TS %>% drop_na("gesture_t")

peak_speed_time <- TS[match(unique(TS$timing_peak_speed), TS$timing_peak_speed),]
peak_speed_time <- peak_speed_time %>% drop_na("timing_peak_speed")

first <- TS[match(unique(TS$gesture_ID), TS$gesture_ID),] #filter down to only the first instance of each gesture phase, the phase start

first <- first %>%      #rename columns 
  rename(
    gesture_ID_first = gesture_ID,
    time_first = time_ms,
  )

peak_speed_time$difference <- (peak_speed_time$time_ms - first$time_first)

peak_speed_time$first <- first$time_first

mdif <- mean(as.numeric(peak_speed_time$difference),na.rm=TRUE)

plot <- ggplot(peak_speed_time, aes(x=difference)) + 
  geom_density() + geom_vline(xintercept = mdif, color= "violet") + xlim(-350,350) + 
  theme_bw()
ggplotly(plot)

write.csv(peak_speed_time, paste0(data_processed, "Deborah_Peak_Timings_Apex.csv"))

