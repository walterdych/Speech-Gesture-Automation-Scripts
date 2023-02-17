library(rstudioapi)    #functions for time series functions (e.g., NA.approx)
library(binhf)
library(tidyverse)
library(plotly)


#CODE AUTHORED BY Walter Dych
#Email: wdych@udel.edu

#FOLDER LOCATIONS
parentfolder <- (dirname(rstudioapi::getSourceEditorContext()$path))  #what is the current folder
TS <- read.csv(paste0(dirname(parentfolder), "/DATA_PROCESSED/Deborah_MT_GT.csv"))      #Load the processed data
TS2 <- read.csv(paste0(dirname(parentfolder), "/DATA_PROCESSED/Deborah_MT_GT.csv"))     #Load a temp copy of the dataframe
data_processed <- paste0(dirname(parentfolder), "/DATA_PROCESSED/") 

#TIMING OF PEAK SPEED
TS$peak_speed <- ave(TS$speed, TS$gesture_ID, FUN = max)            #calculate the max speed
TS$peak_speed[is.na(TS$gesture_ID)] <-NA                            #ignore NA events
TS$peak_speed <- ifelse(TS$peak_speed == TS$speed, TS$speed, NA)    #keep only one observation of peak speed

TS$timing_peak_speed <- ifelse(!is.na(TS$peak_speed), TS$time_ms, NA) #timing of when peak speed occurs

#MIN SPEED
TS$min_speed <- ave(TS$speed, TS$gesture_ID, FUN = min)             #calculate the min speed   
TS$min_speed[is.na(TS$gesture_ID)] <-NA                             #ignore NA events
TS$min_speed <- ifelse(TS$min_speed == TS$speed, TS$speed, NA)      #keep only one observation of min speed

TS$timing_min_speed <- ifelse(!is.na(TS$min_speed), TS$time_ms, NA) #timing of when min speed occurs

#SPEED AT STROKE START
first <- TS[match(unique(TS$gesture_ID), TS$gesture_ID),] #filter down to only the first instance of each gesture phase, the phase start

first <- first %>%      #rename columns 
  rename(
    gesture_ID_first = gesture_ID,
    speed_first = speed,
  )

#hacked together way to remove everything I don't want
TS <- base::merge(TS, select(first, -X.1, -X, -env, -x_index_right, -y_index_right, -speed_UNsmooth, -x_index_right_S, -y_index_right_S, -gesture_t, -peak_speed, -timing_peak_speed, -min_speed, -timing_min_speed), by = "time_ms", all.x = TRUE)

#Some Filtering
TS$gesture_t[TS$gesture_t==""] <- NA
TS <- TS %>% drop_na("gesture_t")

#calculate threshold
threshold <- (TS$speed_first[!is.na(TS$speed_first)]-TS$min_speed[!is.na(TS$min_speed)])/(TS$peak_speed[!is.na(TS$peak_speed)]-TS$min_speed[!is.na(TS$min_speed)])
print(threshold)

#threshold mean    
meanth <- mean(as.numeric(threshold),na.rm=TRUE)
thdf <- as.data.frame(threshold)                    #create a threshold dataframe
TS <- TS %>% drop_na("gesture_ID_first")            #allows for thresholds to align with rest of data
TS$threshold <- threshold                      #create column for threshold

#plot threshold density
thpt <- ggplot(TS, aes(x= threshold, color = gesture_t)) + geom_density() + ggtitle("Deborah Threshold Based on Gesture Type") + theme_bw()
ggplotly(thpt)

#write to file
write.csv(TS, paste0(data_processed, "Deborah_Peak_Timings_GT.csv"))


