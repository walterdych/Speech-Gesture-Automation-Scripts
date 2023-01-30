library(rstudioapi)    #functions for time series functions (e.g., NA.approx)
library(binhf)
library(tidyverse)
library(plotly)


#CODE AUTHORED BY Walter Dych
#Email: wdych@udel.edu

#FOLDER LOCATIONS
parentfolder <- (dirname(rstudioapi::getSourceEditorContext()$path))  #what is the current folder
TS <- read.csv(paste0(dirname(parentfolder), "/DATA_PROCESSED/Benedicte_MT_STROKE.csv"))      #Load the processed data
TS2 <- read.csv(paste0(dirname(parentfolder), "/DATA_PROCESSED/Benedicte_MT_STROKE.csv"))     #Load a temp copy of the dataframe
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

#Filtering out all gesture phases to only calculate thresholds for strokes
TS$gesture_t[TS$gesture_t=="P  "] <- NA
TS$gesture_t[TS$gesture_t=="P"] <- NA
TS$gesture_t[TS$gesture_t=="P "] <- NA
TS$gesture_t[TS$gesture_t==" P"] <- NA
TS$gesture_t[TS$gesture_t=="H"] <- NA
TS$gesture_t[TS$gesture_t=="R "] <- NA
TS$gesture_t[TS$gesture_t==" R"] <- NA
TS$gesture_t[TS$gesture_t==""] <- NA
TS$gesture_t[TS$gesture_t==" S"] <- "S" 
TS <- TS %>% drop_na("gesture_t")


#calculate threshold
threshold <- (TS$speed_first[!is.na(TS$speed_first)]-TS$min_speed[!is.na(TS$min_speed)])/(TS$peak_speed[!is.na(TS$peak_speed)]-TS$min_speed[!is.na(TS$min_speed)])
print(threshold)

#threshold mean    
meanth <- mean(as.numeric(threshold),na.rm=TRUE)
thdf <- as.data.frame(threshold)                    #create a threshold dataframe
TS <- TS %>% drop_na("gesture_ID_first")            #allows for thresholds to align with rest of data
TS$threshold <- threshold                           #create column for threshold

#plot threshold density
thpt <- ggplot(thdf, aes(x= threshold, color = "threshold")) + geom_density() + geom_vline(xintercept = meanth, color= "violet") + ggtitle("Threshold Distribution of Stroke Start") + theme_bw()
ggplotly(thpt)

#merge back other gesture phases
TS <- base::merge(TS2, select(TS, -gesture_ID, -speed, -X.1, -X, -env, -x_index_right, -y_index_right, -speed_UNsmooth, -x_index_right_S, -y_index_right_S, -gesture_t, -peak_speed, -timing_peak_speed, -min_speed, -timing_min_speed), by="time_ms", all = TRUE)

#create a shifted column
TS$shift <- shift(TS$gesture_t, places = 1)

#check the threshold value for each "stroke" occuring with a "prep" before it
prep_precede <- subset(TS, gesture_t=="S" & shift=="P")$threshold

ppdf <- as.data.frame(prep_precede)
meanpp <- mean(as.numeric(prep_precede),na.rm = TRUE)
pppt <- ggplot(ppdf, aes(x= prep_precede, color = "threshold")) + geom_density() + geom_vline(xintercept = meanpp, color= "violet") + ggtitle("Threshold Distribution When P precedes S") + theme_bw()
ggplotly(pppt)

#check the threshold value for each "stroke" occuring with a "hold" before it
hold_precede <- subset(TS, gesture_t=="S" & shift=="H")$threshold
hpdf <- as.data.frame(hold_precede)
meanhp <- mean(as.numeric(hold_precede), na.rm = TRUE)
hppt <- ggplot(hpdf, aes(x= hold_precede, color = "threshold")) + geom_density() + geom_vline(xintercept = meanhp, color= "violet") + ggtitle("Threshold Distribution When H precedes S") + theme_bw()
ggplotly(hppt)

#write to file
write.csv(TS, paste0(data_processed, "Benedicte_Peak_Timings_GP.csv"))


