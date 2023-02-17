library(rstudioapi)   
library(binhf)
library(tidyverse)
library(plotly)

parentfolder <- (dirname(rstudioapi::getSourceEditorContext()$path))
data_processed <- paste0(dirname(parentfolder), "/DATA_PROCESSED/")
data_to_process <- paste0(dirname(parentfolder), "/DATA_TO_PROCESS/")

#LOAD IN APEX TEXTGRID AND PROCESSED STROKE FILE
apex <- read.csv(paste0(dirname(parentfolder), "/DATA_TO_PROCESS/Deborah_Apex_TEXTGRID.csv"))
stroke <- read.csv(paste0(dirname(parentfolder), "/DATA_PROCESSED/Deborah_MT_STROKE.csv"))

#TIMING OF PEAK SPEED
stroke$peak_speed <- ave(stroke$speed, stroke$gesture_ID, FUN = max)              #CALCULATE MAX SPEED
stroke$peak_speed[is.na(stroke$gesture_ID)] <-NA                                  #REMOVE NA EVENTS
stroke$peak_speed <- ifelse(stroke$peak_speed == stroke$speed, stroke$speed, NA)  #KEEP ONLY PEAK SPEED OBSERVATION
stroke$timing_peak_speed <- ifelse(!is.na(stroke$peak_speed), stroke$time_ms, NA) #TIME WHEN PEAK SPEED OCCURS

#SIMPLIFY DATAFRAME
stroke <- stroke[, c("time_ms", "speed", "gesture_t", "gesture_ID", "peak_speed", "timing_peak_speed")]

#FILTER OUT ALL ROWS WITHOUT "S"
stroke <- stroke %>% filter(grepl("S", gesture_t, fixed = TRUE))

#DROP NA VALUES IN THE PEAK SPEED COLUMN
stroke <- stroke %>% drop_na(timing_peak_speed)

#SETUP AND SIMPLIFY THE APEX TEXTGRID
apex <- rename(apex, t1_ax = Begin.Time...msec)
apex$Manual.Apex[apex$Manual.Apex==""] <- NA
apex <- apex %>% drop_na(Manual.Apex)

#MERGE APEX AND STROKE FILES
merged <- cbind(stroke, apex)

#CALCULATE TIMING DIFFERENCES (Manual minus Computational)
merged$difference <- (merged$t1_ax[!is.na(merged$t1_ax)]) - (merged$timing_peak_speed[!is.na(merged$timing_peak_speed)])

#EXPORT TO .CSV FILE
write.csv(merged, paste0(data_processed, "Deborah_Manual_Comp_Dif.csv"))


