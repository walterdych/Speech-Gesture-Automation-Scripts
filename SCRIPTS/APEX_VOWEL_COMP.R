library(rstudioapi)  
library(binhf)
library(tidyverse)
library(plotly)

parentfolder <- (dirname(rstudioapi::getSourceEditorContext()$path))
data_processed <- paste0(dirname(parentfolder), "/DATA_PROCESSED/")
data_to_process <- paste0(dirname(parentfolder), "/DATA_TO_PROCESS/")

TS <- read.csv(paste0(dirname(parentfolder), "/DATA_PROCESSED/Menkam_MT_STROKE.csv")) #Calculated Apex for Stroke tier
phone <- read.csv(paste0(dirname(parentfolder), "/DATA_TO_PROCESS/Menkam_Phone.csv")) #ELAN phone textgrid

TS$peak_speed <- ave(TS$speed, TS$gesture_ID, FUN = max)            #calculate the max speed
TS$peak_speed[is.na(TS$gesture_ID)] <-NA                            #ignore NA events
TS$peak_speed <- ifelse(TS$peak_speed == TS$speed, TS$speed, NA)    #keep only one observation of peak speed


TS$timing_peak_speed <- ifelse(!is.na(TS$peak_speed), TS$time_ms, NA) #timing of when peak speed occurs

colnames(phone)[colnames(phone) == "Begin.Time...msec"] <- "time_ms"
colnames(phone)[colnames(phone) == "MeDuMBa.TraNScriPTioN...phone"] <- "MeDuMBa...phone"

phone <- phone %>% filter(grepl("1", MeDuMBa...phone)) #Comment out if looking for phone not vowel

TS <- TS %>% filter(str_detect(gesture_t, "S"))
TS$gesture_t[TS$timing_peak_speed==""] <- NA
TS <- TS %>% filter(timing_peak_speed != "NA")
TS <- select(TS, time_ms, gesture_ID, timing_peak_speed, gesture_t)

merged_ax_phone <- merge(TS, phone, by = "time_ms", all = TRUE)

merged_ax_phone$shift_phone <- shift(merged_ax_phone$MeDuMBa...phone, places = -1)
merged_ax_phone$shift_time <- shift(merged_ax_phone$time_ms, places = -1)

merged_ax_phone <- merged_ax_phone %>% filter(timing_peak_speed != "NA")

merged_ax_phone$difference <- (merged_ax_phone$shift_time - merged_ax_phone$timing_peak_speed)

write.csv(merged_ax_phone, paste0(data_processed, "Menkam_Capex_To_Vowel.csv"))


