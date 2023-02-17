library(rstudioapi)   
library(binhf)
library(tidyverse)
library(plotly)

parentfolder <- (dirname(rstudioapi::getSourceEditorContext()$path))

p1 <- read.csv(paste0(dirname(parentfolder), "/DATA_PROCESSED/Benedicte_Peak_Timings_Apex.csv")) 
p2 <- read.csv(paste0(dirname(parentfolder), "/DATA_PROCESSED/Bertrand_Peak_Timings_Apex.csv")) 
p3 <- read.csv(paste0(dirname(parentfolder), "/DATA_PROCESSED/Deborah_Peak_Timings_Apex.csv")) 
p4 <- read.csv(paste0(dirname(parentfolder), "/DATA_PROCESSED/Menkam_Peak_Timings_Apex.csv"))

p1$Participant <- "Benedicte"
p2$Participant <- "Bertrand"
p3$Participant <- "Deborah"
p3$Participant <- "Menkam"


df <- bind_rows(p1, p2, p3, p4)
df <- df %>% rename(Manual_Apex = first)
df <- df[, c("Participant", "time_ms", "x_index_right_S", "y_index_right_S", "speed", "gesture_t", "gesture_ID", "peak_speed", "timing_peak_speed", "Manual_Apex", "difference")]

df$Manual_Apex_Start <- annot$Begin.Time...msec

mean_diff <- mean(df$difference)
mean_diff

lower <- mean_diff - 1.96*sd(df$difference)
lower

upper <- mean_diff + 1.96*sd(df$difference)
upper

df$avg <- rowMeans(df[, c("timing_peak_speed", "Manual_Apex_Start")])


plot <- ggplot(df, aes(x = avg, y = difference)) +
  geom_point(size=1) +
  geom_hline(yintercept = mean_diff) +
  geom_hline(yintercept = lower, color = "red", linetype="dashed") +
  geom_hline(yintercept = upper, color = "red", linetype="dashed") +
  ggtitle("Bland-Altman Plot") +
  ylab("Apex Timing Differences") +
  xlab("Average")+theme_bw()
ggplotly(plot)


correlation <- cor.test(df$first, df$timing_peak_speed, 
                           method = "pearson")


plot <- ggplot(df, aes(x =difference, y = after_stat(count))) +
  geom_count() +
  geom_point(size=2, shape=23) +
  
ggplotly(plot)

write.csv(df, paste(parentfolder, "Combined_Apex_Data.csv"))

plot <- ggplot(p1, aes(x=difference, color = "Participant 1")) + 
  geom_density() + 
  geom_density(data = p2, aes(x=difference, color = "Participant 2")) +
  geom_density(data = p3, aes(x=difference, color = "Participant 3")) +
  geom_density(data = p4, aes(x=difference, color = "Participant 4")) +
  xlim(-250, 500) + theme_bw() +
  ggtitle("Apex Comparison Distribution Amongst 4 Participants") +
  scale_color_manual(name = "Participant", values = c("blue", "red", "violet", "green"), labels = c("Participant 1", "Participant 2", "Participant 3", "Participant 4"))
ggplotly(plot)

p1$group <- "p1"
p2$group <- "p2"
p3$group <- "p3"
p4$group <- "p4"

boxplot <- ggplot(p1, aes(x=group, y=difference)) + 
  geom_boxplot(color = "black", fill="lightgray") + scale_y_continuous(breaks=seq(-50, 550, 50), limits = c(-50,550)) + ggtitle("Apex Comparison Amongst 4 Participants") +
  theme_bw() + labs(y= "Difference (msec)", x= "Participant") +
ggtitle("Apex Comparison Distribution Amongst 4 Participants") + theme(axis.text.x = element_blank())
# Add labels for Participant 1
boxplot <- boxplot + geom_text(aes(label = "Participant 1"), data = p1, stat = "summary", fun.y = median, vjust = -0.5)

boxplot <- boxplot + geom_boxplot(data=p2, aes(x=group, y=difference), color="darkgray", fill="lightgray")
# Add labels for Participant 2
boxplot <- boxplot + geom_text(aes(label = "Participant 2"), data = p2, stat = "summary", fun.y = median, vjust = -0.5)

boxplot <- boxplot + geom_boxplot(data=p3, aes(x=group, y=difference), color="black", fill="lightgray")
# Add labels for Participant 3
boxplot <- boxplot + geom_text(aes(label = "Participant 3"), data = p3, stat = "summary", fun.y = median, vjust = -0.5)

boxplot <- boxplot + geom_boxplot(data=p4, aes(x=group, y=difference), color="darkgray", fill="lightgray")
# Add labels for Participant 4
boxplot <- boxplot + geom_text(aes(label = "Participant 4"), data = p4, stat = "summary", fun.y = median, vjust = -0.5)

boxplot

############PERCENTAGE OF OUTLIERS############
data <- p4$difference
mean <- mean(p4$difference)
sd <- sd(p4$difference)
sd
# Calculate the z-score of each data point
z_scores <- (data - mean) / sd

# Identify the outliers
outliers <- data[abs(z_scores) >= 3]

# Calculate the percentage of outliers
percentage_of_outliers <- (length(outliers) / length(data)) * 100

print(paste("Percentage of outliers:", percentage_of_outliers, "%"))
############COMPARING WITH PHONETIC DATA############)

colnames(MAPEX_PHONE_ALL)[colnames(MAPEX_PHONE_ALL) == "Begin.Time...msec"] <- "time_ms"

MAPEX_PHONE_ALL <- select(MAPEX_PHONE_ALL, Participant, time_ms, difference, shift_phone)
CAPEX_PHONE_ALL <- select(CAPEX_PHONE_ALL, Participant, time_ms, difference, shift_phone)

write.csv(CAPEX_PHONE_ALL, paste0(data_processed, "CAPEX_PHONE_ALL.csv"))
write.csv(MAPEX_PHONE_ALL, paste0(data_processed, "MAPEX_PHONE_ALL.csv"))

CAPEX_PHONE_ALL <- read.csv(paste0(data_processed, "CAPEX_PHONE_ALL.csv"))
MAPEX_PHONE_ALL <- read.csv(paste0(data_processed, "MAPEX_PHONE_ALL.csv"))


MAPEX_PHONE_ALL$group <- "Manual Apex"
CAPEX_PHONE_ALL$group <- "OpenPose Apex"


box <- ggplot(rbind(CAPEX_PHONE_ALL, MAPEX_PHONE_ALL), aes(x = group, y = difference, fill = group)) +
        geom_boxplot() +
        theme(text = element_text(size = 18),
              axis.title.y = element_text(margin = margin(t = 20)),
              axis.text.y = element_text(margin = margin(r = 15)),
              axis.text.x = element_text(size = 12),
              legend.text = element_text(size = 14)) +
        xlab("Method") + ylab("Time to Vowel (msec)") + 
        ggtitle("Comparison of Manual vs. OpenPose Apex to Vowel times") +
        facet_grid(. ~ Participant) +
        scale_fill_discrete(name = "") +
        scale_y_continuous(breaks = seq(0, 9000, 200))
ggplotly(box)


mapex_avg <- mean(MAPEX_PHONE_ALL$difference)
print(mapex_avg)

capex_avg <- mean(CAPEX_PHONE_ALL$difference)
print(capex_avg)

####################################################################
# combine the data frames
combined_df <- rbind(MAPEX_PHONE_ALL, CAPEX_PHONE_ALL)

# identify duplicated rows based on the 'name' column
duplicate_rows <- duplicated(APEX_ALL_CM, by.x = 'shift_phone.x', by.y = 'shift_phone.y') *100

# calculate percentage overlap
percent_overlap <- sum(duplicate_rows) / nrow(APEX_ALL_CM)

# print the result
print(percent_overlap)


colnames(P2M)[colnames(P2M) == "Begin.Time...msec"] <- "time_ms"


##################################################################


################################################

P1C <- read.csv(paste0(dirname(parentfolder), "/DATA_PROCESSED/Benedicte_Capex_To_Vowel.csv"))
P2C <- read.csv(paste0(dirname(parentfolder), "/DATA_PROCESSED/Bertrand_Capex_To_Vowel.csv"))
P3C <- read.csv(paste0(dirname(parentfolder), "/DATA_PROCESSED/Deborah_Capex_To_Vowel.csv"))
P4C <- read.csv(paste0(dirname(parentfolder), "/DATA_PROCESSED/Menkam_Capex_To_Vowel.csv"))

P1C$Participant <- "Participant 1"
P2C$Participant <- "Participant 2"
P3C$Participant <- "Participant 3"
P4C$Participant <- "Participant 4"

CAPEX_Vowel_ALL <- bind_rows(P1C, P2C, P3C, P4C)

CAPEX_Vowel_ALL <- CAPEX_Vowel_ALL %>% filter(shift_phone != "sp")
CAPEX_Vowel_ALL <- CAPEX_Vowel_ALL %>% filter(shift_phone != "NA")

boxplot <- ggplot(CAPEX_Vowel_ALL, aes(x = Participant, y = difference)) + 
  ggtitle("Computational Apex to Vowel Comparison") +
  geom_boxplot() + 
  labs(y = "Time to Vowel (msec)") + 
  scale_y_continuous(breaks = seq(-500, 2000, 100))
ggplotly(boxplot)

write.csv(CAPEX_Vowel_ALL, paste0(data_processed, "CAPEX_TO_VOWEL_MERGED.csv"))

# Calculate Q1 and Q3
q1 <- quantile(CAPEX_PHONE_ALL$difference, probs = 0.25)
q3 <- quantile(CAPEX_PHONE_ALL$difference, probs = 0.75)
iqr <- IQR(CAPEX_PHONE_ALL$difference)
# Calculate the amount of data within Q1 and Q3
data_within_q1_q3 <- sum(CAPEX_PHONE_ALL$difference >= q1 & CAPEX_PHONE_ALL$difference <= q3)

percentage_quart <- (data_within_q1_q3 / length(CAPEX_PHONE_ALL$difference)) * 100

# Print the percentage
print(percentage_quart)

upper_fence <- q3 + 1.5 * iqr
lower_fence <- q1 - 1.5 * iqr


data_within_lower_upper <- sum(CAPEX_PHONE_ALL$difference >= lower_fence & CAPEX_PHONE_ALL$difference <= upper_fence)

percentage <- (data_within_lower_upper / length(CAPEX_PHONE_ALL$difference)) * 100

num_outliers <- sum(CAPEX_PHONE_ALL$difference > upper_fence | CAPEX_PHONE_ALL$difference < lower_fence)

# Calculate the percentage of outliers
percentage_outliers <- (num_outliers / length(CAPEX_PHONE_ALL$difference)) * 100

# Print the percentage of outliers
print(percentage_outliers)

colnames(MAPEX_PHONE_ALL)[colnames(MAPEX_PHONE_ALL) == "Begin.Time...msec"] <- "time_ms"


APEX_ALL_CM <- merge(CAPEX_PHONE_ALL, MAPEX_PHONE_ALL, by = "X", all = TRUE)


###################################################################################

P1M <- read.csv(paste0(dirname(parentfolder), "/DATA_PROCESSED/Benedicte_Mapex_To_Phone.csv"))
P2M <- read.csv(paste0(dirname(parentfolder), "/DATA_PROCESSED/Bertrand_Mapex_To_Phone.csv"))
P3M <- read.csv(paste0(dirname(parentfolder), "/DATA_PROCESSED/Deborah_Mapex_To_Phone.csv"))
#P4 <- read.csv(paste0(dirname(parentfolder), "/DATA_PROCESSED/Menkam_Mapex_To_Phone.csv"))

P1M$Participant <- "Participant 1"
P2M$Participant <- "Participant 2"
P3M$Participant <- "Participant 3"
#P4$Participant <- "Participant 4"

MAPEX_PHONE_ALL <- bind_rows(P1M, P2M, P3M)
MAPEX_PHONE_ALL <- MAPEX_PHONE_ALL %>% filter(shift_phone != "sp")
MAPEX_PHONE_ALL <- MAPEX_PHONE_ALL %>% filter(shift_phone != "NA")



boxplot <- ggplot(MAPEX_PHONE_ALL, aes(x = Participant, y = difference)) + 
  ggtitle("Manual Apex to Phone Comparison") +
  geom_boxplot() + 
  labs(y = "Time to Phone (msec)") + 
  scale_y_continuous(breaks = seq(-500, 2000, 100))
ggplotly(boxplot)

# Calculate Q1 and Q3
q1 <- quantile(MAPEX_PHONE_ALL$difference, probs = 0.25)
q3 <- quantile(MAPEX_PHONE_ALL$difference, probs = 0.75)
iqr <- IQR(MAPEX_PHONE_ALL$difference)
# Calculate the amount of data within Q1 and Q3
data_within_q1_q3 <- sum(MAPEX_PHONE_ALL$difference >= q1 & MAPEX_PHONE_ALL$difference <= q3)

percentage_quart <- (data_within_q1_q3 / length(MAPEX_PHONE_ALL$difference)) * 100

# Print the percentage
print(percentage_quart)

upper_fence <- q3 + 1.5 * iqr
lower_fence <- q1 - 1.5 * iqr


data_within_lower_upper <- sum(MAPEX_PHONE_ALL$difference >= lower_fence & MAPEX_PHONE_ALL$difference <= upper_fence)

percentage <- (data_within_lower_upper / length(MAPEX_PHONE_ALL$difference)) * 100

num_outliers <- sum(MAPEX_PHONE_ALL$difference > upper_fence | MAPEX_PHONE_ALL$difference < lower_fence)

# Calculate the percentage of outliers
percentage_outliers <- (num_outliers / length(MAPEX_PHONE_ALL$difference)) * 100

# Print the percentage of outliers
print(percentage_outliers)

APEX_ALL_CM <- merge(P2M, P2C, by = "X", all = TRUE)


Av <- sum(P2M$difference + P2C$difference) / 662


mean_phone <- mean(CAPEX_PHONE_ALL$difference)
mean_phonem <- mean(MAPEX_PHONE_ALL$difference)


