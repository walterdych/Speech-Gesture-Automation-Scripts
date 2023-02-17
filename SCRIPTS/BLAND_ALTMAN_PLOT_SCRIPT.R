library(rstudioapi)   
library(binhf)
library(tidyverse)
library(plotly)

parentfolder <- (dirname(rstudioapi::getSourceEditorContext()$path))
data_processed <- file.path(dirname(parentfolder), "DATA_PROCESSED")
data_to_process <- file.path(dirname(parentfolder), "DATA_TO_PROCESS")

#List files with corresponding file name pattern
file_names <- list.files(data_processed, pattern = "_Manual_Comp_Dif.csv")
P <- list()

#Load files into data frames and create unique data frames per participant, label participant numbers
for (i in seq_along(file_names)) {
  file <- file_names[i]
  df <- read.csv(file.path(data_processed, file))
  assign(paste0("P", i), df %>% mutate(Participant = paste0("Participant ", i)))
  P[[i]] <- get(paste0("P", i))
}

#Combine all P1, P2, P3, etc. data frames into a single data frame called combined_df
combined_df <- bind_rows(P)

#Calculate data points for plotting
mean_diff <- mean(combined_df$difference)
mean_diff

lower <- mean_diff - 1.96*sd(combined_df$difference)
lower

upper <- mean_diff + 1.96*sd(combined_df$difference)
upper

combined_df$avg <- rowMeans(combined_df[, c("time_ms", "t1_ax")])

#Create the Bland-Altman plot of the combined data
plot <- ggplot(combined_df, aes(x = avg, y = difference, color = Participant)) +
  geom_point(size=1) + scale_y_continuous(breaks=seq(-1000, 1000, 250)) +
  geom_hline(yintercept = mean_diff) +
  geom_hline(yintercept = lower, color = "red", linetype="dashed") +
  geom_hline(yintercept = upper, color = "red", linetype="dashed") +
  ggtitle("Bland-Altman Plot") +
  ylab("Apex Timing Differences") +
  xlab("Average")+theme_bw()
ggplotly(plot)

#Save combined file
write.csv(combined_df, file.path(data_processed, "All_Participants_Manual_Comp_Apex_Dif.csv"), row.names = FALSE)



