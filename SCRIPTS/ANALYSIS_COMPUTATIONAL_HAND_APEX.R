library(rstudioapi)   
library(ggplot2)
library(plotly)


parentfolder <- (dirname(rstudioapi::getSourceEditorContext()$path))  
TS <- read.csv(paste0(dirname(parentfolder), "/DATA_TO_PROCESS/BENEDICTE_APEX_TEXGRID_MERGE.csv"))
data_to_process <- paste0(dirname(parentfolder), "/DATA_TO_PROCESS/")


TS[!is.na(TS$timing_peak_speed), ]
#TS[TS$timing_peak_speed != "", ]

TS$timing_peak_speed <- ((TS$timing_peak_speed)/1000)
TS$difference <- round((TS$t1_ax-TS$timing_peak_speed) ,digits = 2)

mdif <- mean(as.numeric(TS$difference),na.rm=TRUE)

plot <- ggplot(TS, aes(x= difference)) + 
    geom_density() + geom_vline(xintercept = mdif, color= "red") + xlim(-5,5) + 
    theme_bw()
ggplotly(plot)


write.csv(TS, paste0(data_to_process, "BENEDICTE_APEX_COMPARISON.csv"))