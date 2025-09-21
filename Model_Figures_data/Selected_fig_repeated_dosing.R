
rm(list = ls())
library(magrittr)
library(dbplyr)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(tidyr)

directory = "C:/Users/alexe/OneDrive - Universiteit Leiden/mcsim_files/Generic_curated_model/Model_Figures_data"

file_name <- list.files(path = paste0(directory),pattern = ".txt")

myfiles = lapply(paste0(directory,"/",file_name), read.delim,sep=" ")
names(myfiles) <- file_name

repeated_dosing_32 = myfiles[c("Andrographolide_R_8_24.txt",  "Sulforaphane_R_8_24.txt", "CDDO_me_R_8_24.txt", "Ethacrynic_Acid_R_8_24.txt" )]
repeated_dosing_48 = myfiles[c("Andrographolide_R_24_24.txt",  "Sulforaphane_R_24_24.txt" , "CDDO_me_R_24_24.txt", "Ethacrynic_Acid_R_24_24.txt" )]


repeated_dosing_32$Andrographolide_R_8_24.txt$Treament = rep("Andro", length(repeated_dosing_32$Andrographolide_R_8_24.txt$Srxn1_max))
repeated_dosing_32$Sulforaphane_R_8_24.txt$Treament = rep("Sul", length(repeated_dosing_32$Andrographolide_R_8_24.txt$Srxn1_max))
repeated_dosing_32$CDDO_me_R_8_24.txt$Treament = rep("CDDO", length(repeated_dosing_32$Andrographolide_R_8_24.txt$Srxn1_max))
repeated_dosing_32$Ethacrynic_Acid_R_8_24.txt$Treament = rep("ETA", length(repeated_dosing_32$Andrographolide_R_8_24.txt$Srxn1_max))

Df = do.call("rbind", list(repeated_dosing_32$Andrographolide_R_8_24.txt,repeated_dosing_32$Sulforaphane_R_8_24.txt,repeated_dosing_32$CDDO_me_R_8_24.txt,repeated_dosing_32$Ethacrynic_Acid_R_8_24.txt))

Df$Experiment = rep("8_24",length(Df$NRF2_max))


repeated_dosing_48$Andrographolide_R_24_24.txt$Treament = rep("Andro", length(repeated_dosing_48$Andrographolide_R_24_24.txt$Srxn1_max))
repeated_dosing_48$Sulforaphane_R_24_24.txt$Treament = rep("Sul", length(repeated_dosing_48$Sulforaphane_R_24_24.txt$Srxn1_max))
repeated_dosing_48$CDDO_me_R_24_24.txt$Treament = rep("CDDO", length(repeated_dosing_48$Sulforaphane_R_24_24.txt$Srxn1_max))
repeated_dosing_48$Ethacrynic_Acid_R_24_24.txt$Treament = rep("ETA", length(repeated_dosing_48$Ethacrynic_Acid_R_24_24.txt$Srxn1_max))

Df1 = do.call("rbind", list(repeated_dosing_48$Andrographolide_R_24_24.txt,repeated_dosing_48$Sulforaphane_R_24_24.txt,repeated_dosing_48$CDDO_me_R_24_24.txt,repeated_dosing_48$Ethacrynic_Acid_R_24_24.txt))

Df1$Experiment = rep("24_24",length(Df1$NRF2_max))

Finalplotdata = do.call("rbind", list(Df, Df1))

final = data.frame(Finalplotdata)
glimpse(Finalplotdata)

unique(Finalplotdata$Treament)

Drug = unique(Finalplotdata$Treament)
Exp = unique(Finalplotdata$Experiment)  # 1 for  "8_24" and 2 for 24_24 

first_dose = unique(Finalplotdata$dose1)  # 1 for  "8_24" and 2 for 24_24 
second_dose = unique(Finalplotdata$dose2)  # 1 for  "8_24" and 2 for 24_24 

plot = final %>% unite("dose",dose1:dose2,remove = FALSE)
dose_Andr = plot %>% filter(.,Treament == Drug[1]) %>% filter(.,Experiment == Exp[2]) 
dose_Andr1 = unique(dose_Andr$dose)
dose_Andr1 = c("0.1_10","0.1_31.62","1_1","3.16_3.16", "10_0.1","31.62_0.1")


dose_sul = plot %>% filter(.,Treament == Drug[2]) %>% filter(.,Experiment == Exp[2]) 
dose_sul1 = unique(dose_sul$dose)
dose_sul1 = c("0.35_3.5","0.35_16.25", "1.62_1.62", "7.54_7.54", "3.5_0.35","16.25_0.35")


dose_Etha = plot %>% filter(.,Treament == Drug[4]) %>% filter(.,Experiment == Exp[2]) 
dose_Etha1 = unique(dose_Etha$dose)
dose_Etha1 = c("4.64_10","1_46.42", "4.64_4.64", "21.54_21.54", "10_4.64","46.42_1")
dose_CDDO1 = c("0.01_0.1","0.01_0.46", "0.02_0.02", "0.22_0.22", "0.1_0.01","0.46_0.01")

dose_CDDO = plot %>% filter(.,Treament == Drug[3]) %>% filter(.,Experiment == Exp[2]) 
dose_CDDO1 = unique(dose_CDDO$dose)


Final_select_dose = c("0.35_3.5","0.35_16.25", "1.62_1.62", "7.54_7.54", "3.5_0.35","16.25_0.35",
                      "0.1_10","0.1_31.62","1_1","3.16_3.16", "10_0.1","31.62_0.1",
                       "4.64_10","1_46.42", "4.64_4.64", "21.54_21.54", "10_4.64","46.42_1",
                      "0.01_0.1","0.01_0.46", "0.02_0.02", "0.22_0.22", "0.1_0.01","0.46_0.01")
   
plot_final = plot %>% filter(.,Experiment == Exp[2]) %>% filter(.,dose %in% c(dose_Andr1,dose_sul1,dose_Etha1,dose_CDDO1)) 

plot_final$dose <- factor(plot_final$dose, levels=(Final_select_dose))


  
  
plot_final1 = plot_final %>% 
  filter(.,Treament == Drug[2]) %>% 
  ggplot() +
  geom_line(aes(x = time, y = Srxn1_max, color = "Simulated_max"),size = 0.7) +
  geom_line(aes(x = time, y = Srxn1_mean, color="Simulated_mean"),size = 0.7) +
  geom_line(aes(x = time, y = Srxn1_min, color="Simulated_min"),size = 0.7) +
  geom_errorbar(aes(x = time, ymin=SRXN1_observed-SRXN1_SD, ymax= SRXN1_observed +SRXN1_SD,color = "Experiments_mean(Sd)"), size=0.5,   
                width=.25) + 
  facet_grid(Treament~dose) +
  theme(strip.text.x = element_text(size = 7),strip.text.y = element_text(size = 7)) +
  labs(x = "Time (hrs)",y="GFP intensity",title = ("A. 48h Repeated Exposure Srxn1 response")) +
  scale_color_manual(name = "Simulation type",
                     breaks = c("Simulated_max", "Simulated_mean","Simulated_min", "Experiments_mean(Sd)"),
                     values = c("Simulated_max" = "red", "Simulated_mean" = "blue", "Simulated_min" = "green","Experiments_mean(Sd)" = "black"))+
  theme(axis.title.x=element_blank(),axis.title.y=element_blank(),axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),axis.text=element_text(size = 7),plot.title=element_text(size = 7),
        legend.text = element_text(size = 7),legend.title = element_text(size = 7))

plot_final2 = plot_final %>%  
  filter(.,Treament == Drug[1]) %>% 
  ggplot() +
  geom_line(aes(x = time, y = Srxn1_max, color = "Simulated_max"),size = 0.7) +
  geom_line(aes(x = time, y = Srxn1_mean, color="Simulated_mean"),size = 0.7) +
  geom_line(aes(x = time, y = Srxn1_min, color="Simulated_min"),size = 0.7) +
  geom_errorbar(aes(x = time, ymin=SRXN1_observed-SRXN1_SD, ymax= SRXN1_observed +SRXN1_SD,color = "Experiments_mean(Sd)"), size=0.5,   
                width=.25) + 
  facet_grid(Treament~dose) +
  theme(strip.text.x = element_text(size = 7),strip.text.y = element_text(size = 7)) +
  scale_color_manual(name = "Simulation type",
                     breaks = c("Simulated_max", "Simulated_mean","Simulated_min", "Experiments_mean(Sd)"),
                     values = c("Simulated_max" = "red", "Simulated_mean" = "blue", "Simulated_min" = "green","Experiments_mean(Sd)" = "black"))+
  theme(axis.title.x=element_blank(),axis.title.y=element_blank(),axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),axis.text=element_text(size = 7),plot.title=element_text(size = 7),
        legend.text = element_text(size = 7),legend.title = element_text(size = 7))
  
plot_final3 = plot_final %>% 
  filter(.,Treament == Drug[4]) %>% 
  ggplot() +
  geom_line(aes(x = time, y = Srxn1_max, color = "Simulated_max"),size = 0.7) +
  geom_line(aes(x = time, y = Srxn1_mean, color="Simulated_mean"),size = 0.7) +
  geom_line(aes(x = time, y = Srxn1_min, color="Simulated_min"),size = 0.7) +
  geom_errorbar(aes(x = time, ymin=SRXN1_observed-SRXN1_SD, ymax= SRXN1_observed +SRXN1_SD,color = "Experiments_mean(Sd)"), size=0.5,   
                width=.25) + 
  facet_grid(Treament~dose) +
  theme(strip.text.x = element_text(size = 7),strip.text.y = element_text(size = 7)) +
  scale_color_manual(name = "Simulation type",
                     breaks = c("Simulated_max", "Simulated_mean","Simulated_min", "Experiments_mean(Sd)"),
                     values = c("Simulated_max" = "red", "Simulated_mean" = "blue", "Simulated_min" = "green","Experiments_mean(Sd)" = "black"))+
  theme(axis.title.x=element_blank(),axis.title.y=element_blank(),axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),axis.text=element_text(size = 7),plot.title=element_text(size = 7),
        legend.text = element_text(size = 7),legend.title = element_text(size = 7))


plot_final4 = plot_final %>%  
  filter(.,Treament == Drug[3]) %>% 
  ggplot() +
  geom_line(aes(x = time, y = Srxn1_max, color = "Simulated_max"),size = 0.7) +
  geom_line(aes(x = time, y = Srxn1_mean, color="Simulated_mean"),size = 0.7) +
  geom_line(aes(x = time, y = Srxn1_min, color="Simulated_min"),size = 0.7) +
  geom_errorbar(aes(x = time, ymin=SRXN1_observed-SRXN1_SD, ymax= SRXN1_observed +SRXN1_SD,color = "Experiments_mean(Sd)"), size=0.5,   
                width=.25) + 
  facet_grid(Treament~dose) +
  theme(strip.text.x = element_text(size = 7),strip.text.y = element_text(size = 7)) +
  labs(x = "Time (hrs)",y="GFP intensity") +
  scale_color_manual(name = "Simulation type",
                     breaks = c("Simulated_max", "Simulated_mean","Simulated_min", "Experiments_mean(Sd)"),
                     values = c("Simulated_max" = "red", "Simulated_mean" = "blue", "Simulated_min" = "green","Experiments_mean(Sd)" = "black"))+
  theme(axis.title.x=element_blank(),axis.title.y=element_blank(),axis.text=element_text(size = 7),plot.title=element_text(size = 7),
        legend.text = element_text(size = 7),legend.title = element_text(size = 7))
  

plot_final_exp2 = plot %>% filter(.,Experiment == Exp[1]) %>% filter(.,dose %in% c(dose_Andr1,dose_sul1,dose_Etha1,dose_CDDO1)) 
plot_final_exp2$dose <- factor(plot_final_exp2$dose, levels=(Final_select_dose))

plot_final5 = plot_final_exp2 %>% 
  filter(.,Treament == Drug[2]) %>% 
  ggplot() +
  geom_line(aes(x = time, y = Srxn1_max, color = "Simulated_max"),size = 0.7) +
  geom_line(aes(x = time, y = Srxn1_mean, color="Simulated_mean"),size = 0.7) +
  geom_line(aes(x = time, y = Srxn1_min, color="Simulated_min"),size = 0.7) +
  geom_errorbar(aes(x = time, ymin=SRXN1_observed-SRXN1_SD, ymax= SRXN1_observed +SRXN1_SD,color = "Experiments_mean(Sd)"), size=0.5,   
                width=.25) + 
  facet_grid(Treament~dose) +
  
  theme(strip.text.x = element_text(size = 7),strip.text.y = element_text(size = 7)) +
  labs(x = "Time (hrs)",y="GFP intensity",title = ("B. 32h Repeated Exposure Srxn1 response")) +
  scale_color_manual(name = "Simulation type",
                     breaks = c("Simulated_max", "Simulated_mean","Simulated_min", "Experiments_mean(Sd)"),
                     values = c("Simulated_max" = "red", "Simulated_mean" = "blue", "Simulated_min" = "green","Experiments_mean(Sd)" = "black"))+
  theme(axis.title.x=element_blank(),axis.title.y=element_blank(),axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),axis.text=element_text(size = 7),plot.title=element_text(size = 7),
        legend.text = element_text(size = 7),legend.title = element_text(size = 7))


plot_final6 = plot_final_exp2 %>%  
  filter(.,Treament == Drug[1]) %>% 
  ggplot() +
  geom_line(aes(x = time, y = Srxn1_max, color = "Simulated_max"),size = 0.7) +
  geom_line(aes(x = time, y = Srxn1_mean, color="Simulated_mean"),size = 0.7) +
  geom_line(aes(x = time, y = Srxn1_min, color="Simulated_min"),size = 0.7) +
  geom_errorbar(aes(x = time, ymin=SRXN1_observed-SRXN1_SD, ymax= SRXN1_observed +SRXN1_SD,color = "Experiments_mean(Sd)"), size=0.5,   
                width=.25) + 
  facet_grid(Treament~dose) +
  theme(strip.text.x = element_text(size = 7),strip.text.y = element_text(size = 7)) +
  scale_color_manual(name = "Simulation type",
                     breaks = c("Simulated_max", "Simulated_mean","Simulated_min", "Experiments_mean(Sd)"),
                     values = c("Simulated_max" = "red", "Simulated_mean" = "blue", "Simulated_min" = "green","Experiments_mean(Sd)" = "black"))+
  theme(axis.title.x=element_blank(),axis.title.y=element_blank(),axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),axis.text=element_text(size = 7),plot.title=element_text(size = 7),
        legend.text = element_text(size = 7),legend.title = element_text(size = 7))

plot_final7 = plot_final_exp2 %>% 
  filter(.,Treament == Drug[4]) %>% 
  ggplot() +
  geom_line(aes(x = time, y = Srxn1_max, color = "Simulated_max"),size=0.7) +
  geom_line(aes(x = time, y = Srxn1_mean, color="Simulated_mean"),size=0.7) +
  geom_line(aes(x = time, y = Srxn1_min, color="Simulated_min"),size=0.7) +
  geom_errorbar(aes(x = time, ymin=SRXN1_observed-SRXN1_SD, ymax= SRXN1_observed +SRXN1_SD,color = "Experiments_mean(Sd)"), size=0.5,   
                width=.25) + 
  facet_grid(Treament~dose) +
  theme(strip.text.x = element_text(size = 7),strip.text.y = element_text(size = 7)) +
  scale_color_manual(name = "Simulation type",
                     breaks = c("Simulated_max", "Simulated_mean","Simulated_min", "Experiments_mean(Sd)"),
                     values = c("Simulated_max" = "red", "Simulated_mean" = "blue", "Simulated_min" = "green","Experiments_mean(Sd)" = "black"))+
  theme(axis.title.x=element_blank(),axis.title.y=element_blank(),axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),axis.text=element_text(size = 7),plot.title=element_text(size = 7),
        legend.text = element_text(size = 7),legend.title = element_text(size = 7))


plot_final8 = plot_final_exp2 %>%  
  filter(.,Treament == Drug[3]) %>% 
  ggplot() +
  geom_line(aes(x = time, y = Srxn1_max, color = "Simulated_max"),size=0.7) +
  geom_line(aes(x = time, y = Srxn1_mean, color="Simulated_mean"),size=0.7) +
  geom_line(aes(x = time, y = Srxn1_min, color="Simulated_min"),size=0.7) +
  geom_errorbar(aes(x = time, ymin=SRXN1_observed-SRXN1_SD, ymax= SRXN1_observed +SRXN1_SD,color = "Experiments_mean(Sd)"), size=0.5,   
                width=.25) + 
  facet_grid(Treament~dose) +
  theme(strip.text.x = element_text(size = 7),strip.text.y = element_text(size = 7)) +
  scale_color_manual(name = "Simulation type",
                     breaks = c("Simulated_max", "Simulated_mean","Simulated_min", "Experiments_mean(Sd)"),
                     values = c("Simulated_max" = "red", "Simulated_mean" = "blue", "Simulated_min" = "green","Experiments_mean(Sd)" = "black"))+
    theme(axis.title.x=element_blank(),axis.title.y=element_blank(),axis.text=element_text(size = 7),plot.title=element_text(size = 7),
          legend.text = element_text(size = 7),legend.title = element_text(size = 7))
  

figdirec = "C:/Users/alexe/OneDrive - Universiteit Leiden/Manusript_Nrf2_repeated_dosing/Images"
#pdf(paste0(figdirec,"/", "Selected_repeated_dose1",".pdf"),onefile=FALSE)
tiff(paste0(figdirec,"/", "Selected_repeated_dose",".png"), units="in", width=6, height=4, res=700, compression = 'lzw')
#png(paste0(figdirec,"/", "Selected_repeated_dose",".png"), width = 600, height = 500, res = 100)

p3 = ggarrange(plot_final1+ scale_y_continuous(limits = c(0,60)), plot_final2 + scale_y_continuous(limits = c(0,60)), 
               plot_final3+ scale_y_continuous(limits = c(0,60)),
               plot_final4+ scale_y_continuous(limits = c(0,60)),
               plot_final5+ scale_y_continuous(limits = c(0,40)), plot_final6 + scale_y_continuous(limits = c(0,40)), 
               plot_final7+ scale_y_continuous(limits = c(0,40)),
               plot_final8+ scale_y_continuous(limits = c(0,40)),
               common.legend = TRUE, legend = "bottom",
               nrow = 8)


annotate_figure(p3,
                left = text_grob("Srxn1 intensity [au]", color = "black", rot = 90,vjust = 1,hjust = -0.1),
                bottom = text_grob("Time [h]",hjust = 0.5, vjust = -4.65,
                                   size = 7))

dev.off()

figdirec = "C:/Users/alexe/OneDrive - Universiteit Leiden/Manusript_Nrf2_repeated_dosing/Images"

tiff(paste0(figdirec,"/", "Selected_repeated_dose",".png"), units="in", width=6, height=4, res=700, compression = 'lzw')
p3 = ggarrange(plot_final1+ scale_y_continuous(limits = c(0,60)), plot_final2 + scale_y_continuous(limits = c(0,60)), 
               plot_final3+ scale_y_continuous(limits = c(0,60)),
          plot_final4+ scale_y_continuous(limits = c(0,60)),
          common.legend = TRUE, legend = "bottom",
          nrow = 4, widths = c(1, 0.05, 1))


annotate_figure(p3,
                left = text_grob("GFP intensity [au]", color = "black", rot = 90,vjust = 1,hjust = -0.1),
                bottom = text_grob("Time [h]",color = "black",hjust = 0.5, vjust = -4,
                                   size = 10))

dev.off()