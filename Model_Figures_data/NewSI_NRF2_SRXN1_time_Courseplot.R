rm(list = ls())
library(magrittr)
library(dbplyr)
library(dplyr)
library(ggplot2)
library(ggnewscale)

library(dplyr)
library(ggplot2)
library(forcats)
library(ggplot2)
library(dplyr)
#install.packages("gridExtra")
library("gridExtra")
# install.packages("ggnewscale")
#https://www.pmxsolutions.com/2019/08/16/plotting-pk-pd-hysteresis-with-variability-in-r-using-ggplot/

directory = "C:/Users/rar/OneDrive - Genmab/Personalfolder/NRFmanuscript/Hysteresis"

file_name <- list.files(path = paste0(directory),pattern = ".txt")

myfiles = lapply(paste0(directory,"/",file_name), read.delim,sep=" ")
names(myfiles) <- file_name


###############################
#Repeated scenarios 
################################
repeated_dosing_32 = myfiles[c("Andrographolide_R_8_24.txt",  "Sulforaphane_R_8_24.txt", "CDDO_me_R_8_24.txt", "Ethacrynic_Acid_R_8_24.txt" )]
repeated_dosing_48 = myfiles[c("Andrographolide_R_24_24.txt",  "Sulforaphane_R_24_24.txt" , "CDDO_me_R_24_24.txt", "Ethacrynic_Acid_R_24_24.txt" )]


repeated_dosing_32$Andrographolide_R_8_24.txt$Treament = rep("Andro", length(repeated_dosing_32$Andrographolide_R_8_24.txt$Srxn1_max))
repeated_dosing_32$Sulforaphane_R_8_24.txt$Treament = rep("Sul", length(repeated_dosing_32$Andrographolide_R_8_24.txt$Srxn1_max))
repeated_dosing_32$CDDO_me_R_8_24.txt$Treament = rep("CDDO", length(repeated_dosing_32$Andrographolide_R_8_24.txt$Srxn1_max))
repeated_dosing_32$Ethacrynic_Acid_R_8_24.txt$Treament = rep("ETA", length(repeated_dosing_32$Andrographolide_R_8_24.txt$Srxn1_max))

Df_32_R = do.call("rbind", list(repeated_dosing_32$Andrographolide_R_8_24.txt,repeated_dosing_32$Sulforaphane_R_8_24.txt,repeated_dosing_32$CDDO_me_R_8_24.txt,repeated_dosing_32$Ethacrynic_Acid_R_8_24.txt))

Df_32_R$Experiment = rep("8_24",length(Df_32_R$NRF2_max))

repeated_dosing_48$Andrographolide_R_24_24.txt$Treament = rep("Andro", length(repeated_dosing_48$Andrographolide_R_24_24.txt$Srxn1_max))
repeated_dosing_48$Sulforaphane_R_24_24.txt$Treament = rep("Sul", length(repeated_dosing_48$Sulforaphane_R_24_24.txt$Srxn1_max))
repeated_dosing_48$CDDO_me_R_24_24.txt$Treament = rep("CDDO", length(repeated_dosing_48$Sulforaphane_R_24_24.txt$Srxn1_max))
repeated_dosing_48$Ethacrynic_Acid_R_24_24.txt$Treament = rep("ETA", length(repeated_dosing_48$Ethacrynic_Acid_R_24_24.txt$Srxn1_max))

Df_48_R = do.call("rbind", list(repeated_dosing_48$Andrographolide_R_24_24.txt,repeated_dosing_48$Sulforaphane_R_24_24.txt,repeated_dosing_48$CDDO_me_R_24_24.txt,repeated_dosing_48$Ethacrynic_Acid_R_24_24.txt))

Df_48_R$Experiment = rep("24_24",length(Df_48_R$NRF2_max))

# Finalplotdata = do.call("rbind", list(Df_32_R, Df_48_R))
# 
# final = data.frame(Finalplotdata)

###############################
#continous scenarios 
################################

continous_data_48 = myfiles[c("Andrographolide_Conti_48.txt","Sulforaphane_Conti_48.txt", "CDDO_me_Conti_48.txt","Ethacrynic_Acid_Conti_48.txt")]
continous_data_32 = myfiles[c("Andrographolide_Conti_32.txt" , "Sulforaphane_Conti_32.txt", "CDDO_me_Conti_32.txt","Ethacrynic_Acid_Conti_32.txt")]

continous_data_32$Andrographolide_Conti_32.txt$Treament = rep("Andro", length(continous_data_32$Andrographolide_Conti_32.txt$Srxn1_max))
continous_data_32$Sulforaphane_Conti_32.txt$Treament = rep("Sul", length(continous_data_32$Andrographolide_Conti_32.txt$Srxn1_max))
continous_data_32$CDDO_me_Conti_32.txt$Treament = rep("CDDO", length(continous_data_32$Andrographolide_Conti_32.txt$Srxn1_max))
continous_data_32$Ethacrynic_Acid_Conti_32.txt$Treament = rep("ETA", length(continous_data_32$Andrographolide_Conti_32.txt$Srxn1_max))

Df_32_continous = do.call("rbind", list(continous_data_32$Andrographolide_Conti_32.txt,continous_data_32$Sulforaphane_Conti_32.txt,continous_data_32$CDDO_me_Conti_32.txt,continous_data_32$Ethacrynic_Acid_Conti_32.txt))

Df_32_continous$Experiment = rep("8_24_continous",length(Df_32_continous$NRF2_max))

Df_32_continous$dose2 = rep(0,length(Df_32_continous$NRF2_max))

continous_data_48$Andrographolide_Conti_48.txt$Treament = rep("Andro", length(continous_data_48$Andrographolide_Conti_48.txt$Srxn1_max))
continous_data_48$Sulforaphane_Conti_48.txt$Treament = rep("Sul", length(continous_data_48$Sulforaphane_Conti_48.txt$Srxn1_max))
continous_data_48$CDDO_me_Conti_48.txt$Treament = rep("CDDO", length(continous_data_48$Sulforaphane_Conti_48.txt$Srxn1_max))
continous_data_48$Ethacrynic_Acid_Conti_48.txt$Treament = rep("ETA", length(continous_data_48$Ethacrynic_Acid_Conti_48.txt$Srxn1_max))

Df_48_continous = do.call("rbind", list(continous_data_48$Andrographolide_Conti_48.txt,continous_data_48$Sulforaphane_Conti_48.txt,
                                        continous_data_48$CDDO_me_Conti_48.txt,continous_data_48$Ethacrynic_Acid_Conti_48.txt))

Df_48_continous$Experiment = rep("24_24_continous",length(Df_48_continous$NRF2_max))

Df_48_continous$dose2 = rep(0,length(Df_48_continous$NRF2_max))


# Full set of experimental data 

Full_set_experimental_simulation = rbind(Df_32_continous,Df_32_R, Df_48_continous, Df_48_R)
glimpse(Full_set_experimental_simulation)
Full_set = data.frame(Full_set_experimental_simulation)
glimpse(Full_set)
names(Full_set)[1] = "Time"
names(Full_set)[18] = "Treatment"
Drug = unique(Full_set$Treatment)
Exp = unique(Full_set$Experiment)
Doses = unique(Full_set$dose1)
Simulations = unique(Full_set$Simulation)
library(ggplot2)
library("ggpubr")
library(tidyr)
#####################################


library(ggplot2)

# Example for one Treatment and dose, adjust as needed
df <- Full_set %>%
  filter(Experiment == "24_24_continous", Simulation == "simulation6") %>%
  select(Time, NRF2_mean,Treatment, SRXN1_observed, dose1)

# Order treatments as requested
treatment_order <- c("Sul", "Andro", "ETA", "CDDO")
df$Treatment <- factor(df$Treatment, levels = treatment_order)


# Example: df_plot has columns: Time, SRXN1_observed, NRF2_mean, Treatment

# Define colors for each protein
color_srxn1 <- "#e41a1c"  # Red
color_nrf2 <- "#377eb8"   # Blue

# Calculate scaling factor to overlay NRF2 on SRXN1 range
scaling_factor <- max(df$SRXN1_observed, na.rm = TRUE) / max(df$NRF2_mean, na.rm = TRUE)

# Create plot
p <- ggplot(df, aes(x = Time)) +
  geom_line(aes(y = SRXN1_observed, color = "Srxn1"), size = 1.2) +
  geom_line(aes(y = NRF2_mean * scaling_factor, color = "Nrf2"), size = 1.2, linetype = "dashed") +
  scale_color_manual(
    name = "Protein",
    values = c("Nrf2" = color_nrf2, "Srxn1" = color_srxn1),
    labels = c("Nrf2", "Srxn1")
  ) +
  facet_wrap(~Treatment, nrow = 1) +
  scale_x_continuous(name = "Time [h]") +
  scale_y_continuous(
    name = "Srxn1 intensity [au]",
    breaks = pretty(df$SRXN1_observed),
    sec.axis = sec_axis(~ . / scaling_factor, name = "Nrf2 intensity [au]", breaks = pretty(df$NRF2_mean))
  ) +
  theme_bw(base_size = 14) +
  theme(
    legend.position = "right",
    strip.background = element_blank(),
    strip.text = element_text(face = "bold", size = 13),
    axis.title.y.left = element_text(color = color_srxn1, size = 13),
    axis.title.y.right = element_text(color = color_nrf2, size = 13),
    axis.text.y.left = element_text(color = color_srxn1),
    axis.text.y.right = element_text(color = color_nrf2)
  )

print(p)
image_dir = "C:/Users/rar/OneDrive - Genmab/Personalfolder/NRFmanuscript/Hysteresis"

# Save for publication
ggsave(paste0(image_dir, "/","newSASRXN1_NRF2_overlay_by_treatment.png"), plot = p, width = 10, height = 4, dpi = 400)
ggsave(paste0(image_dir, "/","newSASRXN1_NRF2_overlay_by_treatment.pdf"), plot = p, width = 10, height = 4)

# Save for publication
ggsave("Nrf2_Srxn1_kinetics_by_treatment.png", plot = p, width = 10, height = 4, dpi = 400)
ggsave("Nrf2_Srxn1_kinetics_by_treatment.pdf", plot = p, width = 10, height = 4)

# Save for publication
ggsave("Nrf2_Srxn1_kinetics_by_treatment.png", plot = p, width = 10, height = 4, dpi = 400)
ggsave("Nrf2_Srxn1_kinetics_by_treatment.pdf", plot = p, width = 10, height = 4)

