rm(list = ls())
library(magrittr)
library(dbplyr)
library(dplyr)
library(ggplot2)
library(ggnewscale)
library(ggplot2)
library("ggpubr")
library(tidyr)
#install.packages("gridExtra")
library("gridExtra")
# install.packages("ggnewscale")
#https://www.pmxsolutions.com/2019/08/16/plotting-pk-pd-hysteresis-with-variability-in-r-using-ggplot/

directory = "C:/Users/rar/OneDrive - Genmab/Personalfolder/NRFmanuscript/Hysteresis"

file_name <- list.files(path = paste0(directory),pattern = ".txt")


image_dir = "C:/Users/rar/OneDrive - Genmab/Personalfolder/NRFmanuscript/Hysteresis"


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
#####################################
# ---- Panel A & B: Hysteresis (continuous only) ----
library(dplyr)
library(ggplot2)
library(ggnewscale)
library(gridExtra)
library(grid)
library(cowplot)   # for easy panel labels

# ---- Panel A & B: Hysteresis (continuous only) ----
pA_32 <- Full_set %>%
  filter(Experiment == "8_24_continous", Simulation == "simulation6") %>%
  ggplot(aes(x = NRF2_mean, y = SRXN1_observed, color = Treatment)) +
  ylim(0, 60) +
  geom_path(size = 1.5) +
  scale_color_manual(values = c("black","yellow","green","grey70"), name = "Treatment") +
  new_scale_color() +
  scale_color_gradient(low = "blue", high = "red") +
  labs(title = "A. Continuous Exposure 32h", x = "NRF2", y = "Srxn1") +
  theme_bw() + theme(legend.position = "none")

pB_48 <- Full_set %>%
  filter(Experiment == "24_24_continous", Simulation == "simulation6") %>%
  ggplot(aes(x = NRF2_mean, y = SRXN1_observed, color = Treatment)) +
  ylim(0, 60) +
  geom_path(size = 1.5) +
  scale_color_manual(values = c("black","yellow","green","grey70"), name = "Treatment") +
  new_scale_color() +
  scale_color_gradient(low = "blue", high = "red") +
  labs(title = "B. Continuous Exposure 48h", x = "NRF2", y = "Srxn1") +
  theme_bw() + theme(legend.position = "none")


# ---- prepare combined time course data (32h + 48h) ----
df_tc <- Full_set %>%
  filter(Experiment %in% c("8_24_continous", "24_24_continous"),
         Simulation == "simulation6") %>%
  mutate(Duration = ifelse(Experiment == "8_24_continous", "32 h", "48 h"),
         Treatment = factor(Treatment, levels = c("Sul","Andro","ETA","CDDO")))

# global scaling factor (based on max across both durations)
sf_global <- max(df_tc$SRXN1_observed, na.rm = TRUE) / max(df_tc$NRF2_mean, na.rm = TRUE)

# global axis limits
ylim_srxn1 <- c(0, max(df_tc$SRXN1_observed, na.rm = TRUE) * 1.05)
ylim_nrf2  <- c(0, max(df_tc$NRF2_mean, na.rm = TRUE) * 1.05)

# helper to make panel with global scaling
make_timecourse_global <- function(exp_code, title_text){
  df <- df_tc %>% filter(Experiment == exp_code)
  ggplot(df, aes(x = Time)) +
    geom_line(aes(y = SRXN1_observed, color = "Srxn1"), size = 1.1) +
    geom_line(aes(y = NRF2_mean * sf_global, color = "Nrf2"), 
              size = 1.1, linetype = "dashed") +
    facet_wrap(~Treatment, nrow = 1) +
    scale_color_manual(name = "Protein",
                       values = c("Nrf2" = "#377eb8", "Srxn1" = "#e41a1c")) +
    scale_x_continuous(
      name   = "Time [h]",
      limits = c(0, 48),
      breaks = seq(0, 48, by = 12)   # tick marks every 12 h
    )+
    scale_y_continuous(
      name = "Srxn1 intensity [au]",
      limits = ylim_srxn1,
      sec.axis = sec_axis(~ . / sf_global, 
                          name = "Nrf2 intensity [au]",
                          breaks = pretty(ylim_nrf2))
    ) +
    labs(title = title_text) +
    theme_bw(base_size = 11) +
    theme(
      legend.position   = "none",
      strip.background  = element_blank(),
      strip.text        = element_text(face = "bold", size = 11),
      axis.title.y.left = element_text(color = "#e41a1c", size = 11),
      axis.title.y.right= element_text(color = "#377eb8", size = 11),
      axis.text.y.left  = element_text(color = "#e41a1c"),
      axis.text.y.right = element_text(color = "#377eb8"),
      axis.text.x       = element_text(angle = 45, hjust = 1),
      plot.title        = element_text(face = "bold", size = 13, hjust = 0)
    )
}

# remake panels with consistent scales
pC_tc32 <- make_timecourse_global("8_24_continous",  "C. Continuous Exposure 32h")
pD_tc48 <- make_timecourse_global("24_24_continous", "D. Continuous Exposure 48h")


library(cowplot)
library(grid)

# ---- legends ----
leg_trt <- get_legend(
  Full_set %>% 
    filter(Experiment == "24_24_continous", Simulation == "simulation6") %>%
    ggplot(aes(NRF2_mean, SRXN1_observed, color = Treatment)) +
    geom_path() +
    scale_color_manual(values = c("black","yellow","green","grey70"),
                       name = "Treatment") +
    theme_bw() + theme(legend.position = "bottom",
                       legend.title = element_text(size=11, face="bold"),
                       legend.text  = element_text(size=10))
)

leg_prot <- get_legend(
  ggplot(df_tc, aes(x = Time)) +
    geom_line(aes(y = SRXN1_observed, color = "Srxn1")) +
    geom_line(aes(y = NRF2_mean, color = "Nrf2")) +
    scale_color_manual(name = "Protein",
                       values = c("Nrf2" = "#377eb8", "Srxn1" = "#e41a1c")) +
    theme_bw() + theme(legend.position = "bottom",
                       legend.title = element_text(size=11, face="bold"),
                       legend.text  = element_text(size=10))
)

# ---- panels (remove legends inside) ----
pA_32c <- pA_32 + theme(legend.position="none")
pB_48c <- pB_48 + theme(legend.position="none")
pC_32c <- pC_tc32 + theme(legend.position="none")
pD_48c <- pD_tc48 + theme(legend.position="none")

title_style <- element_text(size = 13, face = "bold", hjust = 0)

# add to each plot object, e.g.:
pA_32c  <- pA_32c  + theme(plot.title = title_style)
pB_48c  <- pB_48c  + theme(plot.title = title_style)
pC_32c <- pC_32c + theme(plot.title = title_style)
pD_48c <- pD_48c + theme(plot.title = title_style)


# ---- assemble top row (A–B + legend) ----
row_AB <- plot_grid(pA_32c, pB_48c, nrow = 1, rel_widths = c(1,1), align = "hv")
row_AB <- plot_grid(row_AB, leg_trt, ncol = 1, rel_heights = c(1, 0.1))

# ---- assemble bottom row (C–D + legend) ----
row_CD <- plot_grid(pC_32c, pD_48c, nrow = 1, rel_widths = c(1,1), align = "hv")
row_CD <- plot_grid(row_CD, leg_prot, ncol = 1, rel_heights = c(1, 0.1))

# ---- final figure (2×2 stacked) ----
final_pub <- plot_grid(row_AB, row_CD, ncol = 1, rel_heights = c(1,1))


tiff(file.path(image_dir, "Main_Figure_ABCD_pub_ready.tiff"),
     units = "in", width = 8, height = 9, res = 600, compression = "lzw")
print(final_pub)
dev.off()

# ========= Repeated Exposure Figure (A–D) =========
library(dplyr)
library(ggplot2)
library(ggnewscale)
library(cowplot)

# -----------------------------
# HYSTERESIS (Repeated A & B)
# -----------------------------
# -----------------------------
# HYSTERESIS (Repeated A & B)
# -----------------------------
pA_rep32 <- Full_set %>%
  filter(Experiment == "8_24", Simulation == "simulation48") %>%
  ggplot(aes(x = NRF2_mean, y = SRXN1_observed, color = Treatment)) +
  ylim(0, 60) +
  geom_path(size = 1.5) +
  scale_color_manual(values = c("black","yellow","green","grey70"), name = "Treatment") +
  new_scale_color() +
  scale_color_gradient(low = "blue", high = "red") +
  labs(title = "A. Repeated Exposure 32h (8h + 24h)", x = "NRF2", y = "Srxn1") +
  theme_bw(base_size = 11) + theme(legend.position = "none")

pB_rep48 <- Full_set %>%
  filter(Experiment == "24_24", Simulation == "simulation48") %>%
  ggplot(aes(x = NRF2_mean, y = SRXN1_observed, color = Treatment)) +
  ylim(0, 60) +
  geom_path(size = 1.5) +
  scale_color_manual(values = c("black","yellow","green","grey70"), name = "Treatment") +
  new_scale_color() +
  scale_color_gradient(low = "blue", high = "red") +
  labs(title = "B. Repeated Exposure 48h (24h + 24h)", x = "NRF2", y = "Srxn1") +
  theme_bw(base_size = 11) + theme(legend.position = "none")

# ---------- Clean shared legends (no duplication) ----------
# Treatment legend (shows all 4 levels)
trt_levels <- c("Andro","CDDO","ETA","Sul")
trt_cols   <- c("Andro"="black","CDDO"="yellow","ETA"="green","Sul"="grey70")
leg_trt_rep <- cowplot::get_legend(
  ggplot(data.frame(x=1:4, y=1, Treatment=factor(trt_levels, levels=trt_levels)),
         aes(x, y, color = Treatment)) +
    geom_line() +
    scale_color_manual(name = "Treatment", values = trt_cols, breaks = trt_levels) +
    theme_minimal() + theme(legend.position = "bottom")
)

# -------------------------------------------
# TIME COURSES (Repeated C & D) — shared y-axes
# -------------------------------------------
df_rep_tc <- Full_set %>%
  filter(Experiment %in% c("8_24","24_24"), Simulation == "simulation48") %>%
  mutate(Duration = ifelse(Experiment == "8_24", "32 h", "48 h"),
         Treatment = factor(Treatment, levels = c("Sul","Andro","ETA","CDDO")))

# Global scaling so C & D share the same left/right y-axes
sf_rep_global   <- max(df_rep_tc$SRXN1_observed, na.rm = TRUE) / max(df_rep_tc$NRF2_mean, na.rm = TRUE)
ylim_srxn1_rep  <- c(0, max(df_rep_tc$SRXN1_observed, na.rm = TRUE) * 1.05)
ylim_nrf2_rep   <- c(0, max(df_rep_tc$NRF2_mean,       na.rm = TRUE) * 1.05)

# helper to make panel with correct *repeated* data & scales
make_timecourse_rep <- function(exp_code, title_text){
  df <- df_rep_tc %>% filter(Experiment == exp_code)
  # xlims   <- if (exp_code == "8_24") c(0, 32) else c(0, 48)
  # xbreaks <- if (exp_code == "8_24") seq(0, 32, by = 8) else seq(0, 48, by = 12)
  
  ggplot(df, aes(x = Time)) +
    geom_line(aes(y = SRXN1_observed,           color = "Srxn1"), size = 1.1) +
    geom_line(aes(y = NRF2_mean * sf_rep_global, color = "Nrf2"),  size = 1.1, linetype = "dashed") +
    facet_wrap(~Treatment, nrow = 1) +
    scale_color_manual(name = "Protein",
                       values = c("Nrf2" = "#377eb8", "Srxn1" = "#e41a1c"),
                       breaks = c("Srxn1","Nrf2")) +
    scale_x_continuous(
      name   = "Time [h]",
      limits = c(0, 48),
      breaks = seq(0, 48, by = 12)   # tick marks every 12 h
    )+
    scale_y_continuous(
      name = "Srxn1 intensity [au]",
      limits = ylim_srxn1_rep,
      sec.axis = sec_axis(~ . / sf_rep_global,
                          name  = "Nrf2 intensity [au]",
                          breaks = pretty(ylim_nrf2_rep))
    ) +
    labs(title = title_text) +
    theme_bw(base_size = 11) +
    theme(
      legend.position   = "none",
      strip.background  = element_blank(),
      strip.text        = element_text(face = "bold", size = 11),
      axis.title.y.left = element_text(color = "#e41a1c", size = 11),
      axis.title.y.right= element_text(color = "#377eb8", size = 11),
      axis.text.y.left  = element_text(color = "#e41a1c"),
      axis.text.y.right = element_text(color = "#377eb8"),
      axis.text.x       = element_text(angle = 45, hjust = 1),
      plot.title        = element_text(face = "bold", size = 13, hjust = 0)
    )
}

pC_rep32 <- make_timecourse_rep("8_24",  "C. Repeated Exposure 32h (8h + 24h)")
pD_rep48 <- make_timecourse_rep("24_24", "D. Repeated Exposure 48h (24h + 24h)")

# Protein legend (color + dashed/solid)
leg_prot_rep <- cowplot::get_legend(
  ggplot(data.frame(t=c(0,1), v=c(0,1), lab=c("Srxn1","Nrf2")),
         aes(t, v, color = lab, linetype = lab)) +
    geom_line(size = 1.1) +
    scale_color_manual(name = "Protein",
                       values = c("Nrf2"="#377eb8","Srxn1"="#e41a1c"),
                       breaks = c("Srxn1","Nrf2")) +
    scale_linetype_manual(name = "Protein",
                          values = c("Srxn1"="solid","Nrf2"="dashed"),
                          breaks = c("Srxn1","Nrf2")) +
    theme_minimal() + theme(legend.position = "bottom")
)

# ---- strip legends inside panels & unify titles ----
title_style <- element_text(size = 13, face = "bold", hjust = 0)
pA_rep32c <- pA_rep32 + theme(legend.position="none", plot.title = title_style) + guides(color="none", linetype="none")
pB_rep48c <- pB_rep48 + theme(legend.position="none", plot.title = title_style) + guides(color="none", linetype="none")
pC_rep32c <- pC_rep32 + theme(legend.position="none", plot.title = title_style) + guides(color="none", linetype="none")
pD_rep48c <- pD_rep48 + theme(legend.position="none", plot.title = title_style) + guides(color="none", linetype="none")

# ---------------------------------
# Assemble (same layout as main fig)
# ---------------------------------
row_AB_rep <- cowplot::plot_grid(pA_rep32c, pB_rep48c, nrow = 1, rel_widths = c(1,1), align = "hv")
row_AB_rep <- cowplot::plot_grid(row_AB_rep, leg_trt_rep, ncol = 1, rel_heights = c(1, 0.10))

row_CD_rep <- cowplot::plot_grid(pC_rep32c, pD_rep48c, nrow = 1, rel_widths = c(1,1), align = "hv")
row_CD_rep <- cowplot::plot_grid(row_CD_rep, leg_prot_rep, ncol = 1, rel_heights = c(1, 0.10))

final_rep <- cowplot::plot_grid(row_AB_rep, row_CD_rep, ncol = 1, rel_heights = c(1,1))

tiff(file.path(image_dir, "Supp_Figure_ABCD_Repeated_Hysteresis_Timecourse.tiff"),
     units = "in", width = 8, height = 9, res = 600, compression = "lzw")
print(final_rep)
dev.off()
