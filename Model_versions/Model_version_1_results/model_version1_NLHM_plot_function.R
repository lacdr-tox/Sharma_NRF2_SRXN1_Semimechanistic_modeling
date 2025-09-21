rm(list = ls())
library(magrittr)
library(dbplyr)
library(dplyr)
library(ggplot2)
library("gridExtra")
library(ggsci)
library(ggpubr)
library("tidyr")

directory ="C:/Users/alexe/OneDrive - Universiteit Leiden/mcsim_files/Model_versions/Model_version_1_results"

file_name <- list.files(path = paste0(directory),pattern = ".txt")

myfiles = lapply(paste0(directory,"/",file_name), read.delim,sep=" ")
names(myfiles) <- file_name

continous_data_48 = myfiles[c("Andrographolide_Conti_48.txt","Sulforaphane_Conti_48.txt", "CDDO_me_Conti_48.txt","Ethacrynic_Acid_Conti_48.txt")]
continous_data_32 = myfiles[c("Andrographolide_Conti_32.txt" , "Sulforaphane_Conti_32.txt", "CDDO_me_Conti_32.txt","Ethacrynic_Acid_Conti_32.txt")]

continous_data_32$Andrographolide_Conti_32.txt$Treament = rep("Andro", length(continous_data_32$Andrographolide_Conti_32.txt$Srxn1_max))
continous_data_32$Sulforaphane_Conti_32.txt$Treament = rep("Sul", length(continous_data_32$Andrographolide_Conti_32.txt$Srxn1_max))
continous_data_32$CDDO_me_Conti_32.txt$Treament = rep("CDDO", length(continous_data_32$Andrographolide_Conti_32.txt$Srxn1_max))
continous_data_32$Ethacrynic_Acid_Conti_32.txt$Treament = rep("ETA", length(continous_data_32$Andrographolide_Conti_32.txt$Srxn1_max))

Df = do.call("rbind", list(continous_data_32$Andrographolide_Conti_32.txt,continous_data_32$Sulforaphane_Conti_32.txt,continous_data_32$CDDO_me_Conti_32.txt,continous_data_32$Ethacrynic_Acid_Conti_32.txt))

Df$Experiment = rep("8_24_continous",length(Df$NRF2_max))

Df$dose2 = rep(0,length(Df$NRF2_max))

continous_data_48$Andrographolide_Conti_48.txt$Treament = rep("Andro", length(continous_data_48$Andrographolide_Conti_48.txt$Srxn1_max))
continous_data_48$Sulforaphane_Conti_48.txt$Treament = rep("Sul", length(continous_data_48$Sulforaphane_Conti_48.txt$Srxn1_max))
continous_data_48$CDDO_me_Conti_48.txt$Treament = rep("CDDO", length(continous_data_48$Sulforaphane_Conti_48.txt$Srxn1_max))
continous_data_48$Ethacrynic_Acid_Conti_48.txt$Treament = rep("ETA", length(continous_data_48$Ethacrynic_Acid_Conti_48.txt$Srxn1_max))

Df2 = do.call("rbind", list(continous_data_48$Andrographolide_Conti_48.txt,continous_data_48$Sulforaphane_Conti_48.txt,
                            continous_data_48$CDDO_me_Conti_48.txt,continous_data_48$Ethacrynic_Acid_Conti_48.txt))

Df2$Experiment = rep("24_24_continous",length(Df2$NRF2_max))

Df2$dose2 = rep(0,length(Df2$NRF2_max))

Df4 = do.call("rbind", list(Df, Df2))  # combine both continous scenarios
glimpse(Df4)

Df4 = Df4%>% mutate_if(is.numeric, funs(replace_na(., 0)))

Simulations = unique(Df4$Simulation)

Drug = c("Sul" ,"Andro","ETA","CDDO")
Exp = unique(Df4$Experiment)
# png(paste0(directory,"/", "Version_1_2",".png"), width = 630, height = 600)
# op <- par(mfrow = c(2,2),
#           oma = c(4,4,0,0) + 0.1,
#           mar = c(1,0,3,2) + 0.1)
# Simulations = unique(Df4$Simulation)
# final1 = Df4 %>% filter(.,Treament == Drug[1]) %>% filter(.,Experiment == Exp[2]) %>% 
#   filter(.,Simulation  %in% Simulations[c(1,2,3)])
# R_square <- cor(final1$SRXN1_observed, final1$Srxn1_mean)^2
# plot(final1$Srxn1_mean,final1$SRXN1_observed,
#      xlim = c(0.1, 15), ylim = c(0.1, 15),
#      xlab =  "Predicted",ylab ="Observed", 
#      text(8, 14, paste("(R2 =", round(R_square, digits = 3),")"),cex=1.1))
# #main = paste("(R2 =", round(R_square, digits = 3),")"))
# abline(0,1)
# 
# final1 = Df4 %>% filter(.,Treament == Drug[1]) %>% filter(.,Experiment == Exp[2]) %>% 
#   filter(.,Simulation  %in% Simulations[c(4,5,6)])
# R_square <- cor(final1$SRXN1_observed, final1$Srxn1_mean)^2
# plot(final1$Srxn1_mean,final1$SRXN1_observed,
#      xlim = c(0.1, 70), ylim = c(0.1, 70),
#      xlab =  "Predicted",ylab ="Observed", 
#      text(40, 65, paste("(R2 =", round(R_square, digits = 3),")"),cex=1.1))
# #main = paste("(R2 =", round(R_square, digits = 3),")"))
# abline(0,1)

###############################
#time response plot 
##############################
Dose1 = c(35,100,100,1)
Drug = c("Sul" ,"Andro","ETA","CDDO")
Exp = unique(Df4$Experiment)
#Drug = unique(Df4$Treament)
str(Df4)
final1 = Df4 %>% filter(.,Treament == Drug[1]) %>% filter(.,Experiment == Exp[2]) %>% 
  filter(.,Simulation  %in% Simulations[c(1,2,3)])
final2 = Df4 %>% filter(.,Treament == Drug[1]) %>% filter(.,Experiment == Exp[2]) %>% 
  filter(.,Simulation  %in% Simulations[c(4,5,6)])

mypal = pal_npg("nrc", alpha = 0.7)(9)
mypal
plot1 = final1 %>% filter(.,Treament == Drug[1]) %>% filter(.,Experiment == Exp[2]) %>% 
  filter(.,dose1 != Dose1[1])  %>% 
  ggplot(aes(x = time, y = Srxn1_mean,color = as.factor(dose1),group= as.factor(dose1))) +
  geom_line() +
  scale_colour_manual(values=c(mypal[1],mypal[2],mypal[3],mypal[4],mypal[5],mypal[6])) +
  geom_errorbar(aes(ymin=SRXN1_observed-SRXN1_SD, ymax= SRXN1_observed +SRXN1_SD), size=0.5,   
                width=.25) + theme(legend.position="none") +
  labs(x = "Time [h]",y="GFP intensity [au]", title = paste0("A.", Drug[1],"_Model_V.1")) +
  theme(axis.title.x=element_blank(),axis.title.y=element_blank(),axis.text.x=element_blank(),
        axis.ticks.x=element_blank())


plot2 = final2 %>% filter(.,Treament == Drug[1]) %>% filter(.,Experiment == Exp[2]) %>% 
  filter(.,dose1 != Dose1[1])  %>% 
  ggplot(aes(x = time, y = Srxn1_mean,color = as.factor(dose1),group= as.factor(dose1))) +
  geom_line() +
  scale_colour_manual(values=c(mypal[4],mypal[5],mypal[6])) +
  
  geom_errorbar(aes(ymin=SRXN1_observed-SRXN1_SD, ymax= SRXN1_observed +SRXN1_SD), size=0.5,   
                width=.25) + theme(legend.position="none") +
  
  labs(x = "Time [h]",y="GFP intensity [au]", title = paste0(Drug[1],"_Model_V.1"))+
  theme(plot.title=element_blank(),axis.title.x=element_blank(),axis.title.y=element_blank())

final3 = Df4 %>% filter(.,Treament == Drug[2]) %>% filter(.,Experiment == Exp[2]) %>% 
  filter(.,Simulation  %in% Simulations[c(1,2,3)])
final4 = Df4 %>% filter(.,Treament == Drug[2]) %>% filter(.,Experiment == Exp[2]) %>% 
  filter(.,Simulation  %in% Simulations[c(4,5,6)])

plot3 = final3 %>% filter(.,Treament == Drug[2]) %>% filter(.,Experiment == Exp[2]) %>% 
  filter(.,dose1 != Dose1[2])  %>% 
  ggplot(aes(x = time, y = Srxn1_mean,color = as.factor(dose1),group= as.factor(dose1))) +
  geom_line() +
  scale_colour_manual(values=c(mypal[1],mypal[2],mypal[3],mypal[4],mypal[5],mypal[6])) +
  geom_errorbar(aes(ymin=SRXN1_observed-SRXN1_SD, ymax= SRXN1_observed +SRXN1_SD), size=0.5,   
                width=.25) + theme(legend.position="none") +
  labs(x = "Time [h]",y="GFP intensity [au]", title = paste0("B.", Drug[2],"_Model_V.1")) +
  theme(axis.title.x=element_blank(),axis.title.y=element_blank(),axis.text.x=element_blank(),
        axis.ticks.x=element_blank())


plot4 = final4 %>% filter(.,Treament == Drug[2]) %>% filter(.,Experiment == Exp[2]) %>% 
  filter(.,dose1 != Dose1[2])  %>% 
  ggplot(aes(x = time, y = Srxn1_mean,color = as.factor(dose1),group= as.factor(dose1))) +
  geom_line() +
  scale_colour_manual(values=c(mypal[4],mypal[5],mypal[6])) +
  
  geom_errorbar(aes(ymin=SRXN1_observed-SRXN1_SD, ymax= SRXN1_observed +SRXN1_SD), size=0.5,   
                width=.25) + theme(legend.position="none") +
  
  labs(x = "Time [h]",y="GFP intensity [au]", title = paste0(Drug[2],"_Model_V.1"))+
  theme(plot.title=element_blank(),axis.title.x=element_blank(),axis.title.y=element_blank())




#########################
#Model version_2 
#################
directory ="C:/Users/alexe/OneDrive - Universiteit Leiden/mcsim_files/Model_versions/Model_version_2_results"

file_name <- list.files(path = paste0(directory),pattern = ".txt")

myfiles = lapply(paste0(directory,"/",file_name), read.delim,sep=" ")
names(myfiles) <- file_name

continous_data_48 = myfiles[c("Andrographolide_Conti_48.txt","Sulforaphane_Conti_48.txt", "CDDO_me_Conti_48.txt","Ethacrynic_Acid_Conti_48.txt")]
continous_data_32 = myfiles[c("Andrographolide_Conti_32.txt" , "Sulforaphane_Conti_32.txt", "CDDO_me_Conti_32.txt","Ethacrynic_Acid_Conti_32.txt")]

continous_data_32$Andrographolide_Conti_32.txt$Treament = rep("Andro", length(continous_data_32$Andrographolide_Conti_32.txt$Srxn1_max))
continous_data_32$Sulforaphane_Conti_32.txt$Treament = rep("Sul", length(continous_data_32$Andrographolide_Conti_32.txt$Srxn1_max))
continous_data_32$CDDO_me_Conti_32.txt$Treament = rep("CDDO", length(continous_data_32$Andrographolide_Conti_32.txt$Srxn1_max))
continous_data_32$Ethacrynic_Acid_Conti_32.txt$Treament = rep("ETA", length(continous_data_32$Andrographolide_Conti_32.txt$Srxn1_max))

Df = do.call("rbind", list(continous_data_32$Andrographolide_Conti_32.txt,continous_data_32$Sulforaphane_Conti_32.txt,continous_data_32$CDDO_me_Conti_32.txt,continous_data_32$Ethacrynic_Acid_Conti_32.txt))

Df$Experiment = rep("8_24_continous",length(Df$NRF2_max))

Df$dose2 = rep(0,length(Df$NRF2_max))

continous_data_48$Andrographolide_Conti_48.txt$Treament = rep("Andro", length(continous_data_48$Andrographolide_Conti_48.txt$Srxn1_max))
continous_data_48$Sulforaphane_Conti_48.txt$Treament = rep("Sul", length(continous_data_48$Sulforaphane_Conti_48.txt$Srxn1_max))
continous_data_48$CDDO_me_Conti_48.txt$Treament = rep("CDDO", length(continous_data_48$Sulforaphane_Conti_48.txt$Srxn1_max))
continous_data_48$Ethacrynic_Acid_Conti_48.txt$Treament = rep("ETA", length(continous_data_48$Ethacrynic_Acid_Conti_48.txt$Srxn1_max))

Df2 = do.call("rbind", list(continous_data_48$Andrographolide_Conti_48.txt,continous_data_48$Sulforaphane_Conti_48.txt,
                            continous_data_48$CDDO_me_Conti_48.txt,continous_data_48$Ethacrynic_Acid_Conti_48.txt))

Df2$Experiment = rep("24_24_continous",length(Df2$NRF2_max))

Df2$dose2 = rep(0,length(Df2$NRF2_max))

Df4 = do.call("rbind", list(Df, Df2))  # combine both continous scenarios
glimpse(Df4)

Df4 = Df4%>% mutate_if(is.numeric, funs(replace_na(., 0)))
Drug = c("Sul" ,"Andro","ETA","CDDO")
Exp = unique(Df4$Experiment)

Simulations = unique(Df4$Simulation)
#par(mfrow = c(4,2))
# final1 = Df4 %>% filter(.,Treament == Drug[1]) %>% filter(.,Experiment == Exp[2]) %>% 
#   filter(.,Simulation  %in% Simulations[c(1,2,3)])
# R_square <- cor(final1$SRXN1_observed, final1$Srxn1_mean)^2
# plot(final1$Srxn1_mean,final1$SRXN1_observed,
#      xlim = c(0.1, 15), ylim = c(0.1, 15),
#      xlab =  "Predicted",ylab ="Observed", 
#      text(8, 14, paste("(R2 =", round(R_square, digits = 3),")"),cex=1.1))
# #main = paste("(R2 =", round(R_square, digits = 3),")"))
# abline(0,1)
# 
# final1 = Df4 %>% filter(.,Treament == Drug[4]) %>% filter(.,Experiment == Exp[2]) %>% 
#   filter(.,Simulation  %in% Simulations[c(4,5,6)])
# R_square <- cor(final1$SRXN1_observed, final1$Srxn1_mean)^2
# plot(final1$Srxn1_mean,final1$SRXN1_observed,
#      xlim = c(0.1, 70), ylim = c(0.1, 70),
#      xlab =  "Predicted",ylab ="Observed", 
#      text(40, 68, paste("(R2 =", round(R_square, digits = 3),")"),cex=1.1))
# #main = paste("(R2 =", round(R_square, digits = 3),")"))
# abline(0,1)
# 
# mtext("B. Sulforaphane_Model_V.2", side = 3, line = -25, outer = TRUE)
# mtext("A. Sulforaphane_Model_V.1", side = 3, line = -2, outer = TRUE)
# mtext("Predicted data", side = 1, line = 1, outer = TRUE)
# mtext("Observed data", side = 2, line = 2, outer = TRUE)
# # title(xlab = "Predicted data",
# #       ylab = "Observed data",
# #       outer = TRUE, line = 2)
# 
# dev.off()


dev.off()
#################################################
# model version 2 plotting for other chemicals
################################################
png(paste0(directory,"/", "Version_2_dif_chem",".png"), width = 630, height = 600)
op <- par(mfrow = c(3,2),
          oma = c(4,4,0,0) + 0.1,
          mar = c(1,0,3,2) + 0.1)
final1 = Df4 %>% filter(.,Treament == Drug[2]) %>% filter(.,Experiment == Exp[2]) %>%
  filter(.,Simulation  %in% Simulations[c(1,2,3)])
R_square <- cor(final1$SRXN1_observed, final1$Srxn1_mean)^2
plot(final1$Srxn1_mean,final1$SRXN1_observed,
     xlim = c(0.1, 10), ylim = c(0.1, 10),
     xlab =  "Predicted",ylab ="Observed",
     text(5, 9, paste("(R2 =", round(R_square, digits = 3),")"),cex=1.1))
#main = paste("(R2 =", round(R_square, digits = 3),")"))
abline(0,1)

final1 = Df4 %>% filter(.,Treament == Drug[2]) %>% filter(.,Experiment == Exp[2]) %>%
  filter(.,Simulation  %in% Simulations[c(4,5,6)])
R_square <- cor(final1$SRXN1_observed, final1$Srxn1_mean)^2
plot(final1$Srxn1_mean,final1$SRXN1_observed,
     xlim = c(0.1, 40), ylim = c(0.1, 40),
     xlab =  "Predicted",ylab ="Observed",
     text(20, 38, paste("(R2 =", round(R_square, digits = 3),")"),cex=1.1))
#main = paste("(R2 =", round(R_square, digits = 3),")"))
abline(0,1)

final1 = Df4 %>% filter(.,Treament == Drug[3]) %>% filter(.,Experiment == Exp[2]) %>%
  filter(.,Simulation  %in% Simulations[c(1,2,3)])
R_square <- cor(final1$SRXN1_observed, final1$Srxn1_mean)^2
plot(final1$Srxn1_mean,final1$SRXN1_observed,
     xlim = c(0.1, 5), ylim = c(0.1, 5),
     xlab =  "Predicted",ylab ="Observed",
     text(2, 4.5, paste("(R2 =", round(R_square, digits = 3),")"),cex=1.1))
#main = paste("(R2 =", round(R_square, digits = 3),")"))
abline(0,1)

final1 = Df4 %>% filter(.,Treament == Drug[3]) %>% filter(.,Experiment == Exp[2]) %>%
  filter(.,Simulation  %in% Simulations[c(4,5,6)])
R_square <- cor(final1$SRXN1_observed, final1$Srxn1_mean)^2
plot(final1$Srxn1_mean,final1$SRXN1_observed,
     xlim = c(0.1, 40), ylim = c(0.1, 40),
     xlab =  "Predicted",ylab ="Observed",
     text(20, 38, paste("(R2 =", round(R_square, digits = 3),")"),cex=1.1))
#main = paste("(R2 =", round(R_square, digits = 3),")"))
abline(0,1)

final1 = Df4 %>% filter(.,Treament == Drug[4]) %>% filter(.,Experiment == Exp[2]) %>%
  filter(.,Simulation  %in% Simulations[c(1,2,3)])
R_square <- cor(final1$SRXN1_observed, final1$Srxn1_mean)^2
plot(final1$Srxn1_mean,final1$SRXN1_observed,
     xlim = c(0.1, 30), ylim = c(0.1, 30),
     xlab =  "Predicted",ylab ="Observed",
     text(15, 29, paste("(R2 =", round(R_square, digits = 3),")"),cex=1.1))
#main = paste("(R2 =", round(R_square, digits = 3),")"))
abline(0,1)

final1 = Df4 %>% filter(.,Treament == Drug[4]) %>% filter(.,Experiment == Exp[2]) %>%
  filter(.,Simulation  %in% Simulations[c(4,5,6)])
R_square <- cor(final1$SRXN1_observed, final1$Srxn1_mean)^2
plot(final1$Srxn1_mean,final1$SRXN1_observed,
     xlim = c(0.1, 70), ylim = c(0.1, 70),
     xlab =  "Predicted",ylab ="Observed",
     text(35, 68, paste("(R2 =", round(R_square, digits = 3),")"),cex=1.1))
#main = paste("(R2 =", round(R_square, digits = 3),")"))
abline(0,1)

mtext("B. Ethacrynic_Model_V.2", side = 3, line = -22, outer = TRUE)
mtext("A. Andrographolide_Model_V.2", side = 3, line = -2, outer = TRUE)
mtext("C.CDDO_Model_V.2", side = 3, line = -41.5, outer = TRUE)
mtext("Predicted data", side = 1, line = 1, outer = TRUE)
mtext("Observed data", side = 2, line = 2, outer = TRUE)
# title(xlab = "Predicted data",
#       ylab = "Observed data",
#       outer = TRUE, line = 2)

dev.off()



############################
#time response plot classifying doses into two group 
###########################
Dose1 = c(35,100,100,1)
Drug = c("Sul" ,"Andro","ETA","CDDO")
Exp = unique(Df4$Experiment)

final5 = Df4 %>% filter(.,Treament == Drug[1]) %>% filter(.,Experiment == Exp[2]) %>% 
  filter(.,Simulation  %in% Simulations[c(1,2,3)])
final6 = Df4 %>% filter(.,Treament == Drug[1]) %>% filter(.,Experiment == Exp[2]) %>% 
  filter(.,Simulation  %in% Simulations[c(4,5,6)])

mypal = pal_npg("nrc", alpha = 0.7)(9)
mypal
plot5 = final5 %>% filter(.,Treament == Drug[1]) %>% filter(.,Experiment == Exp[2]) %>% 
  filter(.,dose1 != Dose1[1])  %>% 
  ggplot(aes(x = time, y = Srxn1_mean,color = as.factor(dose1),group= as.factor(dose1))) +
  geom_line() +
  scale_colour_manual(values=c(mypal[1],mypal[2],mypal[3])) +
  geom_errorbar(aes(ymin=SRXN1_observed-SRXN1_SD, ymax= SRXN1_observed +SRXN1_SD), size=0.5,   
                width=.25) + theme(legend.position="none") +
  labs(x = "Time [h]",y="GFP intensity [au]", title = paste0("F." ,Drug[1],"_Model_V.2")) +
  theme(axis.title.x=element_blank(),axis.title.y=element_blank(),axis.text.x=element_blank(),
        axis.ticks.x=element_blank())


plot6 = final6 %>% filter(.,Treament == Drug[1]) %>% filter(.,Experiment == Exp[2]) %>% 
  filter(.,dose1 != Dose1[1])  %>% 
  ggplot(aes(x = time, y = Srxn1_mean,group= as.factor(dose1),color = as.factor(dose1))) +
  geom_line() +
  scale_colour_manual(values=c(mypal[4],mypal[5],mypal[6])) +
  
  geom_errorbar(aes(ymin=SRXN1_observed-SRXN1_SD, ymax= SRXN1_observed +SRXN1_SD), size=0.5,   
                width=.25) + theme(legend.position="none") +
  
  labs(x = "Time [h]",y="GFP intensity [au]", title = paste0("Continous_scenario","_", Drug[1],"_Model_V.2"))+
  theme(plot.title=element_blank(),axis.title.x=element_blank(),axis.title.y=element_blank())


#################################
# To extract legend 
plot_legend = Df4 %>% filter(.,Treament == Drug[1]) %>% filter(.,Experiment == Exp[2]) %>% 
  filter(.,dose1 != Dose1[1])  %>% 
  ggplot(aes(x = time, y = Srxn1_mean,color = as.factor(dose1),group= as.factor(dose1))) +
  geom_line() +
  #scale_fill_npg() +
  scale_colour_manual(values=c("#E64B35B2", "#4DBBD5B2", "#00A087B2" ,"#3C5488B2", "#F39B7FB2", "#8491B4B2")) +
  
  geom_errorbar(aes(ymin=SRXN1_observed-SRXN1_SD, ymax= SRXN1_observed +SRXN1_SD), size=0.5,   
                width=.25) + theme(legend.title = element_blank()) +  theme(legend.position = "bottom") +
  labs(x = "Time [h]",y="GFP intensity [au]", title = paste0("Continous_scenario","_", Drug[1],"_Model_V.1"))

library(ggplot2)
library(grid)
library(gridExtra)


get_legend<-function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

legend <- get_legend(plot_legend)
as_ggplot(legend)

#plot for modelversion 1 vs model version 2 figure 1
p3 <- grid.arrange(arrangeGrob(plot1 + theme(legend.position="none"),
                               plot5 + theme(legend.position="none"),
                               plot2 + theme(legend.position="none"),
                               plot6 + theme(legend.position="none"),
                               nrow=2),
                   legend, nrow=3,heights=c(10,2,-0.1))

annotate_figure(p3,
                left = text_grob("Srxn1 intensity [au]", color = "black", rot = 90,vjust = 1,hjust = -0.2),
                bottom = text_grob("Time [h]",color = "black",hjust = 0.5, vjust = -6,
                                   size = 10))

#####################
p3 <- grid.arrange(arrangeGrob(plot1 + theme(legend.position="none"),
                               plot5 + theme(legend.position="none"),
                               plot2 + theme(legend.position="none"),
                               plot6 + theme(legend.position="none"),
                               ncol =2))


plotmodel1 = annotate_figure(p3,
                left = text_grob("Srxn1 intensity [au]", color = "black", rot = 90,vjust = 1),
                bottom = text_grob("Time [h]",color = "black",hjust = 0.5,
                                   size = 10))




##########################################
# for andrographolide
##########################################

# final7 = Df4 %>% filter(.,Treament == Drug[2]) %>% filter(.,Experiment == Exp[2]) %>% 
#   filter(.,Simulation  %in% Simulations[c(1,2,3)])
# final8 = Df4 %>% filter(.,Treament == Drug[2]) %>% filter(.,Experiment == Exp[2]) %>% 
#   filter(.,Simulation  %in% Simulations[c(4,5,6)])
# 
# mypal = pal_npg("nrc", alpha = 0.7)(9)
# mypal
# plot7 = final7 %>% filter(.,Treament == Drug[2]) %>% filter(.,Experiment == Exp[2]) %>% 
#   filter(.,dose1 != Dose1[2])  %>% 
#   ggplot(aes(x = time, y = Srxn1_mean,color = as.factor(dose1),group= as.factor(dose1))) +
#   geom_line() +
#   scale_colour_manual(values=c(mypal[1],mypal[2],mypal[3])) +
#   geom_errorbar(aes(ymin=SRXN1_observed-SRXN1_SD, ymax= SRXN1_observed +SRXN1_SD), size=0.5,   
#                 width=.25) + theme(legend.position="none") +
#   labs(x = "Time [h]",y="GFP intensity [au]", title = paste0( Drug[2],"_Model_V.2")) +
#   theme(axis.title.x=element_blank(),axis.title.y=element_blank(),axis.text.x=element_blank(),
#         axis.ticks.x=element_blank())
# 
# 
# plot8 = final8 %>% filter(.,Treament == Drug[2]) %>% filter(.,Experiment == Exp[2]) %>% 
#   filter(.,dose1 != Dose1[2])  %>% 
#   ggplot(aes(x = time, y = Srxn1_mean,group= as.factor(dose1),color = as.factor(dose1))) +
#   geom_line() +
#   scale_colour_manual(values=c(mypal[4],mypal[5],mypal[6])) +
#   
#   geom_errorbar(aes(ymin=SRXN1_observed-SRXN1_SD, ymax= SRXN1_observed +SRXN1_SD), size=0.5,   
#                 width=.25) + theme(legend.position="none") +
#   
#   labs(x = "Time [h]",y="GFP intensity [au]", title = paste0("Continous_scenario","_", Drug[2],"_Model_V.2"))+
#   theme(plot.title=element_blank(),axis.title.x=element_blank(),axis.title.y=element_blank())
#########################################
final9 = Df4 %>% filter(.,Treament == Drug[2]) %>% filter(.,Experiment == Exp[2]) %>% 
  filter(.,Simulation  %in% Simulations[c(1,2,3)])
final10 = Df4 %>% filter(.,Treament == Drug[2]) %>% filter(.,Experiment == Exp[2]) %>% 
  filter(.,Simulation  %in% Simulations[c(4,5,6)])

mypal = pal_npg("nrc", alpha = 0.7)(9)
mypal
plot9 = final9 %>% filter(.,Treament == Drug[2]) %>% filter(.,Experiment == Exp[2]) %>% 
  filter(.,dose1 != Dose1[2])  %>% 
  ggplot(aes(x = time, y = Srxn1_mean,color = as.factor(dose1),group= as.factor(dose1))) +
  geom_line() +
  scale_colour_manual(values=c(mypal[1],mypal[2],mypal[3])) +
  geom_errorbar(aes(ymin=SRXN1_observed-SRXN1_SD, ymax= SRXN1_observed +SRXN1_SD), size=0.5,   
                width=.25) + theme(legend.position="none") +
  labs(x = "Time [h]",y="GFP intensity [au]", title = paste0("G.",Drug[2],"_Model_V.2")) +
  theme(axis.title.x=element_blank(),axis.title.y=element_blank(),axis.text.x=element_blank(),
        axis.ticks.x=element_blank())


plot10 = final10 %>% filter(.,Treament == Drug[2]) %>% filter(.,Experiment == Exp[2]) %>% 
  filter(.,dose1 != Dose1[2])  %>% 
  ggplot(aes(x = time, y = Srxn1_mean,group= as.factor(dose1),color = as.factor(dose1))) +
  geom_line() +
  scale_colour_manual(values=c(mypal[4],mypal[5],mypal[6])) +
  
  geom_errorbar(aes(ymin=SRXN1_observed-SRXN1_SD, ymax= SRXN1_observed +SRXN1_SD), size=0.5,   
                width=.25) + theme(legend.position="none") +
  
  labs(x = "Time [h]",y="GFP intensity [au]", title = paste0("Continous_scenario","_", Drug[2],"_Model_V.2"))+
  theme(plot.title=element_blank(),axis.title.x=element_blank(),axis.title.y=element_blank())


final11 = Df4 %>% filter(.,Treament == Drug[3]) %>% filter(.,Experiment == Exp[2]) %>% 
  filter(.,Simulation  %in% Simulations[c(1,2,3)])
final12 = Df4 %>% filter(.,Treament == Drug[3]) %>% filter(.,Experiment == Exp[2]) %>% 
  filter(.,Simulation  %in% Simulations[c(4,5,6)])


plot11 = final11 %>% filter(.,Treament == Drug[3]) %>% filter(.,Experiment == Exp[2]) %>% 
  filter(.,dose1 != Dose1[3])  %>% 
  ggplot(aes(x = time, y = Srxn1_mean,color = as.factor(dose1),group= as.factor(dose1))) +
  geom_line() +
  scale_colour_manual(values=c(mypal[1],mypal[2],mypal[3])) +
  geom_errorbar(aes(ymin=SRXN1_observed-SRXN1_SD, ymax= SRXN1_observed +SRXN1_SD), size=0.5,   
                width=.25) + theme(legend.position="none") +
  labs(x = "Time [h]",y="GFP intensity [au]", title = paste0("H.",Drug[3],"_Model_V.2")) +
  theme(axis.title.x=element_blank(),axis.title.y=element_blank(),axis.text.x=element_blank(),
        axis.ticks.x=element_blank())


plot12 = final12 %>% filter(.,Treament == Drug[3]) %>% filter(.,Experiment == Exp[2]) %>% 
  filter(.,dose1 != Dose1[3])  %>% 
  ggplot(aes(x = time, y = Srxn1_mean,group= as.factor(dose1),color = as.factor(dose1))) +
  geom_line() +
  scale_colour_manual(values=c(mypal[4],mypal[5],mypal[6])) +
  
  geom_errorbar(aes(ymin=SRXN1_observed-SRXN1_SD, ymax= SRXN1_observed +SRXN1_SD), size=0.5,   
                width=.25) + theme(legend.position="none") +
  
  labs(x = "Time [h]",y="GFP intensity [au]", title = paste0("Continous_scenario","_", Drug[3],"_Model_V.2"))+
  theme(plot.title=element_blank(),axis.title.x=element_blank(),axis.title.y=element_blank())

final13 = Df4 %>% filter(.,Treament == Drug[4]) %>% filter(.,Experiment == Exp[2]) %>% 
  filter(.,Simulation  %in% Simulations[c(1,2,3)])
final14 = Df4 %>% filter(.,Treament == Drug[4]) %>% filter(.,Experiment == Exp[2]) %>% 
  filter(.,Simulation  %in% Simulations[c(4,5,6)])

plot13 = final13 %>% filter(.,Treament == Drug[4]) %>% filter(.,Experiment == Exp[2]) %>% 
  filter(.,dose1 != Dose1[4])  %>% 
  ggplot(aes(x = time, y = Srxn1_mean,color = as.factor(dose1),group= as.factor(dose1))) +
  geom_line() +
  scale_colour_manual(values=c(mypal[1],mypal[2],mypal[3])) +
  geom_errorbar(aes(ymin=SRXN1_observed-SRXN1_SD, ymax= SRXN1_observed +SRXN1_SD), size=0.5,   
                width=.25) + theme(legend.position="none") +
  labs(x = "Time [h]",y="GFP intensity [au]", title = paste0("I.",Drug[4],"_Model_V.2")) +
  theme(axis.title.x=element_blank(),axis.title.y=element_blank(),axis.text.x=element_blank(),
        axis.ticks.x=element_blank())


plot14 = final14 %>% filter(.,Treament == Drug[4]) %>% filter(.,Experiment == Exp[2]) %>% 
  filter(.,dose1 != Dose1[4])  %>% 
  ggplot(aes(x = time, y = Srxn1_mean,group= as.factor(dose1),color = as.factor(dose1))) +
  geom_line() +
  scale_colour_manual(values=c(mypal[4],mypal[5],mypal[6])) +
  
  geom_errorbar(aes(ymin=SRXN1_observed-SRXN1_SD, ymax= SRXN1_observed +SRXN1_SD), size=0.5,   
                width=.25) + theme(legend.position="none") +
  
  labs(x = "Time [h]",y="GFP intensity [au]", title = paste0("Continous_scenario","_", Drug[4],"_Model_V.2"))+
  theme(plot.title=element_blank(),axis.title.x=element_blank(),axis.title.y=element_blank())



p4 <- grid.arrange(arrangeGrob(plot9 + theme(legend.position="none"),
                               plot11 + theme(legend.position="none"),
                               plot13 + theme(legend.position="none"),
                               plot10 + theme(legend.position="none"),
                               plot12 + theme(legend.position="none"),
                               plot14 + theme(legend.position="none"),
                               ncol=3))

plot_model2 = annotate_figure(p4,
                left = text_grob("Srxn1 intensity [au]", color = "black", rot = 90,vjust = 1),
                bottom = text_grob("Time [h]",color = "black",hjust = 0.5,
                                   size = 10))

grid.arrange(arrangeGrob(plotmodel1,plot_model2,nrow=2))
#################################
# To extract legend 
plot6 = Df4 %>% filter(.,Treament == Drug[2]) %>% filter(.,Experiment == Exp[2]) %>% 
  filter(.,dose1 != Dose1[2])  %>% 
  ggplot(aes(x = time, y = Srxn1_mean,color = as.factor(dose1),group= as.factor(dose1))) +
  geom_line() +
  #scale_fill_npg() +
  scale_colour_manual(values=c("#E64B35B2", "#4DBBD5B2", "#00A087B2" ,"#3C5488B2", "#F39B7FB2", "#8491B4B2")) +
  
  geom_errorbar(aes(ymin=SRXN1_observed-SRXN1_SD, ymax= SRXN1_observed +SRXN1_SD), size=0.5,   
                width=.25) + theme(legend.title = element_blank()) +  theme(legend.position = "bottom") +
  labs(x = "Time [h]",y="GFP intensity [au]", title = paste0("Continous_scenario","_", Drug[1],"_Model_V.1"))

library(ggplot2)
library(grid)
library(gridExtra)

