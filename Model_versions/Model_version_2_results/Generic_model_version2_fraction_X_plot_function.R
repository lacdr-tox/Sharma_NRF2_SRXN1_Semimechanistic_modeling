rm(list = ls())
library(magrittr)
library(dbplyr)
library(dplyr)
library(ggplot2)

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
png(paste0(directory,"/", "Continous_Obs_Vs_Predicted",".png"), width = 700, height = 500)
par(mfrow = c(4,2))
for (i in 1:4){
  final1 = Df4 %>% filter(.,Treament == Drug[i]) %>% filter(.,Experiment == Exp[1]) 
  R_square <- cor(final1$SRXN1_observed, final1$Srxn1_mean)^2
  plot(final1$SRXN1_observed, final1$Srxn1_mean,
       xlim = c(0.1, 70), ylim = c(0.1, 70),
       xlab = "Observed", ylab = "Predicted",
       main = paste(Exp[1],Drug[i],"(R2 =", round(R_square, digits = 3),")"))
  abline(0,1)
  
}
for (i in 1:4){
  final1 = Df4 %>% filter(.,Treament == Drug[i]) %>% filter(.,Experiment == Exp[2]) 
  R_square <- cor(final1$SRXN1_observed, final1$Srxn1_mean)^2
  plot(final1$SRXN1_observed, final1$Srxn1_mean,
       xlim = c(0.1, 70), ylim = c(0.1, 70),
       xlab = "Observed", ylab = "Predicted",
       main = paste(Exp[2],Drug[i],"(R2 =", round(R_square, digits = 3),")"))
  abline(0,1)
  
}
dev.off()

Simulations = unique(Df4$Simulation)
#par(mfrow = c(4,2))
final1 = Df4 %>% filter(.,Treament == Drug[1]) %>% filter(.,Experiment == Exp[2]) %>% 
  filter(.,Simulation  %in% Simulations[c(1,2,3)])
R_square <- cor(final1$SRXN1_observed, final1$Srxn1_mean)^2
plot(final1$Srxn1_mean,final1$SRXN1_observed,
     xlim = c(0.1, 15), ylim = c(0.1, 15),
     xlab =  "Predicted",ylab ="Observed", 
     text(8, 14, paste("(R2 =", round(R_square, digits = 3),")"),cex=1.1))
     #main = paste("(R2 =", round(R_square, digits = 3),")"))
abline(0,1)

final1 = Df4 %>% filter(.,Treament == Drug[4]) %>% filter(.,Experiment == Exp[2]) %>% 
  filter(.,Simulation  %in% Simulations[c(4,5,6)])
R_square <- cor(final1$SRXN1_observed, final1$Srxn1_mean)^2
plot(final1$Srxn1_mean,final1$SRXN1_observed,
     xlim = c(0.1, 70), ylim = c(0.1, 70),
     xlab =  "Predicted",ylab ="Observed", 
     text(40, 68, paste("(R2 =", round(R_square, digits = 3),")"),cex=1.1))
     #main = paste("(R2 =", round(R_square, digits = 3),")"))
abline(0,1)

mtext("Sulforaphane_model_version_2", side = 3, line = -21, outer = TRUE)
mtext("Sulforaphane_model_version_1", side = 3, line = -2, outer = TRUE)
mtext("Predicted data", side = 1, line = 1, outer = TRUE)
mtext("Observed data", side = 2, line = 2, outer = TRUE)
# title(xlab = "Predicted data",
#       ylab = "Observed data",
#       outer = TRUE, line = 2)

dev.off()

####################################
#ggplot2
####################################

ggplot(final1, aes(x =Srxn1_mean, y =SRXN1_observed )) +
  geom_smooth(method = "lm", se = FALSE, color = "lightgrey") +
  geom_segment(aes(xend =Srxn1_mean, yend =SRXN1_observed), alpha = .2) +
  
  # > Color adjustments made here...
  geom_point(aes(color = abs(final1$SRXN1_observed-final1$Srxn1_mean))) + # Color mapped to abs(residuals)
  scale_color_continuous(low = "black", high = "red") +  # Colors to use here
  guides(color = FALSE) +  # Color legend removed
  # <
  
  geom_point(aes(y = Srxn1_mean), shape = 1) +
  theme_bw()

p <- ggplot(final1, aes(x =Srxn1_mean, y =SRXN1_observed )) +
  geom_point()  + geom_smooth()

####################################
# for repeated dosing 

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
Final = final%>% mutate_if(is.numeric, funs(replace_na(., 0)))
Final[is.na(Final)] <- 0
Drug = unique(Final$Treament)
Exp = unique(Final$Experiment) 
png(paste0(directory,"/", "Repeated_Obs_Vs_Predicted",".png"), width = 700, height = 500)
par(mfrow = c(4,2))
for (i in 1:4){
  final1 = Final %>% filter(.,Treament == Drug[i]) %>% filter(.,Experiment == Exp[1]) 
  R_square <- cor(final1$SRXN1_observed, final1$Srxn1_mean)^2
  plot(final1$SRXN1_observed, final1$Srxn1_mean,
       xlim = c(0.1, 50), ylim = c(0.1, 50),
       xlab = "Observed", ylab = "Predicted",
       main = paste(Exp[1],Drug[i],"(R2 =", round(R_square, digits = 3),")"))
  abline(0,1)
  
}
for (i in 1:4){
  final1 = Final %>% filter(.,Treament == Drug[i]) %>% filter(.,Experiment == Exp[2]) %>% na.omit()
  R_square <- cor(final1$SRXN1_observed, final1$Srxn1_mean)^2
  plot(final1$SRXN1_observed, final1$Srxn1_mean,
       xlim = c(0.1, 65), ylim = c(0.1, 65),
       xlab = "Observed", ylab = "Predicted",
       main = paste(Exp[2],Drug[i],"(R2 =", round(R_square, digits = 3),")"))
  abline(0,1)
  
}
dev.off() 


