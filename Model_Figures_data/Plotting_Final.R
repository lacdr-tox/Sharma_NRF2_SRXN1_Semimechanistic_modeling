
rm(list = ls())
library(magrittr)
library(dbplyr)
library(dplyr)
library(ggplot2)

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

Drug = c("Sul" ,"Andro","ETA","CDDO")
Dose1 = c(0.35,0.1,1,0.1)

Experiment = c(" 32h(8h+24h)","48h (24h+24h)")
Drugs = c("Sul ", "Andr ", "ETA ", "CDDO ")
  
for (i in 1:4){
  for (j in 1:2){
plot = final %>% filter(.,Treament == Drug[i]) %>% filter(.,Experiment == Exp[j]) %>% 
  ggplot() +
  geom_line(aes(x = time, y = Srxn1_max, color = "Simulated(max)"),size=0.8) +
  geom_line(aes(x = time, y = Srxn1_mean, color="Simulated(mean)"),size=0.8) +
  geom_line(aes(x = time, y = Srxn1_min, color="Simulated(min)"),size=0.8) +
  geom_point(aes(x = time, y = SRXN1_observed), size = 0.8) +
  geom_errorbar(aes(x = time, ymin=SRXN1_observed-SRXN1_SD, ymax= SRXN1_observed +SRXN1_SD,color = "Experiments mean(Sd)"), size=0.5,   
                width=.25) + 
  facet_grid(dose2~dose1) +
  labs(x = "Time (hrs)",y= "Srxn1 intensity [au]",sec.x="First exposure (然)",
       sec.y="Second exposure (然)", title = paste0(Drugs[i], Experiment[j], " Repeated Exposure")) +
  scale_color_manual(name = "Simulation type",
                     breaks = c("Simulated(max)", "Simulated(mean)","Simulated(min)", "Experiments mean(Sd)"),
                     values = c("Simulated(max)" = "red", "Simulated(mean)" = "blue", "Simulated(min)" = "green","Experiments mean(Sd)" = "black"))+
  theme(legend.title = element_blank()) + theme(legend.position="bottom") +
  theme(legend.text = element_text(size= 7,face="bold")) +
  theme(axis.text = element_text(size = 7))
pdf(paste0(directory,"/", Drug[i],"_",Exp[j],"_Srxn1",".pdf"),onefile=FALSE)
print(plot)
dev.off()

}}


for (i in 1:4){
  for (j in 1:2){
    plot = final %>% filter(.,Treament == Drug[i]) %>% filter(.,Experiment == Exp[j]) %>% 
      ggplot() +
      geom_line(aes(x = time, y = NRF2_max, color = "Experiments(max)"),size=0.8) +
      geom_line(aes(x = time, y = NRF2_mean, color="Experiments(mean)"),size=0.8) +
      geom_line(aes(x = time, y = NRF2_min, color="Experiments(min)"),size=0.8) +
      facet_grid(dose2~dose1) +
      labs(x = "Time (hrs)",y= "Nrf2 intensity [au]",sec.x="First exposure (然)",
           sec.y="Second exposure (然)", title = paste0(Drugs[i], Experiment[i], " Repeated Exposure")) +
      scale_color_manual(name = "Simulation type",
                         breaks = c("Experiments(max)", "Experiments(mean)","Experiments(min)", "Experiments mean(Sd)"),
                         values = c("Experiments(max)" = "red", "Experiments(mean)" = "blue", "Experiments(min)" = "green"))+
      theme(legend.title = element_blank()) + theme(legend.position="bottom") +
      theme(legend.text = element_text(size= 7,face="bold")) +
      theme(axis.text = element_text(size = 7))
    pdf(paste0(directory,"/", Drug[i],"_",Exp[j],"_Nrf2",".pdf"),onefile=FALSE)
    print(plot)
    dev.off()
    
  }}

# plotting code for observed vs predicted for repeated scenarios

#png(paste0(directory,"/", "Repeated_Obs_Vs_Predicted",".png"), width = 700, height = 500)
tiff(paste0(directory,"/", "Repeated_Obs_Vs_Predicted1",".png"), units="in", width=6, height=6, res=700, compression = 'lzw')
par(mfrow = c(4,2))
for (i in 1:4){
  final1 = final %>% filter(.,Treament == Drug[i]) %>% filter(.,Experiment == Exp[1]) 
  R_square <- cor(final1$SRXN1_observed, final1$Srxn1_mean)^2
  plot(final1$SRXN1_observed, final1$Srxn1_mean,
       xlim = c(0.1, 45), ylim = c(0.1, 45),
       xlab = "Observed", ylab = "Predicted",
       main = paste(Exp[1],Drug[i],"(R2 =", round(R_square, digits = 3),")"))
  abline(0,1)
  
}
for (i in 1:4){
  final1 = final %>% filter(.,Treament == Drug[i]) %>% filter(.,Experiment == Exp[2]) %>% na.omit()
  R_square <- cor(final1$SRXN1_observed, final1$Srxn1_mean)^2
  plot(final1$SRXN1_observed, final1$Srxn1_mean,
       xlim = c(0.1, 65), ylim = c(0.1, 65),
       xlab = "Observed", ylab = "Predicted",
       main = paste(Exp[2],Drug[i],"(R2 =", round(R_square, digits = 3),")"))
  abline(0,1)
  
}
dev.off() 








# plot for Fraction_X for repeated scenarios

png(paste0(directory,"/", "Repeated_Fraction_X",".png"), width = 600, height = 480)
final %>% 
  ggplot(aes(x = time, y = Fraction_X_mean,color= as.factor(Experiment))) +
  geom_line(size = 1,lty = 2) +
  facet_wrap(~Treament) +
  labs(x = "Time (hr)",y="Fraction_X", title = paste0("Repeated_Fraction_X vs Time"))

dev.off() 


#######################################

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


final2 = data.frame(Df2)
Drug = unique(Df2$Treament)
Exp = unique(Df2$Experiment)


Df3 = do.call("rbind", list(Finalplotdata, Df2))    #combine repeated dosing and 24+24 continous scenario
Drug = unique(Df3$Treament)
Exp = unique(Df3$Experiment)

png(paste0(directory,"/", "Continous_Fraction_X",".png"), width = 600, height = 480)
final2 %>% 
  ggplot(aes(x = time, y = Fraction_X_mean)) +
  geom_line(size = 1,lty = 2) +
  facet_wrap(~Treament) +
  labs(x = "Time (hr)",y="Fraction_recruitment X", title = paste0("Continous_Fraction_X vs Time"))
dev.off()

# Comparative_plot of Fraction_X for different scenarios and different drugs
Df3$Treament <- factor(Df3$Treament, levels= c("Sul","Andro","ETA","CDDO"))
png(paste0(directory,"/", "Comparative_plot_Fraction_X",".png"), width = 600, height = 480)
ggplot(Df3, (aes(x = time, y = Fraction_X_mean,group=interaction(Experiment))))+
  geom_line(aes(color=Experiment),size = 1.2)+
  facet_grid(Second~First)+
  labs(x = "Time (hr)",y="Nrf2_modifier", title = paste0("Nrf2_modifier vs Time")) +
  facet_wrap(~Treament) 

dev.off()


Df4 = do.call("rbind", list(Df, Df2))  # combine both continous scenarios
glimpse(Df4)
Dose1 = c(35,100,100,1)
Drug = c("Sul" ,"Andro","ETA","CDDO")
Exp = unique(Df4$Experiment)
Df4$Treament <- gsub('Andro', "Andr", Df4$Treament)

###############################
#Plot continous Nrf2 data
###############################
Df4$Treament = factor(Df4$Treament, levels= c("Sul" ,"Andr","ETA","CDDO"))
plot = Df4 %>% filter(.,Experiment == Exp[2]) %>% 
  ggplot() +
  geom_line(aes(x = time, y = NRF2_max, color = "Experiments(max)"),size=0.8) +
  geom_line(aes(x = time, y = NRF2_mean, color="Experiments(mean)"),size=0.8) +
  geom_line(aes(x = time, y = NRF2_min, color="Experiments(min)"),size=0.8) +
  facet_wrap(Treament~dose1,ncol = 6,labeller = label_wrap_gen(multi_line=FALSE)) +
  labs(x = "Time (hrs)",y= "Nrf2 intensity [au]",title = paste0("48h Continous Exposure")) +
  scale_color_manual(name = "Simulation type",
                     breaks = c("Experiments(max)", "Experiments(mean)","Experiments(min)", "Experiments mean(Sd)"),
                     values = c("Experiments(max)" = "red", "Experiments(mean)" = "blue", "Experiments(min)" = "green"))+
  theme(legend.title = element_blank()) + theme(legend.position="bottom") +
  theme(legend.text = element_text(size= 7,face="bold")) +
  theme(axis.text = element_text(size = 7))
#pdf(paste0(directory,"/", "_",Exp[2],"_Nrf2",".pdf"),onefile=FALSE)
tiff(paste0(directory,"/", "_",Exp[2],"_Nrf2",".png"), units="in", width=6, height=6, res=700, compression = 'lzw')
print(plot)
dev.off()

for (i in 1:4){
plot = Df4 %>% filter(.,Treament == Drug[i]) %>% filter(.,Experiment == Exp[2]) %>% 
  filter(.,dose1 != Dose1[i])  %>% 
  ggplot() +
  geom_line(aes(x = time, y = Srxn1_max, color = "Simulated_max"),size=1) +
  geom_line(aes(x = time, y = Srxn1_mean, color="Simulated_mean"),size=1) +
  geom_line(aes(x = time, y = Srxn1_min, color="Simulated_min"),size=1) +
  geom_errorbar(aes(x = time, ymin=SRXN1_observed-SRXN1_SD, ymax= SRXN1_observed +SRXN1_SD,color = "Experiments_mean(Sd)"), size=0.5,   
                width=.25) + 
  facet_wrap(~dose1) +
  labs(x = "Time (hr)",y="GFP intensity", title = paste0(Exp[2],"_", Drug[i],"_GFP vs Time"))+
  scale_color_manual(name = "Simulation type",
                     breaks = c("Simulated_max", "Simulated_mean","Simulated_min", "Experiments_mean(Sd)"),
                     values = c("Simulated_max" = "red", "Simulated_mean" = "blue", "Simulated_min" = "green","Experiments_mean(Sd)" = "black"))+
  theme(legend.title = element_blank()) + theme(legend.position="bottom") +
  theme(legend.text = element_text(size= 8,face="bold")) 
print(plot)
}


# Plot all the chemical in one graph

png(paste0(directory,"/",Exp[1],"_Continous",".png"), width = 700, height = 480)
Df4 %>%  filter(.,Experiment == Exp[1]) %>% 
  ggplot() +
  geom_line(aes(x = time, y = Srxn1_max, color = "Simulated_max"),size=1) +
  geom_line(aes(x = time, y = Srxn1_mean, color="Simulated_mean"),size=1) +
  geom_line(aes(x = time, y = Srxn1_min, color="Simulated_min"),size=1) +
  geom_errorbar(aes(x = time, ymin=SRXN1_observed-SRXN1_SD, ymax= SRXN1_observed +SRXN1_SD,color = "Experiments_mean(Sd)"), size=0.5,   
                width=.25) +  
  facet_wrap(Treament~dose1,ncol = 6) +
  labs(x = "Time (hr)",y="GFP intensity", title = paste0(Exp[1],"_GFP vs Time"))+
  scale_color_manual(name = "Simulation type",
                     breaks = c("Simulated_max", "Simulated_mean","Simulated_min", "Experiments_mean(Sd)"),
                     values = c("Simulated_max" = "red", "Simulated_mean" = "blue", "Simulated_min" = "green","Experiments_mean(Sd)" = "black"))+
  theme(legend.title = element_blank()) + theme(legend.position="bottom") +
  theme(legend.text = element_text(size= 8,face="bold")) 

dev.off()



Df4 %>%  filter(.,Experiment == Exp[1]) %>% 
  ggplot() +
  geom_line(aes(x = time, y = Srxn1_max, color = "Simulated_max"),size=1) +
  geom_line(aes(x = time, y = Srxn1_mean, color="Simulated_mean"),size=1) +
  geom_line(aes(x = time, y = Srxn1_min, color="Simulated_min"),size=1) +
  geom_errorbar(aes(x = time, ymin=SRXN1_observed-SRXN1_SD, ymax= SRXN1_observed +SRXN1_SD,color = "Experiments_mean(Sd)"), size=0.5,   
                width=.25) +  
  facet_wrap(Treament~dose1,ncol = 6) +
  labs(x = "Time (hr)",y="GFP intensity", title = paste0(Exp[1],"_GFP vs Time"))+
  scale_color_manual(name = "Simulation type",
                     breaks = c("Simulated_max", "Simulated_mean","Simulated_min", "Experiments_mean(Sd)"),
                     values = c("Simulated_max" = "red", "Simulated_mean" = "blue", "Simulated_min" = "green","Experiments_mean(Sd)" = "black"))+
  theme(legend.title = element_blank()) + theme(legend.position="bottom") +
  theme(legend.text = element_text(size= 8,face="bold")) 


#png(paste0(directory,"/", "Conti_Obs_Vs_Predicted",".png"), width = 700, height = 500)
tiff(paste0(directory,"/", "Conti_Obs_Vs_Predicted",".png"), units="in", width=6, height=6, res=700, compression = 'lzw')
par(mfrow = c(4,2))
for (i in 1:4){
  final1 = Df4 %>% filter(.,Treament == Drug[i]) %>% filter(.,Experiment == Exp[1]) %>% na.omit()
  R_square <- cor(final1$SRXN1_observed, final1$Srxn1_mean)^2
  plot(final1$SRXN1_observed, final1$Srxn1_mean,
       xlim = c(0.1, 60), ylim = c(0.1, 60),
       xlab = "Observed", ylab = "Predicted",
       main = paste(Exp[1],Drug[i],"(R2 =", round(R_square, digits = 3),")"))
  abline(0,1)
  
}
for (i in 1:4){
  final1 = Df4 %>% filter(.,Treament == Drug[i]) %>% filter(.,Experiment == Exp[2]) %>% na.omit()
  R_square <- cor(final1$SRXN1_observed, final1$Srxn1_mean)^2
  plot(final1$SRXN1_observed, final1$Srxn1_mean,
       xlim = c(0.1, 60), ylim = c(0.1, 60),
       xlab = "Observed", ylab = "Predicted",
       main = paste(Exp[2],Drug[i],"(R2 =", round(R_square, digits = 3),")"))
  abline(0,1)
  
}

dev.off()





##############################
#Repeated and continous scenario of 24 and 24 data set
Df3 = do.call("rbind", list(Finalplotdata, Df2))    #combine repeated dosing and 24+24 continous scenario
Drug = unique(Df3$Treament)
Exp = unique(Df3$Experiment)
glimpse(Df3)

final1 = Df3 %>% filter(.,Treament == Drug[1]) %>% filter(.,Experiment == Exp[3]) %>% 
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

mtext("B. Ethacrynic_model_version_2", side = 3, line = -22, outer = TRUE)
mtext("A. Andrographolide_model_version_2", side = 3, line = -2, outer = TRUE)
mtext("C.CDDO_model_version_2", side = 3, line = -41.5, outer = TRUE)
mtext("Predicted data", side = 1, line = 1, outer = TRUE)
mtext("Observed data", side = 2, line = 2, outer = TRUE)
# title(xlab = "Predicted data",
#       ylab = "Observed data",
#       outer = TRUE, line = 2)

dev.off()
