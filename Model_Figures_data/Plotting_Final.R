
rm(list = ls())
library(magrittr)
library(dbplyr)
library(dplyr)
library(ggplot2)
library(ggpubr)

directory = "C:/Users/alexe/OneDrive - Universiteit Leiden/mcsim_files/Generic_curated_model/Model_Figures_data"

file_name <- list.files(path = paste0(directory),pattern = ".txt")

myfiles = lapply(paste0(directory,"/",file_name), read.delim,sep=" ")
names(myfiles) <- file_name

repeated_dosing_32 = myfiles[c("Andrographolide_R_8_24.txt",  "Sulforaphane_R_8_24.txt", "CDDO_me_R_8_24.txt", "Ethacrynic_Acid_R_8_24.txt" )]
repeated_dosing_48 = myfiles[c("Andrographolide_R_24_24.txt",  "Sulforaphane_R_24_24.txt" , "CDDO_me_R_24_24.txt", "Ethacrynic_Acid_R_24_24.txt" )]


repeated_dosing_32$Andrographolide_R_8_24.txt$Treament = rep("Andr", length(repeated_dosing_32$Andrographolide_R_8_24.txt$Srxn1_max))
repeated_dosing_32$Sulforaphane_R_8_24.txt$Treament = rep("Sul", length(repeated_dosing_32$Andrographolide_R_8_24.txt$Srxn1_max))
repeated_dosing_32$CDDO_me_R_8_24.txt$Treament = rep("CDDO", length(repeated_dosing_32$Andrographolide_R_8_24.txt$Srxn1_max))
repeated_dosing_32$Ethacrynic_Acid_R_8_24.txt$Treament = rep("ETA", length(repeated_dosing_32$Andrographolide_R_8_24.txt$Srxn1_max))

Df = do.call("rbind", list(repeated_dosing_32$Andrographolide_R_8_24.txt,repeated_dosing_32$Sulforaphane_R_8_24.txt,repeated_dosing_32$CDDO_me_R_8_24.txt,repeated_dosing_32$Ethacrynic_Acid_R_8_24.txt))

Df$Experiment = rep("8_24",length(Df$NRF2_max))


repeated_dosing_48$Andrographolide_R_24_24.txt$Treament = rep("Andr", length(repeated_dosing_48$Andrographolide_R_24_24.txt$Srxn1_max))
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

Drug = c("Sul" ,"Andr","ETA","CDDO")
Dose1 = c(0.35,0.1,1,0.1)


# for (i in 1:4){
#   for (j in 1:2){
# plot = final %>% filter(.,Treament == Drug[i]) %>% filter(.,Experiment == Exp[j]) %>% 
#   ggplot() +
#   geom_line(aes(x = time, y = Srxn1_max, color = "Simulated_max"),size=1) +
#   geom_line(aes(x = time, y = Srxn1_mean, color="Simulated_mean"),size=1) +
#   geom_line(aes(x = time, y = Srxn1_min, color="Simulated_min"),size=1) +
#   geom_errorbar(aes(x = time, ymin=SRXN1_observed-SRXN1_SD, ymax= SRXN1_observed +SRXN1_SD,color = "Experiments_mean(Sd)"), size=0.5,   
#                 width=.25) + 
#   facet_grid(dose2~dose1) +
#   labs(x = "Time (hrs)",y="GFP intensity",sec.x="First exposure (µM)",
#        sec.y="Second exposure (µM)", title = paste0(Exp[j],"_", Drug[i],"_Srxn1_GFP vs Time")) +
#   scale_color_manual(name = "Simulation type",
#                      breaks = c("Simulated_max", "Simulated_mean","Simulated_min", "Experiments_mean(Sd)"),
#                      values = c("Simulated_max" = "red", "Simulated_mean" = "blue", "Simulated_min" = "green","Experiments_mean(Sd)" = "black"))+
#   theme(legend.title = element_blank()) + theme(legend.position="bottom") +
#   theme(legend.text = element_text(size= 8,face="bold")) 
# 
# png(paste0(directory,"/", Drug[i],"_",Exp[j],"_Srxn1",".png"), width = 700, height = 480)
# print(plot)
# dev.off()
# 
# }}
# 
# # plot for Fraction_X for repeated scenarios
# 
# png(paste0(directory,"/", "Repeated_Fraction_X",".png"), width = 600, height = 480)
# final %>% 
#   ggplot(aes(x = time, y = Fraction_X_mean,color= as.factor(Experiment))) +
#   geom_line(size = 1,lty = 2) +
#   facet_wrap(~Treament) +
#   labs(x = "Time (hr)",y="Fraction_X", title = paste0("Repeated_Fraction_X vs Time"))
# 
# dev.off() 
# 
# plotting code for observed vs predicted for repeated scenarios


trug = c("Sul" ,"Andr","ETA","CDDO")
txp = c("8h+24h","24h+24h")
typeof(exp)
Dose1 = c(0.35,0.1,1,0.1)
figdirec = "C:/Users/alexe/OneDrive - Universiteit Leiden/Manusript_Nrf2_repeated_dosing/Images"
#pdf(paste0(figdirec,"/", "Repeated_Obs_Vs_Predicted",".pdf"),onefile=FALSE)
tiff(paste0(figdirec,"/","Repeated_Obs_Vs_Predicted" ,".png"), units="in", width=8, height=8, res=700, compression = 'lzw')

par(mfrow = c(4,2))
for (i in 1:4){
  final1 = final %>% filter(.,Treament == Drug[i]) %>% filter(.,Experiment == Exp[1])
  R_square <- cor(final1$SRXN1_observed, final1$Srxn1_mean)^2
  exp = txp[1]
  drug = trug[i]
  plot(final1$SRXN1_observed, final1$Srxn1_mean,
       xlim = c(0.1, 45), ylim = c(0.1, 45),
       xlab = "Observed", ylab = "Predicted",
       #main = str2expression(paste0(drug , 8 ,"h","_",24,"h", "R^2~",format(round(R_square, digits = 3)))), cex.main = 1)
       main = (paste0(drug,", ", exp, " R2 = ", format(round(R_square, digits = 3)))),cex.main = 1)
  abline(0,1)

}
for (i in 1:4){
  final1 = final %>% filter(.,Treament == Drug[i]) %>% filter(.,Experiment == Exp[2]) %>% na.omit()
  exp = txp[2]
  drug = trug[i]
  R_square <- cor(final1$SRXN1_observed, final1$Srxn1_mean)^2
  plot(final1$SRXN1_observed, final1$Srxn1_mean,
       xlim = c(0.1, 65), ylim = c(0.1, 65),
       xlab = "Observed", ylab = "Predicted",
       #main = str2expression(paste0(drug, 24 ,"h","_",24,"h", "R^2~",format(round(R_square, digits = 3)))), cex.main = 1)
      main = (paste0(drug,", ", exp, " R2 = ", format(round(R_square, digits = 3)))),cex.main = 1)
  abline(0,1)

}
dev.off()

main = str2expression(paste0(drug ,48 ,"h", "R^2~",format(round(R_square, digits = 3)))), cex.main = 1)
#######################################

continous_data_48 = myfiles[c("Andrographolide_Conti_48.txt","Sulforaphane_Conti_48.txt", "CDDO_me_Conti_48.txt","Ethacrynic_Acid_Conti_48.txt")]
continous_data_32 = myfiles[c("Andrographolide_Conti_32.txt" , "Sulforaphane_Conti_32.txt", "CDDO_me_Conti_32.txt","Ethacrynic_Acid_Conti_32.txt")]

continous_data_32$Andrographolide_Conti_32.txt$Treament = rep("Andr", length(continous_data_32$Andrographolide_Conti_32.txt$Srxn1_max))
continous_data_32$Sulforaphane_Conti_32.txt$Treament = rep("Sul", length(continous_data_32$Andrographolide_Conti_32.txt$Srxn1_max))
continous_data_32$CDDO_me_Conti_32.txt$Treament = rep("CDDO", length(continous_data_32$Andrographolide_Conti_32.txt$Srxn1_max))
continous_data_32$Ethacrynic_Acid_Conti_32.txt$Treament = rep("ETA", length(continous_data_32$Andrographolide_Conti_32.txt$Srxn1_max))

Df = do.call("rbind", list(continous_data_32$Andrographolide_Conti_32.txt,continous_data_32$Sulforaphane_Conti_32.txt,continous_data_32$CDDO_me_Conti_32.txt,continous_data_32$Ethacrynic_Acid_Conti_32.txt))

Df$Experiment = rep("8_24_continous",length(Df$NRF2_max))

Df$dose2 = rep(0,length(Df$NRF2_max))

continous_data_48$Andrographolide_Conti_48.txt$Treament = rep("Andr", length(continous_data_48$Andrographolide_Conti_48.txt$Srxn1_max))
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

# This was the change made here to plot the legend correctly for fraction X
Df3$Experiment <- gsub("8_24", "32h (8h + 24h) Repeated ", Df3$Experiment)
Df3$Experiment <- gsub("24_24_continous","48h Continuous", Df3$Experiment)
Df3$Experiment <- gsub("24_24", "48h (24h + 24h) Repeated", Df3$Experiment)

Exp = unique(Df3$Experiment)

# png(paste0(directory,"/", "Continous_Fraction_X",".png"), width = 600, height = 480)
# final2 %>% 
#   ggplot(aes(x = time, y = Fraction_X_mean)) +
#   geom_line(size = 1,lty = 2) +
#   facet_wrap(~Treament) +
#   labs(x = "Time (hr)",y="Fraction_recruitment X", title = paste0("Continous_Fraction_X vs Time"))
# dev.off()
# 
# # Comparative_plot of Fraction_X for different scenarios and different drugs
# 

glimpse(Df3)
Df3$Treament <- factor(Df3$Treament, levels= c("Sul","Andr","ETA","CDDO"))
#ICTconferecneplot = Df3 %>%  filter(., Treament  %in% c("Sul","CDDO")) %>% filter(., Experiment %in% c("48h Continuous","48h (24h + 24h) Repeated"))
direc = "C:/Users/alexe/OneDrive - Universiteit Leiden/Manusript_Nrf2_repeated_dosing/Images"
png(paste0(direc,"/", "Comparative_plot_Fraction_X",".png"), width = 600, height = 480)

#This below figure is merged with model emergent properties figure so run this till here and save it as P6 and then merge it. 
P6 = ggplot(ICTconferecneplot, (aes(x = time, y = Fraction_X_mean,group=interaction(Experiment))))+
  geom_line(aes(color=Experiment),size = 1.4)+
  facet_grid(Second~First)+
  labs(x = "Time [h]",y="Fraction of modified Nrf2", title = paste0(""),size= 14) +
  facet_wrap(~Treament, nrow = 2) +
  theme(strip.text.x = element_text(size = 12),strip.text.y = element_text(size = 12)) +
  theme(axis.title = element_text(size= 14))+
  theme(axis.text = element_text(size= 13))+
  theme(axis.text.x.top = element_text(size= 13))+
  theme(legend.title = element_text(size= 14,face="bold"))+
  theme(legend.text = element_text(size= 14))+
  theme(legend.key.size = unit(0.5, 'cm'), #change legend key size
        legend.key.height = unit(0.5, 'cm'), #change legend key height
        legend.key.width = unit(0.5, 'cm'))+ theme(legend.position="bottom") 


ggplot(Df3, (aes(x = time, y = Fraction_X_mean,group=interaction(Experiment))))+
  geom_line(aes(color=Experiment),size = 1.2)+
  facet_grid(Second~First)+
  labs(x = "Time [h]",y="Fraction of modified Nrf2", title = paste0("A.")) +
  facet_wrap(~Treament) + theme(legend.position="bottom") 
 dev.off()


Df4 = do.call("rbind", list(Df, Df2))  # combine both continous scenarios
glimpse(Df4)
Dose1 = c(35,100,100,1)
Drug = c("Sul" ,"Andr","ETA","CDDO")
Exp = unique(Df4$Experiment)
# 
# for (i in 1:4){
# plot = Df4 %>% filter(.,Treament == Drug[i]) %>% filter(.,Experiment == Exp[2]) %>% 
#   filter(.,dose1 != Dose1[i])  %>% 
#   ggplot() +
#   geom_line(aes(x = time, y = Srxn1_max, color = "Simulated_max"),size=1) +
#   geom_line(aes(x = time, y = Srxn1_mean, color="Simulated_mean"),size=1) +
#   geom_line(aes(x = time, y = Srxn1_min, color="Simulated_min"),size=1) +
#   geom_errorbar(aes(x = time, ymin=SRXN1_observed-SRXN1_SD, ymax= SRXN1_observed +SRXN1_SD,color = "Experiments_mean(Sd)"), size=0.5,   
#                 width=.25) + 
#   facet_wrap(~dose1) +
#   labs(x = "Time (hr)",y="GFP intensity", title = paste0(Exp[2],"_", Drug[i],"_GFP vs Time"))+
#   scale_color_manual(name = "Simulation type",
#                      breaks = c("Simulated_max", "Simulated_mean","Simulated_min", "Experiments_mean(Sd)"),
#                      values = c("Simulated_max" = "red", "Simulated_mean" = "blue", "Simulated_min" = "green","Experiments_mean(Sd)" = "black"))+
#   theme(legend.title = element_blank()) + theme(legend.position="bottom") +
#   theme(legend.text = element_text(size= 8,face="bold")) 
# print(plot)
# }


# Plot all the chemical in one graph
Df4$Treament <- factor(Df4$Treament, levels= c("Sul","Andr","ETA","CDDO"))
directory_i = "C:/Users/alexe/OneDrive - Universiteit Leiden/Manusript_Nrf2_repeated_dosing/Images"
pdf(paste0(directory_i,"/",Exp[2],"_Continous",".pdf"),onefile=FALSE)

#png(paste0(directory_i,"/",Exp[2],"_Continous",".png"), width = 700, height = 480)
file1 = Df4 %>%  filter(.,Experiment == Exp[2]) %>% 
  ggplot() +
  geom_line(aes(x = time, y = Srxn1_max, color = "Simulated_max"),size=1) +
  geom_line(aes(x = time, y = Srxn1_mean, color="Simulated_mean"),size=1) +
  geom_line(aes(x = time, y = Srxn1_min, color="Simulated_min"),size=1) +
  geom_errorbar(aes(x = time, ymin=SRXN1_observed-SRXN1_SD, ymax= SRXN1_observed +SRXN1_SD,color = "Experiments_mean(Sd)"), size=0.5,   
                width=.25) +  
  facet_wrap(Treament~dose1,ncol = 6,labeller = label_wrap_gen(multi_line=FALSE)) +
  labs(x = "Time [h]",y=" Srxn1 intensity [au]", title = paste0("A.48h Continous Exposure"),size = 7)+
  scale_color_manual(name = "Simulation type",
                     breaks = c("Simulated_max", "Simulated_mean","Simulated_min", "Experiments_mean(Sd)"),
                     values = c("Simulated_max" = "red", "Simulated_mean" = "blue", "Simulated_min" = "green","Experiments_mean(Sd)" = "black"))+
  theme(legend.title = element_blank()) + theme(legend.position="none") +
  theme(legend.text = element_text(size= 7,face="bold")) +
  theme(axis.text = element_text(size = 7))


file2 = Df4 %>%  filter(.,Experiment == Exp[1]) %>% 
  ggplot() +
  geom_line(aes(x = time, y = Srxn1_max, color = "Simulated_max"),size=1) +
  geom_line(aes(x = time, y = Srxn1_mean, color="Simulated_mean"),size=1) +
  geom_line(aes(x = time, y = Srxn1_min, color="Simulated_min"),size=1) +
  geom_errorbar(aes(x = time, ymin=SRXN1_observed-SRXN1_SD, ymax= SRXN1_observed +SRXN1_SD,color = "Experiments_mean(Sd)"), size=0.5,   
                width=.25) +  
  facet_wrap(Treament~dose1,ncol = 6,labeller = label_wrap_gen(multi_line=FALSE)) +
  labs(x = "Time [h]",y=" Srxn1 intensity [au]", title = paste0("B.32h Continous Exposure"),size = 7)+
  scale_color_manual(name = "Simulation type",
                     breaks = c("Simulated_max", "Simulated_mean","Simulated_min", "Experiments_mean(Sd)"),
                     values = c("Simulated_max" = "red", "Simulated_mean" = "blue", "Simulated_min" = "green","Experiments_mean(Sd)" = "black"))+
  scale_y_continuous(breaks=c(0,20,40))+
  theme(legend.title = element_blank()) + theme(legend.position="bottom") +
  theme(legend.text = element_text(size= 7,face="bold")) +
  theme(axis.text = element_text(size = 7))

ggarrange(file1,file2, nrow = 2)
dev.off()

# library(pryr)
final1 = Df4 %>% filter(.,Treament == Drug[1]) %>% filter(.,Experiment == Exp[2])
R_square <- cor(final1$SRXN1_observed, final1$Srxn1_mean)^2
final1_r1 %<a-% { plot(final1$SRXN1_observed, final1$Srxn1_mean,
     xlim = c(0.1, 60), ylim = c(0.1, 60),
     xlab = "Observed", ylab = "Predicted",
     main = paste(Drug[1],"(R2 =", round(R_square, digits = 3),")"))
abline(0,1)
}
# 
# 
# final1 = Df4 %>% filter(.,Treament == Drug[2]) %>% filter(.,Experiment == Exp[2]) 
# R_square <- cor(final1$SRXN1_observed, final1$Srxn1_mean)^2
# final1_r2 %<a-% {plot(final1$SRXN1_observed, final1$Srxn1_mean,
#                  xlim = c(0.1, 40), ylim = c(0.1, 40),
#                  xlab = "Observed", ylab = "Predicted",
#                  main = paste(Drug[2],"(R2 =", round(R_square, digits = 3),")"))
# abline(0,1)
# }
# 
# 
# final1 = Df4 %>% filter(.,Treament == Drug[3]) %>% filter(.,Experiment == Exp[2]) 
# R_square <- cor(final1$SRXN1_observed, final1$Srxn1_mean)^2
# final1_r3 %<a-% { plot(final1$SRXN1_observed, final1$Srxn1_mean,
#                  xlim = c(0.1, 40), ylim = c(0.1, 40),
#                  xlab = "Observed", ylab = "Predicted",
#   main = paste(Drug[3],"(R2 =", round(R_square, digits = 3),")"))
# abline(0,1)
# }
#  
# 
# final1 = Df4 %>% filter(.,Treament == Drug[4]) %>% filter(.,Experiment == Exp[2]) 
# R_square <- cor(final1$SRXN1_observed, final1$Srxn1_mean)^2
# final1_r4 %<a-% {plot(final1$SRXN1_observed, final1$Srxn1_mean,
#                  xlim = c(0.1, 60), ylim = c(0.1, 60),
#                  xlab = "Observed", ylab = "Predicted",
#                  main = paste(Drug[4],"(R2 =", round(R_square, digits = 3),")"))
# abline(0,1)
# }
# 

final1 = Df4 %>% filter(.,Treament == Drug[3]) %>% filter(.,Experiment == Exp[2]) 
R_square <- cor(final1$SRXN1_observed, final1$Srxn1_mean)^2
final1_r3 <-qplot(final1$SRXN1_observed, final1$Srxn1_mean,
                       xlim = c(0.1, 30), ylim = c(0.1, 30),
                       xlab = "Observed", ylab = "Predicted",
                       main = paste(Drug[3],"(R2 =", round(R_square, digits = 3),")")) + annotate("segment", x=-Inf, xend=Inf,y=-Inf, yend=Inf)



final1 = Df4 %>% filter(.,Treament == Drug[1]) %>% filter(.,Experiment == Exp[2])
R_square <- cor(final1$SRXN1_observed, final1$Srxn1_mean)^2
final1_r1 %<a-% { plot(final1$SRXN1_observed, final1$Srxn1_mean,
                       xlim = c(0.1, 60), ylim = c(0.1, 60),
                       xlab = "Observed", ylab = "Predicted",
                       main = paste(Drug[1],"(R2 =", round(R_square, digits = 3),")"))
  abline(0,1)
}




library(ggplot2)
library(gridExtra)
library(grid)

vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)

grid.newpage()
pushViewport(viewport(layout = grid.layout(10,10))) # 3 rows, 5 columns
print(file1, vp = vplayout(1:5, 1:7))  # the big plot covers rows 1:2 and cols 1:3
print(file2, vp = vplayout(6:10, 1:3))
print(final1_r3, vp = vplayout(1:2, 8:10))  # the big plot covers rows 1:2 and cols 1:3
print(final1_r2, vp = vplayout(2, 4))
print(final1_r3, vp = vplayout(3, 1:3))  # the big plot covers rows 1:2 and cols 1:3
print(final1_r4, vp = vplayout(4, 4))
print(final1_p1, vp = vplayout(5, 1:3))  # the big plot covers rows 1:2 and cols 1:3
print(final1_p2, vp = vplayout(6, 4))
print(final1_p3, vp = vplayout(7, 1:3))  # the big plot covers rows 1:2 and cols 1:3
print(final1_p4, vp = vplayout(8, 4))
plot_grid(file1, final1_r3, rel_heights = c(.6, 1), labels = "auto")
png(paste0(directory,"/", "Conti_Obs_Vs_Predicted",".png"), width = 700, height = 500)
par(mfrow = c(4,2))
for (i in 1:4){
  final1 = Df4 %>% filter(.,Treament == Drug[i]) %>% filter(.,Experiment == Exp[1]) 
  R_square <- cor(final1$SRXN1_observed, final1$Srxn1_mean)^2
  plot(final1$SRXN1_observed, final1$Srxn1_mean,
       xlim = c(0.1, 60), ylim = c(0.1, 60),
       xlab = "Observed", ylab = "Predicted",
       main = paste(Exp[1],Drug[i],"(R2 =", round(R_square, digits = 3),")"))
  abline(0,1)
  
}

for (i in 1:4){
  final1 = Df4 %>% filter(.,Treament == Drug[i]) %>% filter(.,Experiment == Exp[2]) 
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
