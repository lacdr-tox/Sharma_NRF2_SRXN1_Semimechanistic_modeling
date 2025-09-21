rm(list = ls())
library(magrittr)
library(dbplyr)
library(dplyr)
library(ggplot2)
library("gridExtra")
library(ggsci)
library(ggpubr)
library(tidyr)
library(stringr)


direc = "C:/Users/alexe/OneDrive - Universiteit Leiden/Manusript_Nrf2_repeated_dosing/Images"
datadirectory = "C:/Users/alexe/OneDrive - Universiteit Leiden/Data_treatment_new"
Modeling_dir = "Modelversion1a_48h"
directory1 = paste0(direc, "/",Modeling_dir)


Generic_data_read = function (Drug,Scenario,time_steps,exp_timestep) {
  
  directory = "C:/Users/alexe/OneDrive - Universiteit Leiden/Data_treatment_new"
  
  Exp =  ifelse(Scenario %in% c("R_24_24",'R_8_24'),"Rep",
                ifelse(Scenario  %in% c("Conti_48",'Conti_32'),"Conti","NA"))
  
  
  # selection of second dose timing based on scenarios
  
  repeat_dosing_time =  ifelse(Scenario == "R_24_24",24,
                               ifelse(Scenario =='R_8_24',8,0))
  
  
  end_time =  ifelse(Scenario %in% c("R_24_24","Conti_48"),48,
                     ifelse(Scenario %in% c('R_8_24','Conti_32'),32,"NA"))
  
  total_steps = ifelse(Scenario %in% c("R_24_24","Conti_48"),49,                
                       ifelse(Scenario %in% c('R_8_24','Conti_32'),33,"NA"))
  
  Dosing_matrix =  ifelse(Scenario %in% c("R_24_24",'R_8_24'),63,                    #check dosing matrix accounted for number of doses
                          ifelse(Scenario  %in% c("Conti_48",'Conti_32'),7,"NA"))
  
  
  
  
  file_name = ifelse(Drug == 'Sulforaphane'& Scenario %in% c("R_24_24","Conti_48"),'New_all_24_24_Sul_data',ifelse(Drug == 'Sulforaphane'& Scenario %in% c('R_8_24','Conti_32'),'New_all_8_24_Sul_data',
                                                                                                                   ifelse(Drug == 'Andrographolide'& Scenario %in% c("R_24_24","Conti_48"),"New_all_24_24_Andro_data",ifelse(Drug == 'Andrographolide'& Scenario %in% c('R_8_24','Conti_32'),"New_all_8_24_Andro_data",
                                                                                                                                                                                                                             ifelse(Drug == 'Ethacrynic Acid'& Scenario %in% c("R_24_24","Conti_48"),"New_all_24_24_Etha_data",ifelse(Drug == 'Ethacrynic Acid'& Scenario %in% c('R_8_24','Conti_32'),"New_all_8_24_Etha_data", 
                                                                                                                                                                                                                                                                                                                                      ifelse(Drug == "CDDO-me"& Scenario %in% c("R_24_24","Conti_48"),"New_all_24_24_CDDO_data", ifelse(Drug == "CDDO-me"& Scenario %in% c('R_8_24','Conti_32'),"New_all_8_24_CDDO_data",print("Error in file name")))))))))
  
  filenames <- list.files(path = paste0(directory, "/", file_name),pattern = ".txt")
  
  myfiles = lapply(paste0(directory,"/",file_name,"/",filenames), read.delim,sep=" ")
  names(myfiles) <- filenames                                                                                                                                                                                                                
  
  # data_file = myfiles[c(paste0(Drug,"_Nrf2_",Exp,"_Mean_GFP.txt"),paste0(Drug,"_Nrf2_",Exp,"_sd_GFP.txt"),paste0(Drug,"_Srxn1_",Exp,"_Mean_GFP.txt"),paste0(Drug,"_Srxn1_",Exp,"_sd_GFP.txt"),paste0(Drug,"_Srxn1_",Exp,"_Mean_Count.txt"),
  #                       paste0(Drug,"_Srxn1_",Exp,"_sd_count.txt"))]
  # 
  
  data_file = myfiles[c(paste0(Drug,"_Nrf2_",Exp,"_Mean_GFP.txt"),paste0(Drug,"_Nrf2_",Exp,"_sd_GFP.txt"),paste0(Drug,"_Srxn1_",Exp,"_Mean_GFP.txt"),paste0(Drug,"_Srxn1_",Exp,"_sd_GFP.txt"),paste0(Drug,"_Nrf2_",Exp,"_Max_GFP.txt"),paste0(Drug,"_Nrf2_",Exp,"_Min_GFP.txt"))]
  
  
  
  names_data_file = gsub('[^0-9._]','',paste(names(data_file[[paste0(Drug,"_Srxn1_",Exp,"_Mean_GFP.txt")]]))) %>% gsub("_1_.", "/",.)  #%>% gsub("_2", " ",.) 
  
  
  
  data_file1 = vector(mode = "list", length = length(data_file))
  
  
  for (i in 1:length(data_file1)){
    for (j in 1:Dosing_matrix){
      
      data_file1[[i]][[j]] = c(0,data_file[[i]][[j]])
      
    }}
  names(data_file1) = names(data_file)
  names(data_file1[[paste0(Drug,"_Nrf2_",Exp,"_Mean_GFP.txt")]]) = paste0(Exp, "_",names_data_file)
  names(data_file1[[paste0(Drug,"_Nrf2_",Exp,"_sd_GFP.txt")]]) = paste0(Exp, "_",names_data_file)
  names(data_file1[[paste0(Drug,"_Nrf2_",Exp,"_Max_GFP.txt")]]) = paste0(Exp, "_",names_data_file)
  names(data_file1[[paste0(Drug,"_Nrf2_",Exp,"_Min_GFP.txt")]]) = paste0(Exp, "_",names_data_file)
  names(data_file1[[paste0(Drug,"_Srxn1_",Exp,"_Mean_GFP.txt")]]) = paste0(Exp, "_",names_data_file)
  names(data_file1[[paste0(Drug,"_Srxn1_",Exp,"_sd_GFP.txt")]]) = paste0(Exp, "_",names_data_file)
  
  # names(data_file1) = names(data_file)
  # names(data_file1[[paste0(Drug,"_Nrf2_",Exp,"_Mean_GFP.txt")]]) = paste0(Exp, "_",names_data_file)
  # names(data_file1[[paste0(Drug,"_Nrf2_",Exp,"_sd_GFP.txt")]]) = paste0(Exp, "_",names_data_file)
  # names(data_file1[[paste0(Drug,"_Srxn1_",Exp,"_Mean_Count.txt")]]) = paste0(Exp, "_",names_data_file)
  # names(data_file1[[paste0(Drug,"_Srxn1_",Exp,"_sd_count.txt")]]) = paste0(Exp, "_",names_data_file)
  # names(data_file1[[paste0(Drug,"_Srxn1_",Exp,"_Mean_GFP.txt")]]) = paste0(Exp, "_",names_data_file)
  # names(data_file1[[paste0(Drug,"_Srxn1_",Exp,"_sd_GFP.txt")]]) = paste0(Exp, "_",names_data_file)
  
  ##############################
  #Preparation of dosing matrix for matrix plot for observed data
  ################################
  dosing_matrix_exp = str_split(names_data_file[1:Dosing_matrix],"_")
  matrix_exp = do.call(rbind,dosing_matrix_exp)
  colnames(matrix_exp)= c("Dose1","Dose2")
  Dose1 = data.frame(rep(matrix_exp[,c(1)], each = total_steps),stringsAsFactors=FALSE)
  colnames(Dose1) = 'dose1'
  Dose2 = data.frame(rep(matrix_exp[,c(2)], each = total_steps),stringsAsFactors=FALSE)
  colnames(Dose2) = 'dose2'
  Dose_matrix_exp = cbind(Dose1,Dose2)
  Dose_matrix_exp$dose1 = as.numeric(Dose_matrix_exp$dose1)
  Dose_matrix_exp$dose2 = as.numeric(Dose_matrix_exp$dose2)
  
  simulation_data = paste("Simu", seq(1:Dosing_matrix), sep = "" )
  simulation1_data = data.frame(rep(simulation_data, each = total_steps))
  colnames(simulation1_data) = 'Simulation'
  
  ##############################
  #Reshaping of observed data 
  #############################
  Experimentdata_Srxn1 = data_file1[[paste0(Drug,"_Srxn1_",Exp,"_Mean_GFP.txt")]] #name the output
  matrix_exp_data_Srxn1 = data.frame(unlist(Experimentdata_Srxn1))
  colnames(matrix_exp_data_Srxn1) <- 'Srxn1_observed'
  Exp_SD_Srxn1 =data_file1[[paste0(Drug,"_Srxn1_",Exp,"_sd_GFP.txt")]]
  matrix_exp_data_SD_Srxn1 = data.frame(unlist(Exp_SD_Srxn1))
  colnames(matrix_exp_data_SD_Srxn1) <- 'Srxn1_SD'
  Experimentdata_Nrf2 = data_file1[[paste0(Drug,"_Nrf2_",Exp,"_Mean_GFP.txt")]] #name the output
  matrix_exp_data_Nrf2 = data.frame(unlist(Experimentdata_Nrf2))
  colnames(matrix_exp_data_Nrf2) <- 'Nrf2_observed'
  Experimentdata_Nrf2max = data_file1[[paste0(Drug,"_Nrf2_",Exp,"_Max_GFP.txt")]] #name the output
  matrix_exp_data_Nrf2max = data.frame(unlist(Experimentdata_Nrf2max))
  colnames(matrix_exp_data_Nrf2max) <- 'Nrf2max_observed'
  Experimentdata_Nrf2min = data_file1[[paste0(Drug,"_Nrf2_",Exp,"_Min_GFP.txt")]] #name the output
  matrix_exp_data_Nrf2min = data.frame(unlist(Experimentdata_Nrf2min))
  colnames(matrix_exp_data_Nrf2min) <- 'Nrf2min_observed'
  Exp_SD_Nrf2 =data_file1[[paste0(Drug,"_Nrf2_",Exp,"_sd_GFP.txt")]]
  matrix_exp_data_SD_Nrf2 = data.frame(unlist(Exp_SD_Nrf2))
  colnames(matrix_exp_data_SD_Nrf2) <- 'Nrf2_SD'
  
  time = seq(0, end_time, by = exp_timestep)  # 1 is the experiment time step
  Final_matrix_exp_data = cbind(time,simulation1_data,Dose_matrix_exp,matrix_exp_data_Nrf2,matrix_exp_data_Nrf2max,matrix_exp_data_Nrf2min,matrix_exp_data_SD_Nrf2,matrix_exp_data_Srxn1,matrix_exp_data_SD_Srxn1)
  rownames(Final_matrix_exp_data) <- NULL
  # retunfile = Final_matrix_exp_data
  Final_matrix_exp_data <<- Final_matrix_exp_data
  
}

Drug = c('Sulforaphane',"Andrographolide","Ethacrynic Acid","CDDO-me")
Scenario = c("R_8_24","R_24_24","Conti_32", "Conti_48")   # all the scenarios
Toxicdose = c(35, 100, 100, 1)

plotdrug = c("Sul","Andr", "ETA", "CDDO")
drug = c("Sul","Andro", "Etha", "Cddo")
drugs = drug[1]

file = read.table(paste0(direc,"/", Modeling_dir,"/",drugs,Modeling_dir,"_posteriorsimulation.txt"))

drugdata = ifelse(drugs == "Sul",Drug[1], ifelse(drugs == "Andro",Drug[2],ifelse(drugs == 'Etha',Drug[3],ifelse(drugs == "Cddo", Drug[4], "NA"))))

toxicdata = ifelse(drugs == "Sul",Toxicdose[1], ifelse(drugs == "Andro",Toxicdose[2],ifelse(drugs == 'Etha',Toxicdose[3],ifelse(drugs == "Cddo", Toxicdose[4], "NA"))))



Generic_data_read(Drug = drugdata,      # 1 for Sul, 2 for Andro, 3 for Etha, & 4 for CDDO_me
                  Scenario = Scenario[4],  # 1 & 2 for repeated 8+24 & 24+24 respectively  , 2 & 4 for continous 32 and 48 respectively
                  time_steps = 1,
                  exp_timestep = 1)


Data_SimuFun = function (Drug,highdose,Scenario,variable) {
  Final_matrix_exp_data = Final_matrix_exp_data %>% filter(dose1 != highdose & dose2 != highdose)
  x_final = file 
  plot_final = cbind(Final_matrix_exp_data,x_final)
  plot_final = plot_final[,!duplicated(names(plot_final))]  #to remove duplicate columns with the same names
  return(plot_final)
}


Df4 = Data_SimuFun(Drug = drugs,      # 1 for Sul, 2 for Andro, 3 for Etha, & 4 for CDDO_me
                   highdose = toxicdata,
                   Scenario = Scenario[4],  # 1 & 2 for repeated 8+24 & 24+24 respectively  , 2 & 4 for continous 32 and 48 respectively
                   variable = "Srxn1")

Simulations = unique(Df4$Simulation) 

final1 = Df4 %>% filter(.,Simulation  %in% Simulations[c(1,2,3)])
final2 = Df4 %>% filter(.,Simulation  %in% Simulations[c(4,5,6)])

mypal = pal_npg("nrc", alpha = 0.7)(9)
mypal
plot1 = final1 %>% 
  ggplot(aes(x = time, y = medianM,color = as.factor(dose1),group= as.factor(dose1))) +
  geom_line() +
  scale_colour_manual(values=c(mypal[1],mypal[2],mypal[3])) +
  geom_point(aes(x = time, y = Srxn1_observed), size = 1.0) +
  geom_errorbar(aes(ymin=Srxn1_observed-Srxn1_SD , ymax= Srxn1_observed+Srxn1_SD), size=0.5,   
                width=.25) + #theme(legend.position= c(0,1)) +
  labs(x = "Time [h]",y="low_doses", title = paste0("A.",plotdrug[1],", model 1")) +
  theme(plot.title=element_blank(),axis.title.x=element_blank(),axis.title.y=element_text(size=10,face="bold"))+
  theme(legend.position='top', 
        legend.justification='left',
        legend.direction='horizontal')  + labs(col = "Dose")+guides(color = guide_legend(override.aes = list(size = 1)))+
  guides(shape = guide_legend(override.aes = list(size = 1))) +
  theme(legend.key.width = unit(0.2, "cm"))+
theme(legend.title = element_text(size = 5), 
      legend.text = element_text(size = 5))


plot2 = final2 %>%
  ggplot(aes(x = time, y = medianM,color = as.factor(dose1),group= as.factor(dose1))) +
  geom_line() +
  scale_colour_manual(values=c(mypal[4],mypal[5],mypal[6])) +
  geom_point(aes(x = time, y = Srxn1_observed), size = 1.0) +
  geom_errorbar(aes(ymin=Srxn1_observed-Srxn1_SD , ymax= Srxn1_observed+Srxn1_SD), size=0.5,   
                width=.25)  +
  labs(x = "Time [h]",y="High_doses", title = paste0("A.",plotdrug[1],", model 1"))+
  theme(plot.title = element_text(size=10),axis.title.x=element_blank(),axis.title.y=element_text(size=10,face="bold"),axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  theme(legend.position='top', 
        legend.justification='left',
        legend.direction='horizontal') +labs(col = "Dose")+guides(color = guide_legend(override.aes = list(size = 1)))+
  guides(shape = guide_legend(override.aes = list(size = 1))) +
  theme(legend.key.width = unit(0.2, "cm"))+
theme(legend.title = element_text(size = 5), 
      legend.text = element_text(size = 5))

##################
# Andrographolide
drug = c("Sul","Andro", "Etha", "Cddo")
drugs = drug[2]

file = read.table(paste0(direc,"/", Modeling_dir,"/",drugs,Modeling_dir,"_posteriorsimulation.txt"))

drugdata = ifelse(drugs == "Sul",Drug[1], ifelse(drugs == "Andro",Drug[2],ifelse(drugs == 'Etha',Drug[3],ifelse(drugs == "Cddo", Drug[4], "NA"))))

toxicdata = ifelse(drugs == "Sul",Toxicdose[1], ifelse(drugs == "Andro",Toxicdose[2],ifelse(drugs == 'Etha',Toxicdose[3],ifelse(drugs == "Cddo", Toxicdose[4], "NA"))))

Generic_data_read(Drug = drugdata,      # 1 for Sul, 2 for Andro, 3 for Etha, & 4 for CDDO_me
                  Scenario = Scenario[4],  # 1 & 2 for repeated 8+24 & 24+24 respectively  , 2 & 4 for continous 32 and 48 respectively
                  time_steps = 1,
                  exp_timestep = 1)


Data_SimuFun = function (Drug,highdose,Scenario,variable) {
  Final_matrix_exp_data = Final_matrix_exp_data %>% filter(dose1 != highdose & dose2 != highdose)
  x_final = file 
  plot_final = cbind(Final_matrix_exp_data,x_final)
  plot_final = plot_final[,!duplicated(names(plot_final))]  #to remove duplicate columns with the same names
  return(plot_final)
}


Df4 = Data_SimuFun(Drug = drugs,      # 1 for Sul, 2 for Andro, 3 for Etha, & 4 for CDDO_me
                   highdose = toxicdata,
                   Scenario = Scenario[4],  # 1 & 2 for repeated 8+24 & 24+24 respectively  , 2 & 4 for continous 32 and 48 respectively
                   variable = "Srxn1")


final3 = Df4 %>% filter(.,Simulation  %in% Simulations[c(1,2,3)])
final4 = Df4 %>% filter(.,Simulation  %in% Simulations[c(4,5,6)])

plot3 = final3 %>% 
  ggplot(aes(x = time, y = medianM,color = as.factor(dose1),group= as.factor(dose1))) +
  geom_line() +
  scale_colour_manual(values=c(mypal[1],mypal[2],mypal[3],mypal[4],mypal[5],mypal[6])) +
  geom_point(aes(x = time, y = Srxn1_observed), size = 1.0) +
  geom_errorbar(aes(ymin=Srxn1_observed-Srxn1_SD, ymax= Srxn1_observed +Srxn1_SD), size=0.5,
                width=.25)  +
  labs(x = "Time [h]",y="GFP intensity [au]", title = paste0("B.",plotdrug[2],", model 1")) +
  theme(plot.title=element_blank(),axis.title.x=element_blank(),axis.title.y=element_blank())+
  theme(legend.position='top', 
        legend.justification='left',
        legend.direction='horizontal') +labs(col = "Dose")+guides(color = guide_legend(override.aes = list(size = 1)))+
  guides(shape = guide_legend(override.aes = list(size = 1))) +
  theme(legend.key.width = unit(0.2, "cm"))+
theme(legend.title = element_text(size = 5), 
      legend.text = element_text(size = 5))


plot4 = final4 %>%
  ggplot(aes(x = time, y = medianM,color = as.factor(dose1),group= as.factor(dose1))) +
  geom_line() +
  scale_colour_manual(values=c(mypal[4],mypal[5],mypal[6])) +
  geom_point(aes(x = time, y = Srxn1_observed), size = 1.0) +
  
  geom_errorbar(aes(ymin=Srxn1_observed-Srxn1_SD, ymax= Srxn1_observed +Srxn1_SD), size=0.5,
                width=.25)  +
  
  labs(x = "Time [h]",y="GFP intensity [au]", title = paste0("B.",plotdrug[2],", model 1"))+
  theme(plot.title = element_text(size=10),axis.title.x=element_blank(),axis.title.y=element_blank(),axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  theme(legend.position='top', 
        legend.justification='left',
        legend.direction='horizontal') +labs(col = "Dose")+guides(color = guide_legend(override.aes = list(size = 1)))+
  guides(shape = guide_legend(override.aes = list(size = 1))) +
  theme(legend.key.width = unit(0.2, "cm"))+
theme(legend.title = element_text(size = 5), 
      legend.text = element_text(size = 5))


##################
# Ethacrynic acid
drug = c("Sul","Andro", "Etha", "Cddo")
drugs = drug[3]

file = read.table(paste0(direc,"/", Modeling_dir,"/",drugs,Modeling_dir,"_posteriorsimulation.txt"))

drugdata = ifelse(drugs == "Sul",Drug[1], ifelse(drugs == "Andro",Drug[2],ifelse(drugs == 'Etha',Drug[3],ifelse(drugs == "Cddo", Drug[4], "NA"))))

toxicdata = ifelse(drugs == "Sul",Toxicdose[1], ifelse(drugs == "Andro",Toxicdose[2],ifelse(drugs == 'Etha',Toxicdose[3],ifelse(drugs == "Cddo", Toxicdose[4], "NA"))))

Generic_data_read(Drug = drugdata,      # 1 for Sul, 2 for Andro, 3 for Etha, & 4 for CDDO_me
                  Scenario = Scenario[4],  # 1 & 2 for repeated 8+24 & 24+24 respectively  , 2 & 4 for continous 32 and 48 respectively
                  time_steps = 1,
                  exp_timestep = 1)


Data_SimuFun = function (Drug,highdose,Scenario,variable) {
  Final_matrix_exp_data = Final_matrix_exp_data %>% filter(dose1 != highdose & dose2 != highdose)
  x_final = file 
  plot_final = cbind(Final_matrix_exp_data,x_final)
  plot_final = plot_final[,!duplicated(names(plot_final))]  #to remove duplicate columns with the same names
  return(plot_final)
}


Df4 = Data_SimuFun(Drug = drugs,      # 1 for Sul, 2 for Andro, 3 for Etha, & 4 for CDDO_me
                   highdose = toxicdata,
                   Scenario = Scenario[4],  # 1 & 2 for repeated 8+24 & 24+24 respectively  , 2 & 4 for continous 32 and 48 respectively
                   variable = "Srxn1")

final5 = Df4 %>%
  filter(.,Simulation  %in% Simulations[c(1,2,3)])
final6 = Df4 %>% 
  filter(.,Simulation  %in% Simulations[c(4,5,6)])

plot5 = final5 %>%
  ggplot(aes(x = time, y = medianM,color = as.factor(dose1),group= as.factor(dose1))) +
  geom_line() +
  scale_colour_manual(values=c(mypal[1],mypal[2],mypal[3],mypal[4],mypal[5],mypal[6])) +
  geom_point(aes(x = time, y = Srxn1_observed), size = 1.0) +
  geom_errorbar(aes(ymin=Srxn1_observed-Srxn1_SD, ymax= Srxn1_observed +Srxn1_SD), size=0.5,
                width=.25)  +
  labs(x = "Time [h]",y="GFP intensity [au]", title = paste0("C.",plotdrug[3],", model 1")) +
  theme(plot.title=element_blank(),axis.title.x=element_blank(),axis.title.y=element_blank())+
  theme(legend.position='top', 
        legend.justification='left',
        legend.direction='horizontal') +labs(col = "Dose")+guides(color = guide_legend(override.aes = list(size = 1)))+
  guides(shape = guide_legend(override.aes = list(size = 1))) +
  theme(legend.key.width = unit(0.2, "cm"))+
theme(legend.title = element_text(size = 5), 
      legend.text = element_text(size = 5))


plot6 = final6 %>%
  ggplot(aes(x = time, y = medianM,color = as.factor(dose1),group= as.factor(dose1))) +
  geom_line() +
  scale_colour_manual(values=c(mypal[4],mypal[5],mypal[6])) +
  geom_point(aes(x = time, y = Srxn1_observed), size = 1.0) +
  geom_errorbar(aes(ymin=Srxn1_observed-Srxn1_SD, ymax= Srxn1_observed +Srxn1_SD), size=0.5,
                width=.25)  +
  
  labs(x = "Time [h]",y="GFP intensity [au]", title = paste0("C.",plotdrug[3],", model 1"))+
  theme(plot.title = element_text(size=10),axis.title.x=element_blank(),axis.title.y=element_blank(),axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  theme(legend.position='top', 
        legend.justification='left',
        legend.direction='horizontal') +labs(col = "Dose")+guides(color = guide_legend(override.aes = list(size = 1)))+
  guides(shape = guide_legend(override.aes = list(size = 1))) +
  theme(legend.key.width = unit(0.2, "cm"))+
theme(legend.title = element_text(size = 5), 
      legend.text = element_text(size = 5))

##################
# CDDo-me
drug = c("Sul","Andro", "Etha", "Cddo")
drugs = drug[4]

file = read.table(paste0(direc,"/", Modeling_dir,"/",drugs,Modeling_dir,"_posteriorsimulation.txt"))

drugdata = ifelse(drugs == "Sul",Drug[1], ifelse(drugs == "Andro",Drug[2],ifelse(drugs == 'Etha',Drug[3],ifelse(drugs == "Cddo", Drug[4], "NA"))))

toxicdata = ifelse(drugs == "Sul",Toxicdose[1], ifelse(drugs == "Andro",Toxicdose[2],ifelse(drugs == 'Etha',Toxicdose[3],ifelse(drugs == "Cddo", Toxicdose[4], "NA"))))

Generic_data_read(Drug = drugdata,      # 1 for Sul, 2 for Andro, 3 for Etha, & 4 for CDDO_me
                  Scenario = Scenario[4],  # 1 & 2 for repeated 8+24 & 24+24 respectively  , 2 & 4 for continous 32 and 48 respectively
                  time_steps = 1,
                  exp_timestep = 1)


Data_SimuFun = function (Drug,highdose,Scenario,variable) {
  Final_matrix_exp_data = Final_matrix_exp_data %>% filter(dose1 != highdose & dose2 != highdose)
  x_final = file 
  plot_final = cbind(Final_matrix_exp_data,x_final)
  plot_final = plot_final[,!duplicated(names(plot_final))]  #to remove duplicate columns with the same names
  return(plot_final)
}


Df4 = Data_SimuFun(Drug = drugs,      # 1 for Sul, 2 for Andro, 3 for Etha, & 4 for CDDO_me
                   highdose = toxicdata,
                   Scenario = Scenario[4],  # 1 & 2 for repeated 8+24 & 24+24 respectively  , 2 & 4 for continous 32 and 48 respectively
                   variable = "Srxn1")

final7 = Df4 %>%
  filter(.,Simulation  %in% Simulations[c(1,2,3)])
final8 = Df4 %>%
  filter(.,Simulation  %in% Simulations[c(4,5,6)])

plot7 = final7 %>%
  ggplot(aes(x = time, y = medianM,color = as.factor(dose1),group= as.factor(dose1))) +
  geom_line() +
  scale_colour_manual(values=c(mypal[1],mypal[2],mypal[3],mypal[4],mypal[5],mypal[6])) +
  geom_point(aes(x = time, y = Srxn1_observed), size = 1.0) +
  #geom_point() + 
  geom_errorbar(aes(ymin=Srxn1_observed-Srxn1_SD, ymax= Srxn1_observed +Srxn1_SD), size=0.5,
                width=.25)  +
  labs(x = "Time [h]",y="GFP intensity [au]", title = paste0("D.",plotdrug[4],", model 1")) +
  theme(plot.title=element_blank(),axis.title.x=element_blank(),axis.title.y=element_blank())+
  theme(legend.position='top', 
        legend.justification='left',
        legend.direction='horizontal') +labs(col = "Dose")+guides(color = guide_legend(override.aes = list(size = 1)))+
  guides(shape = guide_legend(override.aes = list(size = 1))) +
  theme(legend.key.width = unit(0.2, "cm"))+
theme(legend.title = element_text(size = 5), 
      legend.text = element_text(size = 5))
  


plot8 = final8 %>% 
  ggplot(aes(x = time, y = medianM,color = as.factor(dose1),group= as.factor(dose1))) +
  geom_line() +
  scale_colour_manual(values=c(mypal[4],mypal[5],mypal[6])) +
  geom_point(aes(x = time, y = Srxn1_observed), size = 1) +
  geom_errorbar(aes(ymin=Srxn1_observed-Srxn1_SD, ymax= Srxn1_observed +Srxn1_SD), size=0.5,
                width=.25)  +
  labs(x = "Time [h]",y="GFP intensity [au]", title = paste0("D.",plotdrug[4],", model 1"))+
  theme(plot.title = element_text(size=10),axis.title.x=element_blank(),axis.title.y=element_blank(),axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  theme(legend.position='top', 
        legend.justification='left',
        legend.direction='horizontal') +labs(col = "Dose")+guides(color = guide_legend(override.aes = list(size = 1)))+
  guides(shape = guide_legend(override.aes = list(size = 1))) +
  theme(legend.key.width = unit(0.2, "cm"))+
  theme(legend.title = element_text(size = 5), 
      legend.text = element_text(size = 5))

p3 <- grid.arrange(arrangeGrob(plot2 ,
                               plot4 ,
                               plot6 ,
                               plot8 ,
                               plot1 ,
                               plot3 ,
                               plot5 ,
                               plot7 ,
                               nrow=2,ncol = 4))

plot_model1 = annotate_figure(p3,
                              left = text_grob("Srxn1 intensity [au]", color = "black", rot = 90,vjust = 1),
                              bottom = text_grob("Time [h]",color = "black",hjust = 0.5,
                                                 size = 8))


figdirec = "C:/Users/alexe/OneDrive - Universiteit Leiden/Manusript_Nrf2_repeated_dosing/Images"
# pdf(paste0(figdirec,"/", "Modelversion1&2",".pdf"),onefile=FALSE)
# png(paste0(figdirec,"/", "model1a",".png"), width = 680, height = 500, res = 100)
tiff(paste0(figdirec,"/", "Figure3",".png"), units="in", width=6, height=4, res=700, compression = 'lzw')
print(plot_model1)
dev.off()
#########################
#Model version_2 
#########################

Modeling_dir = "Modelversion1b"

Drug = c('Sulforaphane',"Andrographolide","Ethacrynic Acid","CDDO-me")
Scenario = c("R_8_24","R_24_24","Conti_32", "Conti_48")   # all the scenarios
Toxicdose = c(35, 100, 100, 1)

##############
# for sulforaphane

drug = c("Sul","Andro", "Etha", "Cddo")
drugs = drug[1]

file = read.table(paste0(direc,"/", Modeling_dir,"/",drugs,Modeling_dir,"_posteriorsimulation.txt"))

drugdata = ifelse(drugs == "Sul",Drug[1], ifelse(drugs == "Andro",Drug[2],ifelse(drugs == 'Etha',Drug[3],ifelse(drugs == "Cddo", Drug[4], "NA"))))

toxicdata = ifelse(drugs == "Sul",Toxicdose[1], ifelse(drugs == "Andro",Toxicdose[2],ifelse(drugs == 'Etha',Toxicdose[3],ifelse(drugs == "Cddo", Toxicdose[4], "NA"))))



Generic_data_read(Drug = drugdata,      # 1 for Sul, 2 for Andro, 3 for Etha, & 4 for CDDO_me
                  Scenario = Scenario[4],  # 1 & 2 for repeated 8+24 & 24+24 respectively  , 2 & 4 for continous 32 and 48 respectively
                  time_steps = 1,
                  exp_timestep = 1)


Data_SimuFun = function (Drug,highdose,Scenario,variable) {
  Final_matrix_exp_data = Final_matrix_exp_data %>% filter(dose1 != highdose & dose2 != highdose)
  x_final = file 
  plot_final = cbind(Final_matrix_exp_data,x_final)
  plot_final = plot_final[,!duplicated(names(plot_final))]  #to remove duplicate columns with the same names
  return(plot_final)
}


Df4 = Data_SimuFun(Drug = drugs,      # 1 for Sul, 2 for Andro, 3 for Etha, & 4 for CDDO_me
                   highdose = toxicdata,
                   Scenario = Scenario[4],  # 1 & 2 for repeated 8+24 & 24+24 respectively  , 2 & 4 for continous 32 and 48 respectively
                   variable = "Srxn1")

Simulations = unique(Df4$Simulation) 

# final1 = Df4 %>% filter(.,Simulation  %in% Simulations[c(1,2,3)])
# final2 = Df4 %>% filter(.,Simulation  %in% Simulations[c(4,5,6)])

mypal = pal_npg("nrc", alpha = 0.7)(9)
mypal
final9 = Df4 %>% filter(.,Simulation  %in% Simulations[c(1,2,3)])
final10 = Df4 %>% filter(.,Simulation  %in% Simulations[c(4,5,6)])

plot9 = final9 %>% 
  ggplot(aes(x = time, y = medianM,color = as.factor(dose1),group= as.factor(dose1))) +
  geom_line() +
  scale_colour_manual(values=c(mypal[1],mypal[2],mypal[3])) +
  geom_point(aes(x = time, y = Srxn1_observed), size = 1.0) +
  geom_errorbar(aes(ymin=Srxn1_observed-Srxn1_SD , ymax= Srxn1_observed+Srxn1_SD), size=0.5,   
                width=.25) + #theme(legend.position= c(0,1)) +
  labs(x = "Time [h]",y="low_doses", title = paste0("A.",plotdrug[1],", model 2")) +
  theme(plot.title=element_blank(),axis.title.x=element_blank(),axis.title.y=element_text(size=10,face="bold"))+
  theme(legend.position='top', 
        legend.justification='left',
        legend.direction='horizontal')  + labs(col = "Dose")+guides(color = guide_legend(override.aes = list(size = 1)))+
  guides(shape = guide_legend(override.aes = list(size = 1))) +
  theme(legend.key.width = unit(0.2, "cm"))+
  theme(legend.title = element_text(size = 5), 
        legend.text = element_text(size = 5))


plot10 = final10 %>%
  ggplot(aes(x = time, y = medianM,color = as.factor(dose1),group= as.factor(dose1))) +
  geom_line() +
  scale_colour_manual(values=c(mypal[4],mypal[5],mypal[6])) +
  geom_point(aes(x = time, y = Srxn1_observed), size = 1.0) +
  geom_errorbar(aes(ymin=Srxn1_observed-Srxn1_SD , ymax= Srxn1_observed+Srxn1_SD), size=0.5,   
                width=.25)  +
  labs(x = "Time [h]",y="High_doses", title = paste0("A.",plotdrug[1],", model 2"))+
  theme(plot.title = element_text(size=10),axis.title.x=element_blank(),axis.title.y=element_text(size=10,face="bold"),axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  theme(legend.position='top', 
        legend.justification='left',
        legend.direction='horizontal') +labs(col = "Dose")+guides(color = guide_legend(override.aes = list(size = 1)))+
  guides(shape = guide_legend(override.aes = list(size = 1))) +
  theme(legend.key.width = unit(0.2, "cm"))+
  theme(legend.title = element_text(size = 5), 
        legend.text = element_text(size = 5))


##################
# ANdrographolide

drug = c("Sul","Andro", "Etha", "Cddo")
drugs = drug[2]

file = read.table(paste0(direc,"/", Modeling_dir,"/",drugs,Modeling_dir,"_posteriorsimulation.txt"))

drugdata = ifelse(drugs == "Sul",Drug[1], ifelse(drugs == "Andro",Drug[2],ifelse(drugs == 'Etha',Drug[3],ifelse(drugs == "Cddo", Drug[4], "NA"))))

toxicdata = ifelse(drugs == "Sul",Toxicdose[1], ifelse(drugs == "Andro",Toxicdose[2],ifelse(drugs == 'Etha',Toxicdose[3],ifelse(drugs == "Cddo", Toxicdose[4], "NA"))))

Generic_data_read(Drug = drugdata,      # 1 for Sul, 2 for Andro, 3 for Etha, & 4 for CDDO_me
                  Scenario = Scenario[4],  # 1 & 2 for repeated 8+24 & 24+24 respectively  , 2 & 4 for continous 32 and 48 respectively
                  time_steps = 1,
                  exp_timestep = 1)


Data_SimuFun = function (Drug,highdose,Scenario,variable) {
  Final_matrix_exp_data = Final_matrix_exp_data %>% filter(dose1 != highdose & dose2 != highdose)
  x_final = file 
  plot_final = cbind(Final_matrix_exp_data,x_final)
  plot_final = plot_final[,!duplicated(names(plot_final))]  #to remove duplicate columns with the same names
  return(plot_final)
}


Df4 = Data_SimuFun(Drug = drugs,      # 1 for Sul, 2 for Andro, 3 for Etha, & 4 for CDDO_me
                   highdose = toxicdata,
                   Scenario = Scenario[4],  # 1 & 2 for repeated 8+24 & 24+24 respectively  , 2 & 4 for continous 32 and 48 respectively
                   variable = "Srxn1")

final11 = Df4 %>%
  filter(.,Simulation  %in% Simulations[c(1,2,3)])
final12 = Df4 %>%
  filter(.,Simulation  %in% Simulations[c(4,5,6)])

plot11 = final11 %>% 
  ggplot(aes(x = time, y = medianM,color = as.factor(dose1),group= as.factor(dose1))) +
  geom_line() +
  scale_colour_manual(values=c(mypal[1],mypal[2],mypal[3])) +
  geom_point(aes(x = time, y = Srxn1_observed), size = 1.0) +
  geom_errorbar(aes(ymin=Srxn1_observed-Srxn1_SD, ymax= Srxn1_observed +Srxn1_SD), size=0.5,   
                width=.25)  +
  labs(x = "Time [h]",y="GFP intensity [au]", title = paste0("B.",plotdrug[2],", model 2")) +
  theme(plot.title = element_blank(),axis.title.x=element_blank(),axis.title.y=element_blank(),
        axis.ticks.x=element_blank())+
  theme(legend.position='top', 
        legend.justification='left',
        legend.direction='horizontal') +labs(col = "Dose")+guides(color = guide_legend(override.aes = list(size = 1)))+
  guides(shape = guide_legend(override.aes = list(size = 1))) +
  theme(legend.key.width = unit(0.2, "cm"))+
  theme(legend.title = element_text(size = 5), 
        legend.text = element_text(size = 5))


plot12 = final12 %>% 
  ggplot(aes(x = time, y = medianM,group= as.factor(dose1),color = as.factor(dose1))) +
  geom_line() +
  scale_colour_manual(values=c(mypal[4],mypal[5],mypal[6])) +
  geom_point(aes(x = time, y = Srxn1_observed), size = 1.0) +
  
  geom_errorbar(aes(ymin=Srxn1_observed-Srxn1_SD, ymax= Srxn1_observed +Srxn1_SD), size=0.5,   
                width=.25)  +
  
  labs(x = "Time [h]",y="High_doses", title = paste0("B." ,plotdrug[2],", model 2"))+
  theme(plot.title = element_text(size=10),axis.title.x=element_blank(),axis.title.y=element_blank(),axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  theme(legend.position='top', 
        legend.justification='left',
        legend.direction='horizontal') +labs(col = "Dose")+guides(color = guide_legend(override.aes = list(size = 1)))+
  guides(shape = guide_legend(override.aes = list(size = 1))) +
  theme(legend.key.width = unit(0.2, "cm"))+
  theme(legend.title = element_text(size = 5), 
        legend.text = element_text(size = 5))


##################
# Etha

drug = c("Sul","Andro", "Etha", "Cddo")
drugs = drug[3]

file = read.table(paste0(direc,"/", Modeling_dir,"/",drugs,Modeling_dir,"_posteriorsimulation.txt"))

drugdata = ifelse(drugs == "Sul",Drug[1], ifelse(drugs == "Andro",Drug[2],ifelse(drugs == 'Etha',Drug[3],ifelse(drugs == "Cddo", Drug[4], "NA"))))

toxicdata = ifelse(drugs == "Sul",Toxicdose[1], ifelse(drugs == "Andro",Toxicdose[2],ifelse(drugs == 'Etha',Toxicdose[3],ifelse(drugs == "Cddo", Toxicdose[4], "NA"))))

Generic_data_read(Drug = drugdata,      # 1 for Sul, 2 for Andro, 3 for Etha, & 4 for CDDO_me
                  Scenario = Scenario[4],  # 1 & 2 for repeated 8+24 & 24+24 respectively  , 2 & 4 for continous 32 and 48 respectively
                  time_steps = 1,
                  exp_timestep = 1)


Data_SimuFun = function (Drug,highdose,Scenario,variable) {
  Final_matrix_exp_data = Final_matrix_exp_data %>% filter(dose1 != highdose & dose2 != highdose)
  x_final = file 
  plot_final = cbind(Final_matrix_exp_data,x_final)
  plot_final = plot_final[,!duplicated(names(plot_final))]  #to remove duplicate columns with the same names
  return(plot_final)
}


Df4 = Data_SimuFun(Drug = drugs,      # 1 for Sul, 2 for Andro, 3 for Etha, & 4 for CDDO_me
                   highdose = toxicdata,
                   Scenario = Scenario[4],  # 1 & 2 for repeated 8+24 & 24+24 respectively  , 2 & 4 for continous 32 and 48 respectively
                   variable = "Srxn1")



final13 = Df4 %>% 
  filter(.,Simulation  %in% Simulations[c(1,2,3)])

final14 = Df4 %>% 
  filter(.,Simulation  %in% Simulations[c(4,5,6)])


plot13 = final13 %>% 
  ggplot(aes(x = time, y = medianM,color = as.factor(dose1),group= as.factor(dose1))) +
  geom_line() +
  scale_colour_manual(values=c(mypal[1],mypal[2],mypal[3])) +
  geom_point(aes(x = time, y = Srxn1_observed), size = 1.0) +
  geom_errorbar(aes(ymin=Srxn1_observed-Srxn1_SD, ymax= Srxn1_observed +Srxn1_SD), size=0.5,   
                width=.25)  +
  labs(x = "Time [h]",y="GFP intensity [au]", title = paste0("C.",plotdrug[3],", model 2")) +
  theme(plot.title = element_blank(),axis.title.x=element_blank(),axis.title.y=element_blank(),
        axis.ticks.x=element_blank())+
  theme(legend.position='top', 
        legend.justification='left',
        legend.direction='horizontal') +labs(col = "Dose")+guides(color = guide_legend(override.aes = list(size = 1)))+
  guides(shape = guide_legend(override.aes = list(size = 1))) +
  theme(legend.key.width = unit(0.2, "cm"))+
  theme(legend.title = element_text(size = 5), 
        legend.text = element_text(size = 5))

plot14 = final14 %>%
  ggplot(aes(x = time, y = medianM,group= as.factor(dose1),color = as.factor(dose1))) +
  geom_line() +
  scale_colour_manual(values=c(mypal[4],mypal[5],mypal[6])) +
  geom_point(aes(x = time, y = Srxn1_observed), size = 1.0) +
  
  geom_errorbar(aes(ymin=Srxn1_observed-Srxn1_SD, ymax= Srxn1_observed +Srxn1_SD), size=0.5,   
                width=.25)  +
  
  labs(x = "Time [h]",y="High_doses", title = paste0("C." ,plotdrug[3],", model 2"))+
  theme(plot.title = element_text(size=10),axis.title.x=element_blank(),axis.title.y=element_blank(),axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  theme(legend.position='top', 
        legend.justification='left',
        legend.direction='horizontal') +labs(col = "Dose")+guides(color = guide_legend(override.aes = list(size = 1)))+
  guides(shape = guide_legend(override.aes = list(size = 1))) +
  theme(legend.key.width = unit(0.2, "cm"))+
  theme(legend.title = element_text(size = 5), 
        legend.text = element_text(size = 5))

##################
# CDDO_me

drug = c("Sul","Andro", "Etha", "Cddo")
drugs = drug[4]

file = read.table(paste0(direc,"/", Modeling_dir,"/",drugs,Modeling_dir,"_posteriorsimulation.txt"))

drugdata = ifelse(drugs == "Sul",Drug[1], ifelse(drugs == "Andro",Drug[2],ifelse(drugs == 'Etha',Drug[3],ifelse(drugs == "Cddo", Drug[4], "NA"))))

toxicdata = ifelse(drugs == "Sul",Toxicdose[1], ifelse(drugs == "Andro",Toxicdose[2],ifelse(drugs == 'Etha',Toxicdose[3],ifelse(drugs == "Cddo", Toxicdose[4], "NA"))))

Generic_data_read(Drug = drugdata,      # 1 for Sul, 2 for Andro, 3 for Etha, & 4 for CDDO_me
                  Scenario = Scenario[4],  # 1 & 2 for repeated 8+24 & 24+24 respectively  , 2 & 4 for continous 32 and 48 respectively
                  time_steps = 1,
                  exp_timestep = 1)


Data_SimuFun = function (Drug,highdose,Scenario,variable) {
  Final_matrix_exp_data = Final_matrix_exp_data %>% filter(dose1 != highdose & dose2 != highdose)
  x_final = file 
  plot_final = cbind(Final_matrix_exp_data,x_final)
  plot_final = plot_final[,!duplicated(names(plot_final))]  #to remove duplicate columns with the same names
  return(plot_final)
}


Df4 = Data_SimuFun(Drug = drugs,      # 1 for Sul, 2 for Andro, 3 for Etha, & 4 for CDDO_me
                   highdose = toxicdata,
                   Scenario = Scenario[4],  # 1 & 2 for repeated 8+24 & 24+24 respectively  , 2 & 4 for continous 32 and 48 respectively
                   variable = "Srxn1")

final15 = Df4 %>% 
  filter(.,Simulation  %in% Simulations[c(1,2,3)])
final16 = Df4 %>%  
  filter(.,Simulation  %in% Simulations[c(4,5,6)])

plot15 = final15 %>% 
  ggplot(aes(x = time, y = medianM,color = as.factor(dose1),group= as.factor(dose1))) +
  geom_line() +
  scale_colour_manual(values=c(mypal[1],mypal[2],mypal[3])) +
  geom_point(aes(x = time, y = Srxn1_observed), size = 1.0) +
  geom_errorbar(aes(ymin=Srxn1_observed-Srxn1_SD, ymax= Srxn1_observed +Srxn1_SD), size=0.5,   
                width=.25)  +
  labs(x = "Time [h]",y="GFP intensity [au]", title = paste0("C.",plotdrug[4],", model 2")) +
  theme(plot.title = element_blank(),axis.title.x=element_blank(),axis.title.y=element_blank(),
        axis.ticks.x=element_blank())+
  theme(legend.position='top', 
        legend.justification='left',
        legend.direction='horizontal') +labs(col = "Dose")+guides(color = guide_legend(override.aes = list(size = 1)))+
  guides(shape = guide_legend(override.aes = list(size = 1))) +
  theme(legend.key.width = unit(0.2, "cm"))+
  theme(legend.title = element_text(size = 5), 
        legend.text = element_text(size = 5))

plot16 = final16 %>% 
  ggplot(aes(x = time, y = medianM,group= as.factor(dose1),color = as.factor(dose1))) +
  geom_line() +
  scale_colour_manual(values=c(mypal[4],mypal[5],mypal[6])) +
  geom_point(aes(x = time, y = Srxn1_observed), size = 1.0) +
  
  geom_errorbar(aes(ymin=Srxn1_observed-Srxn1_SD, ymax= Srxn1_observed +Srxn1_SD), size=0.5,   
                width=.25)  +
  
  labs(x = "Time [h]",y="High_doses", title = paste0("D." ,plotdrug[4],", model 2"))+
  theme(plot.title = element_text(size=10),axis.title.x=element_blank(),axis.title.y=element_blank(),axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  theme(legend.position='top', 
        legend.justification='left',
        legend.direction='horizontal') +labs(col = "Dose")+guides(color = guide_legend(override.aes = list(size = 1)))+
  guides(shape = guide_legend(override.aes = list(size = 1))) +
  theme(legend.key.width = unit(0.2, "cm"))+
  theme(legend.title = element_text(size = 5), 
        legend.text = element_text(size = 5))


p4 <- grid.arrange(arrangeGrob(plot10 ,
                               plot12 ,
                               plot14 ,
                               plot16 ,
                               plot9 ,
                               plot11 ,
                               plot13 ,
                               plot15 ,
                               ncol=4))

plot_model2 = annotate_figure(p4,
                              left = text_grob("Srxn1 intensity [au]", color = "black", rot = 90,vjust = 1),
                              bottom = text_grob("Time [h]",color = "black",hjust = 0.5,
                                                 size = 10))

figdirec = "C:/Users/alexe/OneDrive - Universiteit Leiden/Manusript_Nrf2_repeated_dosing/Images"
tiff(paste0(figdirec,"/", "Figure4",".png"), units="in", width=7, height=5, res=700, compression = 'lzw')
print(plot_model2)
dev.off()
# dev.off()
# 

library(dplyr)
library(tidyr)
###################################
#c(noquote(paste0("final",rep(1:16),collapse = ',')))


###########################supplementary figure code
final_list = list(final1,final2,final3,final4,final5,final6,final7,final8,final9,final10,final11,final12,final13,final14,final15,final16)

#pdf(paste0(figdirec,"/", "Modelversion1=2_Rsquare",".pdf"),onefile=FALSE)
tiff(paste0(figdirec,"/", "Modelversion1=2_Rsquare",".png"), units="in", width=8, height=10, res=700, compression = 'lzw')
par(mfrow = c(4,4))
for (i in 1:length(final_list)){
  Final2 = final_list[[i]]%>% mutate_if(is.numeric, funs(replace_na(., 0)))
  #png(paste0(directory,"/", "Repeated_Obs_Vs_Predicted",".png"), width = 700, height = 500)
  R_square <- cor(Final2$Srxn1_observed, Final2$medianM)^2
  lim = max(Final2$medianM)*1.2
  version = c("M1", "M2")
  dose =ifelse(i %in%c(1,3,5,7,9,11,13,15),"L","H")
  name = c("A.", "B.","C.", "D.", "E.", "F.", "G.", "H.", "I.")
  version = ifelse(i >= 1 & i<9, "M1", "M2")
  plot(Final2$Srxn1_observed, Final2$medianM,
       xlim = c(0.1, lim), ylim = c(0.1, lim),
       xlab = "Observed", ylab = "Predicted",
       main = str2expression(paste0(Final2$Chemical[1],":",version,":",dose,":", "R^2 ~ ",format(round(R_square, digits = 3)))),cex.main = 1.0,cex.axis = 1.0)
  abline(0,1)
  
}
dev.off()

   
