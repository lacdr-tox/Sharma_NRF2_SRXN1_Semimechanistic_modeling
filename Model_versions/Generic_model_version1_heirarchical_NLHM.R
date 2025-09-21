# Modelling information:
# Curated semi-mechanistic modeling of Srxn1 dynamics under repeated and continuous Stress 
# Model version 1 where there is no use of fraction x
# Nrf2 does not require any co-activator to activate the Srxn1?
# Model version 1 where different Vmax for different chemicals.

rm(list = ls())
library(magrittr)
library(deSolve)
library(tidyr)
library(magrittr)
library(ggplot2)
library(dplyr)
library(stringr)
#################
#set the directory where the files are located

# the directory where data files are saved. 

directory = "C:/Users/alexe/OneDrive - Universiteit Leiden/mcsim_files/Generic_curated_model"
parameters_dir = 'C:/Users/alexe/OneDrive - Universiteit Leiden/mcsim_files/Model_versions'
# Start of Function
#################################################################################################
# this funciton is useful when there is NA in simulation and you want to keep you axis based on maximum value of y i.e SRXN1.

Generic_function = function (Drug,Scenario,time_steps,Nrf2_plot,Obs_vs_Pred_plot) {
  
  
  Exp =  ifelse(Scenario %in% c("R_24_24",'R_8_24'),"Rep",
                ifelse(Scenario  %in% c("Conti_48",'Conti_32'),"Conti","NA"))
  
  
  # selection of second dose timing based on scenarios
  
  repeat_dosing_time =  ifelse(Scenario == "R_24_24",24,
                               ifelse(Scenario =='R_8_24',8,0))
  
  
  end_time =  ifelse(Scenario %in% c("R_24_24","Conti_48"),48,
                     ifelse(Scenario %in% c('R_8_24','Conti_32'),32,"NA"))
  
  total_steps = ifelse(Scenario %in% c("R_24_24","Conti_48"),49,
                       ifelse(Scenario %in% c('R_8_24','Conti_32'),33,"NA"))
  
  Dosing_matrix =  ifelse(Scenario %in% c("R_24_24",'R_8_24'),48,
                          ifelse(Scenario  %in% c("Conti_48",'Conti_32'),6,"NA"))
  
  
  chem_specific_r =  ifelse(Drug == 'Sulforaphane','r_Sul',
                            ifelse(Drug == 'Andrographolide',"r_Andro",
                                   ifelse(Drug == 'Ethacrynic_Acid',"r_Etha",
                                          ifelse(Drug == "CDDO_me","r_CDDO", "NA"))))
  
  chem_specific_d =  ifelse(Drug == 'Sulforaphane','decay_Sul',
                            ifelse(Drug == 'Andrographolide',"decay_Andro",
                                   ifelse(Drug == 'Ethacrynic_Acid',"decay_Etha",  
                                          ifelse(Drug == "CDDO_me" ,"decay_CDDO", "NA"))))
  
  
  
  chem_time_specific_r2 =  ifelse(Drug == 'Sulforaphane'& Scenario == "R_24_24",'r2_Sul_24',ifelse(Drug == 'Sulforaphane'& Scenario == "R_8_24",'r2_Sul_8',
                                                                                                   ifelse(Drug == 'Andrographolide'& Scenario == "R_24_24","r2_Andro_24",ifelse(Drug == 'Andrographolide'& Scenario == "R_8_24","r2_Andro_8",
                                                                                                                                                                                ifelse(Drug == 'Ethacrynic_Acid'& Scenario == "R_24_24","r2_Etha_24", ifelse(Drug == 'Ethacrynic_Acid'& Scenario == "R_8_24","r2_Etha_8",
                                                                                                                                                                                                                                                             ifelse(Drug == "CDDO_me"& Scenario == "R_24_24","r2_CDDO_24",ifelse(Drug == "CDDO_me"& Scenario == "R_8_24","r2_CDDO_8", "NA"))))))))
  
  chem_time_specific_d2 =  ifelse(Drug == 'Sulforaphane'& Scenario == "R_24_24",'decay2_Sul_24',ifelse(Drug == 'Sulforaphane'& Scenario == "R_8_24",'decay2_Sul_8',
                                                                                                       ifelse(Drug == 'Andrographolide'& Scenario == "R_24_24","decay2_Andro_24",ifelse(Drug == 'Andrographolide'& Scenario == "R_8_24","decay2_Andro_8",
                                                                                                                                                                                        ifelse(Drug == 'Ethacrynic_Acid'& Scenario == "R_24_24","decay2_Etha_24",ifelse(Drug == 'Ethacrynic_Acid'& Scenario == "R_8_24","decay2_Etha_8", 
                                                                                                                                                                                                                                                                        ifelse(Drug == "CDDO_me" & Scenario == "R_24_24","decay2_CDDO_24",ifelse(Drug == "CDDO_me" & Scenario == "R_8_24","decay2_CDDO_8", "NA"))))))))
  
  file_name = ifelse(Drug == 'Sulforaphane'& Scenario %in% c("R_24_24","Conti_48"),'New_24_24_Sul_data',ifelse(Drug == 'Sulforaphane'& Scenario %in% c('R_8_24','Conti_32'),'New_8_24_Sul_data',
                                                                                                               ifelse(Drug == 'Andrographolide'& Scenario %in% c("R_24_24","Conti_48"),"New_24_24_Andro_data",ifelse(Drug == 'Andrographolide'& Scenario %in% c('R_8_24','Conti_32'),"New_8_24_Andro_data",
                                                                                                                                                                                                                     ifelse(Drug == 'Ethacrynic_Acid'& Scenario %in% c("R_24_24","Conti_48"),"New_24_24_Etha_data",ifelse(Drug == 'Ethacrynic_Acid'& Scenario %in% c('R_8_24','Conti_32'),"New_8_24_Etha_data", 
                                                                                                                                                                                                                                                                                                                          ifelse(Drug == "CDDO_me"& Scenario %in% c("R_24_24","Conti_48"),"New_24_24_CDDO_data", ifelse(Drug == "CDDO_me"& Scenario %in% c('R_8_24','Conti_32'),"New_8_24_CDDO_data",print("Error in file name")))))))))
  
  
  
  filenames <- list.files(path = paste0(directory, "/", file_name),pattern = ".txt")
  
  myfiles = lapply(paste0(directory,"/",file_name,"/",filenames), read.delim,sep=" ")
  names(myfiles) <- filenames
  
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
  
  
  ##################
  # intial states time 
  
  xstart <- c(Srxn1 = 0)
  times = seq(0,end_time, by = 1)       # the generated time points here is correspond to the experimental data utilized in spline interpolation function
  t = seq(0,end_time, by = time_steps)  # here the generated time points are used in ODE.
  
  #########################################
  # Parmeters value
  ############################################
  
  chem_specific_Vmax =  ifelse(Drug == 'Sulforaphane','Vmax_buildSrxn1.1.1.',
                            ifelse(Drug == 'Andrographolide',"Vmax_buildSrxn1.1.2.",
                                   ifelse(Drug == 'Ethacrynic_Acid',"Vmax_buildSrxn1.1.3.",
                                          ifelse(Drug == "CDDO_me","Vmax_buildSrxn1.1.4.", "NA"))))
  
  
  parameters = data.frame(read.csv(paste0(parameters_dir,"/model_version_1_heirar.csv"),header = TRUE,stringsAsFactors = FALSE))
  
  buildSrxn1Base = (parameters %>% filter(X == "buildSrxn1Base.1.") %>% select(mean))[[1]]
  degradSrxn1 = (parameters %>% filter(X == "degradSrxn1.1.") %>% select(mean))[[1]]
  Vmax_buildSrxn1 = (parameters %>% filter(X == chem_specific_Vmax) %>% select(mean))[[1]]
  Km = (parameters %>% filter(X == "Km.1.") %>% select(mean))[[1]]
  hill = (parameters %>% filter(X == "hill.1.") %>% select(mean))[[1]]
  
  # dosing scenarios parameter set
  
  parms <- unlist(c(data.frame(
    buildSrxn1Base,
    degradSrxn1,
    Vmax_buildSrxn1,
    Km,
    hill)))              #estimated from 24+24 scenarios (increment first dose + fixed second dose)
  
  print(parms)
  ############################################
  #ODE with mRNA of SRxn1
  ############################################
  
  SRXN1_mod1 <- function(t, x, parms,input1) {
    with(as.list(c(parms, x)), {
      NRF2_T <- input1(t)   # <---- h,ere
      
      dSrxn1 = buildSrxn1Base + ((Vmax_buildSrxn1 * (NRF2_T**hill))/ (Km**hill + NRF2_T**hill)) - degradSrxn1 * Srxn1
      
      
      res <- c(dSrxn1)
      list(res, NRF2 = NRF2_T)
      #list(res)
    })
  }
  
  
  
  
  out_max = vector(mode =  "list", length = length(data_file1[[paste0(Drug,"_Nrf2_",Exp,"_Max_GFP.txt")]]))
  
  input2_max = vector(mode =  "list", length = length(data_file1[[paste0(Drug,"_Nrf2_",Exp,"_Max_GFP.txt")]]))
  
  
  for (i in 1:Dosing_matrix) {
    input2_max[[i]] <- splinefun(times,data_file1[[paste0(Drug,"_Nrf2_",Exp,"_Max_GFP.txt")]][[i]],method = "monoH.FC")# use maximum NRF2_INPUT
    out_max[[i]] <- ode(y = xstart, times = t, func = SRXN1_mod1, parms,input1 = input2_max[[i]])
  } 
  
  out_mean = vector(mode =  "list", length = length(data_file1[[paste0(Drug,"_Nrf2_",Exp,"_Mean_GFP.txt")]]))
  input2 = vector(mode =  "list", length = length(data_file1[[paste0(Drug,"_Nrf2_",Exp,"_Mean_GFP.txt")]]))
  for (i in 1:Dosing_matrix) {
    
    input2[[i]] <- splinefun(times,data_file1[[paste0(Drug,"_Nrf2_",Exp,"_Mean_GFP.txt")]][[i]],method = "monoH.FC") # use mean NRF2_INPUT
    out_mean[[i]] <- ode(y = xstart, times = t, func = SRXN1_mod1, parms,input1 = input2[[i]])
  }
  
  out_min = vector(mode =  "list", length = length(data_file1[[paste0(Drug,"_Nrf2_",Exp,"_Min_GFP.txt")]]))
  input2_min = vector(mode =  "list", length(data_file1[[paste0(Drug,"_Nrf2_",Exp,"_Min_GFP.txt")]]))
  
  for (i in 1:Dosing_matrix) {
    input2_min[[i]] <- splinefun(times,data_file1[[paste0(Drug,"_Nrf2_",Exp,"_Min_GFP.txt")]][[i]],method = "monoH.FC") # 
    
    out_min[[i]] <- ode(y = xstart, times = t, func = SRXN1_mod1, parms,input1 = input2_min[[i]])
    
  } 
  
  
  names(out_max) = paste(names(data_file1[[paste0(Drug,"_Nrf2_",Exp,"_Max_GFP.txt")]])[1:Dosing_matrix]) #name the output
  names(out_mean) = paste(names(data_file1[[paste0(Drug,"_Nrf2_",Exp,"_Mean_GFP.txt")]])[1:Dosing_matrix]) #name the output
  names(out_min) = paste(names(data_file1[[paste0(Drug,"_Nrf2_",Exp,"_Min_GFP.txt")]])[1:Dosing_matrix]) #name the output
  
  
  
  ##############################
  #Preparation of dosing matrix for the matrix plot
  ################################
  dosing_matrix = str_split(names_data_file[1:Dosing_matrix],"_")
  matrix = do.call(rbind,dosing_matrix)
  colnames(matrix)= c("Dose1","Dose2")
  Dose1 = data.frame(rep(matrix[,c(1)], each = total_steps),stringsAsFactors=FALSE)
  colnames(Dose1) = 'dose1'
  Dose2 = data.frame(rep(matrix[,c(2)], each = total_steps),stringsAsFactors=FALSE)
  colnames(Dose2) = 'dose2'
  Dose_matrix = cbind(Dose1,Dose2)
  Dose_matrix$dose1 = as.numeric(Dose_matrix$dose1)
  Dose_matrix$dose2 = as.numeric(Dose_matrix$dose2)
  
  ############################################
  # reshaping simulation output for ggplot (mean output and max output)
  ###########################################
  datafile_max = do.call(rbind,out_max)    #maximum output
  datafile_mean = do.call(rbind,out_mean)  #meanouput
  datafile_min = do.call(rbind,out_min)    #maximum output
  colnames(datafile_max) <- c("time" , "Srxn1_max", "NRF2_max")
  colnames(datafile_mean) <- c("time2" , "Srxn1_mean", "NRF2_mean")
  colnames(datafile_min) <- c("time3" , "Srxn1_min", "NRF2_min")
  
  simulation = paste("simulation", seq(1:Dosing_matrix), sep = "" )
  simulation1 = data.frame(rep(simulation, each = total_steps))
  colnames(simulation1) = 'Simulation'
  
  Final_output = cbind(datafile_max,datafile_mean,datafile_min,Dose_matrix,simulation1)
  
  
  ##############################
  #Reshaping of observed data 
  #############################
  
  Experimentdata = data_file1[[paste0(Drug,"_Srxn1_",Exp,"_Mean_GFP.txt")]] #name the output
  matrix_exp_data = data.frame(unlist(Experimentdata))
  colnames(matrix_exp_data) <- 'SRXN1_observed'
  Exp_SD =data_file1[[paste0(Drug,"_Srxn1_",Exp,"_sd_GFP.txt")]]
  matrix_exp_data_SD = data.frame(unlist(Exp_SD))
  colnames(matrix_exp_data_SD) <- 'SRXN1_SD'
  Final_matrix_exp_data = cbind(matrix_exp_data,matrix_exp_data_SD)
  
  ##########################
  # combine both the simulated data and experimental data
  #########################
  # replace if there is any nan with zeros
  
  Finalplotdata = cbind(Final_output,Final_matrix_exp_data)  #%>% mutate_if(is.numeric, funs(replace_na(., 0)))
  
  max_value_srxn1 = Finalplotdata %>% select(.,Srxn1_max) %>% max(.,na.rm = TRUE)
  
  # # Create plots in default window
  # 
  # dev.new(width = 700,
  #         height = 490) 
  # 
  # 
  plot1 = Finalplotdata %>% 
    ggplot() +
    geom_line(aes(x = time, y = Srxn1_max, color = "Simulated_max"),size=1) +
    geom_line(aes(x = time, y = Srxn1_mean, color="Simulated_mean"),size=1) +
    geom_line(aes(x = time, y = Srxn1_min, color="Simulated_min"),size=1) +
    geom_errorbar(aes(x = time, ymin=SRXN1_observed-SRXN1_SD, ymax= SRXN1_observed +SRXN1_SD,color = "Experiments_mean(Sd)"), size=0.5,   
                  width=.25) + 
    facet_grid(dose2~dose1) +
    labs(x = "Time (hrs)",y="GFP intensity",sec.x="First exposure (然)",
         sec.y="Second exposure (然)", title = paste0(Drug,"_",Scenario,"_SRXN1 vs Time")) +
    scale_y_continuous(breaks=seq(0,max_value_srxn1,by= ceiling(max_value_srxn1/4))) +
    scale_x_continuous(breaks=seq(0,end_time,by= end_time/4)) +
    scale_color_manual(name = "Simulation type",
                       breaks = c("Simulated_max", "Simulated_mean","Simulated_min", "Experiments_mean(Sd)"),
                       values = c("Simulated_max" = "red", "Simulated_mean" = "blue", "Simulated_min" = "green","Experiments_mean(Sd)" = "black"))+
    theme(legend.title = element_blank()) + theme(legend.position="bottom") +
    theme(legend.text = element_text(size= 8,face="bold")) 
  
  
  plot2 = Finalplotdata %>% 
    ggplot() +
    geom_line(aes(x = time, y = Srxn1_max, color = "Simulated_max"),size=1) +
    geom_line(aes(x = time, y = Srxn1_mean, color="Simulated_mean"),size=1) +
    geom_line(aes(x = time, y = Srxn1_min, color="Simulated_min"),size=1) +
    geom_errorbar(aes(x = time, ymin=SRXN1_observed-SRXN1_SD, ymax= SRXN1_observed +SRXN1_SD,color = "Experiments_mean(Sd)"), size=0.5,   
                  width=.25) + 
    facet_wrap(~dose1) +
    labs(x = "Time (hrs)",y="GFP intensity",sec.x="First exposure (然)",
         sec.y="Second exposure (然)", title = paste0(Drug,"_",Scenario,"_SRXN1 vs Time")) +
    scale_y_continuous(breaks=seq(0,max_value_srxn1,by= floor(max_value_srxn1/4))) +
    scale_x_continuous(breaks=seq(0,end_time,by= end_time/4)) +
    scale_color_manual(name = "Simulation type",
                       breaks = c("Simulated_max", "Simulated_mean","Simulated_min", "Experiments_mean(Sd)"),
                       values = c("Simulated_max" = "red", "Simulated_mean" = "blue", "Simulated_min" = "green","Experiments_mean(Sd)" = "black"))+
    theme(legend.title = element_blank()) + theme(legend.position="bottom") +
    theme(legend.text = element_text(size= 8,face="bold"))
  
  ifelse(Exp == "Rep",print(plot1), print(plot2))
  
  
  # Nrf2 vs Time plot 
  
  max_value_Nrf2 = Finalplotdata %>% select(.,NRF2_max) %>% max(.)
  
  if (Nrf2_plot == 'true') {
    
    dev.new(width = 700,
            height = 480) 
    
    plot1 =Finalplotdata %>% 
      ggplot() +
      geom_line(aes(x = time, y = NRF2_max,color = "NRF2_max"),size=1) +
      geom_line(aes(x = time,y = NRF2_mean, color="NRF2_mean"),size=1) +
      geom_line(aes(x = time,y = NRF2_min, color="NRF2_min"),size=1) +
      facet_grid(dose2~dose1) +
      labs(x = "Time (hr)",y="GFP intensity",sec.x="First exposure (然)",
           sec.y="Second exposure (然)", title = paste0(Drug,"_",Scenario,"_Nrf2 vs Time")) +
      scale_y_continuous(breaks=seq(0,max_value_Nrf2,by=ceiling(max_value_Nrf2/4))) +
      scale_x_continuous(breaks=seq(0,end_time,by=end_time/4)) +
      scale_color_manual(
        breaks = c("NRF2_max", "NRF2_mean","NRF2_min"),
        values = c("NRF2_max" = "red", "NRF2_mean" = "blue", "NRF2_min" = "green"))+
      theme(legend.title = element_blank()) + theme(legend.position="bottom") +
      theme(legend.text = element_text(size=10,face="bold"))
    
    plot2 =Finalplotdata %>% 
      ggplot() +
      geom_line(aes(x = time, y = NRF2_max,color = "NRF2_max"),size=1) +
      geom_line(aes(x = time,y = NRF2_mean, color="NRF2_mean"),size=1) +
      geom_line(aes(x = time,y = NRF2_min, color="NRF2_min"),size=1) +
      facet_wrap(~dose1) +
      labs(x = "Time (hr)",y="GFP intensity",sec.x="First exposure (然)",
           sec.y="Second exposure (然)", title = paste0(Drug,"_",Scenario,"_Nrf2 vs Time")) +
      scale_y_continuous(breaks=seq(0,max_value_Nrf2,by=ceiling(max_value_Nrf2/4))) +
      scale_x_continuous(breaks=seq(0,end_time,by=end_time/4)) +
      scale_color_manual(
        breaks = c("NRF2_max", "NRF2_mean","NRF2_min"),
        values = c("NRF2_max" = "red", "NRF2_mean" = "blue", "NRF2_min" = "green"))+
      theme(legend.title = element_blank()) + theme(legend.position="bottom") +
      theme(legend.text = element_text(size=10,face="bold"))
    
    ifelse(Exp == "Rep",print(plot1), print(plot2))
    
  } else {
    
    print ("The Nrf2 plot has not been called: to plot make Nrf2_plot = true")
  }
  
  
  # observed vs predicted regression considering the mean prediction and the mean observed data points.
  # Create empty plot in default window
  
  if (Obs_vs_Pred_plot == 'true') {
    
    R_square <- cor(Finalplotdata$SRXN1_observed, Finalplotdata$Srxn1_mean)^2
    plot(Finalplotdata$SRXN1_observed, Finalplotdata$Srxn1_mean,
         xlim = c(0.1, max_value_srxn1), ylim = c(0.1, max_value_srxn1),
         xlab = "Observation", ylab = "Prediction",
         main = paste(Drug,"_",Scenario,"(R2 =", round(R_square, digits = 3),")"))
    abline(0,1)
    
  } else {
    
    print ("The Obs_vs_Pred_plot has not been called: to plot make Obs_vs_Pred_plot = true")
  }
  
}
# End of Function
#################################################################################################


#Different inputs required for the function

Drug = c('Sulforaphane',"Andrographolide","Ethacrynic_Acid","CDDO_me")
Scenario = c("R_8_24","R_24_24","Conti_32", "Conti_48")   # all the scenarios
time_steps = 1  # default one, the step size was kept one to reduce the simulation time and also to have the uniformity with the exerimental data set.


# note: Plots will appear in new window


Generic_function(Drug = Drug[3],      # 1 for Sul, 2 for Andro, 3 for Etha, & 4 for CDDO_me
                 Scenario = Scenario[4],  # 1 & 2 for repeated 8+24 & 24+24 respectively  , 2 & 4 for continous 32 and 48 respectively
                 time_steps,
                 Nrf2_plot = "false",         # assign true to plot Nrf2
                 Obs_vs_Pred_plot =  "false") # assign true to plot observed vs predicted


# line 238 is half commented in order to not to plot the curve smoothly as some of thE nA converted to zero which not lead to a smooth plot 
# but this step is important to get the plot for observed vs predicted
# may be for ethacrynic acid plot remove that.



getwd()
