# Modelling information:
# Curated semi-mechanistic modeling of Srxn1 dynamics under repeated and continuous Stress 
# to understand the model and its parameters in detail please read the "Read_Me_Modelling_information.doc" file
# To run the model please ensure that all the required packages are installed.
# then set the directory where the data files are located in your system
# This is the generic code i.e. the same code can be run for different chemicals,to do that a function was created 
# called it as "Generic_function" which only requires an input of the following:
# 1) "Drug": the name of Drug for which you would like to run the simulation
# 2) "Scenario": the name of experiment type for which you would like to run the simulation
# 3) "time_steps": the ODE time steps for a moment this is now assigned to the default value of 1. 
# 4) "data_save": Command to save the model output data in a folder.
# 4) "Nrf2_plot": command to see the plot for the Nrf2 input data is to assign it to "true", by default it is false now.
# 5) "Fraction_X_plot": command to see the plot for the Fraction_X is to assign it to "true", by default it is false now. 
# 6) "Obs_vs_Pred_plot" : command to see the plot for Observed vs model prediction is to assign it to "true", by default it is false now.
# Srxn1 dynamic plot is the default main plot.

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
  
  directory = "C:/Users/............./Generic_curated_model"
  
  # directory to save the output data
  
  out_dir = "C:/Users/............./Generic_curated_model/Model_Figures_data"
  
  
                                  # Start of Function
  #################################################################################################
  
  Generic_function = function (Drug,Scenario,time_steps,data_save,Nrf2_plot,Fraction_X_plot,Obs_vs_Pred_plot) {
  

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
  parameters = data.frame(read.csv(paste0(directory,"/Parameters_set.csv"),header = TRUE,stringsAsFactors = FALSE))
  
  buildSrxn1Base = (parameters %>% filter(General_Parameters_name == "buildSrxn1Base") %>% select(Value))[[1]]
  degradSrxn1 = (parameters %>% filter(General_Parameters_name == "degradSrxn1") %>% select(Value))[[1]]
  Vmax_buildSrxn1 = (parameters %>% filter(General_Parameters_name == "Vmax_buildSrxn1") %>% select(Value))[[1]]
  Km = (parameters %>% filter(General_Parameters_name == "Km_Srxn1") %>% select(Value))[[1]]
  hill = (parameters %>% filter(General_Parameters_name == "hill_Srxn1") %>% select(Value))[[1]]
  initial_fraction_X = (parameters %>% filter(General_Parameters_name == "initial_fraction") %>% select(Value))[[1]]
  Km_act = (parameters %>% filter(General_Parameters_name == "Km_NRF2_ACETYLATED_SRXN1") %>% select(Value))[[1]]
  hill_act = (parameters %>% filter(General_Parameters_name == "hill_acetylated_SRXN1") %>% select(Value))[[1]]
  
  
  
  
  #chemical specific parameters: same for all the experimental type
  r = (parameters %>% filter(General_Parameters_name == chem_specific_r) %>% select(Value))[[1]]
  decay = (parameters %>% filter(General_Parameters_name == chem_specific_d) %>% select(Value))[[1]]
  
  # Chemical and experimental type specific parameters for continous these parameters should be zero
  
  r2 = ifelse(Scenario %in% c("R_24_24",'R_8_24'),(parameters %>% filter(General_Parameters_name == chem_time_specific_r2) %>% select(Value))[[1]],0)
  decay2 = ifelse(Scenario %in% c("R_24_24",'R_8_24'),(parameters %>% filter(General_Parameters_name == chem_time_specific_d2) %>% select(Value))[[1]],0)
  
  # estimating initial fraction 2 provided at the time of second dosing in the case of repeated scenarios
  # here repeated dosing time differ based on scenario for e.g. 8 value for 8 +24 scenario and 24 for 24+24 scenario for continous this should be zero.
  
  time_estimate_initial_fraction_X = seq(0,repeat_dosing_time,by = 1)
  initial_fraction_X2 = 1/((1/initial_fraction_X - 1)*exp(-time_estimate_initial_fraction_X*r) + exp(time_estimate_initial_fraction_X*decay))[repeat_dosing_time+1]
  
  # dosing scenarios parameter set
  
  parms <- unlist(c(data.frame(
             buildSrxn1Base,
             degradSrxn1,
             Vmax_buildSrxn1,
             Km,
             hill,
             initial_fraction_X,
             Km_act,
             hill_act,
             initial_fraction_X2,      #fraction at the end of 24 hrs should be filled in the simulation file
             r,                      # this parmamter should be assigned the value in the simulation file estimated from continous exposure scenarios
             decay,                  # this parmamter should be assigned the value in the simulation file estimated from continous exposure scenarios
             r2,                     #estimated from 24+24 scenarios (increment first dose + fixed second dose)
             decay2)))              #estimated from 24+24 scenarios (increment first dose + fixed second dose)
  
  print(parms)
  ############################################
  #ODE with mRNA of SRxn1
  ############################################
  
  SRXN1_mod1 <- function(t, x, parms,input1) {
    with(as.list(c(parms, x)), {
      NRF2_T <- input1(t)   # <---- h,ere
      
      Fraction_X1 = ifelse(t <= repeat_dosing_time, 1/((1/initial_fraction_X - 1)*exp(-t*r) + exp(t*decay)),0)
      
      
      Fraction_X2 = ifelse(t > repeat_dosing_time, 1/((1/initial_fraction_X2 - 1)*exp(-t*r2) + exp(t*decay2)),0)
      
      
      Fraction_X = ifelse(Scenario %in% c("R_24_24",'R_8_24'),(Fraction_X1+Fraction_X2),
                              ifelse(Scenario  %in% c("Conti_48",'Conti_32'),1/((1/initial_fraction_X - 1)*exp(-t*r) + exp(t*decay)),print("Error in Fraction X")))
      
      
      dSrxn1 = buildSrxn1Base + (Vmax_buildSrxn1 * ((NRF2_T*(1-Fraction_X))**hill) *((NRF2_T*(Fraction_X))**hill_act))/
               (((Km**hill + (NRF2_T*(1-Fraction_X))**hill)) *(Km_act**hill_act +(NRF2_T*(Fraction_X))**hill_act)) - degradSrxn1 * Srxn1
                                                                                                                                                                                              
      
      res <- c(dSrxn1)
      list(res, NRF2 = NRF2_T,Fraction_X = Fraction_X)
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
  colnames(datafile_max) <- c("time" , "Srxn1_max", "NRF2_max","Fraction_X_max")
  colnames(datafile_mean) <- c("time2" , "Srxn1_mean", "NRF2_mean","Fraction_X_mean")
  colnames(datafile_min) <- c("time3" , "Srxn1_min", "NRF2_min","Fraction_X_min")
  
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
  
  Finalplotdata = cbind(Final_output,Final_matrix_exp_data)%>% mutate_if(is.numeric, funs(replace_na(., 0)))
  
max_value_srxn1 = Finalplotdata %>% select(.,Srxn1_max) %>% max(.)
  
  # Create plots in default window
  
  dev.new(width = 700,
          height = 490) 
  
  
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
    scale_y_continuous(breaks=seq(0,max_value_srxn1,by= floor(max_value_srxn1/4))) +
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
  
  ######################################################################
  # plot fraction recuritment of x 
  if (Fraction_X_plot == 'true') {
  dev.new(width = 400,
          height = 300) 
  
  plot_4 =Finalplotdata %>% filter(dose1 == noquote(matrix[[1,1]]), dose2 == noquote(matrix[[1,2]])) %>% 
    ggplot(aes(x = time, y = Fraction_X_mean)) +
    geom_line(size = 1,lty = 2) +
    labs(x = "Time (hr)",y="Fraction_recruitment X", title = paste0(Drug,"_",Scenario,"_Fraction_X vs Time"))
  
  print(plot_4)
  
  } else {
    
    print ("The Fraction_X plot has not been called: to plot make Fraction_X_plot = true")
  }
  
  
  # observed vs predicted regression considering the mean prediction and the mean observed data points.
  # Create empty plot in default window
  
  if (Obs_vs_Pred_plot == 'true') {
  dev.new(width = 400,
          height = 300) 
  
  R_square <- cor(Finalplotdata$SRXN1_observed, Finalplotdata$Srxn1_mean)^2
  plot(Finalplotdata$SRXN1_observed, Finalplotdata$Srxn1_mean,
       xlim = c(0.1, max_value_srxn1), ylim = c(0.1, max_value_srxn1),
       xlab = "Observation", ylab = "Prediction",
       main = paste(Drug,"_",Scenario,"(R2 =", round(R_square, digits = 3),")"))
  abline(0,1)
  
  } else {
    
    print ("The Obs_vs_Pred_plot has not been called: to plot make Obs_vs_Pred_plot = true")
  }
  
  
  #################################
  # Save the simulation data in a folder
 
  if (data_save == 'ON') { 
    Finalplotdata %>%  write.table(.,paste0(out_dir,"/",Drug,"_",Scenario,".txt"))  
  } else {
    
    print ("Data saving option is not ON")
  }
  
  }
  
  
                              # End of Function
  #################################################################################################
  
  
 #Different inputs required for the function
  
  Drug = c('Sulforaphane',"Andrographolide","Ethacrynic_Acid","CDDO_me")
  Scenario = c("R_8_24","R_24_24","Conti_32", "Conti_48")   # all the scenarios
  time_steps = 1  # default one, the step size was kept one to reduce the simulation time and also to have the uniformity with the exerimental data set.
  

 # note: Plots will appear in new window
  
  
  Generic_function(Drug = Drug[1],      # 1 for Sul, 2 for Andro, 3 for Etha, & 4 for CDDO_me
                   Scenario = Scenario[1],  # 1 & 2 for repeated 8+24 & 24+24 respectively  , 2 & 4 for continous 32 and 48 respectively
                   time_steps,
                   data_save = 'OFF',       # default off, ON if data need to be saved in output directory
                   Nrf2_plot = "false",         # assign true to plot Nrf2
                   Fraction_X_plot= "false",    # assign true to plot Fraction_X
                   Obs_vs_Pred_plot =  "false") # assign true to plot observed vs predicted
  
    
  