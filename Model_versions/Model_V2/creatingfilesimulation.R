
rm(list = ls())
setwd("~/MCSim/mod")

Modeling_dir = "Model1_Nrf2input"

Generic_data_read = function (Drug,Scenario,time_steps,exp_timestep) {
  
  directory = "~/Experimental_data_Unilever"
  
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

Finaloutput = c('Nrf2_observed', 'Nrf2max_observed', 'Nrf2min_observed')

drug = c('Sul', 'Andro', 'Etha', 'Cddo')

########### Check if this need to change based on whatoutput you would require
Drugs = Drug[1]
condition = Scenario[4]
##############

total_steps = ifelse(Scenario %in% c("R_24_24","Conti_48"),49,                
                     ifelse(Scenario %in% c('R_8_24','Conti_32'),33,"NA"))

file_name = ifelse(Drugs == 'Sulforaphane',"newminsul.txt",
             ifelse(Drugs == 'Andrographolide',"newminandro.txt",
             ifelse(Drugs == 'Ethacrynic Acid',"newminEtha.txt",
             ifelse(Drugs == "CDDO-me","newmincddo.txt",print("Error in file name")))))

file_number = ifelse(Drugs == 'Sulforaphane',1,
                   ifelse(Drugs == 'Andrographolide',2,
                          ifelse(Drugs == 'Ethacrynic Acid',3,
                                 ifelse(Drugs == "CDDO-me",4,print("Error in file name")))))



Generic_data_read(Drug = Drug[file_number],      # 1 for Sul, 2 for Andro, 3 for Etha, & 4 for CDDO_me
                  Scenario = condition,  # 1 & 2 for repeated 8+24 & 24+24 respectively  , 2 & 4 for continous 32 and 48 respectively
                  time_steps = 1,
                  exp_timestep = 1)

Final_matrix_exp_data    #output of generic data read

Final_dose = unique(Final_matrix_exp_data$dose1, incomparables = FALSE)
print(Final_dose)  # check it
Protein = "Nrf2"    #"Srxn1"
#chemical specific inputs and outputs for creating simulaiton file
input = paste0(drug[file_number], Modeling_dir,"set.in", ".R")  # this file will contain the chemical inputs. 

###
#filter the data for chemical and scenarios
###

datafiles = list()
for (j in 1:3){
for (i in 1:6){
  time = data.frame(seq(0,48, by = 1))
  colnames(time)= Finaloutput[j]
  datafile =Final_matrix_exp_data %>% filter(.,dose1 == Final_dose[i]) %>%  select (.,Finaloutput[j]) %>% round(., digits = 3)
  datafiles[[i]] = rbind(49,datafile,time)
}

connect <- file(file_name)
SetPoints
writeLines(paste0("SetPoints",sep="\n","Simulation {", sep="\n", "NRF2_spline= NDoses", sapply(datafiles, paste0, collapse=' '),";"
            ,sep="\n", "Print (Srxn1, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29,
         30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48);", sep="\n", "}"), connect)
close(connect)
file.copy(
  from = paste0(getwd(),"/", file_name),
  to = paste0(getwd(), "/", Modeling_dir,"/", file_name),overwrite = TRUE)
invisible(file.remove(file_name)) 

x <- readLines(paste0(getwd(),"/", Modeling_dir,"/", file_name), warn=FALSE) 
y <- x %>% gsub(pattern ="NDosesc", replace = 'NDoses')
writeLines(y, paste0(getwd(),"/", Modeling_dir,"/", "/",file_name))
x <- readLines(paste0(getwd(),"/", Modeling_dir, "/", file_name), warn=FALSE)
write(x, file=paste0(getwd(),"/", Modeling_dir, "/",input),append=TRUE)

}

# ouput = paste0(drug[file_number], Modeling_dir,"set", ".out")  #check this output file name if it is the same or not
# Setsim = paste0(drug[file_number], Modeling_dir,"setSim", ".out")
# ####
# #append the data now to simulaiton file
# 
# x <- readLines(paste0(getwd(),"/", Modeling_dir, "/", "defaultmodel1setpoint.in.R"), warn=FALSE)      
# y <- x %>% gsub(pattern ="output", replace = ouput)%>% gsub("Setsim",Setsim,.)
# writeLines(y, paste0(getwd(),"/", Modeling_dir, "/",input))



 
 
 out <- sapply(datafiles, paste0, collapse=' ')
 cat(unlist(out, use.names = FALSE), sep="\n")
Simulation {     # Dose 1
  
  NRF2_spline = NDoses(49,0, 0.025, 0.184, 0.215, 0.221, 0.21, 0.186, 0.175, 0.177, 0.164, 0.18, 0.174, 0.175, 0.156, 0.146,
                       0.136, 0.116, 0.121, 0.127, 0.12, 0.118, 0.089, 0.09, 0.089, 0.106, 0.207, 0.125, 0.125, 0.118, 0.119, 0.113, 0.116, 0.113,
                       0.107, 0.098, 0.116, 0.121, 0.113, 0.109, 0.093, 0.094, 0.095, 0.097, 0.1, 0.078, 0.092, 0.1, 0.092, 0.089,
                       0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27,
                       28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48);
  
  Print (Srxn1, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29,
         30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48);
  
  
}



for (i in 1:6){
  datafiles[[i]] =(prefinal[[2]]%>% filter(.,repDose.x == order_dosing_matrix[i]) %>% filter(.,realtimeID %in% time) %>%  
          select (.,BS_Norm)%>% round(., digits = 3) %>%  paste(.,sep = ",",collapse = ", "))
  
}



#preparation of input files for different drugs

x <- readLines(paste0(getwd(),"/", Modeling_dir, "/", "defaultmodel5.1setpoint.in.R"), warn=FALSE)      
y <- x %>% gsub(pattern ="output", replace = ouput)%>% gsub("Setsim",Setsim,.)
writeLines(y, paste0(getwd(),"/", Modeling_dir, "/",input))
