library(tidyverse)
library(rstan)
library(StanHeaders)
library(bayesplot)
library(corrplot)
library(sensitivity)
library(pksensi)
library(foreach)     
library(doParallel) 
library(iterators)
library(parallel)
install.packages(doSNOW)
library(doSNOW)
theme_set(theme_light())
getwd()

source("~/Functions_folder/genericsetfileinputfun.R")  #generat setpoint in file
source("~/Functions_folder/Exp_data_readingfun.R")    # read exp data file
source("~/Functions_folder/MCSIMfun.R")    # read exp data file
figdirec = "~/MCSim/mod/Model2plot_repeated48"
setwd("~/MCSim/mod")
z = 4
Modeling_dir = "Model1_Nrf2input"
treatment1 = c('Sulforaphane',"Andrographolide","Ethacrynic Acid","CDDO-me")
condition1 = c("R_8_24","R_24_24","Conti_32", "Conti_48")   # all the scenarios
Drug = treatment1[z]   # this is important
Scenario = condition1[1]

file_number = ifelse(Drug == 'Sulforaphane',1,
                     ifelse(Drug == 'Andrographolide',2,
                            ifelse(Drug == 'Ethacrynic Acid',3,
                                   ifelse(Drug == "CDDO-me",4,print("Error in file name")))))
drug = c("Sul","Andr", "ETA", "CDDO")
drugs = drug[file_number]

Exp =  ifelse(Scenario %in% c("R_24_24",'R_8_24'),"Rep",
              ifelse(Scenario  %in% c("Conti_48",'Conti_32'),"Conti","NA"))

Experiments =  ifelse(Scenario == 'R_8_24',"32h(8h +24h)Repeated Exposure",ifelse(Scenario == "R_24_24","48h(24h +24h) Repeated Exposure", 
               ifelse(Scenario == "Conti_32"," 32h Continous Exposure",ifelse(Scenario == "Conti_48"," 48h Continous Exposure","NA"))))

              
totalsim =  ifelse(Scenario %in% c("R_24_24",'R_8_24'),48,
              ifelse(Scenario  %in% c("Conti_48",'Conti_32'),6,"NA"))

end_time =  ifelse(Scenario %in% c("R_24_24","Conti_48"),48,
                   ifelse(Scenario %in% c('R_8_24','Conti_32'),32,"NA"))

Toxicdose1 = c(35, 100, 100, 1)

#For 24 +24 repeated
r2 = c(2.186,2.14,0.03,2.06)         # in the order of SUl, ANdr, Etha, CDOO
decay2 = c(0.125,0.125,0.084,0.07)   # in the order of SUl, ANdr, Etha, CDOO

repeatedTime = ifelse(Scenario  == "R_24_24",24.01,
                      ifelse(Scenario== "R_8_24",8.01, "NA"))               # change it to 8.01 in the case of 8+24 h scenarios
Toxicdose = Toxicdose1[file_number]
Parms = c("r2","decay2","repeatedTime")
value = c(r2[file_number],decay2[file_number],repeatedTime[1])

model = ifelse(Scenario %in% c("R_24_24",'R_8_24'),"Repeated_BS_Acetylation_24_24.model.R",
               ifelse(Scenario  %in% c("Conti_48",'Conti_32'),"BS_Acetylation_24_24.model.R","NA"))


# filenames <- list.files(path = paste0(getwd(), "/",Modeling_dir),pattern = "^BS_Acetylation.*\\.out$")
# myfiles = lapply(paste0(getwd(), "/",Modeling_dir,"/",filenames), read.delim, sep = "")
# names(myfiles) <- filenames
# out_final = myfiles
# endparms = length(names(out_final[[1]]))-3
# parms_name_1 <- paste((names(out_final[[1]])[2:endparms]),sep = ",")
# sims <- mcmc_array(list(out_final[[2]],out_final[[4]],
#                              out_final[[3]],out_final[[6]]))
# 
# str <- ceiling(nrow(sims)/2) + 1
# end <- nrow(sims)
# j <- c(str:end)
# length(j)
# color_scheme_set("mix-blue-red")
# mcmc_trace(sims[j,,], pars = parms_name_1) #, facet_args = list(ncol = 1, strip.position = "left"))
# monitor(sims[,,parms_name_1], digit=4)
# 
# monitor_file = (monitor(sims[,,parms_name_1], digit=4))
# 
# X <- sims[j,,] %>% matrix(nrow = 5000*4)
# parms_name_2 <- paste((names(out_final[[2]])),sep = ",")
# colnames(X) = parms_name_2
# 
# write.table(X, file = paste0(getwd(),"/",Modeling_dir, "/", Modeling_dir,"posterior_hierar.out"), row.names = F, sep = "\t")

posteriordisfile <- read.delim(paste0(getwd(),"/",Modeling_dir, "/", Modeling_dir,"posterior_hierar.out"), header = TRUE, sep = "")

params = data.frame(colnames(posteriordisfile))
colnames(params) = "parametersname"
file = params %>% separate(col = parametersname, c("Parms", "Subj", "levels"),sep = "([.])",fill = "right")
level = ifelse(drugs == "Sul",1, ifelse(drugs == "Andr",2,ifelse(drugs == 'ETA',3,ifelse(drugs == "CDDO", 4, "NA"))))
a = file %>% filter(Subj ==1)
b = file %>% filter(Subj ==1 & levels == "")
c = file %>% filter(Subj ==1&levels ==level)
c$levels = paste0(c$levels, ".")  #to add a dot as it is required
d = c(Parmshier = "iter",Parms = "iter", Subj = "", levels = " ")
e = c(Parmshier = "LnPrior",Parms = "LnPrior", Subj = "", levels = " ")
f = c(Parmshier = "LnData",Parms = "LnData", Subj = "", levels = " ")
g = c(Parmshier = "LnPosterior",Parms = "LnPosterior", Subj = "", levels = " ")

file1 = rbind(c,b)
file2 =distinct(file1,Parms,Subj, .keep_all= TRUE) %>% unite(.,"parmshier", Parms:levels, sep= ".", remove = FALSE) %>% rbind(d,.,e,f,g)
filename = file2$parmshier  #


######
# for plotting disable it as you have little bit different Andrsetsimout and etasimout which is exist in modelNrf2inputfolder
randomdata = sample_n(posteriordisfile, 5000)

# setsim is same for all other chemicals here in the case of model version1a
# so remove the drugs
hier_setpoint = randomdata %>% select(filename) %>% write.table(., file = paste0(getwd(),"/", Modeling_dir,"/",drugs,Modeling_dir,Exp,end_time,"setSim", ".out"), row.names = F, sep = "\t")
filesetout <- read.delim(paste0(getwd(),"/", Modeling_dir,"/",drugs,Modeling_dir,Exp,end_time,"setSim", ".out"), header = TRUE, sep = "")
#check if names are matching or not
colnames(filesetout)

#setsim is not same for all other chemicals as this is the case of heirarchical methods
# so remove the drugs 

# Setsim = paste0(drugs,Modeling_dir,Exp,end_time,"setSim", ".out")

#only used when you perform only simulation

Setsim = ifelse(drugs == "Andr", "AndrModel1_Nrf2inputRep48setSim.out", 
                ifelse(drugs == "ETA", "ETAModel1_Nrf2inputRep48setSim.out",
                       paste0(drugs,Modeling_dir,Exp,end_time,"setSim", ".out")))

ouput = paste0(drugs, Modeling_dir,Exp,end_time,"set", ".out")  #check this output file name if it is the same or not

paramsSet = paste(file2$Parms[-c(1,length(file2$Parms)-2,(length(file2$Parms)-1), length(file2$Parms))], collapse = ",") 

###
#gsub(",([A-Za-z])", ", \\1", .)  #to add a space after comma 

setsimparm = paste0( 'SetPoints ','(', '"', ouput , '"',',','"', Setsim ,'"', ',' , 0 , ',', paramsSet,')',';') %>% gsub(',([A-Za-z])', ', \\1', .)

# check the generated input file match with this or not (the same is used in that function) and check the end time
inputs = paste0(drugs, Modeling_dir,Exp,end_time,"set.in", ".R")

#Generate input files using function #Generat_simfile

Generat_simfile(Modeling_dir = "Model1_Nrf2input",   #directory where you want to keep text simulation file
                setsimparm = setsimparm,                # first line of setpoints i.e. outputfile,mcmcoutputfile, params name 
                input = inputs,                      # simulaiton file that you wanted to create
                Drug = Drug,                      #name of the drug 
                toxicdose = Toxicdose,               # dose that need to be removed 
                parms = Parms,                       # parameter that has value and fixed  #r2 and decay2 
                value = value,                       # and here provided the value in the same order as params name
                Scenario = Scenario,               #experiment scenarios
                time_steps = 1,
                exp_timestep = 1)


#################
#ifsetsimout files are not in the mod directory then you have to get them again back to mod from directory
#comment this if you are fitting and again simuating
file.copy(from = paste0(getwd(), "/",Modeling_dir,"/", Setsim),
to = paste0(getwd(), "/",Modeling_dir,"/", Setsim))
#####################
X_setpts <- mcsim(model, inputs,Setsim = Setsim)
invisible(file.remove(Setsim))    # to remove the output file
vars <- names(X_setpts)

if(Scenario %in% c("R_24_24",'R_8_24')){
   
  index1 <- which(vars == "Srxn1_1.1" | vars ==  paste0("Srxn1","_",48,".",end_time+1))
  index2 <- which(vars == "Srxn1_49.1" | vars ==  paste0("Srxn1","_",96,".",end_time+1))
  index3 <- which(vars == "Srxn1_97.1" | vars ==  paste0("Srxn1","_",144,".",end_time+1))
  Xmean <- apply(X_setpts[index1[1]:index1[2]], 2, quantile,  c(0.5, 0.025, 0.975)) %>% t()
  Xhigh <- apply(X_setpts[index2[1]:index2[2]], 2, quantile,  c(0.5, 0.025, 0.975)) %>% t()
  Xlow <- apply(X_setpts[index3[1]:index3[2]], 2, quantile,  c(0.5, 0.025, 0.975)) %>% t()

} else{


vars <- names(X_setpts)
index1 <- which(vars == "Srxn1_1.1" | vars ==  paste0("Srxn1","_",6,".",end_time+1))
index2 <- which(vars == "Srxn1_7.1" | vars ==  paste0("Srxn1","_",12,".",end_time+1))
index3 <- which(vars == "Srxn1_13.1" | vars ==  paste0("Srxn1","_",18,".",end_time+1))
Xmean <- apply(X_setpts[index1[1]:index1[2]], 2, quantile,  c(0.5, 0.025, 0.975)) %>% t()
Xhigh <- apply(X_setpts[index2[1]:index2[2]], 2, quantile,  c(0.5, 0.025, 0.975)) %>% t()
Xlow <- apply(X_setpts[index3[1]:index3[2]], 2, quantile,  c(0.5, 0.025, 0.975)) %>% t()
}
str(Xmean)
x = data.frame(Xmean)
xmean <- data.frame(Xmean)
xhigh <- data.frame(Xhigh)
xlow <- data.frame(Xlow)
file = cbind(xmean,xhigh,xlow)
colnames(file) <- c("medianM", "LCLM", "UCLM","medianH", "LCLH", "UCLH","medianL", "LCLL", "UCLL")

#############################
#prepration of dosing matrix

file$Simulation = (rep(paste0(rep("Sim",totalsim),1:totalsim),each = end_time+1*1))  # as we have 3 variables
file$Chemical = rep(paste0(drugs),each = nrow(file))
Times <- rep(seq(0, end_time, 1),length.out =nrow(file))
file$Times <- Times
file$variables = rep(paste0(c("Srxn1")),each = end_time+1,length.out =nrow(file))
write.table(file,paste0(getwd(),"/", Modeling_dir,"/",drugs, Modeling_dir,Exp,end_time,"_posteriorsimulation.txt"))
file = read.table(paste0(getwd(),"/", Modeling_dir,"/",drugs, Modeling_dir,Exp,end_time,"_posteriorsimulation.txt"))


#############
#function that read data and load 
 
Generic_data_read(Drug = Drug,  
                Scenario = Scenario,
                time_steps = 1,
                exp_timestep = 1)

data_function = function (Drug,highdose,Scenario,variable) {
  Final_matrix_exp_data = Final_matrix_exp_data %>% filter(dose1 != highdose & dose2 != highdose)
  x_final = file 
  plot_final = cbind(Final_matrix_exp_data,x_final)
  plot_final = plot_final[,!duplicated(names(plot_final))]  #to remove duplicate columns with the same names
  write.table(plot_final,paste0(getwd(),"/", Modeling_dir,"/",drugs, Exp,end_time,"_data_posteriorsimulation.txt")) 
}

data_function(Drug = Drug,      # 1 for Sul, 2 for Andro, 3 for Etha, & 4 for CDDO_me
              highdose = Toxicdose,
              Scenario = Scenario,  # 1 & 2 for repeated 8+24 & 24+24 respectively  , 2 & 4 for continous 32 and 48 respectively
              variable = "Srxn1")


plot_functionconti = function (Drug,highdose,Scenario,variable) {
  Final_matrix_exp_data = Final_matrix_exp_data %>% filter(dose1 != highdose & dose2 != highdose)
  x_final = file 
  plot_final = cbind(Final_matrix_exp_data,x_final)
  plot_final = plot_final[,!duplicated(names(plot_final))]  #to remove duplicate columns with the same names
  plot = plot_final %>% 
    ggplot() +
    geom_line(aes(x = Times, y = medianH, color = "Simulated(max)"),size=0.8) +
    geom_line(aes(x = Times, y = medianM, color="Simulated(mean)"),size=0.8) +
    geom_line(aes(x = Times, y = medianL, color="Simulated(min)"),size=0.8) +
    # geom_ribbon(aes(ymin=LCLM, ymax=UCLM, x=Times, fill = "band"), alpha = 0.5)+
    # geom_ribbon(aes(ymin=LCLH, ymax=UCLH, x=Times, fill = "band"), alpha = 0.5)+
    # geom_ribbon(aes(ymin=LCLL, ymax=UCLL, x=Times, fill = "band"), alpha = 0.5)+
    # geom_line(aes(x = Times, y = Nrf2_observed,color="Exp_mean"),linetype=2) +
    # geom_line(aes(x = Times, y = Nrf2max_observed, color="Exp_max"),linetype=2) +
    # geom_line(aes(x = Times, y = Nrf2min_observed, color="Exp_min"),linetype=2) +
    geom_point(aes(x = Times, y = Srxn1_observed), size = 0.8) +
    geom_errorbar(aes(x = Times,ymin=Srxn1_observed-Srxn1_SD, ymax= Srxn1_observed +Srxn1_SD,color = "Experiments mean(Sd)"), size=0.5,
                  width=.25)+
    facet_wrap(~dose1) +
    labs(x = "Time (hr)",y=" Srxn1 intensity [au]",
         title = paste0(drugs, " ", Experiments)) + #title = paste0(drugs,"_",Scenario,"_",variable))
    scale_color_manual(name = "Simulation type",
                       breaks = c("Simulated(max)", "Simulated(mean)","Simulated(min)", "Experiments mean(Sd)"),
                       values = c("Simulated(max)" = "red", "Simulated(mean)" = "blue", "Simulated(min)" = "green","Experiments mean(Sd)" = "black"))+
    theme(legend.title = element_blank()) + theme(legend.position="bottom") +
    theme(legend.text = element_text(size= 7,face="bold")) +
    theme(axis.text = element_text(size = 7))
  
  pdf(paste0(figdirec,"/", Drug,"_",Scenario,"variable",".pdf"),onefile=FALSE)
  print(plot)
  dev.off()
}


plot_functionRepeated = function (Drug,highdose,Scenario,variable) {
  Final_matrix_exp_data = Final_matrix_exp_data %>% filter(dose1 != highdose & dose2 != highdose)
  x_final = file 
  plot_final = cbind(Final_matrix_exp_data,x_final)
  plot_final = plot_final[,!duplicated(names(plot_final))]  #to remove duplicate columns with the same names
  head(plot_final)
  plot = plot_final %>% 
    ggplot() +
    geom_line(aes(x = Times, y = medianH, color = "Simulated(max)"),size=0.8) +
    geom_line(aes(x = Times, y = medianM, color="Simulated(mean)"),size=0.8) +
    geom_line(aes(x = Times, y = medianL, color="Simulated(min)"),size=0.8) +
    # geom_ribbon(aes(ymin=LCLM, ymax=UCLM, x=Times, fill = "band"), alpha = 0.5)+
    # geom_ribbon(aes(ymin=LCLH, ymax=UCLH, x=Times, fill = "band"), alpha = 0.5)+
    # geom_ribbon(aes(ymin=LCLL, ymax=UCLL, x=Times, fill = "band"), alpha = 0.5)+
    # geom_line(aes(x = Times, y = Nrf2_observed,color="Exp_mean"),linetype=2) +
    # geom_line(aes(x = Times, y = Nrf2max_observed, color="Exp_max"),linetype=2) +
    # geom_line(aes(x = Times, y = Nrf2min_observed, color="Exp_min"),linetype=2) +
    geom_point(aes(x = Times, y = Srxn1_observed), size = 0.8) +
    geom_errorbar(aes(x = Times,ymin=Srxn1_observed-Srxn1_SD, ymax= Srxn1_observed +Srxn1_SD,color = "Experiments mean(Sd)"), size=0.5,
                  width=.25)+
    facet_grid(dose2~dose1) +
    labs(x = "Time (hr)",y=" Srxn1 intensity [au]",
         title = paste0(drugs," ", Experiments)) + #title = paste0(drugs,"_",Scenario,"_",variable))
    scale_color_manual(name = "Simulation type",
                       breaks = c("Simulated(max)", "Simulated(mean)","Simulated(min)", "Experiments mean(Sd)"),
                       values = c("Simulated(max)" = "red", "Simulated(mean)" = "blue", "Simulated(min)" = "green","Experiments mean(Sd)" = "black"))+
    theme(legend.title = element_blank()) + theme(legend.position="bottom") +
    theme(legend.text = element_text(size= 7,face="bold")) +
    theme(axis.text = element_text(size = 7))
  
  pdf(paste0(figdirec,"/", Drug,"_",Scenario,"variable",".pdf"),onefile=FALSE)
  print(plot)
  dev.off()
}


if(Scenario %in% c("R_24_24",'R_8_24')){

  plot_functionRepeated(Drug = Drug,      # 1 for Sul, 2 for Andro, 3 for Etha, & 4 for CDDO_me
                highdose = Toxicdose,
                Scenario = Scenario,  # 1 & 2 for repeated 8+24 & 24+24 respectively  , 2 & 4 for continous 32 and 48 respectively
                variable = "Srxn1")


}else{
  plot_functionconti(Drug = Drug,      # 1 for Sul, 2 for Andro, 3 for Etha, & 4 for CDDO_me
                highdose = Toxicdose,
                Scenario = Scenario,  # 1 & 2 for repeated 8+24 & 24+24 respectively  , 2 & 4 for continous 32 and 48 respectively
                variable = "Srxn1")
  

  }
  




# #################
# plot_function = function (Drug,highdose,Scenario,variable) {
#   Final_matrix_exp_data = Final_matrix_exp_data %>% filter(dose1 != highdose & dose2 != highdose)
#   x_final = file 
#   plot_final = cbind(Final_matrix_exp_data,x_final)
#   plot_final = plot_final[,!duplicated(names(plot_final))]  #to remove duplicate columns with the same names
#   head(plot_final)
#   plot_final %>% 
#     ggplot() +
#     geom_line(aes(x = Times, y = medianM, color="Simulated_mean"),size=1) +
#     geom_line(aes(x = Times, y = medianL, color="Simulated_min"),size=1) +
#     geom_line(aes(x = Times, y = medianH, color="Simulated_max"),size=1) +
#     # geom_ribbon(aes(x = Times,ymin = LCLM, ymax = UCLM), fill = "grey70", alpha = 0.5) +
#     # geom_ribbon(aes(x = Times,ymin = LCLL, ymax = UCLL), fill = "grey70", alpha = 0.5) +
#     # geom_ribbon(aes(x = Times,ymin = LCLH, ymax = UCLH), fill = "grey70", alpha = 0.5) +
#     # geom_line(aes(x = Times, y = Nrf2_observed,color="Exp_mean"),linetype=2) +
#     # geom_line(aes(x = Times, y = Nrf2max_observed, color="Exp_max"),linetype=2) +
#     # geom_line(aes(x = Times, y = Nrf2min_observed, color="Exp_min"),linetype=2) +
#     geom_errorbar(aes(x = Times,ymin=Srxn1_observed-Srxn1_SD, ymax= Srxn1_observed +Srxn1_SD,color = "Experiments_mean(Sd)"), size=0.5,
#                   width=.25)+
#     facet_grid(dose2~dose1) + 
#     labs(x = "Time (hr)", y = "GFP expression", sec.x="First exposure (uM)",
#          sec.y="Second exposure (uM)",title = paste0(Drug,"_",Scenario,"_",variable)) +
#     scale_color_manual(name = "type",
#                        breaks = c("Simulated_mean","Simulated_min","Simulated_max", "Experiments_mean(Sd)"),
#                        values = c("Simulated_mean" = "blue","Simulated_min" = "green","Simulated_max" = "red","Experiments_mean(Sd)" = "black"))+
#     theme(legend.title = element_blank()) + theme(legend.position="bottom") +
#     theme(legend.text = element_text(size= 8,face="bold")) 
# }
# 
# 
# plot_function(Drug = Drug,      # 1 for Sul, 2 for Andro, 3 for Etha, & 4 for CDDO_me
#               highdose = Toxicdose,
#               Scenario = Scenario,  # 1 & 2 for repeated 8+24 & 24+24 respectively  , 2 & 4 for continous 32 and 48 respectively
#               variable = "Srxn1")
