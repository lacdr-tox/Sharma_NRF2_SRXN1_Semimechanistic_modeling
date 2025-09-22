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
setwd("~/MCSim/mod")

Modeling_dir = "Model1_Nrf2input"


makemcsim <- function(model,
                      deSolve = F,
                      dir = Modeling_dir) {
  exe_file <- paste0("mcsim_", model, " ")
  
  if (file.exists(Modeling_dir) == F) {
    dir.create(Modeling_dir)
  }
  
  if (file.exists(model)) {
    # Move model file from working directory to Modeling folder
    invisible(file.copy(
      from = paste0(getwd(), "/", model),
      to = paste0(getwd(), "/",Modeling_dir,"/", model)
    ))
    invisible(file.remove(model))
    message(paste0("* The '", model, "' has been moved to Modeling folder."))
  }
  
  if (deSolve == T) {
    system(paste("./MCSim/mod -R ", dir, "/", model, " ", model, ".c", sep = ""))
    system (paste0("R CMD SHLIB ", model, ".c")) # create *.dll files
    dyn.load(paste(model, .Platform$dynlib.ext, sep = "")) # load *.dll
    source(paste0(model, "_inits.R"))
  } else {
    system(paste("../mod/mod ", dir, "/", model, " ", model, ".c", sep = ""))
    system(
      paste(
        "gcc -O3 -I.. -I../sim -o mcsim_",
        model,
        " ",
        model,
        ".c ../sim/*.c -lm ",
        sep = ""
      )
    )
    invisible(file.remove(paste0(model, ".c")))
    if (file.exists(exe_file))
      message(paste0("* Created executable program '", exe_file, "'."))
  }
}


mcsim <- function(model,
                  input,
                  Setsim,  #extra take setpoint file to mod file
                  dir = Modeling_dir,
                  parallel = F) {
  if (file.exists(Modeling_dir) == F) {
    dir.create(Modeling_dir)
  }
  
  exc = paste0("mcsim_", model, "")
  if (file.exists(exc) == F) {
    makemcsim(model, dir = dir)
    if (file.exists(exc) == F) {
      stop("* '", exc, "' is not exist .")
    }
  }
  
  if (file.exists(input)) {
    invisible(file.copy(
      from = paste0(getwd(), "/", input),
      to = paste0(getwd(), "/Modeling/", input)
    ))
    invisible(file.remove(input))
  }
  
  
  tx  <- readLines(paste0(dir, "/", input))
  MCMC_line <- grep("MCMC \\(", x = tx)
  MonteCarlo_line <- grep("MonteCarlo \\(", x = tx)
  SetPoints_line <- grep("SetPoints \\(", x = tx)
  
  if (length(MCMC_line) != 0) {
    #file_defore <- list.files()
    RandomSeed <- exp(runif(1, min = 0, max = log(2147483646.0)))
    tx2 <-
      gsub(pattern = "10101010",
           replace = paste(RandomSeed),
           x = tx)
    checkfile <- "MCMC.check.out"
    
    if (file.exists(checkfile)) {
      file.remove(checkfile)
    }
    
    if (parallel == T) {
      i <- sample(1111:9999, 1)
      name <- gsub("\\..*", "", input)
      mcmc_input <- paste0(name, "_", i, ".in")
      mcmc_output <- paste0(name, "_", i, ".out")
      tx3 <-
        gsub(pattern = "MCMC.default.out",
             replace = mcmc_output,
             x = tx2)
      writeLines(tx3, con = mcmc_input)
      system(paste("./mcsim_", model, " ", mcmc_input, sep = ""))
      
    } else{
      tmp <- "tmp_mcmc.in.R"
      writeLines(tx, con = paste0(dir, "/", input))
      writeLines(tx2, con = paste0(dir, "/", tmp))
      system(paste("./mcsim_", model, " ", tmp, sep = ""))
      outfile <- "MCMC.default.out"
      tx2 <- gsub(pattern = ",0,",
                  replace = ",1,",
                  x = tx)
      tx3 <- gsub(
        pattern = paste0("\"", outfile, "\",\"\""),
        replace = paste0("\"", checkfile, "\",\"", outfile, "\""),
        x = tx2
      )
      writeLines(tx3, con = paste0(dir, "/", tmp))
      
      system(paste("./mcsim_", model, " ", dir, "/", tmp, sep = ""))
      file.remove(paste0(dir, "/", tmp))
    }
    
    if (file.exists(checkfile)) {
      message(paste0("* Create '", checkfile, "' from the last iteration."))
    }
    
    if (parallel == T) {
      df <- read.delim(mcmc_output)
    } else {
      df <- read.delim("MCMC.default.out")
    }
    
  } else if (length(MonteCarlo_line) != 0) {
    RandomSeed <- runif(1, 0, 2147483646)
    tx2 <-
      gsub(pattern = "10101010",
           replace = paste(RandomSeed),
           x = tx)
    writeLines(tx2, con = paste0(dir, "/", input))
    message(paste("Execute:", " ./mcsim_", model, " ", dir, "/", input, sep = ""))
    
    system(paste("./mcsim_", model, " ", dir, "/", input, sep = ""))
    writeLines(tx, con = paste0(dir, "/", input))
    df <- read.delim("simmc.out")
  } else if (length(SetPoints_line) != 0) {
    invisible(file.copy(
      from = paste0(getwd(), "/",Modeling_dir,"/", Setsim),
      to = paste0(getwd(), "/", Setsim)
    ))
    message(paste("Execute:", " ./mcsim_", model, " ", dir, "/", input, sep = ""))
    system(paste("./mcsim_", model, " ", dir, "/", input, sep = ""))
    # df <- read.delim("simmc.out")
    df <- read.delim(paste0(ouput))
  } else {
    message(paste("Execute:", " ./mcsim_", model, " ", dir, "/", input, sep = ""))
    system(paste("./mcsim_", model, " ", dir, "/", input, sep = ""))
    df <- read.delim(paste0(ouput), skip = 1)
  }
  return(df)
}

mcmc_array <- function(data, start_sampling = 0) {
  n_chains <- length(data)
  sample_number <- dim(data[[1]])[1] - start_sampling
  dim <- c(sample_number, n_chains, dim(data[[1]])[2])
  n_iter <- dim(data[[1]])[1]
  n_param <- dim(data[[1]])[2]
  x <- array(sample_number:(n_iter * n_chains * n_param), dim = dim)
  for (i in 1:n_chains) {
    x[, i,] <- as.matrix(data[[i]][(start_sampling + 1):n_iter,])
  }
  dimnames(x)[[3]] <- names(data[[1]])
  x
}

clear <- function() {
  files <- c(
    dir(pattern = c("*.out")),
    dir(pattern = c("sim.in")),
    dir(pattern = c("*.R")),
    dir(pattern = c("*.R.so")),
    dir(pattern = c("*.R.o")),
    dir(pattern = c("*.R.dll")),
    dir(pattern = c("*.R.c")),
    dir(pattern = c("*.R_inits.R")),
    dir(pattern = c("*.perks"))
  )
  invisible(file.remove(files))
}

colnames_para <- c("iter","ks_Srxn1","kd_Srxn1","Ka_Srxn1","hill_Srxn1",
                   "Vmax_Srxn1", "Km_Srxn1", "hillact_Srxn1","r.1.","decay.1.",
                   "r_Sul","decay_Sul","r_Andro","decay_Andro","r_Eta","decay_Eta",
                   "r_CDDO","decay_CDDO","LnPrior","LnData","LnPosterior") 

filenames <- list.files(path = paste0(getwd(), "/",Modeling_dir),pattern = "^BS_Acetylation.*\\.out$")
myfiles = lapply(paste0(getwd(), "/",Modeling_dir,"/",filenames), read.delim, sep = "")
names(myfiles) <- filenames
out_final = lapply(myfiles, setNames, colnames_para)
endparms = length(names(out_final[[1]]))-3
parms_name_1 <- paste((names(out_final[[1]])[2:endparms]),sep = ",")
out1 = list(out_final[[2]],out_final[[4]],
            out_final[[3]],out_final[[6]])

sims = mcmc_array(data = out1,start_sampling = 0)
str <- ceiling(nrow(sims)/2) + 1
end <- nrow(sims)
j <- c(str:end)

X <- sims[j,,] %>% matrix(nrow = 5000*4) 
head(X)
#####################
#model specific
####################
parms_name_2 <- paste((names(out_final[[2]])),sep = ",")

colnames(X) = parms_name_2
head(X)
write.table(X, file = paste0(getwd(),"/",Modeling_dir, "/", Modeling_dir,"posterior_hierar.out"), row.names = F, sep = "\t")

posteriordisfile <- read.delim(paste0(getwd(),"/",Modeling_dir, "/", Modeling_dir,"posterior_hierar.out"), header = TRUE, sep = "")
head(posteriordisfile)
#these are the hierarchical parameters
# "kdrug_ROS.1." ,"kdrug_unmod.1." , "k_keap1_modification.1." ,"k_keap1_unmodification.1." 
colnames(posteriordisfile)[c(1:8,11:12,19:21)]
hierSul_setpoint = posteriordisfile %>% select(1:8,11:12,19:21) %>% write.table(., file = paste0(getwd(),"/", Modeling_dir,"/","Sulall", Modeling_dir,"setSim", ".out"), row.names = F, sep = "\t")
hierAndro_setpoint = posteriordisfile %>% select(1:8,13:14,19:21) %>% write.table(., file = paste0(getwd(),"/",Modeling_dir,"/", "Andro", Modeling_dir, "setSim",".out"), row.names = F, sep = "\t")
hierEtha_setpoint = posteriordisfile %>% select(1:8,15:16,19:21)%>% write.table(., file = paste0(getwd(),"/", Modeling_dir,"/","Etha", Modeling_dir, "setSim",".out"), row.names = F, sep = "\t")
hierCDDO_setpoint = posteriordisfile %>% select(1:8,17:18,19:21)%>% write.table(., file = paste0(getwd(),"/",Modeling_dir, "/", "Cddo", Modeling_dir, "setSim",".out"), row.names = F, sep = "\t")

#####################
model <- "BS_Acetylation_24_24.model.R"
drug = c('Sul', 'Andro', 'Etha', 'Cddo')

Setsim = paste0(drug[1], Modeling_dir,"setSim", ".out")
#chemical specific inputs and outputs
input = paste0(drug[1], Modeling_dir,"set.in", ".R")  # this file will contain the chemical inputs. 
ouput = paste0(drug[1], Modeling_dir,"set", ".out")  #check this output file name if it is the same or not

#preparation of input files for different drugs

# x <- readLines(paste0(getwd(),"/", Modeling_dir, "/", "defaultmodel1setpoint.in.R"), warn=FALSE)      
# y <- x %>% gsub(pattern ="output", replace = ouput)%>% gsub("Setsim",Setsim,.)
# writeLines(y, paste0(getwd(),"/", Modeling_dir, "/",input))

X_setpts <- mcsim(model, input,Setsim = Setsim)
invisible(file.remove(Setsim))    # to remove the output file

vars <- names(X_setpts)
index1 <- which(vars == "Srxn1_1.1" | vars ==  "Srxn1_6.49")
index2 <- which(vars == "Srxn1_7.1" | vars ==  "Srxn1_12.49")
index3 <- which(vars == "Srxn1_13.1" | vars ==  "Srxn1_18.49")
Xmean <- apply(X_setpts[index1[1]:index1[2]], 2, quantile,  c(0.5, 0.025, 0.975)) %>% t()
Xhigh <- apply(X_setpts[index2[1]:index2[2]], 2, quantile,  c(0.5, 0.025, 0.975)) %>% t()
Xlow <- apply(X_setpts[index3[1]:index3[2]], 2, quantile,  c(0.5, 0.025, 0.975)) %>% t()
str(Xmean)
x = data.frame(Xmean)
xmean <- data.frame(Xmean)
xhigh <- data.frame(Xhigh)
xlow <- data.frame(Xlow)
file = cbind(xmean,xhigh,xlow)
colnames(file) <- c("medianM", "LCLM", "UCLM","medianH", "LCLH", "UCLH","medianL", "LCLL", "UCLL")
#############################
#prepration of dosing matrix

file$Simulation = (rep(paste0(rep("Sim",6),1:6),each = 49*1))  # as we have 3 variables
file$Chemical = rep(paste0("Sulf"),each = nrow(file))
Times <- rep(seq(0, 48, 1),length.out =nrow(file))
file$Times <- Times
file$variables = rep(paste0(c("Srxn1")),each = 49,length.out =nrow(file))

######
#save data
####
write.table(file,paste0(getwd(),"/", Modeling_dir,"/","Sulforaphane_posteriorsimulation.txt")) 
file = read.table(paste0(getwd(),"/", Modeling_dir,"/","Sulforaphane_posteriorsimulation.txt"))

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

Generic_data_read(Drug = Drug[1],      # 1 for Sul, 2 for Andro, 3 for Etha, & 4 for CDDO_me
                  Scenario = Scenario[4],  # 1 & 2 for repeated 8+24 & 24+24 respectively  , 2 & 4 for continous 32 and 48 respectively
                  time_steps = 1,
                  exp_timestep = 1)

Toxicdose = c(35, 100, 100, 1)

plot_function = function (Drug,highdose,Scenario,variable) {
  Final_matrix_exp_data = Final_matrix_exp_data %>% filter(dose1 != highdose & dose2 != highdose)
  x_final = file 
  plot_final = cbind(Final_matrix_exp_data,x_final)
  plot_final = plot_final[,!duplicated(names(plot_final))]  #to remove duplicate columns with the same names
  head(plot_final)
  plot_final %>% 
    ggplot() +
    geom_line(aes(x = Times, y = medianM, color="Simulated_mean"),size=1) +
    geom_line(aes(x = Times, y = medianL, color="Simulated_min"),size=1) +
    geom_line(aes(x = Times, y = medianH, color="Simulated_max"),size=1) +
    # geom_line(aes(x = Times, y = Nrf2_observed,color="Exp_mean"),linetype=2) +
    # geom_line(aes(x = Times, y = Nrf2max_observed, color="Exp_max"),linetype=2) +
    # geom_line(aes(x = Times, y = Nrf2min_observed, color="Exp_min"),linetype=2) +
    geom_errorbar(aes(x = Times,ymin=Srxn1_observed-Srxn1_SD, ymax= Srxn1_observed +Srxn1_SD,color = "Experiments_mean(Sd)"), size=0.5,
                  width=.25)+
    facet_wrap(~dose1) +
    labs(x = "Time (hr)", y = "GFP expression", sec.x="First exposure (uM)",
         sec.y="Second exposure (uM)",title = paste0(Drug,"_",Scenario,"_",variable)) +
    scale_color_manual(name = "type",
                       breaks = c("Simulated_mean","Simulated_min","Simulated_max", "Experiments_mean(Sd)"),
                       values = c("Simulated_mean" = "blue","Simulated_min" = "green","Simulated_max" = "red","Experiments_mean(Sd)" = "black"))+
    theme(legend.title = element_blank()) + theme(legend.position="bottom") +
    theme(legend.text = element_text(size= 8,face="bold")) 
}

plot_function(Drug = Drug[1],      # 1 for Sul, 2 for Andro, 3 for Etha, & 4 for CDDO_me
              highdose = Toxicdose[1],
              Scenario = Scenario[4],  # 1 & 2 for repeated 8+24 & 24+24 respectively  , 2 & 4 for continous 32 and 48 respectively
              variable = "Srxn1")
