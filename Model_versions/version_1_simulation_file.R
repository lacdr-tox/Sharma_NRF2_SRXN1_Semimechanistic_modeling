rm(list = ls())

#setwd('C:/Users/User/mcsim-6.2.0/mod')

setwd('C:/Users/alexe/OneDrive - Universiteit Leiden/MCSim/mod')


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
#install.packages(doSNOW)
# library(doSNOW)
# install.packages(c("iterators","parallel"))
theme_set(theme_light())
makemcsim <- function(model, deSolve = F, dir = "modeling"){
  exe_file <- paste0("mcsim_", model, ".exe")
  #if(file.exists(exe_file)) stop(paste0("* '", exe_file, "' had been created."))
  
  if(file.exists("modeling") == F){
    dir.create("modeling")
  }
  
  if(file.exists(model)) { # Move model file from working directory to modeling folder
    invisible(file.copy(from = paste0(getwd(),"/", model), to = paste0(getwd(),"/modeling/", model)))
    invisible(file.remove(model))
    message(paste0("* The '", model, "' has been moved to modeling folder."))
  }
  
  if (deSolve == T){
    system(paste("./MCSim/mod.exe -R ", dir, "/", model, " ", model, ".c", sep = "")) 
    system (paste0("R CMD SHLIB ", model, ".c")) # create *.dll files
    dyn.load(paste(model, .Platform$dynlib.ext, sep="")) # load *.dll
    source(paste0(model,"_inits.R"))
  } else {
    # system(paste("./MCSim/mod/mod.exe ", dir, "/", model, " ", model, ".c", sep = ""))
    # system(paste("gcc -O3 -I.. -I./MCSim/sim -o mcsim.", model, ".exe ", model, ".c ./MCSim/sim/*.c -lm ", sep = ""))
    system(paste("../mod/mod.exe ",dir, "/", model, " ", model, ".c", sep = ""))
    system(paste("gcc -O3 -I.. -I../sim -o mcsim_", model, ".exe ", model, ".c ../sim/*.c -lm ", sep = ""))
    invisible(file.remove(paste0(model, ".c")))
    if(file.exists(exe_file)) message(paste0("* Created executable program '", exe_file, "'.")) 
  }
}


mcsim <- function(model, input, dir = "modeling", parallel = F){
  
  if(file.exists("modeling") == F){
    dir.create("modeling")
  }
  
  exc = paste0("mcsim_", model, ".exe")
  if (file.exists(exc) == F) {
    makemcsim(model, dir = dir)
    if (file.exists(exc) == F) {
      stop("* '", exc, "' is not exist .")
    }
  }
  
  if(file.exists(input)) {
    invisible(file.copy(from = paste0(getwd(),"/", input), to = paste0(getwd(),"/modeling/", input)))
    invisible(file.remove(input))
  }
  
  tx  <- readLines(paste0(dir, "/", input))
  MCMC_line <- grep("MCMC \\(", x=tx)
  MonteCarlo_line <- grep("MonteCarlo \\(", x=tx)
  SetPoints_line <- grep("SetPoints \\(", x=tx)
  
  if (length(MCMC_line) != 0){
    #file_defore <- list.files()
    RandomSeed <- exp(runif(1, min = 0, max = log(2147483646.0)))
    tx2 <- gsub(pattern = "10101010", replace = paste(RandomSeed), x = tx)
    checkfile <- "MCMC.check.out"
    
    if (parallel == T){ 
      i <- sample(1111:9999, 1)
      name <- gsub("\\..*", "", input)
      mcmc_input <- paste0(name, "_", i, ".in")
      mcmc_output <- paste0(name, "_", i, ".out")
      tx3 <- gsub(pattern = "MCMC.default.out", replace = mcmc_output, x = tx2)
      writeLines(tx3, con = mcmc_input)
      if(file.exists(checkfile)){
        file.remove(checkfile)
      }
      
      #system(paste("./mcsim.", model, ".exe ", mcmc_input, sep = ""))
      system(paste("./mcsim_", model, ".exe ", mcmc_input, sep = ""))
      
    } else{ 
      tmp <- "tmp_mcmc.in.R"
      writeLines(tx, con=paste0(dir, "/", input))
      writeLines(tx2, con=paste0(dir, "/", tmp))
      # system(paste("./mcsim.", model, ".exe ", dir, "/", tmp, sep = ""))
      system(paste("./mcsim_", model, ".exe ", tmp, sep = ""))
      outfile <- "MCMC.default.out"
      tx2 <- gsub(pattern = ",0,", replace = ",1,", x = tx)
      tx3 <- gsub(pattern = paste0("\"", outfile, "\",\"\""), 
                  replace = paste0("\"", checkfile, "\",\"", outfile, "\""), 
                  x = tx2)
      writeLines(tx3, con=paste0(dir, "/", tmp))
      
      #system(paste("./mcsim.", model, ".exe ", dir, "/", tmp, sep = ""))
      system(paste("./mcsim_", model, ".exe ", dir, "/", tmp, sep = ""))
      file.remove(paste0(dir, "/", tmp))
    }
    
    if(file.exists(checkfile)){
      message(paste0("* Create '", checkfile, "' from the last iteration."))
    }
    
    if (parallel == T){ 
      df <- read.delim(mcmc_output)
    } else {
      df <- read.delim("MCMC.default.out")
    }
    
  } else if (length(MonteCarlo_line) != 0){
    RandomSeed <- runif(1, 0, 2147483646)
    tx2 <- gsub(pattern = "10101010", replace = paste(RandomSeed), x = tx)
    writeLines(tx2, con=paste0(dir, "/", input))
    #message(paste("Execute:", " ./mcsim.", model, ".exe ", dir, "/", input, sep = ""))
    message(paste("Execute:", " ./mcsim_", model, ".exe ", dir, "/", input, sep = ""))
    # system(paste("./mcsim.", model, ".exe ", dir, "/", input, sep = ""))
    system(paste("./mcsim_", model, ".exe ", dir, "/", input, sep = ""))
    writeLines(tx, con=paste0(dir, "/", input))
    df <- read.delim("simmc.out")
  } else if (length(SetPoints_line) != 0){
    # message(paste("Execute:", " ./mcsim.", model, ".exe ", dir, "/", input, sep = ""))
    # system(paste("./mcsim.", model, ".exe ", dir, "/", input, sep = ""))
    message(paste("Execute:", " ./mcsim_", model, ".exe ", dir, "/", input, sep = ""))
    system(paste("./mcsim_", model, ".exe ", dir, "/", input, sep = ""))
    df <- read.delim("simmc.out")
  } else {
    # message(paste("Execute:", " ./mcsim.", model, ".exe ", dir, "/", input, sep = ""))
    # system(paste("./mcsim.", model, ".exe ", dir, "/", input, sep = ""))
    message(paste("Execute:", " ./mcsim_", model, ".exe ", dir, "/", input, sep = ""))
    system(paste("./mcsim_", model, ".exe ", dir, "/", input, sep = ""))
    df <- read.delim("sim.out", skip = 1)
  }
  return(df)
}


clear <- function(){
  files <- c(dir(pattern = c("*.out")),
             dir(pattern = c("sim.in")),
             dir(pattern = c("*.R.exe")),
             dir(pattern = c("*.R.so")),
             dir(pattern = c("*.R.o")),
             dir(pattern = c("*.R.dll")),
             dir(pattern = c("*.R.c")),
             dir(pattern = c("*.R_inits.R")),
             dir(pattern = c("*.perks")))
  invisible(file.remove(files))
}

report <- function(){
  cat("\n\n-----Report started line-----\n\n")
  cat(Sys.getenv("PATH"), "\n")
  print(Sys.which("gcc"))
  system('gcc -v')
}

readsims <- function(x, exp = 1){
  ncols <- ncol(x)
  index <- which(x[,1] == "Time")
  str <- ifelse(exp == 1, 1, index[exp-1]+1)
  end <- ifelse(exp == length(index)+1, nrow(x), index[exp]-2)
  X <- x[c(str:end),]
  ncolX <- ncol(X) 
  X <- as.data.frame(matrix(as.numeric(as.matrix(X)), ncol = ncolX))
  if (exp > 1) names(X) <- as.matrix(x[index[exp-1],])[1:ncols] else names(X) <- names(x)
  X <- X[, colSums(is.na(X)) != nrow(X)]
  return(X)  
}

mcmc_array <- function(data, start_sampling = 0){
  n_chains <- length(data)
  sample_number <- dim(data[[1]])[1] - start_sampling
  dim <- c(sample_number, n_chains, dim(data[[1]])[2])
  n_iter <- dim(data[[1]])[1]
  n_param <- dim(data[[1]])[2]
  x <- array(sample_number:(n_iter * n_chains * n_param), dim = dim)
  for (i in 1:n_chains) {
    x[, i, ] <- as.matrix(data[[i]][(start_sampling + 1):n_iter, ])
  }
  dimnames(x)[[3]] <- names(data[[1]])
  x
}


#End of function
####################################
# version 1 where just to check whether the model version 1 structure would be suffice to explain the Srxn1 data
# first only test with the sulforaphane data leaving other chemical out. 

model <- "version_1_NLHM.model.R"
input <- "version_1_NLHM.in.R"

input <- "version_1_NLHM_hierar.in.R"
detectCores()
cores <- 4
cl <- makeCluster(cores)
registerDoParallel(cl)

makemcsim(model = model, dir = "modeling")

#mcsim(model = model, input = input, dir = "modeling")
# Parallel computing
strt<-Sys.time()
system.time( 
  out <- foreach(i = 1:2) %dopar% { mcsim(model = model, input = input, dir = "modeling", parallel = T)  }
)
print(Sys.time()-strt)

stopCluster(cl) 

dim(out)

sims = mcmc_array(data = out,start_sampling = 0)
parms_name_1 <- c("buildSrxn1Base.1." , "degradSrxn1.1.",   "Vmax_buildSrxn1.1.", "Km.1." ,"hill.1.")


str <- ceiling(nrow(sims)/2) + 1
end <- nrow(sims)
j <- c(str:end)
length(j)
mcmc_trace(sims[j,,], pars = parms_name_1)
color_scheme_set("mix-blue-red")
mcmc_trace(sims[j,,], pars = parms_name_1) #, facet_args = list(ncol = 1, strip.position = "left"))


mcmc_dens_overlay(x = sims[j,,], pars = parms_name_1)
mcmc_dens_overlay(x = sims[j,,], pars = parms_name)

mcmc_dens_overlay(x = sims[j,,], pars = "LnData")


png(file="pair.png",
    width=3000, height=950)

mcmc_pairs(sims[j,,], pars = parms_name_1, off_diag_fun = "hex")

dev.off()

cor((sims[j,,]))

mcmc_rank_hist(sims, pars = parms_name_1)

mcmc_rank_overlay(sims, pars = parms_name)

monitor(sims[,,parms_name_1], digit=4)

monitor_file = (monitor(sims[,,parms_name_1], digit=6)) 

rhats <- rhat(monitor_file)

dataf$Rhat

color_scheme_set("brightblue") # see help("color_scheme_set")
mcmc_rhat(monitor_file["Rhat", ])
mcmc_rhat(monitor_file["Rhat", ]) + yaxis_text(hjust = 1)
typeof(monitor_file)
dataf = as.data.frame(monitor_file)
str(monitor_file)
write.csv(dataf[,c("mean","sd","2.5%","97.5%","Rhat")], file = "rhat_heirarchical_3.csv")
str(sims[j,,])



setwd('C:/Users/alexe/OneDrive - Universiteit Leiden/MCSim/mod')

# now 3 chain change it to 4 chains

X1 <- sims[j,,] %>% matrix(nrow = 5000*2)   
head(X1)
write.table(X1, file = "setpts.out", row.names = F, sep = "\t")

X_setpts <- mcsim("Nrf2_Srxn1_conti.model.R", "BS_Nrf2_Srxn1_hierarchical_setspoint.in.R")

head(X_setpts)

length(X_setpts)

tail(X_setpts, 1)

X_params <- apply(X_setpts[, 2:12], 2, quantile,  c(0.5, 0.025, 0.975)) %>% t()

X_params_mean <- apply(X_setpts[, 2:12], 2, mean) %>% t()    # can apply for all the doses

vars <- names(X_setpts)
index <- which(vars == "Nrf2_1.1" | vars ==  "Nrf2_6.49")
X <- apply(X_setpts[index[1]:index[2]], 2, quantile,  c(0.5, 0.025, 0.975)) %>% t()
length(X)
x = data.frame(X)
glimpse(x)
x$Simulation = (rep(paste0(rep("Simu",6),1:6),each = 49))
x$Chemical = rep(paste0(c("Sulf")),each = 49*6)      #,"Andro","ETA"
Times <- rep(seq(0, 48, 1),times = 6)
x$Times <- Times
colnames(x) <- c("median", "LCL", "UCL","Simulations","Chemicals","Times")

head(x,50)

str(x)


x %>% 
  ggplot(aes(x = Times, y = median)) +
  geom_ribbon(aes(ymin = LCL, ymax = UCL), fill = "green", alpha = 0.5)+
  geom_line() +
  # geom_errorbar(aes(ymin=SRXN1_observed-SRXN1_SD, ymax= SRXN1_observed +SRXN1_SD), size=0.5,   
  #               width=.25) + 
  facet_grid(Simulations~Chemicals) +
  #scale_y_log10() + 
  labs(x = "Time (hr)", y = "Concentration ", title = "SRXN1 vs Time_continous scenario") 


