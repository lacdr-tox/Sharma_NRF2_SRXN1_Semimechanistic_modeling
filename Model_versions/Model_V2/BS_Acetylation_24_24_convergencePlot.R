rm(list = ls())

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
    message(paste("Execute:", " ./mcsim_", model, " ", dir, "/", input, sep = ""))
    system(paste("./mcsim_", model, " ", dir, "/", input, sep = ""))
    df <- read.delim("simmc.out")
  } else {
    message(paste("Execute:", " ./mcsim_", model, " ", dir, "/", input, sep = ""))
    system(paste("./mcsim_", model, " ", dir, "/", input, sep = ""))
    df <- read.delim("sim.out", skip = 1)
  }
  return(df)
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

report <- function() {
  cat("\n\n-----Report started line-----\n\n")
  cat(Sys.getenv("PATH"), "\n")
  print(Sys.which("gcc"))
  system('gcc -v')
}

readsims <- function(x, exp = 1) {
  ncols <- ncol(x)
  index <- which(x[, 1] == "Time")
  str <- ifelse(exp == 1, 1, index[exp - 1] + 1)
  end <- ifelse(exp == length(index) + 1, nrow(x), index[exp] - 2)
  X <- x[c(str:end), ]
  ncolX <- ncol(X)
  X <- as.data.frame(matrix(as.numeric(as.matrix(X)), ncol = ncolX))
  if (exp > 1)
    names(X) <-
    as.matrix(x[index[exp - 1], ])[1:ncols]
  else
    names(X) <- names(x)
  X <- X[, colSums(is.na(X)) != nrow(X)]
  return(X)
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
length(j)
color_scheme_set("mix-blue-red")
mcmc_trace(sims[j,,], pars = parms_name_1) #, facet_args = list(ncol = 1, strip.position = "left"))

monitor(sims[,,parms_name_1], digit=4)

monitor_file = (monitor(sims[,,parms_name_1], digit=4)) 

pdf(paste0(getwd(),"/",Modeling_dir,"/Model_convergence" ,".pdf"),width = 15, height = 8 ,onefile = T)
color_scheme_set("mix-blue-red")
mcmc_trace(sims[j,,], pars = parms_name_1) #, facet_args = list(ncol = 1, strip.position = "left"))
dev.off()
monitor(sims[,,parms_name_1], digit=4)

monitor_file = data.frame((monitor(sims[,,parms_name_1], digit=4))) 


dataf = as.data.frame((monitor_file))
str(dataf)
typeof(dataf)
rhats <- rhat(dataf)
color_scheme_set("brightblue") # see help("color_scheme_set")
rhats <- rhat(monitor_file)
file = dataf[,"Rhat",]
names(file) = parms_name_1
mcmc_rhat(file)
pdf(paste0(getwd(),"/",Modeling_dir,"/Model_rhat" ,".pdf"))
mcmc_rhat(file) + yaxis_text(hjust = 1)
dev.off()

str(monitor_file)
write.csv(dataf[,c("mean","sd","2.5%","97.5%","Rhat")], file = paste0(getwd(),"/",Modeling_dir, "/Nrf2_input_Srxn1_final.csv"))
