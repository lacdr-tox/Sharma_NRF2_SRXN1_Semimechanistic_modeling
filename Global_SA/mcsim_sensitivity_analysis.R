#################################################################
#model start for pfos (adme), PC,Vol, blood flow


library(pksensi)
library(sensitivity)
library(pksensi)
library(mcmcr)
theme_set(theme_light())

setwd ("C:/Users/alexe/OneDrive - Universiteit Leiden/MCSim/mod")


source("C:/Users/alexe/OneDrive - Universiteit Leiden/MCSim/mod/Mcsim_source_function.R") ###read the function file directly


#End of function
####################################

mName <- "Acetylation_24_24_conti.model.R"
# in this i changed the initial value of srxn1
mName <- "Acetylation_24_24_Conti_sensitivity.model.R"

# after creating mcsim.exe file go to the directory and changed the name by deleting underscore and replacing with 
makemcsim(model = mName, dir = "modeling")


paramsens <- c("buildSrxn1Base","degradSrxn1","Km_Srxn1" ,"hill_acetylated_SRXN1",
               "Vmax_buildSrxn1","Km_NRF2_ACETYLATED_SRXN1","hill_Srxn1","r",'decay')

paramsens <- c("buildSrxn1Base","degradSrxn1","Km_Srxn1" ,"hill_acetylated_SRXN1",
               "Vmax_buildSrxn1","Km_NRF2_ACETYLATED_SRXN1","r",'decay')


parms <- c(buildSrxn1Base = 0.02262,
           degradSrxn1 = 0.01577,
           Km_Srxn1 = 107060   ,
           hill_acetylated_SRXN1 = 9.82335,
           Vmax_buildSrxn1 = 130492,
           Km_NRF2_ACETYLATED_SRXN1 = 0.01361,
           #hill_Srxn1 = 1.00412,
           r = 7.98,
           decay = 0.15)
           # r = 0.65,
           # decay = 0.3)


parm2 = parms[1:9]*(1 - 0.1)
parm3 = parms[1:9]*(1 +1.1)

parm2 = parms[1:8]*(1 - 0.1)
parm3 = parms[1:8]*(1 +1.1)

dist <- rep("Uniform", 8)
q <- rep("qunif", 9)
q.arg <-list(list(parm2[[1]], parm3[[1]]),list(parm2[[2]], parm3[[2]]),
             list(parm2[[3]], parm3[[3]]),list(parm2[[4]], parm3[[4]]),
             list(parm2[[5]], parm3[[5]]),list(parm2[[6]], parm3[[6]]),
             list(parm2[[7]], parm3[[7]]),list(parm2[[8]], parm3[[8]]) )

#,list(parm2[[9]], parm3[[9]]


# paramsens <- c("r",'decay')
# 
# parm2 = parms[1:2]*(1 - 0.1)
# parm3 = parms[1:2]*(1 +1.1)
# 
# dist <- rep("Uniform", 2)
# q <- rep("qunif", 2)
# q.arg <-list(list(parm2[[1]], parm3[[1]]),list(parm2[[2]], parm3[[2]]))
# 


conditions <- c("NRF2_spline = NDoses(33,0,0.972,1.175,1.153,1.104,1.05,1.004,0.966,0.947,1.028,0.924,0.954,0.931,0.899,0.887,0.87,0.855,0.85,0.843,0.837,0.821,0.816,0.802,0.793,0.78,0.779,0.775,0.754,0.757,0.751,0.759,0.748,0.739,
	                      0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32)")

conditions <- c("NRF2_spline = NDoses(49,0, 0.372, 1.264, 1.67, 2.193, 2.684, 3.044, 3.178, 3.157, 3.006, 2.914, 2.742, 2.658, 2.544, 2.381, 2.219,
                     2.195, 2.161, 2.091, 1.974, 1.949, 1.895, 1.883, 1.883, 1.903, 2.186, 2.22, 2.178, 2.184, 2.201, 2.134, 2.093, 2.049, 2.042, 2.015,
                     1.957, 1.928, 1.902, 1.854, 1.777, 1.699, 1.65, 1.569, 1.541, 1.496, 1.478, 1.431, 1.387, 1.353,
                     0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27,
                     28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48)");

conditions <- c("NRF2_spline = NDoses(49,0, 0.262, 1.182, 1.481, 1.627, 1.84, 1.984, 2.121, 2.146, 2.111, 2.115, 2.037, 2.02, 1.943, 1.892, 1.896,
	  1.893, 1.887, 1.828, 1.832, 1.846, 1.828, 1.814, 1.788, 1.791, 1.897, 1.936, 1.835, 1.812, 1.832, 1.801, 1.798, 1.789, 1.732, 1.731,
	  1.634, 1.615, 1.555, 1.483, 1.452, 1.382, 1.355, 1.287, 1.255, 1.228, 1.155, 1.133, 1.091, 1.05,
                           0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27,
                           28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48)");


vars <- c("Srxn1")
times <- seq(from = 0.01, to = 48, by = 1)



#generate parameter matrix
set.seed(1234)

x <- rfast99(params = paramsens, n = 1000, q = q, q.arg = q.arg, replicate = 10)

x$a

#For uncertainity analysis
set.seed(1111)
out <- solve_mcsim(mName = "Acetylation_24_24_conti.model.R", params = paramsens, vars = vars,
                   monte_carlo = 1000, dist = dist, q.arg = q.arg, 
                   time = times, condition = conditions, 
                   rtol = 1e-7, atol = 1e-9)

pksim(out, xlab = "Time (h)", ylab = "Conc. (ug/L)", main = "Srxn1")

y <- solve_mcsim(x, mName = "Acetylation_24_24_Conti_sensitivity.model.R",
                   params = paramsens, 
                   time = times, 
                   vars = vars,
                   condition = conditions, 
                   rtol = 1e-7, atol = 1e-9)


plot(y)

check(y)

pksim(y)

heat_check(y, order = "total order", show.all = T)

heat_check(y, index = "CI", order = "total order")

plot(y, vars = vars,order = c("first order", "Interaction","total order"))


######error plot margin too large

par("mar") 
png("C:/mcsim-5.6.6/mod/t1.png",width= 7.08,height= 9.05,units="in", res= 600)
par(mar=c(1,1,1,1))
#Visualization and decision
plot(out)
dev.off()
plot(out, vars = "cplasma",order = c("first order", "interaction", "total order"))

png("C:/mcsim-5.6.6/mod/t_si1.png",width= 7.08,height= 9.05,units="in", res= 600)
heat_check(out, order = "first order", show.all = T)
dev.off()
heat_check(out, order = "total order", show.all = T)


heat_check(out, index = "CI", order = "total order")
heat_check(out, index = "CI", order = "first order")




solve_mcsim <- function(x,
                        mName,
                        infile.name = NULL,
                        outfile.name = NULL,
                        setpoint.name = NULL,
                        params = NULL,
                        vars = NULL,
                        time = NULL,
                        condition = NULL,
                        generate.infile = T,
                        tell = T,
                        rtol = 1e-06,
                        atol = 1e-06,
                        monte_carlo = NULL,
                        dist = NULL,
                        q.arg = NULL,
                        parallel = FALSE) {
  
  message(paste0("Starting time: ", Sys.time()))
  
  if(is.null(infile.name)) infile.name <- "sim.in"
  if(is.null(outfile.name)) outfile.name <- "simmc.out"
  
  if(generate.infile == T){
    if (is.null(monte_carlo)){
      generate_infile(infile.name = infile.name,
                      outfile.name = outfile.name,
                      params = params,
                      vars = vars,
                      time = time,
                      rtol = rtol, atol = atol,
                      condition = condition)
    } else { # must be Monte Carlo
      generate_infile(infile.name = infile.name,
                      outfile.name = outfile.name,
                      params = params,
                      vars = vars,
                      time = time,
                      rtol = rtol, atol = atol,
                      condition = condition,
                      monte_carlo = monte_carlo, dist = dist, q.arg = q.arg)
    }
  }
  
  if(is.null(condition) && is.null(setpoint.name) && is.null(monte_carlo)){
    stop("Please assign the setpoint.name (parameter matrix defined in input file)")
  }
  
  if(!is.null(condition)){
    setpoint.data <- "setpts.out"
  } else setpoint.data <- setpoint.name
  
  mStr <- strsplit(mName, "/")
  mName <- mStr[[1]][length(mStr[[1]])]
  if (.Platform$OS.type == "unix"){
    mcsim. <- paste0("mcsim.", mName)
  } else if (.Platform$OS.type == "windows"){ # Windows user but not MCSim under R
    mcsim. <- paste0("mcsim.", mName, ".model.exe")
  }
  
  if(length(mStr[[1]]) != 1) mdir <- paste(mStr[[1]][-length(mStr[[1]])], collapse = "/")
  if(exists("mdir")) mcsim. <- paste0(mdir, "/",mcsim.)
  
  if(file.exists(mcsim.) == F){ # Design for MCSim under R
    mcsim. <- paste0("mcsim.", mName, ".exe")
    if(file.exists(mcsim.) == F){
      stop(paste0("The ", mcsim., " doesn't exist."))
    }
  }
  
  #
  if (is.numeric(monte_carlo)){
    n.sample <- monte_carlo
  } else if (!is.null(x$s)){
    n.sample <- length(x$s)
  }
  
  if (!is.null(params)){
    n.factors <- length(params)
  } else if (!is.null(x$factors)){
    n.factors <- ifelse(class(x$factors) == "character", length(x$factors), x$factors)
  }
  
  n.time <- ifelse(is.null(time), 1, length(time))
  n.vars <- length(vars)
  
  #
  if (is.null(monte_carlo)){ # Remember to define n if used external parameter matrix
    X <- cbind(1, apply(x$a, 3L, c))
    
    if (parallel) {
      cores <- 4
      for (i in 1:cores) {
        # input data
        suppressMessages(data.table::fwrite(X[(1+(i-1)*nrow(X)/cores):(i*nrow(X)/cores),],
                                            file = paste0("p", i, ".", setpoint.data), row.names=F, sep="\t"))
        # input file
        tx  <- readLines(infile.name)
        tx2 <- gsub(pattern = setpoint.data, replacement = paste0("p", i, ".", setpoint.data), x = tx)
        tx3 <- gsub(pattern = outfile.name, replacement = paste0("p", i, ".", outfile.name), x = tx2)
        writeLines(tx3, con = paste0("p", i, ".", infile.name))
      }
    } else suppressMessages(data.table::fwrite(X, file=setpoint.data, row.names=F, sep="\t"))
  }
  
  
  if(file.exists(mcsim.) == T){
    if(length(mStr[[1]]) == 1){
      
      message(paste0("Execute: ", "./", mcsim., " ", infile.name))
      if (parallel) {
        cl <- makeCluster(cores)
        registerDoParallel(cl)
        out <- foreach(i = 1:cores) %dopar%
          {
            system( paste0("./", mcsim., " ", paste0("p", i, ".", infile.name)) )
            file.remove(paste0("p", i, ".", infile.name))
            file.remove(paste0("p", i, ".", setpoint.data))
          }
        stopCluster(cl)
      } else {
        system(paste0("./", mcsim., " ", infile.name))
      }
    } else {
      
      message(paste0("Execute: ", mcsim., " ", infile.name))
      if (parallel) {
        cl <- makeCluster(cores)
        registerDoParallel(cl)
        out <- foreach(i = 1:cores) %dopar%
          {
            system( paste0(mcsim., " ", paste0("p", i, ".", infile.name)) )
            file.remove(paste0("p", i, ".", infile.name))
            file.remove(paste0("p", i, ".", setpoint.data))
          }
        stopCluster(cl)
      } else {
        system(paste0(mcsim., " ", infile.name))
        
      }
    }
  }
  
  if (parallel) {
    p.list <- list()
    for (i in 1:cores) {
      p.list[[i]] <- data.table::fread(paste0("p", i, ".", outfile.name), head = T)
      file.remove(paste0("p", i, ".", outfile.name))
    }
  }
  
  if (is.null(monte_carlo)){rm(X)}
  
  invisible(gc()); # clean memory
  
  str <- n.factors + 2
  
  if (parallel) {
    df <- as.data.frame( do.call(rbind, list(p.list[[1]], p.list[[2]], p.list[[3]], p.list[[4]])) )
  } else df <- as.data.frame(data.table::fread(outfile.name, head = T))
  
  invisible(gc()); # clean memory
  
  y <- as.matrix(df[,str:ncol(df)]) # output only
  
  rm(df)
  invisible(gc()); # clean memory
  
  #  if (!is.null(x)){
  #    n.rep <- x$rep
  #  } else {
  n.rep <- nrow(y) / (n.sample * n.factors)
  #  }
  
  if (nrow(y) == n.sample){ # For Monte Carlo
    dim <- c(n.sample, 1, n.time, n.vars)
  } else  dim <- c(n.sample * n.factors, n.rep, n.time, n.vars)
  
  dim(y)<- dim
  
  invisible(gc()); # clean memory
  
  if (length(time) == 1 && length(vars) == 1) {
    dimnames(y)[[3]] <- list(time)
    dimnames(y)[[4]] <- list(vars)
  } else if (length(time) == 1 && length(vars) > 1){
    dimnames(y)[[3]] <- list(time)
    dimnames(y)[[4]] <- vars
  } else {
    dimnames(y)[[3]] <- time
    dimnames(y)[[4]] <- vars
  }
  
  if (is.null(monte_carlo) && tell == T){
    tell2(x, y)
  }
  
  #file.remove(setpoint.data)
  message(paste0("Ending time: ", Sys.time()))
  
  if (is.null(monte_carlo) && tell == T){
    return(x)
  } else return(y)
}

#' @export
#' @describeIn solve_mcsim Generate the \pkg{GNU MCSim} input file.
generate_infile <- function(infile.name = NULL,
                            outfile.name = NULL,
                            params, vars, time,
                            condition, rtol = 1e-6, atol = 1e-6,
                            monte_carlo = NULL, dist = NULL, q.arg = NULL){ # Monte Carlo
  
  if(is.null(infile.name)) infile.name <- "sim.in"
  if(is.null(outfile.name)) outfile.name <- "simmc.out"
  setpoint.data <- "setpts.out"
  
  #if(file.exists(paste0(infile.name)) == T){
  #  if(menu(c("Yes", "No"),
  #          title=paste('The "', infile.name, '" is exist. Do you want to replace it?', sep ="")) == 2){
  #    stop()
  #  }
  #}
  
  cat("#---------------------------------------- \n#",
      " ", infile.name , "\n#",
      " (Created by generate_infile)\n#",
      "----------------------------------------", "\n\n",
      file = infile.name, sep = "")
  cat("Integrate (Lsodes, ", rtol, ", ", atol, " , 1);", "\n\n", file=infile.name, append=TRUE, sep="")
  if(is.null(monte_carlo)){
    cat("SetPoints (", "\n",
        "\"", outfile.name, "\", \n\"", setpoint.data, "\",\n",
        "0, ", "\n",
        paste(params, collapse = ", "),");\n\n",
        file = infile.name, append = TRUE, sep = "")
  } else {
    cat("MonteCarlo (", "\"", outfile.name, "\"", ",", monte_carlo , ",", sample(1:99999, 1), ");\n\n",
        file = infile.name, append = TRUE, sep = "")
    for (i in 1 : length(params)){
      cat("Distrib ( ", params[i], ",", dist[i], ",", paste(unlist(q.arg[i]), collapse = ","), ");", "\n",
          file = infile.name, append=TRUE, sep = "")
    }
  }
  cat("\n#---------------------------------------- \n#",
      " Simulation scenario\n#",
      "----------------------------------------", "\n\n",
      file = infile.name, append = TRUE, sep = "")
  cat("Simulation {", "\n\n", file = infile.name, append = TRUE)
  # cat(paste(conditions, collapse=";"), ";", "\n\n", file = infile.name, append=TRUE, sep = "")
  for (i in 1 : length(condition)){
    cat(paste(condition[i], collapse = ";"), ";", "\n", file = infile.name, append = TRUE, sep = "")
  }
  cat("\n", file = infile.name, append = TRUE)
  for (i in 1 : length(vars)) {
    cat("Print (", paste(vars[i], collapse = ", "), ", ", paste(time, collapse=", "), ");\n",
        file = infile.name, append=TRUE, sep = "")
  }
  cat("}", "END.\n", file = infile.name, append = TRUE)
  message(paste('* Created input file "', infile.name, '".', sep =""))
}
