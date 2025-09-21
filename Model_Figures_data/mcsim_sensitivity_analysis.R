#################################################################
#model start for pfos (adme), PC,Vol, blood flow


library(pksensi)
library(sensitivity)
library(pksensi)
library(mcmcr)
theme_set(theme_light())

setwd ("C:/Users/alexe/OneDrive - Universiteit Leiden/MCSim/mod")

# this function has been modified to get the mcsim. output instead of mcsim_ in line 15 and 37.
makemcsim <- function(model, deSolve = F, dir = "modeling"){
  exe_file <- paste0("mcsim.", model, ".exe")
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
    system(paste("gcc -O3 -I.. -I../sim -o mcsim.", model, ".exe ", model, ".c ../sim/*.c -lm ", sep = ""))
    invisible(file.remove(paste0(model, ".c")))
    if(file.exists(exe_file)) message(paste0("* Created executable program '", exe_file, "'.")) 
  }
}



#source("C:/Users/alexe/OneDrive - Universiteit Leiden/MCSim/mod/Mcsim_source_function.R") ###read the function file directly

####################################

makemcsim(model = mName, dir = "modeling")


paramsens <- c("buildSrxn1Base","degradSrxn1","Km_Srxn1" ,"hill_acetylated_SRXN1", 
               "Vmax_buildSrxn1","Km_NRF2_ACETYLATED_SRXN1","hill_Srxn1","r",'decay')

parms <- c(buildSrxn1Base = 0.02262,
           degradSrxn1 = 0.01577,
           Km_Srxn1 = 107060   ,
           hill_acetylated_SRXN1 = 9.82335,
           Vmax_buildSrxn1 = 130492,
           Km_NRF2_ACETYLATED_SRXN1 = 0.01361,
           hill_Srxn1 = 1.00412,
           r = 7.98,
           decay = 0.15)


parm2 = parms[1:9]*(1 - 0.1)
parm3 = parms[1:9]*(1 +1.1)

dist <- rep("Uniform", 9)
q <- rep("qunif", 9)
q.arg <-list(list(parm2[[1]], parm3[[1]]),list(parm2[[2]], parm3[[2]]),
             list(parm2[[3]], parm3[[3]]),list(parm2[[4]], parm3[[4]]),
             list(parm2[[5]], parm3[[5]]),list(parm2[[6]], parm3[[6]]),
             list(parm2[[7]], parm3[[7]]),list(parm2[[8]], parm3[[8]]),list(parm2[[9]], parm3[[9]]) )



conditions <- c("NRF2_spline = NDoses(33,0,0.972,1.175,1.153,1.104,1.05,1.004,0.966,0.947,1.028,0.924,0.954,0.931,0.899,0.887,0.87,0.855,0.85,0.843,0.837,0.821,0.816,0.802,0.793,0.78,0.779,0.775,0.754,0.757,0.751,0.759,0.748,0.739,
	                      0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32)")
#SUL high dose 
conditions1 <- c("NRF2_spline = NDoses(49,0, 0.372, 1.264, 1.67, 2.193, 2.684, 3.044, 3.178, 3.157, 3.006, 2.914, 2.742, 2.658, 2.544, 2.381, 2.219,
                     2.195, 2.161, 2.091, 1.974, 1.949, 1.895, 1.883, 1.883, 1.903, 2.186, 2.22, 2.178, 2.184, 2.201, 2.134, 2.093, 2.049, 2.042, 2.015,
                     1.957, 1.928, 1.902, 1.854, 1.777, 1.699, 1.65, 1.569, 1.541, 1.496, 1.478, 1.431, 1.387, 1.353,
                     0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27,
                     28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48)");




#generate parameter matrix
set.seed(1234)

vars <- c("Srxn1")
times <- seq(from = 0, to = 48, by = 1)


x <- rfast99(params = paramsens, n = 1000, q = q, q.arg = q.arg, replicate = 1)

#For uncertainity analysis
# set.seed(1111)
# out <- solve_mcsim(mName = "BS_Acetylation_24_24.model.R", params = paramsens, vars = vars,
#                    monte_carlo = 1000, dist = dist, q.arg = q.arg,
#                    time = times, condition = conditions1,
#                    rtol = 1e-7, atol = 1e-9)
# 
# pksim(out, xlab = "Time (h)", ylab = "Conc. (ug/L)", main = "Srxn1")

y_SUL <- solve_mcsim(x, mName = "BS_Acetylation_24_24.model.R",
                     params = paramsens, 
                     time = times, 
                     vars = vars,
                     condition = conditions1, 
                     rtol = 1e-7, atol = 1e-9)


#Andro high dose 

parms <- c(buildSrxn1Base = 0.02262,
           degradSrxn1 = 0.01577,
           Km_Srxn1 = 107060   ,
           hill_acetylated_SRXN1 = 9.82335,
           Vmax_buildSrxn1 = 130492,
           Km_NRF2_ACETYLATED_SRXN1 = 0.01361,
           hill_Srxn1 = 1.00412,
           r = 0.65,
           decay =0.3)


parm2 = parms[1:9]*(1 - 0.1)
parm3 = parms[1:9]*(1 +1.1)

dist <- rep("Uniform", 9)
q <- rep("qunif", 9)
q.arg <-list(list(parm2[[1]], parm3[[1]]),list(parm2[[2]], parm3[[2]]),
             list(parm2[[3]], parm3[[3]]),list(parm2[[4]], parm3[[4]]),
             list(parm2[[5]], parm3[[5]]),list(parm2[[6]], parm3[[6]]),
             list(parm2[[7]], parm3[[7]]),list(parm2[[8]], parm3[[8]]),list(parm2[[9]], parm3[[9]]) )


conditions2 <- c("NRF2_spline =NDoses(49,0, 0.074, 0.839, 1.125, 1.209, 1.265, 1.34, 1.366, 1.364, 1.256, 1.206, 1.195, 1.139, 1.092, 0.999, 1, 0.954,
                     0.949, 0.898, 0.924, 0.881, 0.85, 0.819, 0.764, 0.716, 0.81, 0.838, 0.804, 0.766, 0.741, 0.674, 0.632, 0.607, 0.583, 0.547, 0.534, 0.499,
                     0.472, 0.44, 0.422, 0.391, 0.367, 0.345, 0.321, 0.303, 0.299, 0.285, 0.273, 0.262,
                     0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27,
                     28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48)");


#generate parameter matrix
set.seed(1234)

vars <- c("Srxn1")
times <- seq(from = 0, to = 48, by = 1)


x <- rfast99(params = paramsens, n = 1000, q = q, q.arg = q.arg, replicate = 1)

#For uncertainity analysis
# set.seed(1111)
# out <- solve_mcsim(mName = "BS_Acetylation_24_24.model.R", params = paramsens, vars = vars,
#                    monte_carlo = 1000, dist = dist, q.arg = q.arg,
#                    time = times, condition = conditions1,
#                    rtol = 1e-7, atol = 1e-9)
# 
# pksim(out, xlab = "Time (h)", ylab = "Conc. (ug/L)", main = "Srxn1")

y_Andro <- solve_mcsim(x, mName = "BS_Acetylation_24_24.model.R",
                       params = paramsens, 
                       time = times, 
                       vars = vars,
                       condition = conditions2, 
                       rtol = 1e-7, atol = 1e-9)


#Etha high dose 

parms <- c(buildSrxn1Base = 0.02262,
           degradSrxn1 = 0.01577,
           Km_Srxn1 = 107060   ,
           hill_acetylated_SRXN1 = 9.82335,
           Vmax_buildSrxn1 = 130492,
           Km_NRF2_ACETYLATED_SRXN1 = 0.01361,
           hill_Srxn1 = 1.00412,
           r = 1.082,
           decay =0.126)


parm2 = parms[1:9]*(1 - 0.1)
parm3 = parms[1:9]*(1 +1.1)

dist <- rep("Uniform", 9)
q <- rep("qunif", 9)
q.arg <-list(list(parm2[[1]], parm3[[1]]),list(parm2[[2]], parm3[[2]]),
             list(parm2[[3]], parm3[[3]]),list(parm2[[4]], parm3[[4]]),
             list(parm2[[5]], parm3[[5]]),list(parm2[[6]], parm3[[6]]),
             list(parm2[[7]], parm3[[7]]),list(parm2[[8]], parm3[[8]]),list(parm2[[9]], parm3[[9]]) )

conditions3 <- c("NRF2_spline =NDoses(49,0, 0.076, 0.627, 1.043, 1.125, 1.067, 1.102, 1.068, 0.989, 0.978, 0.962, 0.917, 0.884, 0.872, 0.863, 0.858,
	  0.867, 0.863, 0.852, 0.909, 0.888, 0.839, 0.846, 0.839, 0.823, 0.871, 0.83, 0.845, 0.882, 0.87, 0.881, 0.866, 0.884, 0.869, 0.869,
	  0.877, 0.897, 0.879, 0.871, 0.859, 0.84, 0.873, 0.827, 0.831, 0.803, 0.801, 0.831, 0.804, 0.812,
                           0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27,
                           28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48)");


#generate parameter matrix
set.seed(1234)

vars <- c("Srxn1")
times <- seq(from = 0, to = 48, by = 1)


x <- rfast99(params = paramsens, n = 1000, q = q, q.arg = q.arg, replicate = 1)

#For uncertainity analysis
# set.seed(1111)
# out <- solve_mcsim(mName = "BS_Acetylation_24_24.model.R", params = paramsens, vars = vars,
#                    monte_carlo = 1000, dist = dist, q.arg = q.arg,
#                    time = times, condition = conditions1,
#                    rtol = 1e-7, atol = 1e-9)
# 
# pksim(out, xlab = "Time (h)", ylab = "Conc. (ug/L)", main = "Srxn1")

y_ETA <- solve_mcsim(x, mName = "BS_Acetylation_24_24.model.R",
                     params = paramsens, 
                     time = times, 
                     vars = vars,
                     condition = conditions3, 
                     rtol = 1e-7, atol = 1e-9)

#CDDO-me high dose 

parms <- c(buildSrxn1Base = 0.02262,
           degradSrxn1 = 0.01577,
           Km_Srxn1 = 107060   ,
           hill_acetylated_SRXN1 = 9.82335,
           Vmax_buildSrxn1 = 130492,
           Km_NRF2_ACETYLATED_SRXN1 = 0.01361,
           hill_Srxn1 = 1.00412,
           r = 1.162,
           decay = 0.1034)


parm2 = parms[1:9]*(1 - 0.1)
parm3 = parms[1:9]*(1 +1.1)

dist <- rep("Uniform", 9)
q <- rep("qunif", 9)
q.arg <-list(list(parm2[[1]], parm3[[1]]),list(parm2[[2]], parm3[[2]]),
             list(parm2[[3]], parm3[[3]]),list(parm2[[4]], parm3[[4]]),
             list(parm2[[5]], parm3[[5]]),list(parm2[[6]], parm3[[6]]),
             list(parm2[[7]], parm3[[7]]),list(parm2[[8]], parm3[[8]]),list(parm2[[9]], parm3[[9]]) )

conditions4 <- c("NRF2_spline =NDoses(49,0, 0.384, 1.102, 1.225, 1.388, 1.451, 1.511, 1.536, 1.49, 1.466, 1.444, 1.415, 1.387, 1.35, 1.357,
	  1.349, 1.273, 1.233, 1.237, 1.244, 1.25, 1.24, 1.266, 1.27, 1.294, 1.377, 1.422, 1.421, 1.462, 1.515, 1.551, 1.571, 1.552,
	  1.534, 1.524, 1.507, 1.556, 1.559, 1.534, 1.568, 1.59, 1.611, 1.606, 1.606, 1.613, 1.598, 1.618, 1.601, 1.63,
                           0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27,
                           28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48)");



vars <- c("Srxn1")
times <- seq(from = 0, to = 48, by = 1)


#generate parameter matrix
set.seed(1234)

vars <- c("Srxn1")
times <- seq(from = 0, to = 48, by = 1)


x <- rfast99(params = paramsens, n = 1000, q = q, q.arg = q.arg, replicate = 1)

#For uncertainity analysis
# set.seed(1111)
# out <- solve_mcsim(mName = "BS_Acetylation_24_24.model.R", params = paramsens, vars = vars,
#                    monte_carlo = 1000, dist = dist, q.arg = q.arg,
#                    time = times, condition = conditions1,
#                    rtol = 1e-7, atol = 1e-9)
# 
# pksim(out, xlab = "Time (h)", ylab = "Conc. (ug/L)", main = "Srxn1")

y_CDDO <- solve_mcsim(x, mName = "BS_Acetylation_24_24.model.R",
                      params = paramsens, 
                      time = times, 
                      vars = vars,
                      condition = conditions4, 
                      rtol = 1e-7, atol = 1e-9)


y_SUL$params = c("kp","kd","Kmu" ,"nm", 
                 "Vmax2","Kmm","nu","r",'decay')

colnames(y_SUL$tSI) = c("kp","kd","Kmu" ,"nm", 
                        "Vmax2","Kmm","nu","r1.Sul",'d1.Sul')

colnames(y_Andro32R$tSI) = c("kp","kd","Kmu" ,"nm", 
                             "Vmax2","Kmm","nu","r1.Andr",'d1.Andr',"r2.Andr32",'d2.Andro32')

colnames(y_ETA32R$tSI) = c("kp","kd","Kmu" ,"nm", 
                           "Vmax2","Kmm","nu","r1.ETA",'d1.ETA',"r2.ETA32",'d2.ETA32')

colnames(y_CDDO32R$tSI) = c("kp","kd","Kmu" ,"nm", 
                            "Vmax2","Kmm","nu","r1.CDDO",'d1.CDDO',"r2.CDDO32",'d2.CDDO32')

colnames(y_Andro$tSI) = c("kp","kd","Kmu" ,"nm", 
                          "Vmax2","Kmm","nu","r1.Andr",'d1.Andr')

colnames(y_ETA$tSI) = c("kp","kd","Kmu" ,"nm", 
                        "Vmax2","Kmm","nu","r1.ETA",'d1.ETA')

colnames(y_CDDO$tSI) = c("kp","kd","Kmu" ,"nm", 
                         "Vmax2","Kmm","nu","r1.CDDO",'d1.CDDO')

plot(y_SUL)

final_tSI = cbind(y_SUL$tSI[,1:9,1],r1.Andr = y_Andro$tSI[,8,1],d1.Andr = y_Andro$tSI[,9,1],
                  r1.ETA = y_ETA$tSI[,8,1],d1.ETA = y_ETA$tSI[,9,1],
                  r1.CDDO = y_CDDO$tSI[,8,1],d1.CDDO = y_CDDO$tSI[,9,1])

final_mSI = cbind(y_SUL$mSI[,1:9,1],r1.Andr = y_Andro$mSI[,8,1],d1.Andr = y_Andro$mSI[,9,1],
                  r1.ETA = y_ETA$mSI[,8,1],d1.ETA = y_ETA$mSI[,9,1],
                  r1.CDDO = y_CDDO$mSI[,8,1],d1.CDDO = y_CDDO$mSI[,9,1])


final_iSI = cbind(y_SUL$iSI[,1:9,1],r1.Andr = y_Andro$iSI[,8,1],d1.Andr = y_Andro$iSI[,9,1],
                  r1.ETA = y_ETA$iSI[,8,1],d1.ETA = y_ETA$iSI[,9,1],
                  r1.CDDO = y_CDDO$iSI[,8,1],d1.CDDO = y_CDDO$iSI[,9,1])

# 
# heat_check(y, order = "total order", show.all = T)
# 
# heat_check(y, index = "CI", order = "total order")

# par(mfrow = c(5,5))
# for (i in 1:15){
#   plot(times, final_tSI[,i],ylim = c(0, 1), bty = "n", yaxt = "n",type = "l", col = "black",main =colnames(final_tSI)[i],ylab = "SI",lwd = 2)
#   lines(times, final_mSI[,i], col = "red",ylim = c(0, 1),yaxt = "n",lwd = 2)
#   abline(h = 0.1, col = "black", lty = 2)
#   axis(2, at = seq(0, 1, .2))
# }



#################################
#repeated dosing 
#################################
mName <- "BS_Acetylation_24_24_rep.model.R"

makemcsim(model = mName, dir = "modeling")


paramsens <- c("buildSrxn1Base","degradSrxn1","Km_Srxn1" ,"hill_acetylated_SRXN1", 
               "Vmax_buildSrxn1","Km_NRF2_ACETYLATED_SRXN1","hill_Srxn1","r",'decay',"r2",'decay2')

parms <- c(buildSrxn1Base = 0.02262,
           degradSrxn1 = 0.01577,
           Km_Srxn1 = 107060   ,
           hill_acetylated_SRXN1 = 9.82335,
           Vmax_buildSrxn1 = 130492,
           Km_NRF2_ACETYLATED_SRXN1 = 0.01361,
           hill_Srxn1 = 1.00412,
           r = 7.98,
           decay = 0.15,
           r2 = 2.186,
           decay2 = 0.125)


parm2 = parms[1:11]*(1 - 0.1)
parm3 = parms[1:11]*(1 +1.1)

dist <- rep("Uniform", 11)
q <- rep("qunif", 11)
q.arg <-list(list(parm2[[1]], parm3[[1]]),list(parm2[[2]], parm3[[2]]),
             list(parm2[[3]], parm3[[3]]),list(parm2[[4]], parm3[[4]]),
             list(parm2[[5]], parm3[[5]]),list(parm2[[6]], parm3[[6]]),
             list(parm2[[7]], parm3[[7]]),list(parm2[[8]], parm3[[8]]),list(parm2[[9]], parm3[[9]]),list(parm2[[10]], parm3[[10]]),list(parm2[[11]], parm3[[11]]) )



conditions <- c("NRF2_spline = NDoses(33,0,0.972,1.175,1.153,1.104,1.05,1.004,0.966,0.947,1.028,0.924,0.954,0.931,0.899,0.887,0.87,0.855,0.85,0.843,0.837,0.821,0.816,0.802,0.793,0.78,0.779,0.775,0.754,0.757,0.751,0.759,0.748,0.739,
	                      0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32)")
#SUL high dose 
conditions1 <- c("NRF2_spline = NDoses(49,0, 0.333, 1.314, 1.691, 2.16, 2.685, 2.987, 3.083, 3.042, 2.955, 2.868, 2.726, 2.576, 2.437, 2.282, 2.221, 2.124, 2.038, 1.969, 1.938, 1.84, 1.808, 1.774, 1.772, 1.804, 1.376, 1.541, 1.602, 1.895, 2.214, 2.352, 2.454, 2.476, 2.39, 2.362, 2.365, 2.343, 2.301, 2.271, 2.189, 2.099, 2.087, 2.051, 1.986, 1.974, 1.903, 1.868, 1.827, 1.803,
                           0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 
                           28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48);");



#generate parameter matrix
set.seed(1234)

vars <- c("Srxn1")
times <- seq(from = 0, to = 48, by = 1)


x <- rfast99(params = paramsens, n = 1000, q = q, q.arg = q.arg, replicate = 1)



y_Sul48R <- solve_mcsim(x, mName = "BS_Acetylation_24_24_rep.model.R",
                        params = paramsens, 
                        time = times, 
                        vars = vars,
                        condition = conditions1, 
                        rtol = 1e-7, atol = 1e-9)


#Andro high dose 

parms <- c(buildSrxn1Base = 0.02262,
           degradSrxn1 = 0.01577,
           Km_Srxn1 = 107060   ,
           hill_acetylated_SRXN1 = 9.82335,
           Vmax_buildSrxn1 = 130492,
           Km_NRF2_ACETYLATED_SRXN1 = 0.01361,
           hill_Srxn1 = 1.00412,
           r = 0.65,
           decay =0.3,
           r2 = 0.65,
           decay2 = 0.125)


parm2 = parms[1:11]*(1 - 0.1)
parm3 = parms[1:11]*(1 +1.1)

dist <- rep("Uniform", 11)
q <- rep("qunif", 11)
q.arg <-list(list(parm2[[1]], parm3[[1]]),list(parm2[[2]], parm3[[2]]),
             list(parm2[[3]], parm3[[3]]),list(parm2[[4]], parm3[[4]]),
             list(parm2[[5]], parm3[[5]]),list(parm2[[6]], parm3[[6]]),
             list(parm2[[7]], parm3[[7]]),list(parm2[[8]], parm3[[8]]),list(parm2[[9]], parm3[[9]]),list(parm2[[10]], parm3[[10]]),list(parm2[[11]], parm3[[11]]) )


conditions2 <- c("NRF2_spline = NDoses(49,0, 0.259, 1.163, 1.383, 1.595, 1.731, 1.892, 2.009, 2.078, 2.108, 2.106, 2.059, 2.001, 1.953, 1.951, 1.859, 1.881, 1.869, 1.808, 1.785, 1.808, 1.803, 1.852, 1.809, 1.826, 1.735, 1.813, 1.958, 2.236, 2.441, 2.505, 2.573, 2.635, 2.629, 2.645, 2.649, 2.627, 2.643, 2.66, 2.576, 2.579, 2.56, 2.485, 2.438, 2.363, 2.371, 2.349, 2.277, 2.219,
                           0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 
                           28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48)");

#generate parameter matrix
set.seed(1234)

vars <- c("Srxn1")
times <- seq(from = 0, to = 48, by = 1)


x <- rfast99(params = paramsens, n = 1000, q = q, q.arg = q.arg, replicate = 1)



y_Andro48R <- solve_mcsim(x, mName = "BS_Acetylation_24_24_rep.model.R",
                          params = paramsens, 
                          time = times, 
                          vars = vars,
                          condition = conditions2, 
                          rtol = 1e-7, atol = 1e-9)
#Etha high dose 

parms <- c(buildSrxn1Base = 0.02262,
           degradSrxn1 = 0.01577,
           Km_Srxn1 = 107060   ,
           hill_acetylated_SRXN1 = 9.82335,
           Vmax_buildSrxn1 = 130492,
           Km_NRF2_ACETYLATED_SRXN1 = 0.01361,
           hill_Srxn1 = 1.00412,
           r = 1.08,
           decay = 0.126,
           r2 = 1.082,
           decay2 = 0.084)


parm2 = parms[1:11]*(1 - 0.1)
parm3 = parms[1:11]*(1 +1.1)

dist <- rep("Uniform", 11)
q <- rep("qunif", 11)
q.arg <-list(list(parm2[[1]], parm3[[1]]),list(parm2[[2]], parm3[[2]]),
             list(parm2[[3]], parm3[[3]]),list(parm2[[4]], parm3[[4]]),
             list(parm2[[5]], parm3[[5]]),list(parm2[[6]], parm3[[6]]),
             list(parm2[[7]], parm3[[7]]),list(parm2[[8]], parm3[[8]]),list(parm2[[9]], parm3[[9]]),list(parm2[[10]], parm3[[10]]),list(parm2[[11]], parm3[[11]]) )


conditions3 <- c("NRF2_spline =NDoses(49,0, 0.147, 0.716, 1.14, 1.231, 1.205, 1.214, 1.189, 1.163, 1.108, 1.06, 1.048, 1.007, 0.964, 0.962, 0.978, 0.972, 0.985, 0.939, 0.984, 0.961, 0.947, 0.975, 0.957, 0.962, 0.712, 0.768, 0.962, 1.103, 1.116, 1.033, 1.089, 1.038, 1.022, 1.016, 0.944, 0.973, 0.961, 0.926, 0.912, 0.922, 0.942, 0.902, 0.899, 0.874, 0.861, 0.844, 0.861, 0.856,
                           0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 
                           28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48)");

#generate parameter matrix
set.seed(1234)

vars <- c("Srxn1")
times <- seq(from = 0, to = 48, by = 1)


x <- rfast99(params = paramsens, n = 1000, q = q, q.arg = q.arg, replicate = 1)



y_ETA48R <- solve_mcsim(x, mName = "BS_Acetylation_24_24_rep.model.R",
                        params = paramsens, 
                        time = times, 
                        vars = vars,
                        condition = conditions3, 
                        rtol = 1e-7, atol = 1e-9)
#CDDO-me high dose 

parms <- c(buildSrxn1Base = 0.02262,
           degradSrxn1 = 0.01577,
           Km_Srxn1 = 107060   ,
           hill_acetylated_SRXN1 = 9.82335,
           Vmax_buildSrxn1 = 130492,
           Km_NRF2_ACETYLATED_SRXN1 = 0.01361,
           hill_Srxn1 = 1.00412,
           r = 1.16 ,
           decay = 0.1,
           r2 = 2.06,
           decay2 = 0.07)


parm2 = parms[1:11]*(1 - 0.1)
parm3 = parms[1:11]*(1 +1.1)

dist <- rep("Uniform", 11)
q <- rep("qunif", 11)
q.arg <-list(list(parm2[[1]], parm3[[1]]),list(parm2[[2]], parm3[[2]]),
             list(parm2[[3]], parm3[[3]]),list(parm2[[4]], parm3[[4]]),
             list(parm2[[5]], parm3[[5]]),list(parm2[[6]], parm3[[6]]),
             list(parm2[[7]], parm3[[7]]),list(parm2[[8]], parm3[[8]]),list(parm2[[9]], parm3[[9]]),list(parm2[[10]], parm3[[10]]),list(parm2[[11]], parm3[[11]]) )


conditions4 <- c("NRF2_spline =NDoses(49,0.227, 1.089, 1.23, 1.359, 1.478, 1.533, 1.56, 1.561, 1.51, 1.438, 1.412, 1.347, 1.335, 1.311, 1.323, 1.319, 1.29, 1.233, 1.243, 1.238, 1.235, 1.246, 1.242, 1.261, 1.11, 1.14, 1.163, 1.41, 1.56, 1.532, 1.586, 1.64, 1.592, 1.591, 1.589, 1.597, 1.598, 1.639, 1.632, 1.627, 1.632, 1.609, 1.579, 1.586, 1.58, 1.564, 1.568, 1.607,
                           0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 
                           28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48)");


#generate parameter matrix
set.seed(1234)

vars <- c("Srxn1")
times <- seq(from = 0, to = 48, by = 1)


x <- rfast99(params = paramsens, n = 1000, q = q, q.arg = q.arg, replicate = 1)



y_CDDO48R <- solve_mcsim(x, mName = "BS_Acetylation_24_24_rep.model.R",
                         params = paramsens, 
                         time = times, 
                         vars = vars,
                         condition = conditions1, 
                         rtol = 1e-7, atol = 1e-9)


colnames(y_Sul48R$tSI) = c("kp","kd","Kmu" ,"nm", 
                           "Vmax2","Kmm","nu","r1.Sul",'d1.Sul',"r2.Sul48",'d2.Sul48')


colnames(y_Andro48R$tSI) = c("kp","kd","Kmu" ,"nm", 
                             "Vmax2","Kmm","nu","r1.Andr",'d1.Andr',"r2.Andr48",'d2.Andr48')

colnames(y_ETA48R$tSI) = c("kp","kd","Kmu" ,"nm", 
                           "Vmax2","Kmm","nu","r1.ETA",'d1.ETA',"r2.ETA48",'d2.ETA48')

colnames(y_CDDO48R$tSI) = c("kp","kd","Kmu" ,"nm", 
                            "Vmax2","Kmm","nu","r1.CDDO",'d1.CDDO',"r2.CDDO48",'d2.CDDO48')

plot(y_Sul48R)

final_tSI_R48 = cbind(final_tSI,r2.Sul48 = y_Sul48R$tSI[,10,1],d2.Sul48 = y_Sul48R$tSI[,11,1],
                      r2.Andr48 = y_Andro48R$tSI[,10,1],d2.Andro48 = y_Andro48R$tSI[,11,1],
                      r2.ETA48 = y_ETA48R$tSI[,10,1],d2.ETA48 = y_ETA48R$tSI[,11,1],
                      r2.CDDO48 = y_CDDO48R$tSI[,10,1],d2.CDDO48R = y_CDDO48R$tSI[,11,1])

final_mSI_R48 = cbind(final_mSI,r2.Sul48 = y_Sul48R$mSI[,10,1],d2.Sul48 = y_Sul48R$mSI[,11,1],
                      r2.Andr48 = y_Andro48R$mSI[,10,1],d2.Andro48 = y_Andro48R$mSI[,11,1],
                      r2.ETA48 = y_ETA48R$mSI[,10,1],d2.ETA48 = y_ETA48R$mSI[,11,1],
                      r2.CDDO48 = y_CDDO48R$mSI[,10,1],d2.CDDO48R = y_CDDO48R$mSI[,11,1])

final_iSI_R48 = cbind(final_iSI,r2.Sul48 = y_Sul48R$iSI[,10,1],d2.Sul48 = y_Sul48R$iSI[,11,1],
                      r2.Andr48 = y_Andro48R$iSI[,10,1],d2.Andro48 = y_Andro48R$iSI[,11,1],
                      r2.ETA48 = y_ETA48R$iSI[,10,1],d2.ETA48 = y_ETA48R$iSI[,11,1],
                      r2.CDDO48 = y_CDDO48R$iSI[,10,1],d2.CDDO48R = y_CDDO48R$iSI[,11,1])

# heat_check(y, order = "total order", show.all = T)
# 
# heat_check(y, index = "CI", order = "total order")
# 
# par(mfrow = c(5,5))
# for (i in 1:23){
#   plot(times, final_tSI_R48[,i],ylim = c(0, 1), bty = "n", yaxt = "n",type = "l", col = "black",main =colnames(final_tSI_R48)[i],ylab = "SI",lwd = 2)
#   lines(times, final_mSI_R48[,i], col = "red",ylim = c(0, 1),yaxt = "n",lwd = 2)
#   abline(h = 0.1, col = "black", lty = 2)
#   axis(2, at = seq(0, 1, .2))
# }




#############################################
mName <- "BS_acetylation_8_24_rep_8.model.R"


makemcsim(model = mName, dir = "modeling")

paramsens <- c("buildSrxn1Base","degradSrxn1","Km_Srxn1" ,"hill_acetylated_SRXN1", 
               "Vmax_buildSrxn1","Km_NRF2_ACETYLATED_SRXN1","hill_Srxn1","r",'decay',"r2",'decay2')

parms <- c(buildSrxn1Base = 0.02262,
           degradSrxn1 = 0.01577,
           Km_Srxn1 = 107060   ,
           hill_acetylated_SRXN1 = 9.82335,
           Vmax_buildSrxn1 = 130492,
           Km_NRF2_ACETYLATED_SRXN1 = 0.01361,
           hill_Srxn1 = 1.00412,
           r = 7.98,
           decay = 0.15,
           r2 = 0.0021,
           decay2 = 0.0721)


parm2 = parms[1:11]*(1 - 0.1)
parm3 = parms[1:11]*(1 +1.1)

dist <- rep("Uniform", 11)
q <- rep("qunif", 11)
q.arg <-list(list(parm2[[1]], parm3[[1]]),list(parm2[[2]], parm3[[2]]),
             list(parm2[[3]], parm3[[3]]),list(parm2[[4]], parm3[[4]]),
             list(parm2[[5]], parm3[[5]]),list(parm2[[6]], parm3[[6]]),
             list(parm2[[7]], parm3[[7]]),list(parm2[[8]], parm3[[8]]),list(parm2[[9]], parm3[[9]]),list(parm2[[10]], parm3[[10]]),list(parm2[[11]], parm3[[11]]) )



conditions <- c("NRF2_spline = NDoses(33,0,0.972,1.175,1.153,1.104,1.05,1.004,0.966,0.947,1.028,0.924,0.954,0.931,0.899,0.887,0.87,0.855,0.85,0.843,0.837,0.821,0.816,0.802,0.793,0.78,0.779,0.775,0.754,0.757,0.751,0.759,0.748,0.739,
	                      0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32)")
#SUL high dose 
conditions1 <- c("NRF2_spline = NDoses(49,0, 0.333, 1.314, 1.691, 2.16, 2.685, 2.987, 3.083, 3.042, 2.955, 2.868, 2.726, 2.576, 2.437, 2.282, 2.221, 2.124, 2.038, 1.969, 1.938, 1.84, 1.808, 1.774, 1.772, 1.804, 1.376, 1.541, 1.602, 1.895, 2.214, 2.352, 2.454, 2.476, 2.39, 2.362, 2.365, 2.343, 2.301, 2.271, 2.189, 2.099, 2.087, 2.051, 1.986, 1.974, 1.903, 1.868, 1.827, 1.803,
                           0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 
                           28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48);");



#generate parameter matrix
set.seed(1234)

vars <- c("Srxn1")
times <- seq(from = 0, to = 48, by = 1)


x <- rfast99(params = paramsens, n = 1000, q = q, q.arg = q.arg, replicate = 1)



y_Sul32R <- solve_mcsim(x, mName = "BS_Acetylation_24_24_rep.model.R",
                        params = paramsens, 
                        time = times, 
                        vars = vars,
                        condition = conditions1, 
                        rtol = 1e-7, atol = 1e-9)


#Andro high dose 

parms <- c(buildSrxn1Base = 0.02262,
           degradSrxn1 = 0.01577,
           Km_Srxn1 = 107060   ,
           hill_acetylated_SRXN1 = 9.82335,
           Vmax_buildSrxn1 = 130492,
           Km_NRF2_ACETYLATED_SRXN1 = 0.01361,
           hill_Srxn1 = 1.00412,
           r = 0.65,
           decay =0.3,
           r2 = 2.6558,
           decay2 = 0.2054)


parm2 = parms[1:11]*(1 - 0.1)
parm3 = parms[1:11]*(1 +1.1)

dist <- rep("Uniform", 11)
q <- rep("qunif", 11)
q.arg <-list(list(parm2[[1]], parm3[[1]]),list(parm2[[2]], parm3[[2]]),
             list(parm2[[3]], parm3[[3]]),list(parm2[[4]], parm3[[4]]),
             list(parm2[[5]], parm3[[5]]),list(parm2[[6]], parm3[[6]]),
             list(parm2[[7]], parm3[[7]]),list(parm2[[8]], parm3[[8]]),list(parm2[[9]], parm3[[9]]),list(parm2[[10]], parm3[[10]]),list(parm2[[11]], parm3[[11]]) )


conditions2 <- c("NRF2_spline = NDoses(49,0, 0.259, 1.163, 1.383, 1.595, 1.731, 1.892, 2.009, 2.078, 2.108, 2.106, 2.059, 2.001, 1.953, 1.951, 1.859, 1.881, 1.869, 1.808, 1.785, 1.808, 1.803, 1.852, 1.809, 1.826, 1.735, 1.813, 1.958, 2.236, 2.441, 2.505, 2.573, 2.635, 2.629, 2.645, 2.649, 2.627, 2.643, 2.66, 2.576, 2.579, 2.56, 2.485, 2.438, 2.363, 2.371, 2.349, 2.277, 2.219,
                           0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 
                           28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48)");

#generate parameter matrix
set.seed(1234)

vars <- c("Srxn1")
times <- seq(from = 0, to = 48, by = 1)


x <- rfast99(params = paramsens, n = 1000, q = q, q.arg = q.arg, replicate = 1)



y_Andro32R <- solve_mcsim(x, mName = "BS_Acetylation_24_24_rep.model.R",
                          params = paramsens, 
                          time = times, 
                          vars = vars,
                          condition = conditions2, 
                          rtol = 1e-7, atol = 1e-9)
#Etha high dose 

parms <- c(buildSrxn1Base = 0.02262,
           degradSrxn1 = 0.01577,
           Km_Srxn1 = 107060   ,
           hill_acetylated_SRXN1 = 9.82335,
           Vmax_buildSrxn1 = 130492,
           Km_NRF2_ACETYLATED_SRXN1 = 0.01361,
           hill_Srxn1 = 1.00412,
           r = 1.08,
           decay = 0.126,
           r2 = 2.391,
           decay2 = 2.06)


parm2 = parms[1:11]*(1 - 0.1)
parm3 = parms[1:11]*(1 +1.1)

dist <- rep("Uniform", 11)
q <- rep("qunif", 11)
q.arg <-list(list(parm2[[1]], parm3[[1]]),list(parm2[[2]], parm3[[2]]),
             list(parm2[[3]], parm3[[3]]),list(parm2[[4]], parm3[[4]]),
             list(parm2[[5]], parm3[[5]]),list(parm2[[6]], parm3[[6]]),
             list(parm2[[7]], parm3[[7]]),list(parm2[[8]], parm3[[8]]),list(parm2[[9]], parm3[[9]]),list(parm2[[10]], parm3[[10]]),list(parm2[[11]], parm3[[11]]) )


conditions3 <- c("NRF2_spline =NDoses(49,0, 0.147, 0.716, 1.14, 1.231, 1.205, 1.214, 1.189, 1.163, 1.108, 1.06, 1.048, 1.007, 0.964, 0.962, 0.978, 0.972, 0.985, 0.939, 0.984, 0.961, 0.947, 0.975, 0.957, 0.962, 0.712, 0.768, 0.962, 1.103, 1.116, 1.033, 1.089, 1.038, 1.022, 1.016, 0.944, 0.973, 0.961, 0.926, 0.912, 0.922, 0.942, 0.902, 0.899, 0.874, 0.861, 0.844, 0.861, 0.856,
                           0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 
                           28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48)");

#generate parameter matrix
set.seed(1234)

vars <- c("Srxn1")
times <- seq(from = 0, to = 48, by = 1)


x <- rfast99(params = paramsens, n = 1000, q = q, q.arg = q.arg, replicate = 1)



y_ETA32R <- solve_mcsim(x, mName = "BS_Acetylation_24_24_rep.model.R",
                        params = paramsens, 
                        time = times, 
                        vars = vars,
                        condition = conditions3, 
                        rtol = 1e-7, atol = 1e-9)
#CDDO-me high dose 

parms <- c(buildSrxn1Base = 0.02262,
           degradSrxn1 = 0.01577,
           Km_Srxn1 = 107060   ,
           hill_acetylated_SRXN1 = 9.82335,
           Vmax_buildSrxn1 = 130492,
           Km_NRF2_ACETYLATED_SRXN1 = 0.01361,
           hill_Srxn1 = 1.00412,
           r = 1.16 ,
           decay = 0.1,
           r2 = 2.06,
           decay2 = 0.076)


parm2 = parms[1:11]*(1 - 0.1)
parm3 = parms[1:11]*(1 +1.1)

dist <- rep("Uniform", 11)
q <- rep("qunif", 11)
q.arg <-list(list(parm2[[1]], parm3[[1]]),list(parm2[[2]], parm3[[2]]),
             list(parm2[[3]], parm3[[3]]),list(parm2[[4]], parm3[[4]]),
             list(parm2[[5]], parm3[[5]]),list(parm2[[6]], parm3[[6]]),
             list(parm2[[7]], parm3[[7]]),list(parm2[[8]], parm3[[8]]),list(parm2[[9]], parm3[[9]]),list(parm2[[10]], parm3[[10]]),list(parm2[[11]], parm3[[11]]) )


conditions4 <- c("NRF2_spline =NDoses(49,0.227, 1.089, 1.23, 1.359, 1.478, 1.533, 1.56, 1.561, 1.51, 1.438, 1.412, 1.347, 1.335, 1.311, 1.323, 1.319, 1.29, 1.233, 1.243, 1.238, 1.235, 1.246, 1.242, 1.261, 1.11, 1.14, 1.163, 1.41, 1.56, 1.532, 1.586, 1.64, 1.592, 1.591, 1.589, 1.597, 1.598, 1.639, 1.632, 1.627, 1.632, 1.609, 1.579, 1.586, 1.58, 1.564, 1.568, 1.607,
                           0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 
                           28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48)");


#generate parameter matrix
set.seed(1234)

vars <- c("Srxn1")
times <- seq(from = 0, to = 48, by = 1)


x <- rfast99(params = paramsens, n = 1000, q = q, q.arg = q.arg, replicate = 1)



y_CDDO32R <- solve_mcsim(x, mName = "BS_Acetylation_24_24_rep.model.R",
                         params = paramsens, 
                         time = times, 
                         vars = vars,
                         condition = conditions1, 
                         rtol = 1e-7, atol = 1e-9)


colnames(y_Sul32R$tSI) = c("kp","kd","Kmu" ,"nm", 
                           "Vmax2","Kmm","nu","r1.Sul",'d1.Sul',"r2.Sul32",'d2.Sul32')

colnames(y_Andro32R$tSI) = c("kp","kd","Kmu" ,"nm", 
                             "Vmax2","Kmm","nu","r1.Andr",'d1.Andr',"r2.Andr32",'d2.Andro32')

colnames(y_ETA32R$tSI) = c("kp","kd","Kmu" ,"nm", 
                           "Vmax2","Kmm","nu","r1.ETA",'d1.ETA',"r2.ETA32",'d2.ETA32')

colnames(y_CDDO32R$tSI) = c("kp","kd","Kmu" ,"nm", 
                            "Vmax2","Kmm","nu","r1.CDDO",'d1.CDDO',"r2.CDDO32",'d2.CDDO32')

plot(y_Sul32R)

final_tSI_R48_32 = cbind(final_tSI_R48,r2.Sul32R = y_Sul32R$tSI[,10,1],d2.Sul32R = y_Sul32R$tSI[,11,1],
                         r2.Andro32R = y_Andro32R$tSI[,10,1],d2.Andr32 = y_Andro32R$tSI[,11,1],
                         r2.ETA32 = y_ETA32R$tSI[,10,1],d2.ETA32 = y_ETA32R$tSI[,11,1],
                         r2.CDDO32 = y_CDDO32R$tSI[,10,1],d2.CDDO32 = y_CDDO32R$tSI[,11,1])

final_mSI_R48_32 = cbind(final_mSI_R48,
                         r2.Sul48 = y_Sul48R$mSI[,10,1],d2.Sul48 = y_Sul48R$mSI[,11,1],  
                         r2.Andro32R = y_Andro32R$mSI[,10,1],d2.Andr32 = y_Andro32R$mSI[,11,1],
                         r2.ETA32 = y_ETA32R$mSI[,10,1],d2.ETA32 = y_ETA32R$mSI[,11,1],
                         r2.CDDO32 = y_CDDO32R$mSI[,10,1],d2.CDDO32 = y_CDDO32R$mSI[,11,1])


final_iSI_R48_32 = cbind(final_iSI_R48,
                         r2.Sul48 = y_Sul48R$iSI[,10,1],d2.Sul48 = y_Sul48R$iSI[,11,1],  
                         r2.Andro32R = y_Andro32R$iSI[,10,1],d2.Andr32 = y_Andro32R$iSI[,11,1],
                         r2.ETA32 = y_ETA32R$iSI[,10,1],d2.ETA32 = y_ETA32R$iSI[,11,1],
                         r2.CDDO32 = y_CDDO32R$iSI[,10,1],d2.CDDO32 = y_CDDO32R$iSI[,11,1])

figdirec = "C:/Users/alexe/OneDrive - Universiteit Leiden/Manusript_Nrf2_repeated_dosing/Images"
tiff(paste0(figdirec,"/", "Sensitivity1",".png"), units="in", width=10, height=8, res=700, compression = 'lzw')
# pdf(paste0(figdirec,"/", "Sensitivity1",".pdf"),onefile=FALSE)
# par(mfrow = c(4,8))
par(mfrow = c(4,8),
    oma = c(5,4,1,1) + 0.1,
    mar = c(1,1,1,1) + 1)
for (i in 1:31){
  plot(times, final_tSI_R48_32[,i],ylim = c(0, 0.8), bty = "n", yaxt = "n",type = "l", col = "black",main =colnames(final_tSI_R48_32)[i],ylab = "SI",lwd = 1.5,cex.main=1.0,cex.lab=1.0, cex.axis=1.0,xlab="Time[h]")
  #lines(times, final_mSI_R48_32[,i], col = "red",ylim = c(0, 1),yaxt = "n",lwd = 1.5)
  lines(times, final_iSI_R48_32[,i], col = "red",ylim = c(0, 0.8),yaxt = "n",lty = 3,lwd = 1.5)
  abline(h = 0.05, col = "blue", lty = 2,lwd = 1.2)
  axis(2, at = seq(0, 1, .2))
}
par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
plot(0, 0, type = 'l', bty = 'n', xaxt = 'n', yaxt = 'n')
legend('bottom',legend = c("Total order", "first order","cut-off"), col = c("black","red", "blue"), lty=c(1,1,2),lwd = 2, xpd = TRUE, horiz = TRUE, cex = 1, seg.len=2, bty = 'n')

# legend('bottom',legend = c("Total order", "first order", "mixed order", "cut-off"), col = c("black", "grey", "red", "blue"), lty=c(1,3,1,2),lwd = 2, xpd = TRUE, horiz = TRUE, cex = 1, seg.len=2, bty = 'n')
# # xpd = TRUE makes the legend plot to the figure
dev.off()
