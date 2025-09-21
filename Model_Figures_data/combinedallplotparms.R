

setwd("C:/Users/rar/OneDrive - Genmab/Personalfolder/NRFmanuscript/Final_model")

Modeling_dir = "C:/Users/rar/OneDrive - Genmab/Personalfolder/NRFmanuscript/Final_model"



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

theme_set(theme_light())

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

parmsname = c("iter","CDDO,r2,32hR","CDDO,d2,32hR","ETA,r2,32hR","ETA,d2,32hR","Andr,r2,32hR","Andr,d2,32hR","Sul,r2,32hR","Sul,d2,32hR",
              "CDDO,r2,48hR","CDDO,d2,48hR","ETA,r2,48hR","ETA,d2,48hR","Andr,r2,48hR","Andr,d2,48hR","Sul,r2,48hR","Sul,d2,48hR", 
              "kp", "kd", "Kmm","nu","Vm2", "Kmu", "nm", "rall", "decayall","Sul,r1","Sul,d2","Andr,r1","Andr,d2",
              "ETA,r1","ETA,d2", "CDDO,r1","CDDO,d2",  "LnPrior","LnData" ,"LnPosterior")

filenames <- list.files(path = paste0(getwd()),pattern = "^combinedall.*\\.out$")
myfiles = lapply(paste0(getwd(), "/",filenames), read.delim, sep = "")
names(myfiles) <- filenames
colnames_para <- c("iter","CDDO.r2.32hR","CDDO.d2.32hR","ETA.r2.32hR","ETA.d2.32hR","Andr.r2.32hR","Andr.d2.32hR","Sul.r2.32hR","Sul.d2.32hR",
                   "CDDO.r2.48hR","CDDO.d2.48hR","ETA.r2.48hR","ETA.d2.48hR","Andr.r2.48hR","Andr.d2.48hR","Sul.r2.48hR","Sul.d2.48hR",
                   "kp", "kd", "Kmm","nu","Vmax2", "Kmu", "nm","rall", "decayall","Sul.r1","Sul.d2","Andr.r1","Andr.d2",
                   "ETA.r1","ETA.d2", "CDDO.r1","CDDO.d2","LnPrior","LnData","LnPosterior") 


myfiles$combinedall1.out
out_final2 = lapply(myfiles, setNames, colnames_para)

out_final2 <- lapply(myfiles, function(df) {
  df <- df[-1, ]                        # remove first row
  setNames(df, colnames_para)           # apply correct column names
})


out_final = out_final2
out_final[[1]]["kp"] = out_final[[1]]["kp"]/10
out_final[[2]]["kp"]= out_final[[2]]["kp"]/10
out_final[[3]]["kp"] = out_final[[3]]["kp"]/10
out_final[[4]]["kp"] = out_final[[4]]["kp"]/10


endparms = length(names(out_final[[1]]))-3
parms_name_1 <- paste((names(out_final[[1]])[2:endparms]),sep = ",")
sims <- mcmc_array(list(out_final[[1]],out_final[[2]],
                        out_final[[3]],out_final[[4]]))                      # i removed the 2 extra files manually 
str <- ceiling(nrow(sims)/2) + 1
end <- nrow(sims)
j <- c(str:end)
length(j)
color_scheme_set("mix-blue-red")

parms_name_2 = c("kp", "kd", "Kmm","nu","Vmax2", "Kmu", "nm","Sul.r1","Sul.d2","Andr.r1","Andr.d2",
              "ETA.r1","ETA.d2", "CDDO.r1","CDDO.d2","Sul.r2.48hR","Sul.d2.48hR","Andr.r2.48hR","Andr.d2.48hR","ETA.r2.48hR","ETA.d2.48hR",
              "CDDO.r2.48hR","CDDO.d2.48hR","Sul.r2.32hR","Sul.d2.32hR","Andr.r2.32hR","Andr.d2.32hR","ETA.r2.32hR","ETA.d2.32hR",
              "CDDO.r2.32hR","CDDO.d2.32hR","rall", "decayall")


color_scheme_set("mix-blue-red")
plot1 = mcmc_trace(sims[j,,], pars = parms_name_2)+ scale_x_continuous(breaks=c(0,2500,5000)) #, facet_args = list(ncol = 1, strip.position = "left")) ,window = c(0,4000)

mcmc_trace(sims[j,,], pars = parms_name_2) #, facet_args = list(ncol = 1, strip.position = "left"))
monitor_file = data.frame((monitor(sims[,,parms_name_2], digit=4, probs = c(0.025, 0.5, 0.975)))) 


dataf = as.data.frame((monitor_file))
str(dataf)
typeof(dataf)
rhats <- rhat(dataf)
color_scheme_set("brightblue") # see help("color_scheme_set")
rhats <- rhat(monitor_file)
file = dataf[,"Rhat",]
names(file) = parms_name_2
mcmc_rhat(file)
#pdf(paste0(getwd(),"/",Modeling_dir,"/Model2_rhat" ,".pdf"))
#tiff(paste0(getwd(),"/",Modeling_dir,"/Model2_rhat",".png"), units="in", width=8, height=10, res=700, compression = 'lzw')
plot2 = mcmc_rhat(file) + yaxis_text(hjust = 1)

tiff(paste0(getwd(),"/",Modeling_dir,"/Model2_convergence",".png"), units="in", width=12, height=8, res=600, compression = 'lzw')
# par(mfrow = c(1,2))
grid.arrange(plot1,plot2,ncol = 2,widths = c(3, 1))
dev.off()


library(posterior)
library(bayesplot)
library(ggplot2)

# Convert to a posterior object (detects dims automatically)
draws <- as.array(sims)

parms_name_1 = c("kp", "kd")

parms_name_2 = c("kp", "kd", "Kmm","nu","Vmax2", "Kmu", "nm")

df = as.data.frame(draws)

mcmc_pairs(draws,pars = parms_name_1)


# install once if needed
# install.packages(c("GGally","hexbin","posterior"))

library(GGally)
library(hexbin)     # for hex-binned density panels (optional)
library(posterior)  # to convert Stan/cmdstanr draws -> data frame
library(dplyr)

# pick your parameter sets
parms_name_1 <- c("kp", "kd")
parms_name_2 <- c("kp", "kd", "Kmm", "Vmax2", "Kmu")

library(GGally)
library(posterior)

parms_name_1 <- c("kp","kd")
df <- posterior::as_draws_df(draws)[, parms_name_1, drop = FALSE]

nice_names <- c("kp"="k[p]", "kd"="k[d]")
colnames(df) <- unname(nice_names[names(df)])

p <- ggpairs(
  df,
  labeller = "label_parsed",
  lower = list(continuous = wrap("points", alpha = .15, size = .5)),
  upper = list(continuous = wrap("cor", method = "spearman", digits = 2, size = 4)),
  diag  = list(continuous = wrap("densityDiag"))
) + theme_bw(base_size = 11)

print(p)


# install.packages("hexbin")  # if not installed
library(GGally); library(ggplot2); library(posterior)

parms_name_2 <- c("kp","kd","Kmm","nu","Vmax2","Kmu","nm","rall", "decayall")

# parms_name_2 = c("kp", "kd", "Kmm","nu","Vmax2", "Kmu", "nm","Sul.r1","Sul.d2")
df <- posterior::as_draws_df(draws)[, parms_name_2]

nice_names <- c("kp"="kp", "kd"="kd", "Kmm"="Kmm", "nu"="nu",
                "Vmax2"="Vmax1", "Kmu"="Kmu", "nm"="nm","rall" = "rall","decayall"= "dall")
colnames(df) <- unname(nice_names[names(df)])


# --- Helpers --------------------------------------------------------------
library(GGally)
library(ggplot2)
library(rlang)

# upper panel: print ONLY the number (Spearman by default)
panel_cor_only <- function(data, mapping, method = "spearman", digits = 2, ...) {
  x <- data[[as_label(mapping$x)]]
  y <- data[[as_label(mapping$y)]]
  r <- suppressWarnings(cor(x, y, method = method, use = "complete.obs"))
  GGally::ggally_text(label = formatC(r, digits = digits, format = "f"), ...)
}

# lower panel: hex bins to avoid overplotting (falls back to points if hexbin missing)
# panel_hex <- function(data, mapping, bins = 30, ...) {
#   ggplot(data, mapping) + geom_hex(bins = bins, ...) + theme_bw()
# }

panel_hex <- function(data, mapping, bins = 30, ...) {
  ggplot(data, mapping) +
    geom_hex(bins = bins, ...) +
    scale_x_continuous(
      n.breaks = 4
      # labels   = label_scientific(digits = 2) # or: label_number(accuracy = 0.001)
    ) +
    scale_y_continuous(
      n.breaks = 4
      # labels   = label_scientific(digits = 2)
    ) +
    theme_bw()
}


# --- Pairs plot -----------------------------------------------------------
p <- ggpairs(
  df,
  # labeller = "label_parsed",  # enable only if you used nice_names above
  lower = list(continuous = wrap(panel_hex, bins = 30)),
  upper = list(continuous = wrap(panel_cor_only, method = "spearman", digits = 2, size = 3.2)),
  diag  = list(continuous = wrap("densityDiag"))
) +
  theme_bw(base_size = 9) +
  theme(strip.text = element_text(size = 8),
        axis.text  = element_text(size = 6))

# p <- p + theme(axis.text = element_blank(), axis.ticks = element_blank())

p <- p +
  theme(
    axis.text.x = element_text(hjust = 1, size = 6),   #angle = 10, 
    axis.text.y = element_text(hjust = 1, size = 6)
  )


library(scales)



# --- Save high-quality figure (auto-sized by number of params) -----------
nvars <- ncol(df)
cell_mm <-18             # ~cell size per variable; tweak if needed
w_mm <- max(280, nvars * cell_mm)  # minimum width 160 mm
h_mm <- max(280, nvars * cell_mm)

# PDF (vector)
#ggsave("pairs_all.pdf", p, device = cairo_pdf, width = w_mm, height = h_mm, units = "mm", useDingbats = FALSE)

# 600-dpi TIFF (raster)
ragg::agg_tiff("pairs_all1.tiff", width = w_mm, height = h_mm, units = "mm",
               res = 600, compression = "lzw", background = "white")
print(p); dev.off()

# (Optional) also save a compact single-column version with smaller base_size if needed




# --- Correlation table with 95% bootstrap CIs for ALL pairs --------------
boot_spearman_ci <- function(data, B = 2000, conf = 0.95) {
  vars <- colnames(data)
  combs <- combn(vars, 2, simplify = FALSE)
  n <- nrow(data)
  res <- lapply(combs, function(v) {
    x <- data[[v[1]]]; y <- data[[v[2]]]
    r <- suppressWarnings(cor(x, y, method = "spearman", use = "complete.obs"))
    set.seed(1)
    boot <- replicate(B, {
      idx <- sample.int(n, replace = TRUE)
      suppressWarnings(cor(x[idx], y[idx], method = "spearman", use = "complete.obs"))
    })
    ci <- quantile(boot, c((1 - conf)/2, 1 - (1 - conf)/2), na.rm = TRUE)
    data.frame(var1 = v[1], var2 = v[2], rho = r, ci_lo = ci[1], ci_hi = ci[2])
  })
  do.call(rbind, res)
}

spearman_ci_tbl <- boot_spearman_ci(df, B = 2000, conf = 0.95)
# Save the table for the supplement
write.csv(spearman_ci_tbl, "spearman_correlation_CI_table.csv", row.names = FALSE)

library(GGally)
library(rlang)

# custom upper panel (prints τ without stars)
panel_kendall <- function(data, mapping, ...) {
  x <- data[[as_label(mapping$x)]]
  y <- data[[as_label(mapping$y)]]
  tau <- suppressWarnings(cor(x, y, method = "kendall", use = "complete.obs"))
  GGally::ggally_text(label = sprintf("Kendall \u03C4 = %.2f", tau), size = 4, ...)
}

# nice labels (optional)
nice_names <- c(kp="J[0]", kd="k[d]", Kmm="K[m*m]", Vmax2="V[max*2]", Kmu="K[m*u]")
names(df) <- unname(replace(names(df), TRUE, nice_names[names(df)]))

p <- ggpairs(
  df,
  labeller = "label_parsed",
  lower = list(continuous = wrap("points", alpha = .15, size = .4)),
  upper = list(continuous = panel_kendall),
  diag  = list(continuous = wrap("densityDiag"))
) + theme_bw(base_size = 11)

print(p)
