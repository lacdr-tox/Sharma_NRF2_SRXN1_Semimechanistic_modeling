library(httk)
library("ggplot2")
setwd("C:/Users/alexe/OneDrive - Universiteit Leiden/mcsim_files/Model_versions/Model_emegent_propeties")


png("Model_emegent_properties.png", width = 700, height = 480)
layout(matrix(c(1,1,2,3), 2, 2, byrow = TRUE))


file = read.table("Model_emegent_properties_data.txt")
colnames(file) = c("Time","Srxn1")
file1 = file[, -1]
file2 = list()

for (i in 1:11){
  
  c = (102*i)-101   
  d = c + 101
  file2[[i]] = data.frame(file1[c:d])
  
}

dd  <-  as.data.frame(matrix(unlist(file2), nrow=102))
dd$time = file$Time[1:102]
#remove the first row 
dd = dd[-1, ]
ETAModel1_Nrf2inputRep48setSim.out

matplot(dd$time,dd[,1:11], t="l", lty=1, lwd = 2,xlab = "Time [h]",ylab = "Srxn1 [AU]",main=expression(italic('A. Effect of K'[a])))


file = read.table("Model_emegent_properties_data_km_srxn1.txt")
colnames(file) = c("Time","Srxn1")
file1 = file[, -1]
file2 = list()

for (i in 1:11){
  
  c = (102*i)-101   
  d = c + 101
  file2[[i]] = data.frame(file1[c:d])

}

dd  <-  as.data.frame(matrix(unlist(file2), nrow=102))
dd$time = file$Time[1:102]
#remove the first row 
dd = dd[-1, ]

matplot(dd$time,dd[,1:11], t="l", lty=1, lwd = 2,xlab = "Time [h]",ylab = "Srxn1 [AU]",main = expression(italic('B. Effect of K'[m])))




file = read.table("Model_emegent_properties_data_Vmax_srxn1.txt")
colnames(file) = c("Time","Srxn1")
file1 = file[, -1]
file2 = list()

for (i in 1:11){
  
  c = (102*i)-101   
  d = c + 101
  file2[[i]] = data.frame(file1[c:d])
  
}

dd  <-  as.data.frame(matrix(unlist(file2), nrow=102))
dd$time = file$Time[1:102]
#remove the first row 
dd = dd[-1, ]

matplot(dd$time,dd[,1:11], t="l", lty=1, lwd = 2,xlab = "Time [h]",ylab = "Srxn1 [AU]",main =expression(italic('C. Effect of V'[max])))
dev.off()
