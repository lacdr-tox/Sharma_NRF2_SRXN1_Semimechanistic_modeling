library(httk)
library("ggplot2")
library(tidyverse)
library(ggtext)
setwd("C:/Users/alexe/OneDrive - Universiteit Leiden/mcsim_files/Model_versions/Model_emegent_propeties")
direc = "C:/Users/alexe/OneDrive - Universiteit Leiden/Manusript_Nrf2_repeated_dosing/Images"

png(paste0(direc, "/", "Model_emegent_properties.png"), width = 600, height = 480)
layout(matrix(c(1,2,3,3), 2, 2, byrow = TRUE))


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

colnames(dd) = c("2X", "1.8X", "1.6X","1.4X", "1.2X", "1X", "0.9X", "0.8X", "0.7X", "0.6X", "0.5X","Time")

ddlong = gather(dd, "Multiplication", Srxn1, "2X":"0.5X", factor_key=TRUE)
str(ddlong)

P2 = ddlong %>% 
ggplot() +
geom_line(aes(x = Time, y = Srxn1, color = as.factor(Multiplication)),size=1) +
  labs(x = "Time [h]",y="Srxn1 intensity [AU]", title = expression('B. Effect of ' *italic(V)[max1]),size = 1) +
  theme(plot.title = element_text(size = 8)) +
  theme(legend.title = element_blank()) + theme(legend.position="none") +
  theme(legend.text = element_text(size= 7,face="bold")) +
  theme(axis.text = element_text(size = 7))



# 
# matplot(dd$time,dd[,1:11], t="l", lty=1, lwd = 2,xlab = "Time [h]",ylab = "Srxn1 [AU]",main =expression('B. Effect of V'[max]))


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
colnames(dd) = c("2X", "1.8X", "1.6X","1.4X", "1.2X", "1X", "0.9X", "0.8X", "0.7X", "0.6X", "0.5X","Time")

ddlong = gather(dd, "Multiplication", Srxn1, "2X":"0.5X", factor_key=TRUE)
str(ddlong)

P3 = ddlong %>% 
  ggplot() +
  geom_line(aes(x = Time, y = Srxn1, color = as.factor(Multiplication)),size=1) +
  labs(x = "Time [h]",y="", title = expression('C. ' *italic(K)[m[u]]),size = 1) +
  theme(plot.title = element_text(size = 8)) +
  theme(legend.title = element_blank()) + theme(legend.position="none") +
  theme(legend.text = element_text(size= 5,face="bold")) +
  theme(axis.text = element_text(size = 7))


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
colnames(dd) = c("2X", "1.8X", "1.6X","1.4X", "1.2X", "1X", "0.9X", "0.8X", "0.7X", "0.6X", "0.5X","Time")

ddlong = gather(dd, "Multiplication", Srxn1, "2X":"0.5X", factor_key=TRUE)
str(ddlong)


P4 = ddlong %>% 
  ggplot() +
  geom_line(aes(x = Time, y = Srxn1, color = as.factor(Multiplication)),size=1) +
  labs(x = "Time [h]",y="", title = expression('D. Effect of ' *italic(K)[m[m]]),size = 1) +
  theme(plot.title = element_text(size = 8)) +
  theme(legend.title = element_blank()) + theme(legend.position="right") +
  theme(legend.text = element_text(size= 5,face="bold")) +
  theme(axis.text = element_text(size = 7))+
  theme(legend.key.size = unit(0.35, 'cm'), #change legend key size
 legend.key.height = unit(0.35, 'cm'), #change legend key height
 legend.key.width = unit(0.35, 'cm')) #change legend key width

library(gridExtra)

library(ggplot2)
library(gridExtra)
library(grid)
vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
#pdf(paste0(figdirec,"/", "Figure7",".pdf"),onefile=FALSE)
#tiff(paste0(figdirec,"/", "Figure7",".png"),res = 300)
tiff(paste0(direc,"/", "Figure7",".png"), units="in", width=8.5, height=6.5, res=600, compression = 'lzw')
pushViewport(viewport(layout = grid.layout(10,10))) # 3 rows, 5 columns
print(P6, vp = vplayout(1:6, 1:10))  # the big plot covers rows 1:2 and cols 1:3
print(P2, vp = vplayout(7:10, 1:3))
print(P3, vp = vplayout(7:10, 4:6))
print(P4, vp = vplayout(7:10, 7:10))
dev.off()

daily.pnl <- cars
nn <- ncol(daily.pnl)
layout(matrix(c(1,2),nrow=1), width=c(4,1)) 
par(mar=c(5,4,4,0)) #No margin on the right side
matplot(cumsum(as.data.frame(daily.pnl)),type="l")
par(mar=c(5,0,4,2)) #No margin on the left side
plot(c(0,1),type="n", axes=F, xlab="", ylab="")
legend("center", colnames(daily.pnl),col=seq_len(nn),cex=0.8,fill=seq_len(nn))