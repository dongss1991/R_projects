library(sqldf)
library(dplyr)
library(stringr)
library(data.table)
library(ggplot2)
setwd("C:/Users/310243978/Documents/R&D/01.Project/LR5/06.Task/Resistor_Mapping")
target <- read.csv(file = "matrix.csv")

###merge panel test data###
a <- list.files("Panel/0327-0327", pattern = "LPs*")
dir <- paste("./Panel/0327-0327/",a,sep="")
n <- length(dir)
#read first file
if(fread(dir[1],header=FALSE ,sep= "," , skip=137 , dec = ".", quote= "\"",fill = TRUE,select = 1, nrows =1) == "Date"){
  dat <- fread(dir[1],header=TRUE ,sep= "," , skip=137 , dec = ".", quote= "\"",fill = TRUE,select = c(1:116))
  lot <- data.frame(substr(dir[1],19,nchar(dir[1])))
  names(lot) <- "FileName"
  dat <- cbind(dat,lot)
  lastrow <- dim(dat)[1]
  dat <- dat[-lastrow,]
}else{
  dat <- fread(dir[1],header=TRUE ,sep= "," , skip=155 , dec = ".", quote= "\"",fill = TRUE,select = c(1:116))
  lot <- data.frame(substr(dir[1],19,nchar(dir[1])))
  names(lot) <- "FileName"
  dat <- cbind(dat,lot)
  lastrow <- dim(dat)[1]
  dat <- dat[-lastrow,]
}
#read the rest files
for(i in 2:n) {
  if(fread(dir[i],header=FALSE ,sep= "," , skip=137 , dec = ".", quote= "\"",fill = TRUE,select = 1, nrows =1) == "Date"){
    new.data <- fread(dir[i],header=TRUE ,sep= "," , sep2 = "\r", skip=137 , dec = ".", quote= "\"",fill = TRUE,select = c(1:116))
    new.lot <- data.frame(substr(dir[i],19,nchar(dir[i])))
    names(new.lot) <- "FileName"
    new.data <- cbind(new.data,new.lot)
    lastrow <- dim(new.data)[1]
    new.data <- new.data[-lastrow,]
    dat <- rbind(dat,new.data)
    rm(new.data)
    rm(new.lot)
  }else{
    new.data <- fread(dir[i],header=TRUE ,sep= "," , sep2 = "\r", skip=155  , dec = ".", quote= "\"",fill = TRUE,select = c(1:116))
    new.lot <- data.frame(substr(dir[i],19,nchar(dir[i])))
    names(new.lot) <- "FileName"
    new.data <- cbind(new.data,new.lot)
    lastrow <- dim(new.data)[1]
    new.data <- new.data[-lastrow,]
    dat <- rbind(dat,new.data)
    rm(new.data)
    rm(new.lot)
  }
}
write.csv(dat, file= "merged_panel_20170224.csv" , row.names = FALSE)
dat0 <- dat %>% filter(`Bin Label` =="Default") %>% mutate(LotID = substr(FileName,1,11))
dat0 <- dat0 %>% mutate ( Min.Flux = round(`Bulb@30:Luminous Flux(lm)`,0) )
dat0 <- left_join(dat0,target, by = "Min.Flux")
dat0 <- dat0 %>% mutate(Res = (9-`D1:Forward Voltage1(V)`-`Bulb@30:Optical VF(V)` )/Target.If)
dat0.summary <- dat0 %>% group_by(LotID) %>% summarize(RD_VF_SD = sd(`D1:Forward Voltage1(V)`,na.rm=TRUE),
                                                       LED_VF_SD = sd(`Bulb@30:Optical VF(V)`,na.rm=TRUE), 
                                                       LED_Flux_SD = sd(`Bulb@30:Luminous Flux(lm)`,na.rm= TRUE),
                                                       RD_VF_Q2 = median(`D1:Forward Voltage1(V)`,na.rm=TRUE),
                                                       LED_VF_Q2 = median(`Bulb@30:Optical VF(V)`,na.rm=TRUE), 
                                                       LED_Flux_Q2 = median(`Bulb@30:Luminous Flux(lm)`,na.rm= TRUE))
dat0.Res <- dat0 %>% group_by(LotID) %>% summarize(Res_SD = sd(Res,na.rm=TRUE),
                                                       Res_Q2 = median(Res,na.rm=TRUE))
dat0.summary <- mutate(dat0.summary, Res = (9-RD_VF_Q2-LED_VF_Q2)/0.0315)
ggplot(dat0,aes(`D1:Forward Voltage1(V)`, color= LotID)) + geom_histogram()+facet_wrap(~LotID)
ggplot(dat0,aes(`Bulb@30:Luminous Flux(lm)`, color= LotID)) + geom_histogram()+facet_wrap(~LotID)
ggplot(dat0,aes(`Bulb@30:Optical VF(V)`, color= LotID)) + geom_histogram()+facet_wrap(~LotID)
ggplot(dat0,aes( x =Res, color= LotID)) + stat_ecdf()+facet_wrap(~LotID)
###merge EOL cold 9V test data###
a <- list.files("EOL", pattern = "PCI011_T1.")
dir.create("./EOL/9V/")
dir <- paste("./EOL/",a,sep="")
file.copy(dir, "./EOL/9V/")
file.remove(dir)
b <- list.files("EOL/9V")
dir <- paste("./EOL/9V/",b,sep="")
n <- length(dir)
dat <- fread(dir[1],header=TRUE ,sep= "," , skip=20 , dec = ".", quote= "\"",fill = TRUE,select = c(1:18))
lot <- data.frame(substr(dir[1],20,nchar(dir[1])))
names(lot) <- "FileName"
dat <- cbind(dat,lot)
for(i in 2:n) {
  # browser()
  new.data <- fread(dir[i],header=TRUE ,sep= "," , skip=20 , dec = ".", quote= "\"",fill = TRUE,select = c(1:18))
  new.lot <- data.frame(substr(dir[i],20,nchar(dir[i])))
  names(new.lot) <- "FileName"
  new.data <- cbind(new.data,new.lot)
  dat <- rbind(dat,new.data)
  rm(new.data)
  rm(new.lot)
}
write.csv(dat, file= "EOL_9V_Cold.csv" , row.names = FALSE)

###merge EOL cold 13.5V test data###
a <- list.files("EOL", pattern = "PCI011_T2.")
dir.create("./EOL/13.5V/")
dir <- paste("./EOL/",a,sep="")
file.copy(dir, "./EOL/13.5V/")
file.remove(dir)
b <- list.files("EOL/13.5V")
dir <- paste("./EOL/13.5V/",b,sep="")
n <- length(dir)
dat <- fread(dir[1],header=TRUE ,sep= "," , skip=20 , dec = ".", quote= "\"",fill = TRUE,select = c(1:18), integer64 = 'numeric')
lot <- data.frame(substr(dir[1],23,nchar(dir[1])))
names(lot) <- "FileName"
dat <- cbind(dat,lot)
for(i in 2:n) {
  # browser()
  new.data <- fread(dir[i],header=TRUE ,sep= "," , skip=20 , dec = ".", quote= "\"",fill = TRUE,select = c(1:18), integer64 = 'numeric')
  new.lot <- data.frame(substr(dir[i],23,nchar(dir[i])))
  names(new.lot) <- "FileName"
  new.data <- cbind(new.data,new.lot)
  dat <- rbind(dat,new.data)
  rm(new.data)
  rm(new.lot)
}
write.csv(dat, file= "EOL_13.5V_Cold.csv" , row.names = FALSE)

###merge EOL hot electrical test data###
a <- list.files("EOL", pattern = "PCI005_E_LPL.|PCI005_E_lpl.")
dir.create("./EOL/Hot_E/")
dir <- paste("./EOL/",a,sep="")
file.copy(dir, "./EOL/Hot_E/")
file.remove(dir)
b <- list.files("EOL/Hot_E", pattern = "PCI005_E_LPL.|PCI005_E_lpl.")
dir <- paste("./EOL/Hot_E/",b,sep="")
n <- length(dir)
dat <- fread(dir[1],header=TRUE ,sep= "," , skip=48 , dec = ".", quote= "\"",fill = TRUE,select = c(1:18))
lot <- data.frame(substr(dir[1],22,nchar(dir[1])))
names(lot) <- "FileName"
dat <- cbind(dat,lot)
for(i in 2:n){
    new.data <- fread(dir[i],header=TRUE ,sep= "," , skip=48 , dec = ".", quote= "\"",fill = TRUE,select = c(1:18))
    new.lot <- data.frame(substr(dir[i],22,nchar(dir[i])))
    names(new.lot) <- "FileName"
    new.data <- cbind(new.data,new.lot)
    dat <- rbind(dat,new.data)
    rm(new.data)
    rm(new.lot)
  }
write.csv(dat, file= "EOL_Hot_Electrical.csv" , row.names = FALSE)

###merge EOL hot optical test data###
a <- list.files("EOL", pattern = "PCI005_O_LPL.|PCI005_O_lpl.")
dir.create("./EOL/Hot_O/")
dir <- paste("./EOL/",a,sep="")
file.copy(dir, "./EOL/Hot_O/")
file.remove(dir)
b <- list.files("EOL/Hot_O", pattern = "PCI005_O_LPL.|PCI005_O_lpl.")
dir <- paste("./EOL/Hot_O/",b,sep="")
n <- length(dir)
dat <- fread(dir[2],header=TRUE ,sep= "," , skip=48 , dec = ".", quote= "\"",fill = TRUE,select = c(1:26))
lot <- data.frame(substr(dir[1],22,nchar(dir[1])))
names(lot) <- "FileName"
dat <- cbind(dat,lot)
for(i in 3:n){
  new.data <- fread(dir[i],header=TRUE ,sep= "," , skip=48 , dec = ".", quote= "\"",fill = TRUE,select = c(1:26))
  new.lot <- data.frame(substr(dir[i],22,nchar(dir[i])))
  names(new.lot) <- "FileName"
  new.data <- cbind(new.data,new.lot)
  dat <- rbind(dat,new.data)
  rm(new.data)
  rm(new.lot)
}
write.csv(dat, file= "EOL_Hot_Optical.csv" , row.names = FALSE)

###merge the panel test and EOL cold/hot test data###
###
panel <- read.csv(file = "merged_panel_low.csv")
panel <- panel %>% arrange(desc(DUT.Count)) %>% distinct(Panel.ID,Barcode, .keep_all = TRUE) %>% 
          filter(Bin.Label == "Default") 
panel <- panel %>% mutate(Lot_ID = str_split_fixed(panel$FileName, "_", 3)[,1])
panel <- panel %>% mutate(Target_Current = 
                            ifelse(Bulb.30.Luminous.Flux.lm. <25,NA,ifelse(Bulb.30.Luminous.Flux.lm. <=26 && Bulb.30.Luminous.Flux.lm. >25,0.0315,
                                   ifelse(Bulb.30.Luminous.Flux.lm.<=27,0.030,
                                          ifelse(Bulb.30.Luminous.Flux.lm.<=28,0.029,
                                                 ifelse(Bulb.30.Luminous.Flux.lm.<=29,0.028,
                                                        ifelse(Bulb.30.Luminous.Flux.lm.<=30,0.027,
                                                               ifelse(Bulb.30.Luminous.Flux.lm.<=32,0.026,
                                                                      ifelse(Bulb.30.Luminous.Flux.lm.<=33,0.025,1))))))))) 
#remark:here use the target current for each indivudal bulb instead of one lot distrbution
panel <- panel %>% mutate(Resistor = (9-D1.Forward.Voltage1.V.-Bulb.30.Optical.VF.V.)/Target_Current)
give.n <- function(x){
  return(c(y = median(x)*1.05, label = length(x))) 
  # experiment with the multiplier to find the perfect position
}

# function for mean labels
median.n <- function(x){
  return(c(y = median(x)*0.97, label = round(median(x),2))) 
  # experiment with the multiplier to find the perfect position
}

# plot resistor

p <- ggplot(panel, aes(Lot_ID, Resistor))+geom_boxplot(aes(color=Lot_ID)) +
  stat_summary(fun.data = give.n, geom = "text", fun.y = median) +
  stat_summary(fun.data = mean.n, geom = "text", fun.y = median, colour = "red") +
  theme(axis.text.x=element_text(color="black",size=10,angle=50, hjust=1))
p
#statics summary
panel.summary <- panel %>% group_by(Lot_ID) %>% summarise(P1_Vf_30mA = mean(P1.30.Optical.VF.V.,na.rm = TRUE),
                                                          P2_Vf_30mA = mean(P2.30.Optical.VF.V.,na.rm = TRUE),
                                                          P3_Vf_30mA = mean(P3.30.Optical.VF.V.,na.rm = TRUE),
                                                          P4_Vf_30mA = mean(P4.30.Optical.VF.V.,na.rm = TRUE),
                                                          RD_40mA = mean(D1.Forward.Voltage1.V., na.rm = TRUE),
                                                          Bulb_Flux_30mA = mean(Bulb.30.Luminous.Flux.lm.,na.rm = TRUE))
panel.summary <- panel.summary %>% mutate(LED_Vf_30mA = rowMeans(select(panel.summary,starts_with("P")), na.rm = TRUE)) %>% 
                 select(Lot_ID,LED_Vf_30mA,RD_40mA,Bulb_Flux_30mA)
###merge 9V and 13.5V
EOL_C_9V <- read.csv(file = "EOL_9V_Cold.csv")
EOL_C_135V <- read.csv(file = "EOL_13.5V_Cold.csv")
EOL_C <- left_join(EOL_C_9V,EOL_C_135V, by = "Serial.ID")
EOL_C <- EOL_C %>% arrange(desc(DUT.Count.x)) %>% distinct(Serial.ID, .keep_all = TRUE)
write.csv(EOL_C , file = "EOL_C.csv", row.names = FALSE)

panel.summary <- dat %>% group_by(Vf.min) %>% summarise(IQR.Vf.30 = IQR(P1_Vf_30mAV,na.rm = TRUE),
                                                        IQR.Vf.160 = IQR(P1_Vf_160mAV,na.rm = TRUE))


####plot panel 3 key parameters
p <- ggplot(dat, aes(dat$`D1:Forward Voltage1(V)`, colour = Lot_ID)) + geom_histogram() + 
  facet_wrap(~Lot_ID)
p <- ggplot(dat, aes(dat$`Bulb@30:Luminous Flux(lm)`, colour = Lot_ID)) + geom_histogram() + 
  facet_wrap(~Lot_ID)
p <- ggplot(dat, aes(dat$`Bulb@30:Optical VF(V)`, colour = Lot_ID)) + geom_histogram() + 
  facet_wrap(~Lot_ID)
