###PLOT COLOR COORDINATE
library(ggplot2)
library(ggquiver)
setwd("C:\\Users\\310243978\\Documents\\R&D\\01.Project\\FF\\01.Reliability\\2017_A1")
dat <- read.csv(file = "output.csv")
xx <- c(0.31,0.453,0.5,0.5,0.443,0.31)
yy <- c(0.348,0.44,0.44,0.382,0.382,0.283)
positions <- data.frame(
  x = xx,
  y = yy
)
p <- ggplot(positions, aes(x = x, y = y)) +
  geom_polygon(fill = "light blue") + xlim(0.3,0.5) + ylim(0.28,0.44)
p

p1 <- ggplot(dat, aes(x = x, y = y)) +
  geom_point(aes(colour = dat$Rel.Test), show.legend = FALSE) + xlim(0.3,0.5) + ylim(0.28,0.44)
p1

ggplot(dat, aes(x = x0, y = y0, u=delta_x, v=delta_y, colour = dat$Rel.Test)) + 
  geom_quiver(vecsize = 0, rescale= FALSE) + scale_x_continuous(limits = c(0.3, 0.5)) + 
  scale_y_continuous(limits = c(0.28, 0.44))
##Update
ggplot(dat, aes(x = x0, y = y0, u=delta_x, v=delta_y, colour = dat$Rel.Test)) + 
  geom_quiver(vecsize = 0, rescale= FALSE, show.legend = FALSE) + scale_x_continuous(limits = c(0.3, 0.5)) + 
  scale_y_continuous(limits = c(0.28, 0.44))


###-------------------LR5 ECE-----------
library(ggplot2)
library(RODBC)
library(RODBCext)
library(dplyr)
library(lubridate)
channel <- odbcConnect("MES_LR5", uid = "LR5", pwd = "LR5")


xx <- c(0.645,0.665,0.735,0.722)
yy <- c(0.335,0.335,0.265,0.259)
positions <- data.frame(
  Bulb_CIEX_160mA = xx,
  Bulb_CIEY_160mA = yy
)
p <- ggplot(positions, aes(x = Bulb_CIEX_160mA, y = Bulb_CIEY_160mA)) +
  geom_polygon(fill = "white", color = "blue") + xlim(0.645,0.735) + ylim(0.259,0.335)
p

#extract the panel data
query <- 'SELECT "Start_Date","Unit_ID", "Bulb_CIEX_160mA","Bulb_CIEY_160mA","Bulb_Flux_160mAlm" FROM "TEST_PNL_RAW_LR5_KOI" WHERE "Start_Date" like ? '
parameters <- data.frame("%18%")
panel <- sqlExecute(channel, query, parameters, fetch = TRUE)
panel <- panel %>% mutate(Test_Date = mdy(Start_Date))
dat <- panel %>% filter(substr(Start_Date,7,8) == "18") %>% filter(Bulb_Flux_160mAlm > 10)
dat <- dat %>% mutate(yboundary = -Bulb_CIEX_160mA + 1 , deltay = Bulb_CIEY_160mA- yboundary,
                      yflag = ifelse(deltay >0, 1,0))
dat <- dat %>% mutate(xboundary = -Bulb_CIEY_160mA + 1 , deltax = Bulb_CIEX_160mA- xboundary,
                      xflag = ifelse(deltax >0, 1,0))
mean(dat$yflag)
mean(dat$xflag)

p1 <- ggplot(dat, aes(x = Bulb_CIEX_160mA, y = Bulb_CIEY_160mA, colour = as.factor(xflag))) +
  geom_point( size = 0.4, show.legend = FALSE) +  xlim(0.645,0.735) + ylim(0.259,0.335) + 
  scale_color_manual(values=c("#81C2D6", "#E69F00"))
p1

ggplot(dat, aes(deltay)) + stat_density() + xlim(0,0.001)
ggplot(dat, aes(deltax)) + stat_density() + xlim(0,0.001)

#extract the EOL Hot data

query2 <- 'SELECT "Start_DateTime","Unit_ID",  "Flux_13_5V_mW" , "CIEx_13_5V_pts" , "CIEy_13_5V_pts" FROM "TEST_EOLHOT_RAW_LR5_KOI" WHERE "Start_DateTime" like ? '
hot <- sqlExecute(channel, query2,parameters, fetch = TRUE)
hot <- hot %>% mutate(Test_Date = mdy(substr(Start_DateTime,1,8)))
dat2 <- hot %>% filter(substr(Test_Date,1,4) == "2018") %>% filter(Flux_13_5V_mW > 100)
dat2 <- dat2 %>% mutate(delta = round(CIEy_13_5V_pts + CIEx_13_5V_pts,4) -1,
                        flag = ifelse(delta >0, 1,0))

mean(dat2$flag)
p2 <- ggplot(dat2, aes(x = CIEx_13_5V_pts, y = CIEy_13_5V_pts)) +
  geom_point( size = 0.4, show.legend = FALSE) +  xlim(0.645,0.735) + ylim(0.259,0.335) 
p2


###------trace
dat2$Unit_ID <- as.integer(as.character(dat2$Unit_ID))
dat0 <- left_join(dat, dat2, by = "Unit_ID")
dat0 <- dat0 %>% filter(!is.na(flag)) %>% mutate(flag_panel2eol = paste(yflag, flag, sep = "-"))
table(dat0$flag_panel2eol)

###lab data
lab <- read.csv(file = "C:\\Users\\310243978\\Documents\\R&D\\01.Project\\LR5\\07.Special Request\\CIE\\lab.csv")
lab$tester <- c("lab")
dat2$tester <- c("EOL hot") 
temp <- dat2 %>% select(CIEx_13_5V_pts,CIEy_13_5V_pts,tester) %>%
  rename(Coord.x = CIEx_13_5V_pts, Coord.y = CIEy_13_5V_pts )
lab <- rbind(lab, temp)
lab <- lab %>% arrange(tester)

p3 <- ggplot(lab, aes(x =  Coord.x, y =  Coord.y, colour = tester)) +
  geom_point( size = 0.6, alpha = 0.2,show.legend = FALSE) +  xlim(0.645,0.735) + ylim(0.259,0.335) 
p3