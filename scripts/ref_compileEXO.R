#### read me ####

# the purpose of this script is to compile and plot EXO1 files from the Webster Lab BEGI project

# NOTE: It is STRONGLY recommended that you delete all your local EXO1 files from the last time you downloaded them from google drive, then use the script below to import them anew EACH TIME you run this script. 
# As some point, I should write this into the script!
# DO NOT push raw EXO1 files to the github repo! there are too many to push all at once. THe purpose of the google drive is to handle all these files, whereas github handles the script :)

#### libraries ####
library(googledrive)
library(tidyverse)
library(broom)
library(zoo)
library(stringr)
library(suncalc)


#### load data from google drive ####

# NOTE: It is STRONGLY recommended that you delete all your local EXO1 files from the last time you downloaded them from google drive, then use the script below to import them anew EACH TIME you run this script. 
# As some point, I should write this into the script!
# DO NOT push raw EXO1 files to the github repo! there are too many to push all at once. THe purpose of the google drive is to handle all these files, whereas github handles the script :)

ls_tibble <- googledrive::drive_ls("https://drive.google.com/drive/folders/1w9mhxwI4FU7Nu9HEFmrdH56FufOrv3Lp")
3
for (file_id in ls_tibble$id) {
  try({googledrive::drive_download(as_id(file_id))})
}
# add overwrite = TRUE if for some reason you want to replace files previously downloaded. 



#### load and stitch EXO data ####

# import data
siteIDz = c("VDOW", "VDOS", "SLOW", "SLOC")
BEGI_EXOz = list()
for(i in siteIDz){
  file_list <- list.files(recursive=F, pattern=paste(i, ".csv", sep=""))
  BEGI_EXOz[[i]] = lapply(file_list, read.csv, 
                          stringsAsFactors=FALSE, skip=8,header=T,
                          fileEncoding="utf-8") # this line makes it such that if there are any offending utf-16 encodings, it will show the offending file in the error message. If any utf-16 files are found, be sure to fix them in the Google Drive, not just your locally saved file!!
}

# use a set of column names as a template and match columns in all other files to that one. Note that this drops columns like Depth where the sensor isn't avialable on all sondes
universalnames = c("Date..MM.DD.YYYY.","Time..HH.mm.ss.","Time..Fract..Sec.","Site.Name","Cond.µS.cm","fDOM.QSU","fDOM.RFU","nLF.Cond.µS.cm","ODO...sat","ODO...local","ODO.mg.L","Sal.psu",  "SpCond.µS.cm","TDS.mg.L","Turbidity.FNU","TSS.mg.L","Temp..C","Battery.V","Cable.Pwr.V")
for(i in siteIDz){
  for(n in 1:length(BEGI_EXOz[[i]])){
    BEGI_EXOz[[i]][[n]] = 
      BEGI_EXOz[[i]][[n]] [, intersect(universalnames, names(BEGI_EXOz[[i]][[n]] )), drop=FALSE]
  }
}

# bind files within sites into one dataframe per site
for(i in siteIDz){
  BEGI_EXOz[[i]] = do.call(plyr::rbind.fill, BEGI_EXOz[[i]])
}

#
#### format dates ####

for(i in siteIDz){
  # put date and time in same column
  BEGI_EXOz[[i]]$datetime = paste( BEGI_EXOz[[i]]$Date..MM.DD.YYYY.,  BEGI_EXOz[[i]]$Time..HH.mm.ss., sep = " ")
  # convert to POIXct and set timezone
  BEGI_EXOz[[i]]$datetimeMT<-as.POSIXct( BEGI_EXOz[[i]]$datetime, 
                                         format = "%m/%d/%Y %H:%M:%S",
                                         tz="US/Mountain")
  # replace two digit years that are converted incorrectly
  BEGI_EXOz[[i]]$year = year(BEGI_EXOz[[i]]$datetimeMT)
  BEGI_EXOz[[i]]$year[BEGI_EXOz[[i]]$year==0023] = 2023
  BEGI_EXOz[[i]]$year[BEGI_EXOz[[i]]$year==0024] = 2024
  year(BEGI_EXOz[[i]]$datetimeMT) = BEGI_EXOz[[i]]$year
}


#
#### Check variable names ####
#check the variable order for each sonde and edit names if necessary

names(BEGI_EXOz[["VDOW"]]) == names(BEGI_EXOz[["VDOS"]])
names(BEGI_EXOz[["VDOW"]]) == names(BEGI_EXOz[["SLOW"]])
names(BEGI_EXOz[["VDOW"]]) == names(BEGI_EXOz[["SLOC"]])



#### Compile bursts within 1 min ####

# make sure all columns with numeric data data are numeric
BEGI_EXOz <- lapply(BEGI_EXOz, function(x) {x[5:19] <- lapply(x[5:19], as.numeric);x})

# get means and standard deviations of numeric burst values
BEGI_EXO.stz = list()
for(i in siteIDz){
  min<-round_date(BEGI_EXOz[[i]]$datetimeMT, "minute") # note rounding instead of using the function cut()!! cut was what was causing our memory issues!!
  BEGI_EXO.stz[[i]] <- as.data.frame(as.list(aggregate(cbind(Cond.µS.cm, fDOM.QSU, fDOM.RFU,
                                                             nLF.Cond.µS.cm,
                                                             ODO...sat,ODO.mg.L,
                                                             Sal.psu,SpCond.µS.cm,
                                                             TDS.mg.L,Turbidity.FNU,TSS.mg.L,Temp..C,
                                                             Battery.V,Cable.Pwr.V) 
                                                       ~ min, data=BEGI_EXOz[[i]], na.action=na.pass, FUN=function(x) c(mn=mean(x), SD=sd(x)))))
  BEGI_EXO.stz[[i]]$datetimeMT<-as.POSIXct(BEGI_EXO.stz[[i]]$min, "%Y-%m-%d %H:%M:%S", tz="US/Mountain")
}

#### save and re-add burst-compiled files ####

saveRDS(BEGI_EXO.stz, "EXO_compiled/BEGI_EXO.rds")
rm(list = ls())
BEGI_EXO.stz = readRDS("EXO_compiled/BEGI_EXO.rds")



#### stitch in water level data ####

# get data from googledrive
beeper_tibble <- googledrive::drive_ls("https://drive.google.com/drive/folders/1L5ywkdYUOxhE3GPm7vbMiwgObOyn3awF")
2
googledrive::drive_download(as_id(beeper_tibble$id[beeper_tibble$name=="BEGI_beeper"]), overwrite = TRUE,
                            path="googledrive/BEGI_beeper.csv")
beeper = read.csv("googledrive/BEGI_beeper.csv")

# format date/times
# beeper$time[is.na(beeper$time)] = "12:00"
# beeper$datetimeMT = as.POSIXct(
#   paste(beeper$date, beeper$time, sep=" "), 
#   "%Y-%m-%d %H:%M", tz="US/Mountain")
beeper$date = as.Date(beeper$date)

# format siteID to be same as wellID
beeper$siteID = beeper$wellID

# join
siteIDz = c("VDOW", "VDOS", "SLOW", "SLOC")
for (i in siteIDz){
  BEGI_EXO.stz[[i]]$date = as.Date(BEGI_EXO.stz[[i]]$datetimeMT, tz="US/Mountain")
  BEGI_EXO.stz[[i]]$siteID = i
  BEGI_EXO.stz[[i]] = left_join(BEGI_EXO.stz[[i]], beeper, by=c("date", "siteID"))
}

saveRDS(BEGI_EXO.stz, "EXO_compiled/BEGI_EXO.rds")
# rm(list = ls())
BEGI_EXO.stz = readRDS("EXO_compiled/BEGI_EXO.rds")

#### remove servicing times from data ####

# get data from googledrive
service_tibble <- googledrive::drive_ls("https://drive.google.com/drive/folders/1quyArAKgI5qn_lz4n1vjnoMrM0XJWdDl")
2
googledrive::drive_download(as_id(service_tibble$id[service_tibble$name=="sensor_event_log.xlsx"]), overwrite = TRUE,path="googledrive/sensor_event_log.xlsx")

# read in file and filter to EXO1 removal and deployments
service = readxl::read_excel("googledrive/sensor_event_log.xlsx")
service = service[service$model=="EXO1",]
service = service[service$observation=="removed" | service$observation=="deployed",]

# format date and time
service$datetime = paste(service$date,  service$time, sep = " ")
# convert to POIXct and set timezone
service$datetimeMT<-as.POSIXct(service$datetime, 
                               format = "%Y-%m-%d %H:%M",
                               tz="US/Mountain")
service$date = as.Date(service$date)

# remove rows with no exact times
servicetimes = service[!is.na(service$datetimeMT),]

## make list of times of each service sequence, adding an hour to the end of each
# VDOW
VDOW_servicedates = unique(servicetimes$date[servicetimes$location=="VDOW"])
VDOW_servicetimes = list()
for(i in c(1:length(VDOW_servicedates))){
  VDOW_servicetimes[[i]] = seq(
    from = servicetimes$datetimeMT[
      servicetimes$location=="VDOW" & servicetimes$observation=="removed" & servicetimes$date==VDOW_servicedates[i]],
    to = (servicetimes$datetimeMT[
      servicetimes$location=="VDOW" & servicetimes$observation=="deployed" & servicetimes$date==VDOW_servicedates[i]])+(60*60),
    by = "5 min")
}
VDOW_servicetimes_vector = do.call("c", VDOW_servicetimes)
# VDOS
VDOS_servicedates = unique(servicetimes$date[servicetimes$location=="VDOS"])
VDOS_servicetimes = list()
for(i in c(1:length(VDOS_servicedates))){
  VDOS_servicetimes[[i]] = seq(
    from = servicetimes$datetimeMT[
      servicetimes$location=="VDOS" & servicetimes$observation=="removed" & servicetimes$date==VDOS_servicedates[i]],
    to = servicetimes$datetimeMT[
      servicetimes$location=="VDOS" & servicetimes$observation=="deployed" & servicetimes$date==VDOS_servicedates[i]]+(60*60),
    by = "5 min")
}
VDOS_servicetimes_vector = do.call("c", VDOS_servicetimes)
# SLOC
SLOC_servicedates = unique(servicetimes$date[servicetimes$location=="SLOC"])
SLOC_servicetimes = list()
for(i in c(1:length(SLOC_servicedates))){
  SLOC_servicetimes[[i]] = seq(
    from = servicetimes$datetimeMT[
      servicetimes$location=="SLOC" & servicetimes$observation=="removed" & servicetimes$date==SLOC_servicedates[i]],
    to = servicetimes$datetimeMT[
      servicetimes$location=="SLOC" & servicetimes$observation=="deployed" & servicetimes$date==SLOC_servicedates[i]]+(60*60),
    by = "5 min")
}
SLOC_servicetimes_vector = do.call("c", SLOC_servicetimes)
# SLOW
SLOW_servicedates = unique(servicetimes$date[servicetimes$location=="SLOW"])
SLOW_servicedates = SLOW_servicedates[! SLOW_servicedates %in% as.Date(c("2024-04-17","2024-04-19"))]
SLOW_servicetimes = list()
for(i in c(1:length(SLOW_servicedates))){
  SLOW_servicetimes[[i]] = seq(
    from = (servicetimes$datetimeMT[
      servicetimes$location=="SLOW" & servicetimes$observation=="removed" & servicetimes$date==SLOW_servicedates[i]])[1],
    to = (servicetimes$datetimeMT[
      servicetimes$location=="SLOW" & servicetimes$observation=="deployed" & servicetimes$date==SLOW_servicedates[i]])[1]+(60*60),
    by = "5 min")
}
# add missing date/times
SLOW_servicetimes[[26]] = seq(
  from = as.POSIXct("2024-04-17 18:15", tz="US/Mountain"),
  to = as.POSIXct("2024-04-19 17:30", tz="US/Mountain")+(60*60),
  by = "5 min")
SLOW_servicetimes[[27]] = seq(
  from = as.POSIXct("2023-11-03 14:15:00", tz="US/Mountain"),
  to = as.POSIXct("2023-11-03 15:00:00", tz="US/Mountain")+(60*60),
  by = "5 min")
# compile
SLOW_servicetimes_vector = do.call("c", SLOW_servicetimes)


## remove EXO data from servicing times
BEGI_EXO.or = BEGI_EXO.stz
# VDOW
BEGI_EXO.or[["VDOW"]][2:25] [BEGI_EXO.or[["VDOW"]]$datetimeMT %in% VDOW_servicetimes_vector,] = NA
# VDOS
BEGI_EXO.or[["VDOS"]][2:25] [BEGI_EXO.or[["VDOS"]]$datetimeMT %in% VDOS_servicetimes_vector,] = NA
# SLOC
BEGI_EXO.or[["SLOC"]][2:25] [BEGI_EXO.or[["SLOC"]]$datetimeMT %in% SLOC_servicetimes_vector,] = NA
# SLOW
BEGI_EXO.or[["SLOW"]][2:25] [BEGI_EXO.or[["SLOW"]]$datetimeMT %in% SLOW_servicetimes_vector,] = NA


# there is also randomly a datapoint from the year 2072 in the VDOS dataset. Removing any years that are way off here:
BEGI_EXO.or[["VDOS"]][BEGI_EXO.or[["VDOS"]]$datetimeMT>as.POSIXct("2025-01-01 01:00:00"),] = NA

#

#### save and re-add data with servicing times removed ####

saveRDS(BEGI_EXO.or, "EXO_compiled/BEGI_EXO.rds")
#rm(list = ls())
BEGI_EXO.or = readRDS("EXO_compiled/BEGI_EXO.rds")



#### get service times and sunrise/sunset for plotting ####

# service dates

service = unique(service$date)

# sunrise/sunset

suntimes = 
  getSunlightTimes(date = seq.Date(from = as.Date(service[1]), to = as.Date(service[length(service)]), by = 1),
                   keep = c("sunrise", "sunset"),
                   lat = 34.9, lon = -106.7, tz = "US/Mountain")

pm.pts = suntimes$sunset[-(nrow(suntimes))]
am.pts = suntimes$sunrise[-1]


#### plot to check - SLOC ####

## SLOC last month ##
tempdat = BEGI_EXO.or[["SLOC"]][BEGI_EXO.or[["SLOC"]]$datetimeMT > Sys.time()-days(30),]

# Save plot 
jpeg("plots/SLOC_lastmonth.jpg", width = 12, height = 8, units="in", res=1000)

plot.new()
par(mfrow=c(4,2), mar=c(4,4,2,1.5))

plot(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"),(tempdat$waterlevelbelowsurface_cm*-1),
     pch=20,col="black", xlab="", xaxt = "n", type="n", ylab="")
rect(xleft=pm.pts,xright=am.pts,ybottom=-350, ytop=100, col="lightgrey", lwd = 0)
lines(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"),(tempdat$waterlevelbelowsurface_cm*-1),
      pch=20,col="black", xlab="", xaxt = "n", type="b")
abline(v=as.POSIXct(service), col="red")
#abline(h=-300, col="red")
abline(h=0, col="green")
axis.POSIXct(side=1,at=cut(tempdat$datetimeMT, breaks="24 hours"),format="%m-%d", las=2)
title(main="Water depth below surface (cm)")

plot(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"),(tempdat$Turbidity.FNU.mn),
     pch=20,col="black", xlab="", xaxt = "n", type="n", ylab="")
rect(xleft=pm.pts,xright=am.pts,ybottom=-4, ytop=2000, col="lightgrey", lwd = 0)
lines(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"),(tempdat$Turbidity.FNU.mn),
      pch=20,col="black", xlab="", xaxt = "n", type="o")
abline(v=as.POSIXct(service), col="red")
axis.POSIXct(side=1,at=cut(tempdat$datetimeMT, breaks="24 hours"),format="%m-%d", las=2)
title(main="Turbidity (FNU)")

plot(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"),(tempdat$Depth.m.mn),pch=20,col="black", xlab="", xaxt = "n", type="n", ylab="")
rect(xleft=pm.pts,xright=am.pts,ybottom=0, ytop=2000, col="lightgrey", lwd = 0)
lines(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"),(tempdat$Temp..C.mn),pch=20,col="black", xlab="", xaxt = "n", type="o")
abline(v=as.POSIXct(service), col="red")
axis.POSIXct(side=1,at=cut(tempdat$datetimeMT, breaks="24 hours"),format="%m-%d", las=2)
title(main="Uncorrected Depth")

plot(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"),(tempdat$ODO.mg.L.mn),
     pch=20,col="black", xlab="", xaxt = "n", type="n", ylab="")
rect(xleft=pm.pts,xright=am.pts,ybottom=-4, ytop=100, col="lightgrey", lwd = 0)
lines(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"),(tempdat$ODO.mg.L.mn),
      pch=20,col="black", xlab="", xaxt = "n", type="o")
abline(v=as.POSIXct(service), col="red")
axis.POSIXct(side=1,at=cut(tempdat$datetimeMT, breaks="24 hours"),format="%m-%d", las=2)
title(main="Dissolved Oxygen (mg/L)")

plot(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"),(tempdat$Temp..C.mn),
     pch=20,col="black", xlab="", xaxt = "n", type="n", ylab="")
rect(xleft=pm.pts,xright=am.pts,ybottom=-4, ytop=100, col="lightgrey", lwd = 0)
lines(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"),(tempdat$Temp..C.mn),
      pch=20,col="black", xlab="", xaxt = "n", type="o")#,ylim=c(22.5,24.5))
abline(v=as.POSIXct(service), col="red")
axis.POSIXct(side=1,at=cut(tempdat$datetimeMT, breaks="24 hours"),format="%m-%d", las=2)
title(main="Temperature (deg C)")

plot(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"),(tempdat$fDOM.QSU.mn),
     pch=20,col="black", xlab="", xaxt = "n", type="n", ylab="n")
rect(xleft=pm.pts,xright=am.pts,ybottom=-4, ytop=1000, col="lightgrey", lwd = 0)
lines(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"),(tempdat$fDOM.QSU.mn),
      pch=20,col="black", xlab="", xaxt = "n", type="o")#,ylim=c(22.5,24.5))
abline(v=as.POSIXct(service), col="red")
axis.POSIXct(side=1,at=cut(tempdat$datetimeMT, breaks="24 hours"),format="%m-%d", las=2)
title(main="fDOM (QSU)")

plot(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"),(tempdat$SpCond.µS.cm.mn),
     pch=20,col="black", xlab="", xaxt = "n", type="n", ylab="")
rect(xleft=pm.pts,xright=am.pts,ybottom=-4, ytop=2000, col="lightgrey", lwd = 0)
lines(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"),(tempdat$SpCond.µS.cm.mn),
      pch=20,col="black", xlab="", xaxt = "n", type="o")
abline(v=as.POSIXct(service), col="red")
axis.POSIXct(side=1,at=cut(tempdat$datetimeMT, breaks="24 hours"),format="%m-%d", las=2)
title(main="Specific Conductance (us/cm)")

plot(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"),(tempdat$Battery.V.mn),
     pch=20,col="black", xlab="", xaxt = "n", type="n", ylab="", ylim=c(-.2,4))
rect(xleft=pm.pts,xright=am.pts,ybottom=-4, ytop=2000, col="lightgrey", lwd = 0)
lines(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"),(tempdat$Battery.V.mn),
      pch=20,col="black", xlab="", xaxt = "n", type="o")
abline(v=as.POSIXct(service), col="red")
abline(h=2.2, col="red")
axis.POSIXct(side=1,at=cut(tempdat$datetimeMT, breaks="24 hours"),format="%m-%d", las=2)
title(main="Battery (volts)")

dev.off()


## SLOC ##

# Save plot 
jpeg("plots/SLOC.jpg", width = 12, height = 8, units="in", res=1000)

plot.new()
par(mfrow=c(4,2), mar=c(4,4,2,1.5))

plot(ymd_hms(BEGI_EXO.or[["SLOC"]]$datetimeMT, tz="US/Mountain"),(BEGI_EXO.or[["SLOC"]]$waterlevelbelowsurface_cm*-1),
     pch=20,col="black", xlab="", xaxt = "n", type="n", ylab="", ylim=c(-10, 10))
rect(xleft=pm.pts,xright=am.pts,ybottom=-350, ytop=100, col="lightgrey", lwd = 0)
lines(ymd_hms(BEGI_EXO.or[["SLOC"]]$datetimeMT, tz="US/Mountain"),(BEGI_EXO.or[["SLOC"]]$waterlevelbelowsurface_cm*-1),
      pch=20,col="black", xlab="", xaxt = "n", type="b")
abline(v=as.POSIXct(service), col="red")
#abline(h=-300, col="red")
abline(h=0, col="green")
axis.POSIXct(side=1,at=cut(BEGI_EXO.or[["SLOC"]]$datetimeMT, breaks="24 hours"),format="%m-%d", las=2)
title(main="Water depth below surface (cm)")

plot(ymd_hms(BEGI_EXO.or[["SLOC"]]$datetimeMT, tz="US/Mountain"),(BEGI_EXO.or[["SLOC"]]$Turbidity.FNU.mn),
     pch=20,col="black", xlab="", xaxt = "n", type="n", ylab="")
rect(xleft=pm.pts,xright=am.pts,ybottom=-4, ytop=2000, col="lightgrey", lwd = 0)
lines(ymd_hms(BEGI_EXO.or[["SLOC"]]$datetimeMT, tz="US/Mountain"),(BEGI_EXO.or[["SLOC"]]$Turbidity.FNU.mn),
      pch=20,col="black", xlab="", xaxt = "n", type="o")
abline(v=as.POSIXct(service), col="red")
axis.POSIXct(side=1,at=cut(BEGI_EXO.or[["SLOC"]]$datetimeMT, breaks="24 hours"),format="%m-%d", las=2)
title(main="Turbidity (FNU)")

plot(ymd_hms(BEGI_EXO.or[["SLOC"]]$datetimeMT, tz="US/Mountain"),(BEGI_EXO.or[["SLOC"]]$Depth.m.mn),pch=20,col="black", xlab="", xaxt = "n", type="n", ylab="", ylim = c(0,30))
rect(xleft=pm.pts,xright=am.pts,ybottom=0, ytop=2000, col="lightgrey", lwd = 0)
lines(ymd_hms(BEGI_EXO.or[["SLOC"]]$datetimeMT, tz="US/Mountain"),(BEGI_EXO.or[["SLOC"]]$Temp..C.mn),pch=20,col="black", xlab="", xaxt = "n", type="o")
abline(v=as.POSIXct(service), col="red")
axis.POSIXct(side=1,at=cut(BEGI_EXO.or[["SLOC"]]$datetimeMT, breaks="24 hours"),format="%m-%d", las=2)
title(main="Uncorrected Depth")

plot(ymd_hms(BEGI_EXO.or[["SLOC"]]$datetimeMT, tz="US/Mountain"),(BEGI_EXO.or[["SLOC"]]$ODO.mg.L.mn),
     pch=20,col="black", xlab="", xaxt = "n",ylim=c(-.4,10), type="n", ylab="")
rect(xleft=pm.pts,xright=am.pts,ybottom=-4, ytop=100, col="lightgrey", lwd = 0)
lines(ymd_hms(BEGI_EXO.or[["SLOC"]]$datetimeMT, tz="US/Mountain"),(BEGI_EXO.or[["SLOC"]]$ODO.mg.L.mn),
      pch=20,col="black", xlab="", xaxt = "n",ylim=c(-.4,1), type="o")
abline(v=as.POSIXct(service), col="red")
axis.POSIXct(side=1,at=cut(BEGI_EXO.or[["SLOC"]]$datetimeMT, breaks="24 hours"),format="%m-%d", las=2)
title(main="Dissolved Oxygen (mg/L)")

plot(ymd_hms(BEGI_EXO.or[["SLOC"]]$datetimeMT, tz="US/Mountain"),(BEGI_EXO.or[["SLOC"]]$Temp..C.mn),
     pch=20,col="black", xlab="", xaxt = "n", type="n", ylab="")
rect(xleft=pm.pts,xright=am.pts,ybottom=-4, ytop=100, col="lightgrey", lwd = 0)
lines(ymd_hms(BEGI_EXO.or[["SLOC"]]$datetimeMT, tz="US/Mountain"),(BEGI_EXO.or[["SLOC"]]$Temp..C.mn),
      pch=20,col="black", xlab="", xaxt = "n", type="o")#,ylim=c(22.5,24.5))
abline(v=as.POSIXct(service), col="red")
axis.POSIXct(side=1,at=cut(BEGI_EXO.or[["SLOC"]]$datetimeMT, breaks="24 hours"),format="%m-%d", las=2)
title(main="Temperature (deg C)")

plot(ymd_hms(BEGI_EXO.or[["SLOC"]]$datetimeMT, tz="US/Mountain"),(BEGI_EXO.or[["SLOC"]]$fDOM.QSU.mn),
     pch=20,col="black", xlab="", xaxt = "n", type="n",ylim=c(0,120), ylab="n")
rect(xleft=pm.pts,xright=am.pts,ybottom=-4, ytop=1000, col="lightgrey", lwd = 0)
lines(ymd_hms(BEGI_EXO.or[["SLOC"]]$datetimeMT, tz="US/Mountain"),(BEGI_EXO.or[["SLOC"]]$fDOM.QSU.mn),
      pch=20,col="black", xlab="", xaxt = "n", type="o")#,ylim=c(22.5,24.5))
abline(v=as.POSIXct(service), col="red")
axis.POSIXct(side=1,at=cut(BEGI_EXO.or[["SLOC"]]$datetimeMT, breaks="24 hours"),format="%m-%d", las=2)
title(main="fDOM (QSU)")

plot(ymd_hms(BEGI_EXO.or[["SLOC"]]$datetimeMT, tz="US/Mountain"),(BEGI_EXO.or[["SLOC"]]$SpCond.µS.cm.mn),
     pch=20,col="black", xlab="", xaxt = "n",ylim=c(-1,1300), type="n", ylab="")
rect(xleft=pm.pts,xright=am.pts,ybottom=-4, ytop=2000, col="lightgrey", lwd = 0)
lines(ymd_hms(BEGI_EXO.or[["SLOC"]]$datetimeMT, tz="US/Mountain"),(BEGI_EXO.or[["SLOC"]]$SpCond.µS.cm.mn),
      pch=20,col="black", xlab="", xaxt = "n", type="o")
abline(v=as.POSIXct(service), col="red")
axis.POSIXct(side=1,at=cut(BEGI_EXO.or[["SLOC"]]$datetimeMT, breaks="24 hours"),format="%m-%d", las=2)
title(main="Specific Conductance (us/cm)")

plot(ymd_hms(BEGI_EXO.or[["SLOC"]]$datetimeMT, tz="US/Mountain"),(BEGI_EXO.or[["SLOC"]]$Battery.V.mn),
     pch=20,col="black", xlab="", xaxt = "n", type="n", ylab="")
rect(xleft=pm.pts,xright=am.pts,ybottom=-4, ytop=2000, col="lightgrey", lwd = 0)
lines(ymd_hms(BEGI_EXO.or[["SLOC"]]$datetimeMT, tz="US/Mountain"),(BEGI_EXO.or[["SLOC"]]$Battery.V.mn),
      pch=20,col="black", xlab="", xaxt = "n", type="o")
abline(v=as.POSIXct(service), col="red")
axis.POSIXct(side=1,at=cut(BEGI_EXO.or[["SLOC"]]$datetimeMT, breaks="24 hours"),format="%m-%d", las=2)
title(main="Battery (volts)")

dev.off()





# 
# ## Plotting ONLY SLOC DO, zoomed in ##
# 
# #save plot
# 
# jpeg("plots/SLOC_DO.jpg", width = 20, height = 8, units="in", res=1000)
# 
# plot.new()
# 
# plot(ymd_hms(BEGI_EXO.or[["SLOC"]]$datetimeMT, tz="US/Mountain"),(BEGI_EXO.or[["SLOC"]]$ODO.mg.L.mn),
#      pch=20,col="black", xlab="", xaxt = "n",ylim=c(-1,.2), type="n", ylab="")
# rect(xleft=pm.pts,xright=am.pts,ybottom=-4, ytop=100, col="lightgrey", lwd = 0)
# lines(ymd_hms(BEGI_EXO.or[["SLOC"]]$datetimeMT, tz="US/Mountain"),(BEGI_EXO.or[["SLOC"]]$ODO.mg.L.mn),
#       pch=20,col="black", xlab="", xaxt = "n",ylim=c(-.4,1), type="o")
# abline(v=as.POSIXct(service), col="red")
# axis.POSIXct(side=1,at=cut(BEGI_EXO.or[["SLOC"]]$datetimeMT, breaks="24 hours"),format="%m-%d", las=2)
# title(main="Dissolved Oxygen (mg/L)")
# 
# 
# dev.off()

#++++++++++++++++++++++++++++++++

# 
# 
# # plot inset of DO and water depth or fDOM
# plot.new()
# par(mfrow=c(3,1), mar=c(7,4,2,1.5))
# plot(ymd_hms(BEGI_EXO.or[["SLOC"]]$datetimeMT, tz="US/Mountain"),(BEGI_EXO.or[["SLOC"]]$waterlevelbelowsurface_cm*-1),
#      pch=20,col="black", xlab="", xaxt = "n", type="n", ylab="", ylim=c(-10, 5))
# #rect(xleft=pm.pts,xright=am.pts,ybottom=-350, ytop=100, col="lightgrey")
# lines(ymd_hms(BEGI_EXO.or[["SLOC"]]$datetimeMT, tz="US/Mountain"),(BEGI_EXO.or[["SLOC"]]$waterlevelbelowsurface_cm*-1),
#       pch=20,col="black", xlab="", xaxt = "n", type="b")
# abline(v=as.POSIXct(service), col="red")
# #abline(h=-300, col="red")
# abline(h=0, col="green")
# axis.POSIXct(side=1,at=cut(BEGI_EXO.or[["SLOC"]]$datetimeMT, breaks="24 hours"),format="%m-%d", las=2)
# title(main="Water depth below surface (cm)")
# 
# plot(ymd_hms(BEGI_EXO.or[["SLOC"]]$datetimeMT, tz="US/Mountain"),(BEGI_EXO.or[["SLOC"]]$ODO.mg.L.mn),
#      pch=20,col="black", xlab="", xaxt = "n",ylim=c(-.4,1), type="n", ylab="",
#      xlim=c(as.POSIXct("2023-10-05"),as.POSIXct("2023-10-14")))
# rect(xleft=pm.pts,xright=am.pts,ybottom=-4, ytop=100, col="lightgrey")
# lines(ymd_hms(BEGI_EXO.or[["SLOC"]]$datetimeMT, tz="US/Mountain"),(BEGI_EXO.or[["SLOC"]]$ODO.mg.L.mn),
#       pch=20,col="black", xlab="", xaxt = "n",ylim=c(-.4,1), type="o")
# abline(v=as.POSIXct(service), col="red")
# axis.POSIXct(side=1,at=cut(BEGI_EXO.or[["SLOC"]]$datetimeMT, breaks="24 hours"),format="%m-%d %R", las=2)
# title(main="Dissolved Oxygen (mg/L)")
# 
# plot(ymd_hms(BEGI_EXO.or[["SLOC"]]$datetimeMT, tz="US/Mountain"),(BEGI_EXO.or[["SLOC"]]$fDOM.QSU.mn),
#      pch=20,col="black", xlab="", xaxt = "n",ylim=c(20,80), type="n", ylab="",
#      xlim=c(as.POSIXct("2023-10-05"),as.POSIXct("2023-10-14")))
# rect(xleft=pm.pts,xright=am.pts,ybottom=-4, ytop=250, col="lightgrey")
# lines(ymd_hms(BEGI_EXO.or[["SLOC"]]$datetimeMT, tz="US/Mountain"),(BEGI_EXO.or[["SLOC"]]$fDOM.QSU.mn),
#       pch=20,col="black", xlab="", xaxt = "n",ylim=c(-.4,1), type="o")
# abline(v=as.POSIXct(service), col="red")
# axis.POSIXct(side=1,at=cut(BEGI_EXO.or[["SLOC"]]$datetimeMT, breaks="24 hours"),format="%m-%d %R", las=2)
# title(main="fDOM (QSU)")
# 
# 

#### plot to check - SLOW ####

## SLOW last month ##
tempdat = BEGI_EXO.or[["SLOW"]][BEGI_EXO.or[["SLOW"]]$datetimeMT > Sys.time()-days(30),]

# Save plot 
jpeg("plots/SLOW_lastmonth.jpg", width = 12, height = 8, units="in", res=1000)

plot.new()
par(mfrow=c(4,2), mar=c(4,4,2,1.5))

plot(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"),(tempdat$waterlevelbelowsurface_cm*-1),
     pch=20,col="black", xlab="", xaxt = "n", type="n", ylab="")
rect(xleft=pm.pts,xright=am.pts,ybottom=-350, ytop=100, col="lightgrey", lwd = 0)
lines(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"),(tempdat$waterlevelbelowsurface_cm*-1),
      pch=20,col="black", xlab="", xaxt = "n", type="b")
abline(v=as.POSIXct(service), col="red")
#abline(h=-300, col="red")
abline(h=0, col="green")
axis.POSIXct(side=1,at=cut(tempdat$datetimeMT, breaks="24 hours"),format="%m-%d", las=2)
title(main="Water depth below surface (cm)")

plot(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"),(tempdat$Turbidity.FNU.mn),
     pch=20,col="black", xlab="", xaxt = "n", type="n", ylab="")
rect(xleft=pm.pts,xright=am.pts,ybottom=-4, ytop=2000, col="lightgrey", lwd = 0)
lines(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"),(tempdat$Turbidity.FNU.mn),
      pch=20,col="black", xlab="", xaxt = "n", type="o")
abline(v=as.POSIXct(service), col="red")
axis.POSIXct(side=1,at=cut(tempdat$datetimeMT, breaks="24 hours"),format="%m-%d", las=2)
title(main="Turbidity (FNU)")

plot(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"),(tempdat$Depth.m.mn),pch=20,col="black", xlab="", xaxt = "n", type="n", ylab="")
rect(xleft=pm.pts,xright=am.pts,ybottom=0, ytop=2000, col="lightgrey", lwd = 0)
lines(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"),(tempdat$Temp..C.mn),pch=20,col="black", xlab="", xaxt = "n", type="o")
abline(v=as.POSIXct(service), col="red")
axis.POSIXct(side=1,at=cut(tempdat$datetimeMT, breaks="24 hours"),format="%m-%d", las=2)
title(main="Uncorrected Depth")

plot(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"),(tempdat$ODO.mg.L.mn),
     pch=20,col="black", xlab="", xaxt = "n", type="n", ylab="")
rect(xleft=pm.pts,xright=am.pts,ybottom=-4, ytop=100, col="lightgrey", lwd = 0)
lines(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"),(tempdat$ODO.mg.L.mn),
      pch=20,col="black", xlab="", xaxt = "n", type="o")
abline(v=as.POSIXct(service), col="red")
axis.POSIXct(side=1,at=cut(tempdat$datetimeMT, breaks="24 hours"),format="%m-%d", las=2)
title(main="Dissolved Oxygen (mg/L)")

plot(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"),(tempdat$Temp..C.mn),
     pch=20,col="black", xlab="", xaxt = "n", type="n", ylab="")
rect(xleft=pm.pts,xright=am.pts,ybottom=-4, ytop=100, col="lightgrey", lwd = 0)
lines(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"),(tempdat$Temp..C.mn),
      pch=20,col="black", xlab="", xaxt = "n", type="o")#,ylim=c(22.5,24.5))
abline(v=as.POSIXct(service), col="red")
axis.POSIXct(side=1,at=cut(tempdat$datetimeMT, breaks="24 hours"),format="%m-%d", las=2)
title(main="Temperature (deg C)")

plot(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"),(tempdat$fDOM.QSU.mn),
     pch=20,col="black", xlab="", xaxt = "n", type="n", ylab="n")
rect(xleft=pm.pts,xright=am.pts,ybottom=-4, ytop=1000, col="lightgrey", lwd = 0)
lines(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"),(tempdat$fDOM.QSU.mn),
      pch=20,col="black", xlab="", xaxt = "n", type="o")#,ylim=c(22.5,24.5))
abline(v=as.POSIXct(service), col="red")
axis.POSIXct(side=1,at=cut(tempdat$datetimeMT, breaks="24 hours"),format="%m-%d", las=2)
title(main="fDOM (QSU)")

plot(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"),(tempdat$SpCond.µS.cm.mn),
     pch=20,col="black", xlab="", xaxt = "n", type="n", ylab="")
rect(xleft=pm.pts,xright=am.pts,ybottom=-4, ytop=2000, col="lightgrey", lwd = 0)
lines(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"),(tempdat$SpCond.µS.cm.mn),
      pch=20,col="black", xlab="", xaxt = "n", type="o")
abline(v=as.POSIXct(service), col="red")
axis.POSIXct(side=1,at=cut(tempdat$datetimeMT, breaks="24 hours"),format="%m-%d", las=2)
title(main="Specific Conductance (us/cm)")

plot(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"),(tempdat$Battery.V.mn),
     pch=20,col="black", xlab="", xaxt = "n", type="n", ylab="")
rect(xleft=pm.pts,xright=am.pts,ybottom=-4, ytop=2000, col="lightgrey", lwd = 0)
lines(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"),(tempdat$Battery.V.mn),
      pch=20,col="black", xlab="", xaxt = "n", type="o")
abline(v=as.POSIXct(service), col="red")
axis.POSIXct(side=1,at=cut(tempdat$datetimeMT, breaks="24 hours"),format="%m-%d", las=2)
title(main="Battery (volts)")

dev.off()


## SLOW ##

# Save plot 
jpeg("plots/SLOW.jpg", width = 12, height = 8, units="in", res=1000)

plot.new()
par(mfrow=c(4,2), mar=c(4,4,2,1.5))

plot(ymd_hms(BEGI_EXO.or[["SLOW"]]$datetimeMT, tz="US/Mountain"),(BEGI_EXO.or[["SLOW"]]$waterlevelbelowsurface_cm*-1),
     pch=20,col="black", xlab="", xaxt = "n", type="n", ylab="", ylim=c(-200, 10))
rect(xleft=pm.pts,xright=am.pts,ybottom=-350, ytop=100, col="lightgrey", lwd = 0)
lines(ymd_hms(BEGI_EXO.or[["SLOW"]]$datetimeMT, tz="US/Mountain"),(BEGI_EXO.or[["SLOW"]]$waterlevelbelowsurface_cm*-1),
      pch=20,col="black", xlab="", xaxt = "n", type="b")
abline(v=as.POSIXct(service), col="red")
#abline(h=-300, col="red")
abline(h=0, col="green")
axis.POSIXct(side=1,at=cut(BEGI_EXO.or[["SLOW"]]$datetimeMT, breaks="24 hours"),format="%m-%d", las=2)
title(main="Water depth below surface (cm)")

plot(ymd_hms(BEGI_EXO.or[["SLOW"]]$datetimeMT, tz="US/Mountain"),(BEGI_EXO.or[["SLOW"]]$Turbidity.FNU.mn),
     pch=20,col="black", xlab="", xaxt = "n", type="n", ylab="")
rect(xleft=pm.pts,xright=am.pts,ybottom=-4, ytop=2000, col="lightgrey", lwd = 0)
lines(ymd_hms(BEGI_EXO.or[["SLOW"]]$datetimeMT, tz="US/Mountain"),(BEGI_EXO.or[["SLOW"]]$Turbidity.FNU.mn),
      pch=20,col="black", xlab="", xaxt = "n", type="o")
abline(v=as.POSIXct(service), col="red")
axis.POSIXct(side=1,at=cut(BEGI_EXO.or[["SLOW"]]$datetimeMT, breaks="24 hours"),format="%m-%d", las=2)
title(main="Turbidity (FNU)")

plot(ymd_hms(BEGI_EXO.or[["SLOW"]]$datetimeMT, tz="US/Mountain"),(BEGI_EXO.or[["SLOW"]]$Depth.m.mn),pch=20,col="black", xlab="", xaxt = "n", type="n", ylab="", ylim = c(0,30))
rect(xleft=pm.pts,xright=am.pts,ybottom=0, ytop=2000, col="lightgrey", lwd = 0)
lines(ymd_hms(BEGI_EXO.or[["SLOW"]]$datetimeMT, tz="US/Mountain"),(BEGI_EXO.or[["SLOW"]]$Temp..C.mn),pch=20,col="black", xlab="", xaxt = "n", type="o")
abline(v=as.POSIXct(service), col="red")
axis.POSIXct(side=1,at=cut(BEGI_EXO.or[["SLOW"]]$datetimeMT, breaks="24 hours"),format="%m-%d", las=2)
title(main="Uncorrected Depth")

plot(ymd_hms(BEGI_EXO.or[["SLOW"]]$datetimeMT, tz="US/Mountain"),(BEGI_EXO.or[["SLOW"]]$ODO.mg.L.mn),
     pch=20,col="black", xlab="", xaxt = "n",ylim=c(-.4,10), type="n", ylab="")
rect(xleft=pm.pts,xright=am.pts,ybottom=-4, ytop=100, col="lightgrey", lwd = 0)
lines(ymd_hms(BEGI_EXO.or[["SLOW"]]$datetimeMT, tz="US/Mountain"),(BEGI_EXO.or[["SLOW"]]$ODO.mg.L.mn),
      pch=20,col="black", xlab="", xaxt = "n",ylim=c(-.4,1), type="o")
abline(v=as.POSIXct(service), col="red")
axis.POSIXct(side=1,at=cut(BEGI_EXO.or[["SLOW"]]$datetimeMT, breaks="24 hours"),format="%m-%d", las=2)
title(main="Dissolved Oxygen (mg/L)")

plot(ymd_hms(BEGI_EXO.or[["SLOW"]]$datetimeMT, tz="US/Mountain"),(BEGI_EXO.or[["SLOW"]]$Temp..C.mn),
     pch=20,col="black", xlab="", xaxt = "n", type="n", ylab="")
rect(xleft=pm.pts,xright=am.pts,ybottom=-4, ytop=100, col="lightgrey", lwd = 0)
lines(ymd_hms(BEGI_EXO.or[["SLOW"]]$datetimeMT, tz="US/Mountain"),(BEGI_EXO.or[["SLOW"]]$Temp..C.mn),
      pch=20,col="black", xlab="", xaxt = "n", type="o")#,ylim=c(22.5,24.5))
abline(v=as.POSIXct(service), col="red")
axis.POSIXct(side=1,at=cut(BEGI_EXO.or[["SLOW"]]$datetimeMT, breaks="24 hours"),format="%m-%d", las=2)
title(main="Temperature (deg C)")

plot(ymd_hms(BEGI_EXO.or[["SLOW"]]$datetimeMT, tz="US/Mountain"),(BEGI_EXO.or[["SLOW"]]$fDOM.QSU.mn),
     pch=20,col="black", xlab="", xaxt = "n", type="n",ylim=c(0,120), ylab="n")
rect(xleft=pm.pts,xright=am.pts,ybottom=-4, ytop=1000, col="lightgrey", lwd = 0)
lines(ymd_hms(BEGI_EXO.or[["SLOW"]]$datetimeMT, tz="US/Mountain"),(BEGI_EXO.or[["SLOW"]]$fDOM.QSU.mn),
      pch=20,col="black", xlab="", xaxt = "n", type="o")#,ylim=c(22.5,24.5))
abline(v=as.POSIXct(service), col="red")
axis.POSIXct(side=1,at=cut(BEGI_EXO.or[["SLOW"]]$datetimeMT, breaks="24 hours"),format="%m-%d", las=2)
title(main="fDOM (QSU)")

plot(ymd_hms(BEGI_EXO.or[["SLOW"]]$datetimeMT, tz="US/Mountain"),(BEGI_EXO.or[["SLOW"]]$SpCond.µS.cm.mn),
     pch=20,col="black", xlab="", xaxt = "n",ylim=c(-1,1300), type="n", ylab="")
rect(xleft=pm.pts,xright=am.pts,ybottom=-4, ytop=2000, col="lightgrey", lwd = 0)
lines(ymd_hms(BEGI_EXO.or[["SLOW"]]$datetimeMT, tz="US/Mountain"),(BEGI_EXO.or[["SLOW"]]$SpCond.µS.cm.mn),
      pch=20,col="black", xlab="", xaxt = "n", type="o")
abline(v=as.POSIXct(service), col="red")
axis.POSIXct(side=1,at=cut(BEGI_EXO.or[["SLOW"]]$datetimeMT, breaks="24 hours"),format="%m-%d", las=2)
title(main="Specific Conductance (us/cm)")

plot(ymd_hms(BEGI_EXO.or[["SLOW"]]$datetimeMT, tz="US/Mountain"),(BEGI_EXO.or[["SLOW"]]$Battery.V.mn),
     pch=20,col="black", xlab="", xaxt = "n", type="n", ylab="")
rect(xleft=pm.pts,xright=am.pts,ybottom=-4, ytop=2000, col="lightgrey", lwd = 0)
lines(ymd_hms(BEGI_EXO.or[["SLOW"]]$datetimeMT, tz="US/Mountain"),(BEGI_EXO.or[["SLOW"]]$Battery.V.mn),
      pch=20,col="black", xlab="", xaxt = "n", type="o")
abline(v=as.POSIXct(service), col="red")
axis.POSIXct(side=1,at=cut(BEGI_EXO.or[["SLOW"]]$datetimeMT, breaks="24 hours"),format="%m-%d", las=2)
title(main="Battery (volts)")

dev.off()



#### plot to check - VDOS ####

## VDOS last month ##
tempdat = BEGI_EXO.or[["VDOS"]][BEGI_EXO.or[["VDOS"]]$datetimeMT > Sys.time()-days(30),]

# Save plot 
jpeg("plots/VDOS_lastmonth.jpg", width = 12, height = 8, units="in", res=1000)

plot.new()
par(mfrow=c(4,2), mar=c(4,4,2,1.5))

plot(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"),(tempdat$waterlevelbelowsurface_cm*-1),
     pch=20,col="black", xlab="", xaxt = "n", type="n", ylab="")
rect(xleft=pm.pts,xright=am.pts,ybottom=-350, ytop=100, col="lightgrey", lwd = 0)
lines(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"),(tempdat$waterlevelbelowsurface_cm*-1),
      pch=20,col="black", xlab="", xaxt = "n", type="b")
abline(v=as.POSIXct(service), col="red")
#abline(h=-300, col="red")
abline(h=0, col="green")
axis.POSIXct(side=1,at=cut(tempdat$datetimeMT, breaks="24 hours"),format="%m-%d", las=2)
title(main="Water depth below surface (cm)")

plot(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"),(tempdat$Turbidity.FNU.mn),
     pch=20,col="black", xlab="", xaxt = "n", type="n", ylab="")
rect(xleft=pm.pts,xright=am.pts,ybottom=-4, ytop=2000, col="lightgrey", lwd = 0)
lines(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"),(tempdat$Turbidity.FNU.mn),
      pch=20,col="black", xlab="", xaxt = "n", type="o")
abline(v=as.POSIXct(service), col="red")
axis.POSIXct(side=1,at=cut(tempdat$datetimeMT, breaks="24 hours"),format="%m-%d", las=2)
title(main="Turbidity (FNU)")

plot(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"),(tempdat$Depth.m.mn),pch=20,col="black", xlab="", xaxt = "n", type="n", ylab="")
rect(xleft=pm.pts,xright=am.pts,ybottom=0, ytop=2000, col="lightgrey", lwd = 0)
lines(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"),(tempdat$Temp..C.mn),pch=20,col="black", xlab="", xaxt = "n", type="o")
abline(v=as.POSIXct(service), col="red")
axis.POSIXct(side=1,at=cut(tempdat$datetimeMT, breaks="24 hours"),format="%m-%d", las=2)
title(main="Uncorrected Depth")

plot(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"),(tempdat$ODO.mg.L.mn),
     pch=20,col="black", xlab="", xaxt = "n", type="n", ylab="")
rect(xleft=pm.pts,xright=am.pts,ybottom=-4, ytop=100, col="lightgrey", lwd = 0)
lines(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"),(tempdat$ODO.mg.L.mn),
      pch=20,col="black", xlab="", xaxt = "n", type="o")
abline(v=as.POSIXct(service), col="red")
axis.POSIXct(side=1,at=cut(tempdat$datetimeMT, breaks="24 hours"),format="%m-%d", las=2)
title(main="Dissolved Oxygen (mg/L)")

plot(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"),(tempdat$Temp..C.mn),
     pch=20,col="black", xlab="", xaxt = "n", type="n", ylab="")
rect(xleft=pm.pts,xright=am.pts,ybottom=-4, ytop=100, col="lightgrey", lwd = 0)
lines(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"),(tempdat$Temp..C.mn),
      pch=20,col="black", xlab="", xaxt = "n", type="o")#,ylim=c(22.5,24.5))
abline(v=as.POSIXct(service), col="red")
axis.POSIXct(side=1,at=cut(tempdat$datetimeMT, breaks="24 hours"),format="%m-%d", las=2)
title(main="Temperature (deg C)")

plot(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"),(tempdat$fDOM.QSU.mn),
     pch=20,col="black", xlab="", xaxt = "n", type="n", ylab="n")
rect(xleft=pm.pts,xright=am.pts,ybottom=-4, ytop=1000, col="lightgrey", lwd = 0)
lines(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"),(tempdat$fDOM.QSU.mn),
      pch=20,col="black", xlab="", xaxt = "n", type="o")#,ylim=c(22.5,24.5))
abline(v=as.POSIXct(service), col="red")
axis.POSIXct(side=1,at=cut(tempdat$datetimeMT, breaks="24 hours"),format="%m-%d", las=2)
title(main="fDOM (QSU)")

plot(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"),(tempdat$SpCond.µS.cm.mn),
     pch=20,col="black", xlab="", xaxt = "n", type="n", ylab="")
rect(xleft=pm.pts,xright=am.pts,ybottom=-4, ytop=2000, col="lightgrey", lwd = 0)
lines(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"),(tempdat$SpCond.µS.cm.mn),
      pch=20,col="black", xlab="", xaxt = "n", type="o")
abline(v=as.POSIXct(service), col="red")
axis.POSIXct(side=1,at=cut(tempdat$datetimeMT, breaks="24 hours"),format="%m-%d", las=2)
title(main="Specific Conductance (us/cm)")

plot(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"),(tempdat$Battery.V.mn),
     pch=20,col="black", xlab="", xaxt = "n", type="n", ylab="")
rect(xleft=pm.pts,xright=am.pts,ybottom=-4, ytop=2000, col="lightgrey", lwd = 0)
lines(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"),(tempdat$Battery.V.mn),
      pch=20,col="black", xlab="", xaxt = "n", type="o")
abline(v=as.POSIXct(service), col="red")
axis.POSIXct(side=1,at=cut(tempdat$datetimeMT, breaks="24 hours"),format="%m-%d", las=2)
title(main="Battery (volts)")

dev.off()


## VDOS ALL DATA ##

# Save plot 
jpeg("plots/VDOS.jpg", width = 12, height = 8, units="in", res=1000)

plot.new()
par(mfrow=c(4,2), mar=c(4,4,2,1.5))

plot(ymd_hms(BEGI_EXO.or[["VDOS"]]$datetimeMT, tz="US/Mountain"),(BEGI_EXO.or[["VDOS"]]$waterlevelbelowsurface_cm*-1),
     pch=20,col="black", xlab="", xaxt = "n", type="n", ylab="", ylim=c(-200, 10))
rect(xleft=pm.pts,xright=am.pts,ybottom=-350, ytop=100, col="lightgrey", lwd = 0)
lines(ymd_hms(BEGI_EXO.or[["VDOS"]]$datetimeMT, tz="US/Mountain"),(BEGI_EXO.or[["VDOS"]]$waterlevelbelowsurface_cm*-1),
      pch=20,col="black", xlab="", xaxt = "n", type="b")
abline(v=as.POSIXct(service), col="red")
#abline(h=-300, col="red")
abline(h=0, col="green")
axis.POSIXct(side=1,at=cut(BEGI_EXO.or[["VDOS"]]$datetimeMT, breaks="24 hours"),format="%m-%d", las=2)
title(main="Water depth below surface (cm)")

plot(ymd_hms(BEGI_EXO.or[["VDOS"]]$datetimeMT, tz="US/Mountain"),(BEGI_EXO.or[["VDOS"]]$Turbidity.FNU.mn),
     pch=20,col="black", xlab="", xaxt = "n", type="n", ylab="")
rect(xleft=pm.pts,xright=am.pts,ybottom=-4, ytop=2000, col="lightgrey", lwd = 0)
lines(ymd_hms(BEGI_EXO.or[["VDOS"]]$datetimeMT, tz="US/Mountain"),(BEGI_EXO.or[["VDOS"]]$Turbidity.FNU.mn),
      pch=20,col="black", xlab="", xaxt = "n", type="o")
abline(v=as.POSIXct(service), col="red")
axis.POSIXct(side=1,at=cut(BEGI_EXO.or[["VDOS"]]$datetimeMT, breaks="24 hours"),format="%m-%d", las=2)
title(main="Turbidity (FNU)")

plot(ymd_hms(BEGI_EXO.or[["VDOS"]]$datetimeMT, tz="US/Mountain"),(BEGI_EXO.or[["VDOS"]]$Depth.m.mn),pch=20,col="black", xlab="", xaxt = "n", type="n", ylab="", ylim = c(0,30))
rect(xleft=pm.pts,xright=am.pts,ybottom=0, ytop=2000, col="lightgrey", lwd = 0)
lines(ymd_hms(BEGI_EXO.or[["VDOS"]]$datetimeMT, tz="US/Mountain"),(BEGI_EXO.or[["VDOS"]]$Temp..C.mn),pch=20,col="black", xlab="", xaxt = "n", type="o")
abline(v=as.POSIXct(service), col="red")
axis.POSIXct(side=1,at=cut(BEGI_EXO.or[["VDOS"]]$datetimeMT, breaks="24 hours"),format="%m-%d", las=2)
title(main="Uncorrected Depth")

plot(ymd_hms(BEGI_EXO.or[["VDOS"]]$datetimeMT, tz="US/Mountain"),(BEGI_EXO.or[["VDOS"]]$ODO.mg.L.mn),
     pch=20,col="black", xlab="", xaxt = "n",ylim=c(-.4,10), type="n", ylab="")
rect(xleft=pm.pts,xright=am.pts,ybottom=-4, ytop=100, col="lightgrey", lwd = 0)
lines(ymd_hms(BEGI_EXO.or[["VDOS"]]$datetimeMT, tz="US/Mountain"),(BEGI_EXO.or[["VDOS"]]$ODO.mg.L.mn),
      pch=20,col="black", xlab="", xaxt = "n",ylim=c(-.4,1), type="o")
abline(v=as.POSIXct(service), col="red")
axis.POSIXct(side=1,at=cut(BEGI_EXO.or[["VDOS"]]$datetimeMT, breaks="24 hours"),format="%m-%d", las=2)
title(main="Dissolved Oxygen (mg/L)")

plot(ymd_hms(BEGI_EXO.or[["VDOS"]]$datetimeMT, tz="US/Mountain"),(BEGI_EXO.or[["VDOS"]]$Temp..C.mn),
     pch=20,col="black", xlab="", xaxt = "n", type="n", ylab="")
rect(xleft=pm.pts,xright=am.pts,ybottom=-4, ytop=100, col="lightgrey", lwd = 0)
lines(ymd_hms(BEGI_EXO.or[["VDOS"]]$datetimeMT, tz="US/Mountain"),(BEGI_EXO.or[["VDOS"]]$Temp..C.mn),
      pch=20,col="black", xlab="", xaxt = "n", type="o")#,ylim=c(22.5,24.5))
abline(v=as.POSIXct(service), col="red")
axis.POSIXct(side=1,at=cut(BEGI_EXO.or[["VDOS"]]$datetimeMT, breaks="24 hours"),format="%m-%d", las=2)
title(main="Temperature (deg C)")

plot(ymd_hms(BEGI_EXO.or[["VDOS"]]$datetimeMT, tz="US/Mountain"),(BEGI_EXO.or[["VDOS"]]$fDOM.QSU.mn),
     pch=20,col="black", xlab="", xaxt = "n", type="n",ylim=c(0,120), ylab="n")
rect(xleft=pm.pts,xright=am.pts,ybottom=-4, ytop=1000, col="lightgrey", lwd = 0)
lines(ymd_hms(BEGI_EXO.or[["VDOS"]]$datetimeMT, tz="US/Mountain"),(BEGI_EXO.or[["VDOS"]]$fDOM.QSU.mn),
      pch=20,col="black", xlab="", xaxt = "n", type="o")#,ylim=c(22.5,24.5))
abline(v=as.POSIXct(service), col="red")
axis.POSIXct(side=1,at=cut(BEGI_EXO.or[["VDOS"]]$datetimeMT, breaks="24 hours"),format="%m-%d", las=2)
title(main="fDOM (QSU)")

plot(ymd_hms(BEGI_EXO.or[["VDOS"]]$datetimeMT, tz="US/Mountain"),(BEGI_EXO.or[["VDOS"]]$SpCond.µS.cm.mn),
     pch=20,col="black", xlab="", xaxt = "n",ylim=c(-1,1300), type="n", ylab="")
rect(xleft=pm.pts,xright=am.pts,ybottom=-4, ytop=2000, col="lightgrey", lwd = 0)
lines(ymd_hms(BEGI_EXO.or[["VDOS"]]$datetimeMT, tz="US/Mountain"),(BEGI_EXO.or[["VDOS"]]$SpCond.µS.cm.mn),
      pch=20,col="black", xlab="", xaxt = "n", type="o")
abline(v=as.POSIXct(service), col="red")
axis.POSIXct(side=1,at=cut(BEGI_EXO.or[["VDOS"]]$datetimeMT, breaks="24 hours"),format="%m-%d", las=2)
title(main="Specific Conductance (us/cm)")

plot(ymd_hms(BEGI_EXO.or[["VDOS"]]$datetimeMT, tz="US/Mountain"),(BEGI_EXO.or[["VDOS"]]$Battery.V.mn),
     pch=20,col="black", xlab="", xaxt = "n", type="n", ylab="")
rect(xleft=pm.pts,xright=am.pts,ybottom=-4, ytop=2000, col="lightgrey", lwd = 0)
lines(ymd_hms(BEGI_EXO.or[["VDOS"]]$datetimeMT, tz="US/Mountain"),(BEGI_EXO.or[["VDOS"]]$Battery.V.mn),
      pch=20,col="black", xlab="", xaxt = "n", type="o")
abline(v=as.POSIXct(service), col="red")
axis.POSIXct(side=1,at=cut(BEGI_EXO.or[["VDOS"]]$datetimeMT, breaks="24 hours"),format="%m-%d", las=2)
title(main="Battery (volts)")

dev.off()

#### plot to check - VDOW ####


## VDOW last month ##
tempdat = BEGI_EXO.or[["VDOW"]][BEGI_EXO.or[["VDOW"]]$datetimeMT > Sys.time()-days(30),]

# Save plot 
jpeg("plots/VDOW_lastmonth.jpg", width = 12, height = 8, units="in", res=1000)

plot.new()
par(mfrow=c(4,2), mar=c(4,4,2,1.5))

plot(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"),(tempdat$waterlevelbelowsurface_cm*-1),
     pch=20,col="black", xlab="", xaxt = "n", type="n", ylab="")
rect(xleft=pm.pts,xright=am.pts,ybottom=-350, ytop=100, col="lightgrey", lwd = 0)
lines(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"),(tempdat$waterlevelbelowsurface_cm*-1),
      pch=20,col="black", xlab="", xaxt = "n", type="b")
abline(v=as.POSIXct(service), col="red")
#abline(h=-300, col="red")
abline(h=0, col="green")
axis.POSIXct(side=1,at=cut(tempdat$datetimeMT, breaks="24 hours"),format="%m-%d", las=2)
title(main="Water depth below surface (cm)")

plot(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"),(tempdat$Turbidity.FNU.mn),
     pch=20,col="black", xlab="", xaxt = "n", type="n", ylab="")
rect(xleft=pm.pts,xright=am.pts,ybottom=-4, ytop=2000, col="lightgrey", lwd = 0)
lines(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"),(tempdat$Turbidity.FNU.mn),
      pch=20,col="black", xlab="", xaxt = "n", type="o")
abline(v=as.POSIXct(service), col="red")
axis.POSIXct(side=1,at=cut(tempdat$datetimeMT, breaks="24 hours"),format="%m-%d", las=2)
title(main="Turbidity (FNU)")

plot(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"),(tempdat$Depth.m.mn),pch=20,col="black", xlab="", xaxt = "n", type="n", ylab="")
rect(xleft=pm.pts,xright=am.pts,ybottom=0, ytop=2000, col="lightgrey", lwd = 0)
lines(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"),(tempdat$Temp..C.mn),pch=20,col="black", xlab="", xaxt = "n", type="o")
abline(v=as.POSIXct(service), col="red")
axis.POSIXct(side=1,at=cut(tempdat$datetimeMT, breaks="24 hours"),format="%m-%d", las=2)
title(main="Uncorrected Depth")

plot(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"),(tempdat$ODO.mg.L.mn),
     pch=20,col="black", xlab="", xaxt = "n", type="n", ylab="")
rect(xleft=pm.pts,xright=am.pts,ybottom=-4, ytop=100, col="lightgrey", lwd = 0)
lines(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"),(tempdat$ODO.mg.L.mn),
      pch=20,col="black", xlab="", xaxt = "n", type="o")
abline(v=as.POSIXct(service), col="red")
axis.POSIXct(side=1,at=cut(tempdat$datetimeMT, breaks="24 hours"),format="%m-%d", las=2)
title(main="Dissolved Oxygen (mg/L)")

plot(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"),(tempdat$Temp..C.mn),
     pch=20,col="black", xlab="", xaxt = "n", type="n", ylab="")
rect(xleft=pm.pts,xright=am.pts,ybottom=-4, ytop=100, col="lightgrey", lwd = 0)
lines(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"),(tempdat$Temp..C.mn),
      pch=20,col="black", xlab="", xaxt = "n", type="o")#,ylim=c(22.5,24.5))
abline(v=as.POSIXct(service), col="red")
axis.POSIXct(side=1,at=cut(tempdat$datetimeMT, breaks="24 hours"),format="%m-%d", las=2)
title(main="Temperature (deg C)")

plot(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"),(tempdat$fDOM.QSU.mn),
     pch=20,col="black", xlab="", xaxt = "n", type="n", ylab="n")
rect(xleft=pm.pts,xright=am.pts,ybottom=-4, ytop=1000, col="lightgrey", lwd = 0)
lines(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"),(tempdat$fDOM.QSU.mn),
      pch=20,col="black", xlab="", xaxt = "n", type="o")#,ylim=c(22.5,24.5))
abline(v=as.POSIXct(service), col="red")
axis.POSIXct(side=1,at=cut(tempdat$datetimeMT, breaks="24 hours"),format="%m-%d", las=2)
title(main="fDOM (QSU)")

plot(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"),(tempdat$SpCond.µS.cm.mn),
     pch=20,col="black", xlab="", xaxt = "n", type="n", ylab="")
rect(xleft=pm.pts,xright=am.pts,ybottom=-4, ytop=2000, col="lightgrey", lwd = 0)
lines(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"),(tempdat$SpCond.µS.cm.mn),
      pch=20,col="black", xlab="", xaxt = "n", type="o")
abline(v=as.POSIXct(service), col="red")
axis.POSIXct(side=1,at=cut(tempdat$datetimeMT, breaks="24 hours"),format="%m-%d", las=2)
title(main="Specific Conductance (us/cm)")

plot(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"),(tempdat$Battery.V.mn),
     pch=20,col="black", xlab="", xaxt = "n", type="n", ylab="")
rect(xleft=pm.pts,xright=am.pts,ybottom=-4, ytop=2000, col="lightgrey", lwd = 0)
lines(ymd_hms(tempdat$datetimeMT, tz="US/Mountain"),(tempdat$Battery.V.mn),
      pch=20,col="black", xlab="", xaxt = "n", type="o")
abline(v=as.POSIXct(service), col="red")
axis.POSIXct(side=1,at=cut(tempdat$datetimeMT, breaks="24 hours"),format="%m-%d", las=2)
title(main="Battery (volts)")

dev.off()



## VDOW ALL DATA ##

# Save plot 
jpeg("plots/VDOW.jpg", width = 12, height = 8, units="in", res=1000)

plot.new()
par(mfrow=c(4,2), mar=c(4,4,2,1.5))

plot(ymd_hms(BEGI_EXO.or[["VDOW"]]$datetimeMT, tz="US/Mountain"),(BEGI_EXO.or[["VDOW"]]$waterlevelbelowsurface_cm*-1),
     pch=20,col="black", xlab="", xaxt = "n", type="n", ylab="", ylim=c(-200, 10))
rect(xleft=pm.pts,xright=am.pts,ybottom=-350, ytop=100, col="lightgrey", lwd = 0)
lines(ymd_hms(BEGI_EXO.or[["VDOW"]]$datetimeMT, tz="US/Mountain"),(BEGI_EXO.or[["VDOW"]]$waterlevelbelowsurface_cm*-1),
      pch=20,col="black", xlab="", xaxt = "n", type="b")
abline(v=as.POSIXct(service), col="red")
#abline(h=-300, col="red")
abline(h=0, col="green")
axis.POSIXct(side=1,at=cut(BEGI_EXO.or[["VDOW"]]$datetimeMT, breaks="24 hours"),format="%m-%d", las=2)
title(main="Water depth below surface (cm)")

plot(ymd_hms(BEGI_EXO.or[["VDOW"]]$datetimeMT, tz="US/Mountain"),(BEGI_EXO.or[["VDOW"]]$Turbidity.FNU.mn),
     pch=20,col="black", xlab="", xaxt = "n", type="n", ylab="")
rect(xleft=pm.pts,xright=am.pts,ybottom=-4, ytop=2000, col="lightgrey", lwd = 0)
lines(ymd_hms(BEGI_EXO.or[["VDOW"]]$datetimeMT, tz="US/Mountain"),(BEGI_EXO.or[["VDOW"]]$Turbidity.FNU.mn),
      pch=20,col="black", xlab="", xaxt = "n", type="o")
abline(v=as.POSIXct(service), col="red")
axis.POSIXct(side=1,at=cut(BEGI_EXO.or[["VDOW"]]$datetimeMT, breaks="24 hours"),format="%m-%d", las=2)
title(main="Turbidity (FNU)")

plot(ymd_hms(BEGI_EXO.or[["VDOW"]]$datetimeMT, tz="US/Mountain"),(BEGI_EXO.or[["VDOW"]]$Depth.m.mn),pch=20,col="black", xlab="", xaxt = "n", type="n", ylab="", ylim = c(0,30))
rect(xleft=pm.pts,xright=am.pts,ybottom=0, ytop=2000, col="lightgrey", lwd = 0)
lines(ymd_hms(BEGI_EXO.or[["VDOW"]]$datetimeMT, tz="US/Mountain"),(BEGI_EXO.or[["VDOW"]]$Temp..C.mn),pch=20,col="black", xlab="", xaxt = "n", type="o")
abline(v=as.POSIXct(service), col="red")
axis.POSIXct(side=1,at=cut(BEGI_EXO.or[["VDOW"]]$datetimeMT, breaks="24 hours"),format="%m-%d", las=2)
title(main="Uncorrected Depth")

plot(ymd_hms(BEGI_EXO.or[["VDOW"]]$datetimeMT, tz="US/Mountain"),(BEGI_EXO.or[["VDOW"]]$ODO.mg.L.mn),
     pch=20,col="black", xlab="", xaxt = "n",ylim=c(-.4,10), type="n", ylab="")
rect(xleft=pm.pts,xright=am.pts,ybottom=-4, ytop=100, col="lightgrey", lwd = 0)
lines(ymd_hms(BEGI_EXO.or[["VDOW"]]$datetimeMT, tz="US/Mountain"),(BEGI_EXO.or[["VDOW"]]$ODO.mg.L.mn),
      pch=20,col="black", xlab="", xaxt = "n",ylim=c(-.4,1), type="o")
abline(v=as.POSIXct(service), col="red")
axis.POSIXct(side=1,at=cut(BEGI_EXO.or[["VDOW"]]$datetimeMT, breaks="24 hours"),format="%m-%d", las=2)
title(main="Dissolved Oxygen (mg/L)")

plot(ymd_hms(BEGI_EXO.or[["VDOW"]]$datetimeMT, tz="US/Mountain"),(BEGI_EXO.or[["VDOW"]]$Temp..C.mn),
     pch=20,col="black", xlab="", xaxt = "n", type="n", ylab="")
rect(xleft=pm.pts,xright=am.pts,ybottom=-4, ytop=100, col="lightgrey", lwd = 0)
lines(ymd_hms(BEGI_EXO.or[["VDOW"]]$datetimeMT, tz="US/Mountain"),(BEGI_EXO.or[["VDOW"]]$Temp..C.mn),
      pch=20,col="black", xlab="", xaxt = "n", type="o")#,ylim=c(22.5,24.5))
abline(v=as.POSIXct(service), col="red")
axis.POSIXct(side=1,at=cut(BEGI_EXO.or[["VDOW"]]$datetimeMT, breaks="24 hours"),format="%m-%d", las=2)
title(main="Temperature (deg C)")

plot(ymd_hms(BEGI_EXO.or[["VDOW"]]$datetimeMT, tz="US/Mountain"),(BEGI_EXO.or[["VDOW"]]$fDOM.QSU.mn),
     pch=20,col="black", xlab="", xaxt = "n", type="n",ylim=c(0,120), ylab="n")
rect(xleft=pm.pts,xright=am.pts,ybottom=-4, ytop=1000, col="lightgrey", lwd = 0)
lines(ymd_hms(BEGI_EXO.or[["VDOW"]]$datetimeMT, tz="US/Mountain"),(BEGI_EXO.or[["VDOW"]]$fDOM.QSU.mn),
      pch=20,col="black", xlab="", xaxt = "n", type="o")#,ylim=c(22.5,24.5))
abline(v=as.POSIXct(service), col="red")
axis.POSIXct(side=1,at=cut(BEGI_EXO.or[["VDOW"]]$datetimeMT, breaks="24 hours"),format="%m-%d", las=2)
title(main="fDOM (QSU)")

plot(ymd_hms(BEGI_EXO.or[["VDOW"]]$datetimeMT, tz="US/Mountain"),(BEGI_EXO.or[["VDOW"]]$SpCond.µS.cm.mn),
     pch=20,col="black", xlab="", xaxt = "n",ylim=c(-1,1300), type="n", ylab="")
rect(xleft=pm.pts,xright=am.pts,ybottom=-4, ytop=2000, col="lightgrey", lwd = 0)
lines(ymd_hms(BEGI_EXO.or[["VDOW"]]$datetimeMT, tz="US/Mountain"),(BEGI_EXO.or[["VDOW"]]$SpCond.µS.cm.mn),
      pch=20,col="black", xlab="", xaxt = "n", type="o")
abline(v=as.POSIXct(service), col="red")
axis.POSIXct(side=1,at=cut(BEGI_EXO.or[["VDOW"]]$datetimeMT, breaks="24 hours"),format="%m-%d", las=2)
title(main="Specific Conductance (us/cm)")

plot(ymd_hms(BEGI_EXO.or[["VDOW"]]$datetimeMT, tz="US/Mountain"),(BEGI_EXO.or[["VDOW"]]$Battery.V.mn),
     pch=20,col="black", xlab="", xaxt = "n", type="n", ylab="")
rect(xleft=pm.pts,xright=am.pts,ybottom=-4, ytop=2000, col="lightgrey", lwd = 0)
lines(ymd_hms(BEGI_EXO.or[["VDOW"]]$datetimeMT, tz="US/Mountain"),(BEGI_EXO.or[["VDOW"]]$Battery.V.mn),
      pch=20,col="black", xlab="", xaxt = "n", type="o")
abline(v=as.POSIXct(service), col="red")
axis.POSIXct(side=1,at=cut(BEGI_EXO.or[["VDOW"]]$datetimeMT, breaks="24 hours"),format="%m-%d", las=2)
title(main="Battery (volts)")

dev.off()





#
#### just well depths ####

# plot all together
plot.new()
par(mfrow=c(2,2), mar=c(4,4,2,1.5))

plot(ymd_hms(BEGI_EXO.stz[["VDOS"]]$datetimeMT, tz="US/Mountain"),(BEGI_EXO.stz[["VDOS"]]$waterlevelbelowsurface_cm*-1),
     pch=20,col="black", xlab="", xaxt = "n", type="n", ylab="", ylim=c(-200, 10))
points(ymd_hms(BEGI_EXO.stz[["VDOS"]]$datetimeMT, tz="US/Mountain"),(BEGI_EXO.stz[["VDOS"]]$waterlevelbelowsurface_cm*-1),
       pch=20,col="black", xlab="", xaxt = "n", ylab="", ylim=c(-200, 10))
#abline(v=as.POSIXct(service), col="red")
abline(h=0, col="green")
axis.POSIXct(side=1,at=cut(BEGI_EXO.stz[["VDOS"]]$datetimeMT, breaks="24 hours"),format="%m-%d", las=2)
title(main="VDO South")

plot(ymd_hms(BEGI_EXO.stz[["VDOW"]]$datetimeMT, tz="US/Mountain"),(BEGI_EXO.stz[["VDOW"]]$waterlevelbelowsurface_cm*-1),
     pch=20,col="black", xlab="", xaxt = "n", type="n", ylab="", ylim=c(-200, 10))
points(ymd_hms(BEGI_EXO.stz[["VDOW"]]$datetimeMT, tz="US/Mountain"),(BEGI_EXO.stz[["VDOW"]]$waterlevelbelowsurface_cm*-1),
       pch=20,col="black", xlab="", xaxt = "n", ylab="", ylim=c(-200, 10))
#abline(v=as.POSIXct(service), col="red")
abline(h=0, col="green")
axis.POSIXct(side=1,at=cut(BEGI_EXO.stz[["VDOW"]]$datetimeMT, breaks="24 hours"),format="%m-%d", las=2)
title(main="VDO West")

plot(ymd_hms(BEGI_EXO.stz[["SLOC"]]$datetimeMT, tz="US/Mountain"),(BEGI_EXO.stz[["SLOC"]]$waterlevelbelowsurface_cm*-1),
     pch=20,col="black", xlab="", xaxt = "n", type="n", ylab="", ylim=c(-200, 10))
points(ymd_hms(BEGI_EXO.stz[["SLOC"]]$datetimeMT, tz="US/Mountain"),(BEGI_EXO.stz[["SLOC"]]$waterlevelbelowsurface_cm*-1),
       pch=20,col="black", xlab="", xaxt = "n", ylab="", ylim=c(-200, 10))
#abline(v=as.POSIXct(service), col="red")
abline(h=0, col="green")
axis.POSIXct(side=1,at=cut(BEGI_EXO.stz[["SLOC"]]$datetimeMT, breaks="24 hours"),format="%m-%d", las=2)
title(main="SLO Center")

plot(ymd_hms(BEGI_EXO.stz[["SLOW"]]$datetimeMT, tz="US/Mountain"),(BEGI_EXO.stz[["SLOW"]]$waterlevelbelowsurface_cm*-1),
     pch=20,col="black", xlab="", xaxt = "n", type="n", ylab="", ylim=c(-200, 10))
points(ymd_hms(BEGI_EXO.stz[["SLOW"]]$datetimeMT, tz="US/Mountain"),(BEGI_EXO.stz[["SLOW"]]$waterlevelbelowsurface_cm*-1),
       pch=20,col="black", xlab="", xaxt = "n", ylab="", ylim=c(-200, 10))
#abline(v=as.POSIXct(service), col="red")
abline(h=0, col="green")
axis.POSIXct(side=1,at=cut(BEGI_EXO.stz[["SLOW"]]$datetimeMT, breaks="24 hours"),format="%m-%d", las=2)
title(main="SLO West")


#### extra plots ####

# smoothed DO close up
BEGI_EXO.stz[["SLOC"]]$ODO.mg.L.mn_sm =
  c(rollmean(BEGI_EXO.stz[["SLOC"]]$ODO.mg.L.mn, 4, align="left"),
    NA,NA,NA)

BEGI_EXO.stz[["SLOC"]]$ODO.mg.L.mn_sm_bc =
  BEGI_EXO.stz[["SLOC"]]$ODO.mg.L.mn_sm +
  abs(min(BEGI_EXO.stz[["SLOC"]]$ODO.mg.L.mn_sm, na.rm = T))

BEGI_EXO.stz[["SLOC"]]$ODO.mg.L.mn_sm_bc_c = BEGI_EXO.stz[["SLOC"]]$ODO.mg.L.mn_sm_bc

# BEGI_EXO.stz[["SLOC"]]$ODO.mg.L.mn_sm_bc_c[BEGI_EXO.stz[["SLOC"]]$datetimeMT >
#                                              as.POSIXct("2023-11-01 08:00:00") &
#                                              BEGI_EXO.stz[["SLOC"]]$datetimeMT <
#                                              as.POSIXct("2023-12-09 15:00:00")   ] = NA
plot.new()
par(mfrow=c(1,1), mar=c(7,4,2,1.5))
plot(ymd_hms(BEGI_EXO.stz[["SLOC"]]$datetimeMT, tz="US/Mountain"),
     BEGI_EXO.stz[["SLOC"]]$ODO.mg.L.mn_sm_bc_c,
     pch=20,col="black", xlab="", xaxt = "n",ylim=c(0,0.8), type="n", ylab="",
     xlim=c(as.POSIXct("2023-11-01"),as.POSIXct("2024-01-06")))
rect(xleft=pm.pts,xright=am.pts,ybottom=-4, ytop=100, col="lightgrey", lwd = 0)
lines(ymd_hms(BEGI_EXO.stz[["SLOC"]]$datetimeMT, tz="US/Mountain"),
      BEGI_EXO.stz[["SLOC"]]$ODO.mg.L.mn_sm_bc_c,
      pch=20,col="black", xlab="", xaxt = "n", type="o")
#abline(v=as.POSIXct(service), col="red")
axis.POSIXct(side=1,at=cut(BEGI_EXO.stz[["SLOC"]]$datetimeMT, breaks="24 hours"),format="%m-%d %R", las=2)
title(main="Dissolved Oxygen (mg/L)")


### just latest DO and fDOM ###

# SLOC
plot.new()
par(mfrow=c(2,1), mar=c(7,4,2,1.5))
# DO
plot(ymd_hms(BEGI_EXO.stz[["SLOC"]]$datetimeMT, tz="US/Mountain"),(BEGI_EXO.stz[["SLOC"]]$ODO.mg.L.mn),
     pch=20,col="black", xlab="", xaxt = "n",ylim=c(-.4,3), type="n", ylab="",
     xlim=c(as.POSIXct("2024-03-01"),as.POSIXct("2024-04-18")))
rect(xleft=pm.pts,xright=am.pts,ybottom=-4, ytop=100, col="lightgrey", lwd = 0)
lines(ymd_hms(BEGI_EXO.stz[["SLOC"]]$datetimeMT, tz="US/Mountain"),(BEGI_EXO.stz[["SLOC"]]$ODO.mg.L.mn),
      pch=20,col="black", xlab="", xaxt = "n",ylim=c(-.4,1), type="o")
abline(v=as.POSIXct(service), col="red")
axis.POSIXct(side=1,at=cut(BEGI_EXO.stz[["SLOC"]]$datetimeMT, breaks="24 hours"),format="%m-%d", las=2)
title(main="Dissolved Oxygen (mg/L)")
# fDOM
plot(ymd_hms(BEGI_EXO.stz[["SLOC"]]$datetimeMT, tz="US/Mountain"),(BEGI_EXO.stz[["SLOC"]]$fDOM.QSU.mn),
     pch=20,col="black", xlab="", xaxt = "n",ylim=c(40,100), type="n", ylab="",
     xlim=c(as.POSIXct("2024-03-01"),as.POSIXct("2024-04-18")))
rect(xleft=pm.pts,xright=am.pts,ybottom=-4, ytop=100, col="lightgrey", lwd = 0)
lines(ymd_hms(BEGI_EXO.stz[["SLOC"]]$datetimeMT, tz="US/Mountain"),(BEGI_EXO.stz[["SLOC"]]$fDOM.QSU.mn),
      pch=20,col="black", xlab="", xaxt = "n",ylim=c(-.4,1), type="o")
abline(v=as.POSIXct(service), col="red")
axis.POSIXct(side=1,at=cut(BEGI_EXO.stz[["SLOC"]]$datetimeMT, breaks="24 hours"),format="%m-%d", las=2)
title(main="fDOM (QSU)")

# SLOW
plot.new()
par(mfrow=c(2,1), mar=c(7,4,2,1.5))
# DO
plot(ymd_hms(BEGI_EXO.stz[["SLOW"]]$datetimeMT, tz="US/Mountain"),(BEGI_EXO.stz[["SLOW"]]$ODO.mg.L.mn),
     pch=20,col="black", xlab="", xaxt = "n",ylim=c(-.4,3), type="n", ylab="",
     xlim=c(as.POSIXct("2024-03-01"),as.POSIXct("2024-04-18")))
rect(xleft=pm.pts,xright=am.pts,ybottom=-4, ytop=100, col="lightgrey", lwd = 0)
lines(ymd_hms(BEGI_EXO.stz[["SLOW"]]$datetimeMT, tz="US/Mountain"),(BEGI_EXO.stz[["SLOW"]]$ODO.mg.L.mn),
      pch=20,col="black", xlab="", xaxt = "n",ylim=c(-.4,1), type="o")
abline(v=as.POSIXct(service), col="red")
axis.POSIXct(side=1,at=cut(BEGI_EXO.stz[["SLOW"]]$datetimeMT, breaks="24 hours"),format="%m-%d", las=2)
title(main="Dissolved Oxygen (mg/L)")
# fDOM
plot(ymd_hms(BEGI_EXO.stz[["SLOW"]]$datetimeMT, tz="US/Mountain"),(BEGI_EXO.stz[["SLOW"]]$fDOM.QSU.mn),
     pch=20,col="black", xlab="", xaxt = "n",ylim=c(40,100), type="n", ylab="",
     xlim=c(as.POSIXct("2024-03-01"),as.POSIXct("2024-04-18")))
rect(xleft=pm.pts,xright=am.pts,ybottom=-4, ytop=100, col="lightgrey", lwd = 0)
lines(ymd_hms(BEGI_EXO.stz[["SLOW"]]$datetimeMT, tz="US/Mountain"),(BEGI_EXO.stz[["SLOW"]]$fDOM.QSU.mn),
      pch=20,col="black", xlab="", xaxt = "n",ylim=c(-.4,1), type="o")
abline(v=as.POSIXct(service), col="red")
axis.POSIXct(side=1,at=cut(BEGI_EXO.stz[["SLOW"]]$datetimeMT, breaks="24 hours"),format="%m-%d", las=2)
title(main="fDOM (QSU)")

# VDOW
plot.new()
par(mfrow=c(2,1), mar=c(7,4,2,1.5))
# DO
plot(ymd_hms(BEGI_EXO.stz[["VDOW"]]$datetimeMT, tz="US/Mountain"),(BEGI_EXO.stz[["VDOW"]]$ODO.mg.L.mn),
     pch=20,col="black", xlab="", xaxt = "n",ylim=c(-.4,3), type="n", ylab="",
     xlim=c(as.POSIXct("2024-03-01"),as.POSIXct("2024-04-18")))
rect(xleft=pm.pts,xright=am.pts,ybottom=-4, ytop=100, col="lightgrey", lwd = 0)
lines(ymd_hms(BEGI_EXO.stz[["VDOW"]]$datetimeMT, tz="US/Mountain"),(BEGI_EXO.stz[["VDOW"]]$ODO.mg.L.mn),
      pch=20,col="black", xlab="", xaxt = "n",ylim=c(-.4,1), type="o")
abline(v=as.POSIXct(service), col="red")
axis.POSIXct(side=1,at=cut(BEGI_EXO.stz[["VDOW"]]$datetimeMT, breaks="24 hours"),format="%m-%d", las=2)
title(main="Dissolved Oxygen (mg/L)")
# fDOM
plot(ymd_hms(BEGI_EXO.stz[["VDOW"]]$datetimeMT, tz="US/Mountain"),(BEGI_EXO.stz[["VDOW"]]$fDOM.QSU.mn),
     pch=20,col="black", xlab="", xaxt = "n",ylim=c(40,100), type="n", ylab="",
     xlim=c(as.POSIXct("2024-03-01"),as.POSIXct("2024-04-18")))
rect(xleft=pm.pts,xright=am.pts,ybottom=-4, ytop=100, col="lightgrey", lwd = 0)
lines(ymd_hms(BEGI_EXO.stz[["VDOW"]]$datetimeMT, tz="US/Mountain"),(BEGI_EXO.stz[["VDOW"]]$fDOM.QSU.mn),
      pch=20,col="black", xlab="", xaxt = "n",ylim=c(-.4,1), type="o")
abline(v=as.POSIXct(service), col="red")
axis.POSIXct(side=1,at=cut(BEGI_EXO.stz[["VDOW"]]$datetimeMT, breaks="24 hours"),format="%m-%d", las=2)
title(main="fDOM (QSU)")


# VDOS
plot.new()
par(mfrow=c(2,1), mar=c(7,4,2,1.5))
# DO
plot(ymd_hms(BEGI_EXO.stz[["VDOS"]]$datetimeMT, tz="US/Mountain"),(BEGI_EXO.stz[["VDOS"]]$ODO.mg.L.mn),
     pch=20,col="black", xlab="", xaxt = "n",ylim=c(-.4,3), type="n", ylab="",
     xlim=c(as.POSIXct("2024-03-01"),as.POSIXct("2024-04-18")))
rect(xleft=pm.pts,xright=am.pts,ybottom=-4, ytop=100, col="lightgrey", lwd = 0)
lines(ymd_hms(BEGI_EXO.stz[["VDOS"]]$datetimeMT, tz="US/Mountain"),(BEGI_EXO.stz[["VDOS"]]$ODO.mg.L.mn),
      pch=20,col="black", xlab="", xaxt = "n",ylim=c(-.4,1), type="o")
abline(v=as.POSIXct(service), col="red")
axis.POSIXct(side=1,at=cut(BEGI_EXO.stz[["VDOS"]]$datetimeMT, breaks="24 hours"),format="%m-%d", las=2)
title(main="Dissolved Oxygen (mg/L)")
# fDOM
plot(ymd_hms(BEGI_EXO.stz[["VDOS"]]$datetimeMT, tz="US/Mountain"),(BEGI_EXO.stz[["VDOS"]]$fDOM.QSU.mn),
     pch=20,col="black", xlab="", xaxt = "n",ylim=c(40,100), type="n", ylab="",
     xlim=c(as.POSIXct("2024-03-01"),as.POSIXct("2024-04-18")))
rect(xleft=pm.pts,xright=am.pts,ybottom=-4, ytop=100, col="lightgrey", lwd = 0)
lines(ymd_hms(BEGI_EXO.stz[["VDOS"]]$datetimeMT, tz="US/Mountain"),(BEGI_EXO.stz[["VDOS"]]$fDOM.QSU.mn),
      pch=20,col="black", xlab="", xaxt = "n",ylim=c(-.4,1), type="o")
abline(v=as.POSIXct(service), col="red")
axis.POSIXct(side=1,at=cut(BEGI_EXO.stz[["VDOS"]]$datetimeMT, breaks="24 hours"),format="%m-%d", las=2)
title(main="fDOM (QSU)")