library(tidyverse)
library(data.table)
library(dplyr)
###############################################################################
### Hubbard Brook Grab Sample Data                                          ###
### The code snippet below is pulled directly from lternet to download data ###
### from Hubbard Brook                                                      ###
###############################################################################
#####

inUrl2  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-hbr/208/9/3b3cf7ea447cb875d7c7d68ebdfd24c7" 
infile2 <- tempfile()
try(download.file(inUrl2,infile2,method="curl"))
if (is.na(file.size(infile2))) download.file(inUrl2,infile2,method="auto")


dt2 <-read.csv(infile2,header=F 
               ,skip=1
               ,sep=","  
               ,quot='"' 
               , col.names=c(
                 "site",     
                 "date",     
                 "timeEST",     
                 "barcode",     
                 "pH",     
                 "DIC",     
                 "spCond",     
                 "temp",     
                 "ANC960",     
                 "ANCMet",     
                 "gageHt",     
                 "hydroGraph",     
                 "flowGageHt",     
                 "fieldCode",     
                 "notes",     
                 "uniqueID",     
                 "waterYr",     
                 "Ca",     
                 "Mg",     
                 "K",     
                 "Na",     
                 "TMAl",     
                 "OMAl",     
                 "Al_ICP",     
                 "Al_ferron",     
                 "NH4",     
                 "SO4",     
                 "NO3",     
                 "Cl",     
                 "PO4",     
                 "DOC",     
                 "TDN",     
                 "DON",     
                 "SiO2",     
                 "Mn",     
                 "Fe",     
                 "F",     
                 "cationCharge",     
                 "anionCharge",     
                 "ionError",     
                 "duplicate",     
                 "sampleType",     
                 "ionBalance",     
                 "canonical",     
                 "pHmetrohm"    ), check.names=TRUE)

unlink(infile2)

# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings

if (class(dt2$site)!="factor") dt2$site<- as.factor(dt2$site)                                   
# attempting to convert dt2$date dateTime string to R date structure (date or POSIXct)                                
tmpDateFormat<-"%Y-%m-%d"
tmp2date<-as.Date(dt2$date,format=tmpDateFormat)
# Keep the new dates only if they all converted correctly
if(length(tmp2date) == length(tmp2date[!is.na(tmp2date)])){dt2$date <- tmp2date } else {print("Date conversion failed for dt2$date. Please inspect the data and do the date conversion yourself.")}                                                                    
rm(tmpDateFormat,tmp2date) 
if (class(dt2$barcode)!="factor") dt2$barcode<- as.factor(dt2$barcode)
if (class(dt2$pH)=="factor") dt2$pH <-as.numeric(levels(dt2$pH))[as.integer(dt2$pH) ]               
if (class(dt2$pH)=="character") dt2$pH <-as.numeric(dt2$pH)
if (class(dt2$DIC)=="factor") dt2$DIC <-as.numeric(levels(dt2$DIC))[as.integer(dt2$DIC) ]               
if (class(dt2$DIC)=="character") dt2$DIC <-as.numeric(dt2$DIC)
if (class(dt2$spCond)=="factor") dt2$spCond <-as.numeric(levels(dt2$spCond))[as.integer(dt2$spCond) ]               
if (class(dt2$spCond)=="character") dt2$spCond <-as.numeric(dt2$spCond)
if (class(dt2$temp)=="factor") dt2$temp <-as.numeric(levels(dt2$temp))[as.integer(dt2$temp) ]               
if (class(dt2$temp)=="character") dt2$temp <-as.numeric(dt2$temp)
if (class(dt2$ANC960)=="factor") dt2$ANC960 <-as.numeric(levels(dt2$ANC960))[as.integer(dt2$ANC960) ]               
if (class(dt2$ANC960)=="character") dt2$ANC960 <-as.numeric(dt2$ANC960)
if (class(dt2$ANCMet)=="factor") dt2$ANCMet <-as.numeric(levels(dt2$ANCMet))[as.integer(dt2$ANCMet) ]               
if (class(dt2$ANCMet)=="character") dt2$ANCMet <-as.numeric(dt2$ANCMet)
if (class(dt2$gageHt)=="factor") dt2$gageHt <-as.numeric(levels(dt2$gageHt))[as.integer(dt2$gageHt) ]               
if (class(dt2$gageHt)=="character") dt2$gageHt <-as.numeric(dt2$gageHt)
if (class(dt2$hydroGraph)!="factor") dt2$hydroGraph<- as.factor(dt2$hydroGraph)
if (class(dt2$flowGageHt)=="factor") dt2$flowGageHt <-as.numeric(levels(dt2$flowGageHt))[as.integer(dt2$flowGageHt) ]               
if (class(dt2$flowGageHt)=="character") dt2$flowGageHt <-as.numeric(dt2$flowGageHt)
if (class(dt2$fieldCode)!="factor") dt2$fieldCode<- as.factor(dt2$fieldCode)
if (class(dt2$notes)!="factor") dt2$notes<- as.factor(dt2$notes)
if (class(dt2$uniqueID)!="factor") dt2$uniqueID<- as.factor(dt2$uniqueID)
if (class(dt2$Ca)=="factor") dt2$Ca <-as.numeric(levels(dt2$Ca))[as.integer(dt2$Ca) ]               
if (class(dt2$Ca)=="character") dt2$Ca <-as.numeric(dt2$Ca)
if (class(dt2$Mg)=="factor") dt2$Mg <-as.numeric(levels(dt2$Mg))[as.integer(dt2$Mg) ]               
if (class(dt2$Mg)=="character") dt2$Mg <-as.numeric(dt2$Mg)
if (class(dt2$K)=="factor") dt2$K <-as.numeric(levels(dt2$K))[as.integer(dt2$K) ]               
if (class(dt2$K)=="character") dt2$K <-as.numeric(dt2$K)
if (class(dt2$Na)=="factor") dt2$Na <-as.numeric(levels(dt2$Na))[as.integer(dt2$Na) ]               
if (class(dt2$Na)=="character") dt2$Na <-as.numeric(dt2$Na)
if (class(dt2$TMAl)=="factor") dt2$TMAl <-as.numeric(levels(dt2$TMAl))[as.integer(dt2$TMAl) ]               
if (class(dt2$TMAl)=="character") dt2$TMAl <-as.numeric(dt2$TMAl)
if (class(dt2$OMAl)=="factor") dt2$OMAl <-as.numeric(levels(dt2$OMAl))[as.integer(dt2$OMAl) ]               
if (class(dt2$OMAl)=="character") dt2$OMAl <-as.numeric(dt2$OMAl)
if (class(dt2$Al_ICP)=="factor") dt2$Al_ICP <-as.numeric(levels(dt2$Al_ICP))[as.integer(dt2$Al_ICP) ]               
if (class(dt2$Al_ICP)=="character") dt2$Al_ICP <-as.numeric(dt2$Al_ICP)
if (class(dt2$Al_ferron)=="factor") dt2$Al_ferron <-as.numeric(levels(dt2$Al_ferron))[as.integer(dt2$Al_ferron) ]               
if (class(dt2$Al_ferron)=="character") dt2$Al_ferron <-as.numeric(dt2$Al_ferron)
if (class(dt2$NH4)=="factor") dt2$NH4 <-as.numeric(levels(dt2$NH4))[as.integer(dt2$NH4) ]               
if (class(dt2$NH4)=="character") dt2$NH4 <-as.numeric(dt2$NH4)
if (class(dt2$SO4)=="factor") dt2$SO4 <-as.numeric(levels(dt2$SO4))[as.integer(dt2$SO4) ]               
if (class(dt2$SO4)=="character") dt2$SO4 <-as.numeric(dt2$SO4)
if (class(dt2$NO3)=="factor") dt2$NO3 <-as.numeric(levels(dt2$NO3))[as.integer(dt2$NO3) ]               
if (class(dt2$NO3)=="character") dt2$NO3 <-as.numeric(dt2$NO3)
if (class(dt2$Cl)=="factor") dt2$Cl <-as.numeric(levels(dt2$Cl))[as.integer(dt2$Cl) ]               
if (class(dt2$Cl)=="character") dt2$Cl <-as.numeric(dt2$Cl)
if (class(dt2$PO4)=="factor") dt2$PO4 <-as.numeric(levels(dt2$PO4))[as.integer(dt2$PO4) ]               
if (class(dt2$PO4)=="character") dt2$PO4 <-as.numeric(dt2$PO4)
if (class(dt2$DOC)=="factor") dt2$DOC <-as.numeric(levels(dt2$DOC))[as.integer(dt2$DOC) ]               
if (class(dt2$DOC)=="character") dt2$DOC <-as.numeric(dt2$DOC)
if (class(dt2$TDN)=="factor") dt2$TDN <-as.numeric(levels(dt2$TDN))[as.integer(dt2$TDN) ]               
if (class(dt2$TDN)=="character") dt2$TDN <-as.numeric(dt2$TDN)
if (class(dt2$DON)=="factor") dt2$DON <-as.numeric(levels(dt2$DON))[as.integer(dt2$DON) ]               
if (class(dt2$DON)=="character") dt2$DON <-as.numeric(dt2$DON)
if (class(dt2$SiO2)=="factor") dt2$SiO2 <-as.numeric(levels(dt2$SiO2))[as.integer(dt2$SiO2) ]               
if (class(dt2$SiO2)=="character") dt2$SiO2 <-as.numeric(dt2$SiO2)
if (class(dt2$Mn)=="factor") dt2$Mn <-as.numeric(levels(dt2$Mn))[as.integer(dt2$Mn) ]               
if (class(dt2$Mn)=="character") dt2$Mn <-as.numeric(dt2$Mn)
if (class(dt2$Fe)=="factor") dt2$Fe <-as.numeric(levels(dt2$Fe))[as.integer(dt2$Fe) ]               
if (class(dt2$Fe)=="character") dt2$Fe <-as.numeric(dt2$Fe)
if (class(dt2$F)=="factor") dt2$F <-as.numeric(levels(dt2$F))[as.integer(dt2$F) ]               
if (class(dt2$F)=="character") dt2$F <-as.numeric(dt2$F)
if (class(dt2$cationCharge)=="factor") dt2$cationCharge <-as.numeric(levels(dt2$cationCharge))[as.integer(dt2$cationCharge) ]               
if (class(dt2$cationCharge)=="character") dt2$cationCharge <-as.numeric(dt2$cationCharge)
if (class(dt2$anionCharge)=="factor") dt2$anionCharge <-as.numeric(levels(dt2$anionCharge))[as.integer(dt2$anionCharge) ]               
if (class(dt2$anionCharge)=="character") dt2$anionCharge <-as.numeric(dt2$anionCharge)
if (class(dt2$ionError)=="factor") dt2$ionError <-as.numeric(levels(dt2$ionError))[as.integer(dt2$ionError) ]               
if (class(dt2$ionError)=="character") dt2$ionError <-as.numeric(dt2$ionError)
if (class(dt2$duplicate)!="factor") dt2$duplicate<- as.factor(dt2$duplicate)
if (class(dt2$sampleType)!="factor") dt2$sampleType<- as.factor(dt2$sampleType)
if (class(dt2$ionBalance)=="factor") dt2$ionBalance <-as.numeric(levels(dt2$ionBalance))[as.integer(dt2$ionBalance) ]               
if (class(dt2$ionBalance)=="character") dt2$ionBalance <-as.numeric(dt2$ionBalance)
if (class(dt2$canonical)!="factor") dt2$canonical<- as.factor(dt2$canonical)
if (class(dt2$pHmetrohm)!="factor") dt2$pHmetrohm<- as.factor(dt2$pHmetrohm)

# Convert Missing Values to NA for non-dates

dt2$site <- as.factor(ifelse((trimws(as.character(dt2$site))==trimws("NA")),NA,as.character(dt2$site)))
dt2$barcode <- as.factor(ifelse((trimws(as.character(dt2$barcode))==trimws("NA")),NA,as.character(dt2$barcode)))
dt2$pH <- ifelse((trimws(as.character(dt2$pH))==trimws("NA")),NA,dt2$pH)               
suppressWarnings(dt2$pH <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$pH))==as.character(as.numeric("NA"))),NA,dt2$pH))
dt2$DIC <- ifelse((trimws(as.character(dt2$DIC))==trimws("NA")),NA,dt2$DIC)               
suppressWarnings(dt2$DIC <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$DIC))==as.character(as.numeric("NA"))),NA,dt2$DIC))
dt2$spCond <- ifelse((trimws(as.character(dt2$spCond))==trimws("NA")),NA,dt2$spCond)               
suppressWarnings(dt2$spCond <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$spCond))==as.character(as.numeric("NA"))),NA,dt2$spCond))
dt2$temp <- ifelse((trimws(as.character(dt2$temp))==trimws("NA")),NA,dt2$temp)               
suppressWarnings(dt2$temp <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$temp))==as.character(as.numeric("NA"))),NA,dt2$temp))
dt2$ANC960 <- ifelse((trimws(as.character(dt2$ANC960))==trimws("NA")),NA,dt2$ANC960)               
suppressWarnings(dt2$ANC960 <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$ANC960))==as.character(as.numeric("NA"))),NA,dt2$ANC960))
dt2$ANCMet <- ifelse((trimws(as.character(dt2$ANCMet))==trimws("NA")),NA,dt2$ANCMet)               
suppressWarnings(dt2$ANCMet <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$ANCMet))==as.character(as.numeric("NA"))),NA,dt2$ANCMet))
dt2$gageHt <- ifelse((trimws(as.character(dt2$gageHt))==trimws("NA")),NA,dt2$gageHt)               
suppressWarnings(dt2$gageHt <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$gageHt))==as.character(as.numeric("NA"))),NA,dt2$gageHt))
dt2$hydroGraph <- as.factor(ifelse((trimws(as.character(dt2$hydroGraph))==trimws("NA")),NA,as.character(dt2$hydroGraph)))
dt2$flowGageHt <- ifelse((trimws(as.character(dt2$flowGageHt))==trimws("NA")),NA,dt2$flowGageHt)               
suppressWarnings(dt2$flowGageHt <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$flowGageHt))==as.character(as.numeric("NA"))),NA,dt2$flowGageHt))
dt2$fieldCode <- as.factor(ifelse((trimws(as.character(dt2$fieldCode))==trimws("NA")),NA,as.character(dt2$fieldCode)))
dt2$notes <- as.factor(ifelse((trimws(as.character(dt2$notes))==trimws("NA")),NA,as.character(dt2$notes)))
dt2$uniqueID <- as.factor(ifelse((trimws(as.character(dt2$uniqueID))==trimws("NA")),NA,as.character(dt2$uniqueID)))
dt2$Ca <- ifelse((trimws(as.character(dt2$Ca))==trimws("NA")),NA,dt2$Ca)               
suppressWarnings(dt2$Ca <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$Ca))==as.character(as.numeric("NA"))),NA,dt2$Ca))
dt2$Mg <- ifelse((trimws(as.character(dt2$Mg))==trimws("NA")),NA,dt2$Mg)               
suppressWarnings(dt2$Mg <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$Mg))==as.character(as.numeric("NA"))),NA,dt2$Mg))
dt2$K <- ifelse((trimws(as.character(dt2$K))==trimws("NA")),NA,dt2$K)               
suppressWarnings(dt2$K <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$K))==as.character(as.numeric("NA"))),NA,dt2$K))
dt2$Na <- ifelse((trimws(as.character(dt2$Na))==trimws("NA")),NA,dt2$Na)               
suppressWarnings(dt2$Na <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$Na))==as.character(as.numeric("NA"))),NA,dt2$Na))
dt2$TMAl <- ifelse((trimws(as.character(dt2$TMAl))==trimws("NA")),NA,dt2$TMAl)               
suppressWarnings(dt2$TMAl <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$TMAl))==as.character(as.numeric("NA"))),NA,dt2$TMAl))
dt2$OMAl <- ifelse((trimws(as.character(dt2$OMAl))==trimws("NA")),NA,dt2$OMAl)               
suppressWarnings(dt2$OMAl <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$OMAl))==as.character(as.numeric("NA"))),NA,dt2$OMAl))
dt2$Al_ICP <- ifelse((trimws(as.character(dt2$Al_ICP))==trimws("NA")),NA,dt2$Al_ICP)               
suppressWarnings(dt2$Al_ICP <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$Al_ICP))==as.character(as.numeric("NA"))),NA,dt2$Al_ICP))
dt2$Al_ferron <- ifelse((trimws(as.character(dt2$Al_ferron))==trimws("NA")),NA,dt2$Al_ferron)               
suppressWarnings(dt2$Al_ferron <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$Al_ferron))==as.character(as.numeric("NA"))),NA,dt2$Al_ferron))
dt2$NH4 <- ifelse((trimws(as.character(dt2$NH4))==trimws("NA")),NA,dt2$NH4)               
suppressWarnings(dt2$NH4 <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$NH4))==as.character(as.numeric("NA"))),NA,dt2$NH4))
dt2$SO4 <- ifelse((trimws(as.character(dt2$SO4))==trimws("NA")),NA,dt2$SO4)               
suppressWarnings(dt2$SO4 <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$SO4))==as.character(as.numeric("NA"))),NA,dt2$SO4))
dt2$NO3 <- ifelse((trimws(as.character(dt2$NO3))==trimws("NA")),NA,dt2$NO3)               
suppressWarnings(dt2$NO3 <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$NO3))==as.character(as.numeric("NA"))),NA,dt2$NO3))
dt2$Cl <- ifelse((trimws(as.character(dt2$Cl))==trimws("NA")),NA,dt2$Cl)               
suppressWarnings(dt2$Cl <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$Cl))==as.character(as.numeric("NA"))),NA,dt2$Cl))
dt2$PO4 <- ifelse((trimws(as.character(dt2$PO4))==trimws("NA")),NA,dt2$PO4)               
suppressWarnings(dt2$PO4 <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$PO4))==as.character(as.numeric("NA"))),NA,dt2$PO4))
dt2$DOC <- ifelse((trimws(as.character(dt2$DOC))==trimws("NA")),NA,dt2$DOC)               
suppressWarnings(dt2$DOC <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$DOC))==as.character(as.numeric("NA"))),NA,dt2$DOC))
dt2$TDN <- ifelse((trimws(as.character(dt2$TDN))==trimws("NA")),NA,dt2$TDN)               
suppressWarnings(dt2$TDN <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$TDN))==as.character(as.numeric("NA"))),NA,dt2$TDN))
dt2$DON <- ifelse((trimws(as.character(dt2$DON))==trimws("NA")),NA,dt2$DON)               
suppressWarnings(dt2$DON <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$DON))==as.character(as.numeric("NA"))),NA,dt2$DON))
dt2$SiO2 <- ifelse((trimws(as.character(dt2$SiO2))==trimws("NA")),NA,dt2$SiO2)               
suppressWarnings(dt2$SiO2 <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$SiO2))==as.character(as.numeric("NA"))),NA,dt2$SiO2))
dt2$Mn <- ifelse((trimws(as.character(dt2$Mn))==trimws("NA")),NA,dt2$Mn)               
suppressWarnings(dt2$Mn <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$Mn))==as.character(as.numeric("NA"))),NA,dt2$Mn))
dt2$Fe <- ifelse((trimws(as.character(dt2$Fe))==trimws("NA")),NA,dt2$Fe)               
suppressWarnings(dt2$Fe <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$Fe))==as.character(as.numeric("NA"))),NA,dt2$Fe))
dt2$F <- ifelse((trimws(as.character(dt2$F))==trimws("NA")),NA,dt2$F)               
suppressWarnings(dt2$F <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$F))==as.character(as.numeric("NA"))),NA,dt2$F))
dt2$cationCharge <- ifelse((trimws(as.character(dt2$cationCharge))==trimws("NA")),NA,dt2$cationCharge)               
suppressWarnings(dt2$cationCharge <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$cationCharge))==as.character(as.numeric("NA"))),NA,dt2$cationCharge))
dt2$anionCharge <- ifelse((trimws(as.character(dt2$anionCharge))==trimws("NA")),NA,dt2$anionCharge)               
suppressWarnings(dt2$anionCharge <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$anionCharge))==as.character(as.numeric("NA"))),NA,dt2$anionCharge))
dt2$ionError <- ifelse((trimws(as.character(dt2$ionError))==trimws("NA")),NA,dt2$ionError)               
suppressWarnings(dt2$ionError <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$ionError))==as.character(as.numeric("NA"))),NA,dt2$ionError))
dt2$duplicate <- as.factor(ifelse((trimws(as.character(dt2$duplicate))==trimws("NA")),NA,as.character(dt2$duplicate)))
dt2$sampleType <- as.factor(ifelse((trimws(as.character(dt2$sampleType))==trimws("NA")),NA,as.character(dt2$sampleType)))
dt2$ionBalance <- ifelse((trimws(as.character(dt2$ionBalance))==trimws("NA")),NA,dt2$ionBalance)               
suppressWarnings(dt2$ionBalance <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$ionBalance))==as.character(as.numeric("NA"))),NA,dt2$ionBalance))
dt2$canonical <- as.factor(ifelse((trimws(as.character(dt2$canonical))==trimws("NA")),NA,as.character(dt2$canonical)))
dt2$pHmetrohm <- as.factor(ifelse((trimws(as.character(dt2$pHmetrohm))==trimws("NA")),NA,as.character(dt2$pHmetrohm)))


##########################################################################
### The snippet below is to wrangle our data into a usable form for W9 ###
##########################################################################

# Filtering data to only include W9 data from 2013 onwards.
# Also converts the datetime into a datetime format and removes duplicates
# Finally filters data to only include columns of interest
dfGrab <- data.frame(dt2)
dfGrab <- dfGrab[dfGrab$site == 'W9' & dfGrab$waterYr >= 2013,]
dfGrab$DATETIME <- paste(dfGrab$date, dfGrab$timeEST, sep=" ")
dfGrab$DATETIME<-as.POSIXct(dfGrab$DATETIME, format = '%Y-%m-%d %H:%M')
dfGrab <- dfGrab[!(dfGrab$duplicate %in% "Dup"),]
dfGrab$BkpGrabDate <- dfGrab$DATETIME

dfGrab <- dfGrab[ , names(dfGrab) %in% c("Ca", "Mg", "K", "Na", "NH4", "SO4", "NO3", 
                                         "Cl", "PO4", "DOC", "DON", "DATETIME", "BkpGrabDate")]


###################
### W9 Cleaning ###
###################

# W9 sensor data is currently pulled from a local file.
dfSensor<-read.csv('Raw_Data/W9_Sensor_Data.csv')
colnames(dfSensor)

# Changing Date to Datetime was merely to keep things consistent across dataframes
setnames(dfSensor, old = c("Date"), 
         new = c('DATETIME'))

# Converting the datetime column to an actual datetime data type
# DOYSin and DOYCos was included to mimic seasonality in the dataset
dfSensor$DATETIME<-as.POSIXct(dfSensor$DATETIME, format = '%m/%d/%Y %H:%M')
dfSensor$DOY <- yday(dfSensor$DATETIME)
dfSensor$DOYSin <- sin((dfSensor$DOY-173) * (2*pi)/365.25) #Day 173 = June 22nd. DOYSin of 1 is around Sept 21st
dfSensor$DOYCos <- cos((dfSensor$DOY-173) * (2*pi)/365.25)
dfSensor$BkpSenseDate <- dfSensor$DATETIME

#Changing column datatypes and removing unnecessary columns
dfSensor <- dfSensor %>% mutate_at(c('Nitrate_mg', 'TempC', 'Conductivity', 'SpConductivity', 'pH',
                                     'ODOPerCent', 'ODOMGL', 'TurbidityFNU', 'FDOMRFU',
                                     'FDOMQSU', 'ABS254_SUNA'), as.numeric)
dfSensor <- dfSensor[ , names(dfSensor) %in% c('DATETIME', 'Nitrate_mg', 'TempC', 'SpConductivity',
                                               'FDOMQSU', 'DOYSin', 'DOYCos','BkpSenseDate', 'pH',
                                               'ODOPerCent', 'ODOMGL')]


# Data tables are used here for the join function below
dtSensor <- as.data.table(dfSensor)
dtGrab <- as.data.table(dfGrab)

# Rolling join to join grab sample data and sensor data
# Grab sample data and sensor data must be within 900 seconds (15 minutes)
setkey( dtSensor, DATETIME )
setkey( dtGrab, DATETIME )
dfDisSenseGrab <- dtSensor[ dtGrab, roll = "nearest" ]
str(dfDisSenseGrab)
dfDisSenseGrab$SenseGrabTimeDiff <- abs(dfDisSenseGrab$BkpSenseDate - dfDisSenseGrab$BkpGrabDate)
dfDisSenseGrab <- subset(dfDisSenseGrab, SenseGrabTimeDiff  < (900))
dfDisSenseGrab <- dfDisSenseGrab %>% drop_na()
sapply(dfDisSenseGrab, function(x) sum(is.na(x)))


# Changing names to match other dataframes
setnames(dfDisSenseGrab, old = c("FDOMQSU","Nitrate_mg"), 
         new = c("FDOM_corrected_QSU","NO3_corrected"))

# Renames for use in all other programs
W9TrainingData <- dfDisSenseGrab

str(W9TrainingData)


##########################################################################
##########################################################################
### The snippet below is to wrangle our data into a usable form for W3 ###
##########################################################################
##########################################################################

# Filtering data to only include W3 data from 2013 onwards.
# Also converts the datetime into a datetime format and removes duplicates
# Finally filters data to only include columns of interest
dfGrab <- data.frame(dt2)
dfGrab <- dfGrab[dfGrab$site == 'W3' & dfGrab$waterYr >= 2013,]

# Converting the datetime column to an actual datetime data type
# DOYSin and DOYCos was included to mimic seasonality in the dataset
dfGrab$DATETIME <- paste(dfGrab$date, dfGrab$timeEST, sep=" ")
dfGrab$DATETIME<-as.POSIXct(dfGrab$DATETIME, format = '%Y-%m-%d %H:%M')
dfGrab <- dfGrab[!(dfGrab$duplicate %in% "Dup"),]
dfGrab$BkpGrabDate <- dfGrab$DATETIME

dfGrab <- dfGrab[ , names(dfGrab) %in% c("Ca", "Mg", "K", "Na", "NH4", "SO4", "NO3", 
                                         "Cl", "PO4", "DOC", "DON", "DATETIME", "BkpGrabDate")]

###################
### W3 Cleaning ###
###################

# W9 sensor data is currently pulled from a local file.
dfSensor<-read.csv('Raw_Data/HBF_WQual_Level4.csv')
dfSensor$DATETIME<-as.POSIXct(dfSensor$Date, format = '%m/%d/%Y %H:%M')
dfSensor$DOY <- yday(dfSensor$DATETIME)
dfSensor$DOYSin <- sin((dfSensor$DOY-173) * (2*pi)/365.25) #Day 173 = June 22nd. DOYSin of 1 is around Sept 21st
dfSensor$DOYCos <- cos((dfSensor$DOY-173) * (2*pi)/365.25)
dfSensor$BkpSenseDate <- dfSensor$DATETIME

# Removing unnecessary columns
dfSensor <- dfSensor[ , names(dfSensor) %in% c('DATETIME', 'Nitrate_mg', 'TempC', 'SpConductivity',
                                               'FDOMQSU', 'DOYSin', 'DOYCos','BkpSenseDate', 'pH',
                                               'ODOPerCent', 'ODOMGL')]

# Data tables are used here for the join function below
dtSensor <- as.data.table(dfSensor)
dtGrab <- as.data.table(dfGrab)

# Rolling join to join grab sample data and sensor data
# Grab sample data and sensor data must be within 15 minutes
setkey( dtSensor, DATETIME )
setkey( dtGrab, DATETIME )
dfDisSenseGrab <- dtSensor[ dtGrab, roll = "nearest" ]
dfDisSenseGrab$SenseGrabTimeDiff <- abs(dfDisSenseGrab$BkpSenseDate - dfDisSenseGrab$BkpGrabDate)
dfDisSenseGrab <- subset(dfDisSenseGrab, SenseGrabTimeDiff  < 15)
dfDisSenseGrab <- dfDisSenseGrab %>% drop_na()
sapply(dfDisSenseGrab, function(x) sum(is.na(x)))

# Changing names to match other dataframes
setnames(dfDisSenseGrab, old = c("FDOMQSU","Nitrate_mg"), 
         new = c("FDOM_corrected_QSU","NO3_corrected"))

# Renames for use in all other programs
W3TrainingData <- dfDisSenseGrab


###################################################################################
### Each section below imports data for each watershed in New Hampshire         ###
### You can run them one at a time or all at once                               ###
### If you need to look for a different file then you can use the following URL ###
### https://www.hydroshare.org/resource/8217eab0997d493782ff321ca5f95f28/       ###
###                                                                             ###
### Comments are only left on the BDC data import as all of the New Hampshire   ###
### watersheds are imported in the same fashion                                 ###
###                                                                             ###
###################################################################################


###################################
### BDC Watershed Data Download ###
###################################

# Data import, converting date to datetime data type, and creading the sin and cos DOY columns
tempData<-read.csv('Raw_Data/Corrected Hydroshare Data 2024/CR1000_BDC_WQual_Level3_Grab.csv')
names(tempData)[names(tempData) == "DATETIME"] <- "Date"
tempData$Date<-as.POSIXct(tempData$Date, format = '%Y-%m-%d %H:%M:%S')
tempData$DOY <- yday(tempData$Date)
tempData$DOYSin <- sin((tempData$DOY-173) * (2*pi)/365.25)
tempData$DOYCos <- cos((tempData$DOY-173) * (2*pi)/365.25)

# Excluding unnecessary data columns and dropping rows with NaN data
tempData <- subset(tempData, select = c("Date", "FDOM_corrected_QSU", "NO3_corrected", 
                                        "SpConductivity", "TempC", "DOYSin", "DOYCos", 
                                        "pH", "ODOPerCent", "ODOMGL"))
tempData <- subset(tempData, !apply(tempData, 1, function(row) any(grepl('NAN', row, fixed = TRUE))))
str(tempData)
# Converting column data types
transform(tempData, FDOM_corrected_QSU = as.numeric(FDOM_corrected_QSU),
          NO3_corrected = as.numeric(NO3_corrected),
          SpConductivity = as.numeric(SpConductivity),
          TempC = as.numeric(TempC),
          pH = as.numeric(pH),
          ODOPerCent = as.numeric(ODOPerCent),
          ODOMGL = as.numeric(ODOMGL))



BDC <- na.omit(tempData)



###################################
### BEF Watershed Data Download ###
###################################

tempData<-read.csv('Raw_Data/Corrected Hydroshare Data 2024/CR1000_BEF_WQual_Level3_Grab.csv')
names(tempData)[names(tempData) == "DATETIME"] <- "Date"
tempData$Date<-as.POSIXct(tempData$Date, format = '%Y-%m-%d %H:%M:%S')
tempData$DOY <- yday(tempData$Date)
tempData$DOYSin <- sin((tempData$DOY-173) * (2*pi)/365.25)
tempData$DOYCos <- cos((tempData$DOY-173) * (2*pi)/365.25)

tempData <- subset(tempData, select = c("Date", "FDOM_corrected_QSU", "NO3_corrected", 
                                        "SpConductivity", "TempC", "DOYSin", "DOYCos", 
                                        "pH", "ODOPerCent", "ODOMGL"))
tempData <- subset(tempData, !apply(tempData, 1, function(row) any(grepl('NAN', row, fixed = TRUE))))
str(tempData)
transform(tempData, FDOM_corrected_QSU = as.numeric(FDOM_corrected_QSU),
          NO3_corrected = as.numeric(NO3_corrected),
          SpConductivity = as.numeric(SpConductivity),
          TempC = as.numeric(TempC),
          pH = as.numeric(pH),
          ODOPerCent = as.numeric(ODOPerCent),
          ODOMGL = as.numeric(ODOMGL))

BEF <- na.omit(tempData)


###################################
### DCF Watershed Data Download ###
###################################

tempData<-read.csv('Raw_Data/Corrected Hydroshare Data 2024/CR1000_DCF_WQual_Level3_Grab.csv')
names(tempData)[names(tempData) == "DATETIME"] <- "Date"
tempData$Date<-as.POSIXct(tempData$Date, format = '%Y-%m-%d %H:%M:%S')
tempData$DOY <- yday(tempData$Date)
tempData$DOYSin <- sin((tempData$DOY-173) * (2*pi)/365.25)
tempData$DOYCos <- cos((tempData$DOY-173) * (2*pi)/365.25)

tempData <- subset(tempData, select = c("Date", "FDOM_corrected_QSU", "NO3_corrected", 
                                        "SpConductivity", "TempC", "DOYSin", "DOYCos", 
                                        "pH", "ODOPerCent", "ODOMGL"))
tempData <- subset(tempData, !apply(tempData, 1, function(row) any(grepl('NAN', row, fixed = TRUE))))
str(tempData)
transform(tempData, FDOM_corrected_QSU = as.numeric(FDOM_corrected_QSU),
          NO3_corrected = as.numeric(NO3_corrected),
          SpConductivity = as.numeric(SpConductivity),
          TempC = as.numeric(TempC),
          pH = as.numeric(pH),
          ODOPerCent = as.numeric(ODOPerCent),
          ODOMGL = as.numeric(ODOMGL))

DCF <- na.omit(tempData)



###################################
### LMP Watershed Data Download ###
###################################

tempData<-read.csv('Raw_Data/Corrected Hydroshare Data 2024/CR1000_LMP_WQual_Level3_Grab.csv')
names(tempData)[names(tempData) == "DATETIME"] <- "Date"
tempData$Date<-as.POSIXct(tempData$Date, format = '%Y-%m-%d %H:%M:%S')
tempData$DOY <- yday(tempData$Date)
tempData$DOYSin <- sin((tempData$DOY-173) * (2*pi)/365.25)
tempData$DOYCos <- cos((tempData$DOY-173) * (2*pi)/365.25)

tempData <- subset(tempData, select = c("Date", "FDOM_corrected_QSU", "NO3_corrected", 
                                        "SpConductivity", "TempC", "DOYSin", "DOYCos", 
                                        "pH", "ODOPerCent", "ODOMGL"))
tempData <- subset(tempData, !apply(tempData, 1, function(row) any(grepl('NAN', row, fixed = TRUE))))
str(tempData)
transform(tempData, FDOM_corrected_QSU = as.numeric(FDOM_corrected_QSU),
          NO3_corrected = as.numeric(NO3_corrected),
          SpConductivity = as.numeric(SpConductivity),
          TempC = as.numeric(TempC),
          pH = as.numeric(pH),
          ODOPerCent = as.numeric(ODOPerCent),
          ODOMGL = as.numeric(ODOMGL))

LMP <- na.omit(tempData)


###################################
### MCQ Watershed Data Download ###
###################################

tempData<-read.csv('Raw_Data/Corrected Hydroshare Data 2024/CR1000_MCQ_WQual_Level3_Grab.csv')
names(tempData)[names(tempData) == "DATETIME"] <- "Date"
tempData$Date<-as.POSIXct(tempData$Date, format = '%Y-%m-%d %H:%M:%S')
tempData$DOY <- yday(tempData$Date)
tempData$DOYSin <- sin((tempData$DOY-173) * (2*pi)/365.25)
tempData$DOYCos <- cos((tempData$DOY-173) * (2*pi)/365.25)
tempData <- subset(tempData, select = c("Date", "FDOM_corrected_QSU", "NO3_corrected", 
                                        "SpConductivity", "TempC", "DOYSin", "DOYCos", 
                                        "pH", "ODOPerCent", "ODOMGL"))
tempData <- subset(tempData, !apply(tempData, 1, function(row) any(grepl('NAN', row, fixed = TRUE))))
str(tempData)
tempData <- transform(tempData, FDOM_corrected_QSU = as.numeric(FDOM_corrected_QSU),
                      NO3_corrected = as.numeric(NO3_corrected),
                      SpConductivity = as.numeric(SpConductivity),
                      TempC = as.numeric(TempC),
                      pH = as.numeric(pH),
                      ODOPerCent = as.numeric(ODOPerCent),
                      ODOMGL = as.numeric(ODOMGL))
MCQ <- na.omit(tempData)


###################################
### SBM Watershed Data Download ###
###################################
# SBM works the same as the rest of the datasets, it just has column different names
tempData<-read.csv('Raw_Data/Corrected Hydroshare Data 2024/CR1000_SBM_WQual_Level3_Grab.csv')
names(tempData)[names(tempData) == "DATETIME"] <- "Date"
tempData$Date<-as.POSIXct(tempData$Date, format = '%Y-%m-%d %H:%M:%S')
tempData$DOY <- yday(tempData$Date)
tempData$DOYSin <- sin((tempData$DOY-173) * (2*pi)/365.25)
tempData$DOYCos <- cos((tempData$DOY-173) * (2*pi)/365.25)
setnames(tempData, old = c("NO3_corrected_mgL"), 
         new = c("NO3_corrected"))

tempData <- subset(tempData, select = c("Date", "FDOM_corrected_QSU", "NO3_corrected", 
                                        "SpConductivity", "TempC", "DOYSin", "DOYCos", 
                                        "pH", "ODOMGL", 
                                        "NPOC..mg.C.L.","Cl..mg.Cl.L.", "NO3..mg.N.L.", "SO4..mg.S.L.", 
                                        "Na..mg.Na.L.", "K..mg.K.L.", "Mg..mg.Mg.L.", 
                                        "Ca..mg.Ca.L.", "DON"))

tempData <- subset(tempData, !apply(tempData, 1, function(row) any(grepl('NAN', row, fixed = TRUE))))
str(tempData)
setnames(tempData, old = c("NPOC..mg.C.L.", "Cl..mg.Cl.L.", 
                           "NO3..mg.N.L.", "SO4..mg.S.L.", "Na..mg.Na.L.", "K..mg.K.L.", 
                           "Mg..mg.Mg.L.", "Ca..mg.Ca.L."), 
         new = c("DOC", "Cl", "NO3", "SO4", "Na", "K", 
                 "Mg", "Ca"))



tempData <- transform(tempData, FDOM_corrected_QSU = as.numeric(FDOM_corrected_QSU),
                      NO3_corrected = as.numeric(NO3_corrected),
                      SpConductivity = as.numeric(SpConductivity),
                      TempC = as.numeric(TempC),
                      pH = as.numeric(pH),
                      ODOMGL = as.numeric(ODOMGL))

SBM <- na.omit(tempData)


###################################
### TPB Watershed Data Download ###
###################################

tempData<-read.csv('Raw_Data/Corrected Hydroshare Data 2024/CR1000_TPB_WQual_Level3_Grab.csv')
names(tempData)[names(tempData) == "DATETIME"] <- "Date"
tempData$Date<-as.POSIXct(tempData$Date, format = '%Y-%m-%d %H:%M:%S')
tempData$DOY <- yday(tempData$Date)
tempData$DOYSin <- sin((tempData$DOY-173) * (2*pi)/365.25)
tempData$DOYCos <- cos((tempData$DOY-173) * (2*pi)/365.25)

tempData <- subset(tempData, select = c("Date", "FDOM_corrected_QSU", "NO3_corrected", 
                                        "SpConductivity", "TempC", "DOYSin", "DOYCos", 
                                        "pH", "ODOPerCent", "ODOMGL"))
tempData <- subset(tempData, !apply(tempData, 1, function(row) any(grepl('NAN', row, fixed = TRUE))))
str(tempData)
tempData <- transform(tempData, FDOM_corrected_QSU = as.numeric(FDOM_corrected_QSU),
                      NO3_corrected = as.numeric(NO3_corrected),
                      SpConductivity = as.numeric(SpConductivity),
                      TempC = as.numeric(TempC),
                      pH = as.numeric(pH),
                      ODOPerCent = as.numeric(ODOPerCent),
                      ODOMGL = as.numeric(ODOMGL))

TPB <- na.omit(tempData)


###################################
### WHB Watershed Data Download ###
###################################

tempData<-read.csv('Raw_Data/Corrected Hydroshare Data 2024/CR1000_WHB_WQual_Level3_Grab.csv')
names(tempData)[names(tempData) == "DATETIME"] <- "Date"
tempData$Date<-as.POSIXct(tempData$Date, format = '%Y-%m-%d %H:%M:%S')
tempData$DOY <- yday(tempData$Date)
tempData$DOYSin <- sin((tempData$DOY-173) * (2*pi)/365.25)
tempData$DOYCos <- cos((tempData$DOY-173) * (2*pi)/365.25)

tempData <- subset(tempData, select = c("Date", "FDOM_corrected_QSU", "NO3_corrected", 
                                        "SpConductivity", "TempC", "DOYSin", "DOYCos", 
                                        "pH", "ODOPerCent", "ODOMGL"))
tempData <- subset(tempData, !apply(tempData, 1, function(row) any(grepl('NAN', row, fixed = TRUE))))
str(tempData)
tempData <- transform(tempData, FDOM_corrected_QSU = as.numeric(FDOM_corrected_QSU),
                      NO3_corrected = as.numeric(NO3_corrected),
                      SpConductivity = as.numeric(SpConductivity),
                      TempC = as.numeric(TempC),
                      pH = as.numeric(pH),
                      ODOPerCent = as.numeric(ODOPerCent),
                      ODOMGL = as.numeric(ODOMGL))

WHB <- na.omit(tempData)


##############################
### Import Hydroshare Grab ###
##############################

# Imports grab sample data for New Hampshire watersheds
dfHydroGrab<-read.csv('Raw_Data/Hydroshare Grab Samples.csv')
colnames(dfHydroGrab)
dfHydroGrab <- dfHydroGrab[ , names(dfHydroGrab) %in% c("Sample.Name", "DateTime", "NPOC..mg.C.L.",
                                                        "Cl..mg.Cl.L.", 
                                                        "NO3..mg.N.L.", "SO4..mg.S.L.", "Na..mg.Na.L.", "K..mg.K.L.", 
                                                        "Mg..mg.Mg.L.", "Ca..mg.Ca.L.",  
                                                        "NH4..ug.N.L.", "DON.mg.L", "PO4..ug.P.L.")]#, "Closed.Cell.pH")]

setnames(dfHydroGrab, old = c("DateTime", "NPOC..mg.C.L.", "Cl..mg.Cl.L.", 
                              "NO3..mg.N.L.", "SO4..mg.S.L.", "Na..mg.Na.L.", "K..mg.K.L.", 
                              "Mg..mg.Mg.L.", "Ca..mg.Ca.L.",  
                              "NH4..ug.N.L.", "DON.mg.L", "PO4..ug.P.L."),#, "Closed.Cell.pH"), 
         new = c("DATETIME", "DOC", "Cl", "NO3", "SO4", "Na", "K", 
                 "Mg", "Ca", "NH4", "DON", "PO4"))#, "pHGrab"))

# Converts NH4, NO3, PO4, and SO4 to their polyatomic masses
dfHydroGrab$NO3 <- (dfHydroGrab$NO3 * (62.0049/14.0067))
dfHydroGrab$SO4 <- (dfHydroGrab$SO4 * (96.06/32.065))
dfHydroGrab$NH4 <- (dfHydroGrab$NH4 * (18.04/14.0067))
dfHydroGrab$PO4 <- ((dfHydroGrab$PO4 * (94.9714/30.973762))/1000)

# Converts date to datetime and renames columns
dfHydroGrab$DATETIME<-as.POSIXct(dfHydroGrab$DATETIME, format = '%m/%d/%Y %H:%M')
dfHydroGrab <- dfHydroGrab %>%
  mutate(Sample.Name = recode(Sample.Name, BDC0.30 = 'BDC', DCF03 = 'DCF', LMP72= 'LMP', 
                              SBM0.2 = 'SBM', WHB01 = 'WHB'))
dfHydroGrab <- dfHydroGrab %>% drop_na()



##########################################################
### Let's start cleaning and combining Hydroshare data ###
##########################################################


ListOfHydroShare <- list("BDC", "BEF", "DCF", 
                         "LMP", "MCQ", "SBM", "TPB", "WHB")
ListOfHydroData <- list(BDC, BEF, DCF,
                        LMP, MCQ, SBM, TPB, WHB)

# The following loop is the same wrangling function used in for W3 and W9
# The only real difference is a unit conversion for NH4
for (i in 1:length(ListOfHydroShare)) {
  TempHydroGrab <- dfHydroGrab
  print(ListOfHydroShare[[i]])
  
  TempHydroGrab <- TempHydroGrab[TempHydroGrab$Sample.Name == ListOfHydroShare[[i]], ]
  setnames(ListOfHydroData[[i]], old = c("Date"), new = c("DATETIME"), skip_absent=TRUE)
  ListOfHydroData[[i]]$BkpSenseDate <- ListOfHydroData[[i]]$DATETIME
  TempHydroGrab$BkpGrabDate <- TempHydroGrab$DATETIME
  dfSensorGrab <- as.data.table(ListOfHydroData[[i]])[as.data.table(TempHydroGrab), on = .(DATETIME), roll = TRUE]
  dfSensorGrab$SenseTimeDiff <- abs(dfSensorGrab$BkpSenseDate - dfSensorGrab$BkpGrabDate)
  dfSensorGrab <- subset(dfSensorGrab, SenseTimeDiff  < 900)
  dfSensorGrab <- dfSensorGrab %>% drop_na()
  
  dfSensorGrab <- subset(dfSensorGrab, select = c("DATETIME", "FDOM_corrected_QSU", "NO3_corrected", "SpConductivity", "TempC", "DOYSin", "DOYCos", "pH", "ODOMGL",
                                                  "DOC", "Cl", "NO3", "SO4", "Na", "K", "Mg", "Ca", "NH4", "DON", "PO4"))#, "pHGrab"))
  dfSensorGrab <- dfSensorGrab %>% mutate(Watershed = ListOfHydroShare[[i]])
  dfSensorGrab <- dfSensorGrab %>% mutate(NH4 = NH4/1000)
  
  # Assigns dataframe a name
  assign(paste("Training_", ListOfHydroShare[[i]], sep = ""), dfSensorGrab)
  
}

# There is one data points that need to be omitted after the above combination.
# The SBM data point is a DON measurement below zero.
# This should be corrected when the dataset is published.
Training_SBM <- subset(Training_SBM, DON > 0)

# Selects a subset of columns for W3 and W9 and adds a column with the watershed name
W3TrainingData <- subset(W3TrainingData, select = c("DATETIME", "FDOM_corrected_QSU", "NO3_corrected", "SpConductivity", "TempC", "DOYSin", "DOYCos", "pH", "ODOMGL",
                                                    "DOC", "Cl", "NO3", "SO4", "Na", "K", "Mg", "Ca", "NH4", "DON", "PO4"))
W3TrainingData <- W3TrainingData %>% mutate(Watershed = "W3")

W9TrainingData <- subset(W9TrainingData, select = c("DATETIME", "FDOM_corrected_QSU", "NO3_corrected", "SpConductivity", "TempC", "DOYSin", "DOYCos", "pH", "ODOMGL",
                                                    "DOC", "Cl", "NO3", "SO4", "Na", "K", "Mg", "Ca", "NH4", "DON", "PO4"))
W9TrainingData <- W9TrainingData %>% mutate(Watershed = "W9")


# Combines all watersheds into a "master" watershed. This is used for the
# all-but-one model tests.
dfMaster <- do.call("rbind", list(W3TrainingData, W9TrainingData, Training_BDC,
                                  Training_BEF, Training_DCF, #Training_GOF, Training_HBF, 
                                  Training_LMP, Training_MCQ, Training_SBM, Training_TPB,
                                  Training_WHB))

rm(list = c('drop', 'Temporarydf', 'TemporaryList', 'TemporaryDis', 
            'TemporaryfDOM', 'TemporaryNitrate', 'TemporarySpecCond', 'TemporaryTemp', 
            'inUrl2', 'inUrl10', 'infile10', 'infile2', 'dt2', 'dt10', 'dfDis', 
            'DfDis15', 'dfGrab', 'dfDisSense', 'dfDisSenseGrab', 'dfSensor', 'dtDis15', 
            'dtSensor', 'dt18', 'dfW9Grab', 'infile18', 'inUrl18', 'i', 
            'BDC', 'BEF', 'DCF', 'GOF', 'HBF', 
            'LMP', 'MCQ', 'SBM', 'TPB', 'WHB', 'TempHydroGrab', 'dfHydroGrab', 'dfSensorGrab',
            'ListOfHydroData', 'ListOfHydroShare', 'W1SensorData', 'W3SensorData', 'W9SensorData', 
            'combined', 'dtGrab', 'tempData'))


save.image(file = 'Processed_Data/Watershed Dataframes.Rdata')
