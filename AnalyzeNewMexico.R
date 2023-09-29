rm(list=ls())

datadirectory = "C:/Users/lbeatty/OneDrive - Environmental Defense Fund - edf.org/OrphanedWells/State Advocacy/NM/Data"
codedirectory = "C:/Users/lbeatty/Documents/NewMexicoAnalytics/"

setwd(datadirectory)

library(tidyverse)
library(lubridate)
library(data.table)

#Wellhistory is from a csv conversion from the FTP server
wellhistory=fread("OCD_Converted/wellhistory.csv", colClasses = "character")

## Read in production
production = NULL
prodfiles = list.files("CountyProduction/", full.names = T)
for(j in prodfiles){
  tempdata=fread(j,colClasses="character")
  production=rbind(production, tempdata)
  print(j)
}


#basic data cleaning and formatting
wellhistory=data.table(wellhistory)
wellhistory[nchar(`ns1:api_cnty_cde`)==1, `ns1:api_cnty_cde`:=paste("0", `ns1:api_cnty_cde`, sep="")]
wellhistory[nchar(`ns1:api_well_idn`)==1, `ns1:api_well_idn`:=paste("00000", `ns1:api_well_idn`, sep="")]
wellhistory[nchar(`ns1:api_well_idn`)==2, `ns1:api_well_idn`:=paste("0000", `ns1:api_well_idn`, sep="")]
wellhistory[nchar(`ns1:api_well_idn`)==3, `ns1:api_well_idn`:=paste("000", `ns1:api_well_idn`, sep="")]
wellhistory[nchar(`ns1:api_well_idn`)==4, `ns1:api_well_idn`:=paste("00", `ns1:api_well_idn`, sep="")]
wellhistory[nchar(`ns1:api_well_idn`)==5, `ns1:api_well_idn`:=paste("0", `ns1:api_well_idn`, sep="")]

wellhistory[,API:=paste(`ns1:api_st_cde`,`ns1:api_cnty_cde`, `ns1:api_well_idn`, sep="")]

names(wellhistory) = gsub("ns1.", "", names(wellhistory))


#lease_type_cde P - Private, S - State, F - Federal, I - Indian, N- Navajo, J - Jicarilla, U - Ute
#well_type C - CO2, G -Gas, I - Injection, M - Miscellaneous, O - Oil, S - Salt Water Disposal, W - Water
#status P - Plugged (site released), C - Cancelled, H - Plugged (not released), S - Shut-in, A - Active, T - Temp Abandon, E - Temp Abandon (expired), N - New, Z - Zone Plugged (temp), Q - Zone Plugged (permanent), D - Dry Hole, X - Never Drilled

wellhistory = wellhistory[status!="C",]

#fill in missing spud date, plug date
wellhistory[,spud_dte:=as.Date(spud_dte, "%m/%d/%Y")]
wellhistory[,plug_dte:=as.Date(plug_dte, "%m/%d/%Y")]

#filter out some more
wellhistory = wellhistory[plug_dte>="2010-01-01",]


wellhistory[,spud_dte:=min(spud_dte,na.rm=T), by=API]
wellhistory[,plug_dte:=min(plug_dte,na.rm=T), by=API]

wellhistory[year(spud_dte)==9999, spud_dte:=NA]

wellhistory[year(plug_dte)==9999, plug_dte:=NA]

wellhistory[,eff_dte:=as.Date(eff_dte, "%m/%d/%Y")]
wellhistory[,rec_termn_dte:=as.Date(rec_termn_dte, "%m/%d/%Y")]

###############
## Create a panel -- need at least 10GB memory available (probably more lol)
panel = expand.grid(unique(wellhistory$API), seq.Date(as.Date("2010-01-01"), as.Date("2022-12-01"), by="month"))
names(panel) = c("API", "date")
panel = left_join(panel, wellhistory, by="API")
panel = panel%>%
  filter(date<rec_termn_dte,
         date>=eff_dte)

gc()

panel=panel%>%
  filter(date>=spud_dte|is.na(spud_dte),
         date<=plug_dte|is.na(plug_dte))
