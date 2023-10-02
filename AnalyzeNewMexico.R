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

#only look at like 2018 onward for space
prodfiles = prodfiles[grep("2018|2019|2020|2021|2022", prodfiles)]
for(j in prodfiles){
  tempdata=fread(j,colClasses="character")
  production=rbind(production, tempdata)
  print(j)
}

oilprod = pivot_longer(production, cols=starts_with("Oil"), names_prefix="Oil")
gasprod = pivot_longer(production, cols=starts_with("Gas"), names_prefix="Gas")

gasprod = gasprod%>%
  mutate(value=as.numeric(value))%>%
  group_by(API, Year, name)%>%
  summarise(Gas=sum(value))

oilprod = oilprod%>%
  mutate(value=as.numeric(value))%>%
  group_by(API, Year, name)%>%
  summarise(Oil=sum(value))

production = left_join(oilprod, gasprod, by=c("API", "Year", "name"))
production = production%>%
  mutate(date=paste(Year, name, "01", sep="-"),
         date=as.Date(date, "%Y-%m-%d"))

#basic data cleaning and formatting
wellhistory=data.table(wellhistory)
wellhistory[nchar(`ns1:api_cnty_cde`)==2, `ns1:api_cnty_cde`:=paste("0", `ns1:api_cnty_cde`, sep="")]
wellhistory[nchar(`ns1:api_cnty_cde`)==1, `ns1:api_cnty_cde`:=paste("00", `ns1:api_cnty_cde`, sep="")]


wellhistory[nchar(`ns1:api_well_idn`)==1, `ns1:api_well_idn`:=paste("0000", `ns1:api_well_idn`, sep="")]
wellhistory[nchar(`ns1:api_well_idn`)==2, `ns1:api_well_idn`:=paste("000", `ns1:api_well_idn`, sep="")]
wellhistory[nchar(`ns1:api_well_idn`)==3, `ns1:api_well_idn`:=paste("00", `ns1:api_well_idn`, sep="")]
wellhistory[nchar(`ns1:api_well_idn`)==4, `ns1:api_well_idn`:=paste("0", `ns1:api_well_idn`, sep="")]

wellhistory[,API:=paste(`ns1:api_st_cde`,`ns1:api_cnty_cde`, `ns1:api_well_idn`, sep="")]

names(wellhistory) = gsub("ns1.", "", names(wellhistory))


#lease_type_cde P - Private, S - State, F - Federal, I - Indian, N- Navajo, J - Jicarilla, U - Ute
#well_type C - CO2, G -Gas, I - Injection, M - Miscellaneous, O - Oil, S - Salt Water Disposal, W - Water
#status P - Plugged (site released), C - Cancelled, H - Plugged (not released), S - Shut-in, A - Active, T - Temp Abandon, E - Temp Abandon (expired), N - New, Z - Zone Plugged (temp), Q - Zone Plugged (permanent), D - Dry Hole, X - Never Drilled


#fill in missing spud date, plug date
wellhistory[,spud_dte:=as.Date(spud_dte, "%m/%d/%Y")]
wellhistory[,plug_dte:=as.Date(plug_dte, "%m/%d/%Y")]

#filter out some more
wellhistory = wellhistory[plug_dte>="2018-01-01",]
wellhistory = wellhistory[!status%in%c("C", "Z", "Q", "X"),]


#fill in missing spud and plug dates
wellhistory[,spud_dte:=min(spud_dte,na.rm=T), by=API]
wellhistory[,plug_dte:=min(plug_dte,na.rm=T), by=API]

wellhistory[year(spud_dte)==9999, spud_dte:=NA]
wellhistory[year(plug_dte)==9999, plug_dte:=NA]

wellhistory[,eff_dte:=as.Date(eff_dte, "%m/%d/%Y")]
wellhistory[,rec_termn_dte:=as.Date(rec_termn_dte, "%m/%d/%Y")]

###############
## Create a panel -- need at least 10GB memory available (probably more lol)
panel = expand.grid(unique(wellhistory$API), seq.Date(as.Date("2018-01-01"), as.Date("2022-12-01"), by="month"))
names(panel) = c("API", "date")
panel = left_join(panel, wellhistory, by="API")

#throw out rows where date is outside effective range in wellhistory
panel = panel%>%
  filter(date<rec_termn_dte,
         date>=eff_dte)

gc()

panel=panel%>%
  filter(date>=spud_dte|is.na(spud_dte),
         date<=plug_dte|is.na(plug_dte))

nrow(panel)
panel = left_join(panel, production, by=c("API", "date"))
nrow(panel)

#lets throw out some stuff
panel = panel[,c(1,2,6,7,8,9,13,14,19,20,32,33,34,38,39,40,41,43,44)]
rm(production, wellhistory, tempdata, gasprod, oilprod)
gc()

#Notes for Aaron:
View(panel%>%arrange(API, date))
#what the hell is status=="J" ??  your guess is as good as mine
#Looks like well statuses are not updated super super well - lots of zero production with "A" status
View(panel%>%filter(API==3000500233))  #clearly not active lol

#Also look making the panel makes the status changes right as I mentioned on slack
#Now these should be a good interpretation
panel[order(API,date),lagstatus:=shift(status, n=1, type="lag")]
panel[,transition:=paste(lagstatus, status, sep="-->")]

transitions = panel%>%
  filter(status!=lagstatus,
         !is.na(lagstatus))%>%
  group_by(transition)%>%
  summarise(n=n())

View(transitions)

######################################################################################
## Create an imputed status, create a counter for how long status has been running####
######################################################################################

panel = data.table(panel)

panel[,BOE:= Oil+6*Gas]
panel[,producing_flag:= BOE>0]
#there's about 500,000 rows with no reported production... let's assign producing_flag=0 to those observations
panel[is.na(BOE), producing_flag:=0]


#counter of producing/nonproducing time
panel[order(date),time_shutin:=rowid(rleid(producing_flag)),by=API]
panel[,time_producing:=time_shutin*producing_flag]
panel[,time_shutin:=time_shutin*(1-producing_flag)]

View(panel%>%arrange(API, date)%>%select(API,date, Oil, Gas, BOE, status, producing_flag, time_producing, time_shutin))
View(panel[API=="3001521672"])  #This guy produced like $4 worth of Gas over two years and seemingly kepth their "A" status

     