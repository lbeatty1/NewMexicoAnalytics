rm(list=ls())

datadirectory = "C:/Users/lbeatty/OneDrive - Environmental Defense Fund - edf.org/OrphanedWells/State Advocacy/NM/Data"
codedirectory = "C:/Users/lbeatty/Documents/NewMexicoAnalytics/"

setwd(datadirectory)

library(tidyverse)
library(lubridate)
library(data.table)
library(ggplot2)
library(scales)
library(sf)


#Wellhistory is from a csv conversion from the FTP server
wellhistory=fread("OCD_Converted/wellhistory.csv", colClasses = "character")

## Read in production
production = NULL
prodfiles = list.files("CountyProduction/", full.names = T)

#only look at like 2018 onward for space
prodfiles = prodfiles[grep("2019|2020|2021|2022", prodfiles)]
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
nrow(wellhistory)
wellhistory = wellhistory[plug_dte>="2018-01-01",]
wellhistory = wellhistory[!status%in%c("C", "Z", "Q", "X"),]
wellhistory = wellhistory[well_typ_cde%in%c("G", "O")]
nrow(wellhistory)

#fill in missing spud and plug dates
wellhistory[,spud_dte:=min(spud_dte,na.rm=T), by=API]
wellhistory[,plug_dte:=min(plug_dte,na.rm=T), by=API]

wellhistory[year(spud_dte)==9999, spud_dte:=NA]
wellhistory[year(plug_dte)==9999, plug_dte:=NA]

wellhistory[,eff_dte:=as.Date(eff_dte, "%m/%d/%Y")]
wellhistory[,rec_termn_dte:=as.Date(rec_termn_dte, "%m/%d/%Y")]

###############
## Create a panel -- need at least 10GB memory available (probably more lol)
panel = expand.grid(unique(wellhistory$API), seq.Date(as.Date("2019-01-01"), as.Date("2022-12-01"), by="month"))
names(panel) = c("API", "date")
panel = left_join(panel, wellhistory, by="API")

length(unique(panel$API))
#throw out rows where date is outside effective range in wellhistory
panel = panel%>%
  filter(date<rec_termn_dte,
         date>=eff_dte)
length(unique(panel$API))

gc()

panel=panel%>%
  filter(date>=spud_dte|is.na(spud_dte),
         date<=plug_dte|is.na(plug_dte))

nrow(panel)
panel = left_join(panel, production, by=c("API", "date"))
nrow(panel)

#lets throw out some stuff
panel = panel[,c(1,2,6,7,8,9,13,14,19,20,32,33,34,38,39,40,41,43,44)]
rm(production, tempdata, gasprod, oilprod)

panel = data.table(panel)
panel[order(API,date),lagstatus:=shift(status, n=1, type="lag")]
panel[,transition:=paste(lagstatus, status, sep="-->")]

######################################################################################
## Create an imputed status, create a counter for how long status has been running####
######################################################################################

panel = data.table(panel)

panel[is.na(Oil), Oil:=0]
panel[is.na(Gas), Gas:=0]
panel[,BOE:= Oil+6*Gas]
panel[,producing_flag:= BOE>0]
#there's about 500,000 rows with no reported production... let's assign producing_flag=0 to those observations
panel[is.na(BOE), producing_flag:=0]



#counter of producing/nonproducing time
panel[order(date),time_shutin:=rowid(rleid(producing_flag)),by=API]
panel[,time_producing:=time_shutin*producing_flag]
panel[,time_shutin:=time_shutin*(1-producing_flag)]


panel[,inactive_flag:=time_shutin>=15]
panel[,spud_flag:= date<spud_dte%m+%months(1)]

## Just want a snapshot at end of 2022

snapshot = panel[date=="2022-12-01",]
snapshot = snapshot[!status %in% c("P", "N"),]

rm(panel, wellhistory)
gc()
snapshot[,county:=substr(API, 3,5)]

## calculate depth, plug cost
snapshot = snapshot%>%
  mutate(dpth_tvd_num = replace(dpth_tvd_num, dpth_tvd_num=="99999", NA),
         dpth_tvd_num = replace(dpth_tvd_num, dpth_tvd_num=="0", NA),
         dpth_mvd_num = replace(dpth_mvd_num, dpth_mvd_num=="99999", NA),
         dpth_mvd_num = replace(dpth_mvd_num, dpth_mvd_num=="99999", NA),
         dpth_tvd_num = as.numeric(dpth_tgt_num),
         dpth_mvd_num = as.numeric(dpth_mvd_num),
         depth = coalesce(dpth_tvd_num,dpth_mvd_num)) 
meandepth = mean(snapshot$depth, na.rm=T)
snapshot = snapshot%>%
  mutate(depth = replace(depth, depth==0, meandepth),
         plug_cost = depth*12)


#find total plug cost, n wells inactive for more than 15 months by county
plug_snapshot = snapshot%>%
  filter(inactive_flag==1,
         lease_typ_cde%in%c("P", "S"))%>%
  group_by(county)%>%
  summarise(plug_cost=sum(plug_cost),
            n=n())%>%
  mutate(plug_cost2 = n*100000,
         jobs = (n/10)*2.4,
         jobs = round(jobs, digits=0), 
         jobs = replace(jobs, jobs==0, NA))

#group by county, sum wells, costs

counties = st_read('cb_2018_us_county_500k/cb_2018_us_county_500k.shp')
counties = counties%>%filter(STATEFP=="35")

counties=left_join(counties, plug_snapshot, by=c('COUNTYFP'='county'))
counties=counties%>%
  mutate(n=replace(n, is.na(n),0),
         plug_cost=replace(plug_cost, is.na(plug_cost),0))

ggplot(counties)+
  geom_sf(aes(fill=jobs))+
  geom_sf_text(aes(label=jobs))+
  scale_fill_gradient(low="#EDF8FA",high="#216C2B", na.value="#EDF8FA",space ="Lab" )+
  theme(legend.position = "none",
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        panel.grid = element_blank(),
        panel.background = element_rect(fill = "white"),
        axis.title.x=element_blank(),
        axis.title.y=element_blank())

ggsave(filename=paste(codedirectory,"Figures/Jobs_FeeState.jpg", sep=""),
       device="jpg",
       height=5,
       width=7)

#re-do for all state wells
plug_snapshot = snapshot%>%
  filter(inactive_flag==1)%>%
  group_by(county)%>%
  summarise(plug_cost=sum(plug_cost),
            n=n())%>%
  mutate(plug_cost2 = n*100000,
         jobs = (n/10)*2.4, 
         jobs = round(jobs, digits=0), 
         jobs = replace(jobs, jobs==0, NA))

counties = st_read('cb_2018_us_county_500k/cb_2018_us_county_500k.shp')
counties = counties%>%filter(STATEFP=="35")


counties=left_join(counties, plug_snapshot, by=c('COUNTYFP'='county'))
counties=counties%>%
  mutate(n=replace(n, is.na(n),0),
         plug_cost=replace(plug_cost, is.na(plug_cost),0))

ggplot(counties)+
  geom_sf(aes(fill=jobs))+
  geom_sf_text(aes(label=jobs))+
  scale_fill_gradient(low="#EDF8FA",high="#216C2B", na.value="#EDF8FA",space ="Lab" )+
  theme(legend.position = "none",
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        panel.grid = element_blank(),
        panel.background = element_rect(fill = "white"),
        axis.title.x=element_blank(),
        axis.title.y=element_blank())

ggsave(filename=paste(codedirectory,"Figures/Jobs_AllWells.jpg", sep=""),
       device="jpg",
       height=5,
       width=7)
