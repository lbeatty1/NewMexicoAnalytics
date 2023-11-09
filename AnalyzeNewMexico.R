rm(list=ls())

datadirectory = "C:/Users/lbeatty/OneDrive - Environmental Defense Fund - edf.org/OrphanedWells/State Advocacy/NM/Data"
codedirectory = "C:/Users/lbeatty/Documents/NewMexicoAnalytics/"

setwd(datadirectory)

library(tidyverse)
library(lubridate)
library(data.table)
library(ggplot2)
library(scales)


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
waterprod = pivot_longer(production, cols=starts_with("Water"), names_prefix="Water")

gasprod = gasprod%>%
  mutate(value=as.numeric(value))%>%
  group_by(API, Year, name)%>%
  summarise(Gas=sum(value))

oilprod = oilprod%>%
  mutate(value=as.numeric(value))%>%
  group_by(API, Year, name)%>%
  summarise(Oil=sum(value))

waterprod = waterprod%>%
  mutate(value=as.numeric(value))%>%
  group_by(API, Year, name)%>%
  summarise(water=sum(value))

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
rm(production, tempdata, gasprod, oilprod)
gc()

#Notes for Aaron:
View(panel%>%arrange(API, date))
#what the hell is status=="J" ??  your guess is as good as mine
#Looks like well statuses are not updated super super well - lots of zero production with "A" status
View(panel%>%filter(API==3000500233))  #clearly not active lol

#Also look making the panel makes the status changes right as I mentioned on slack
#Now these should be a good interpretation
panel = data.table(panel)
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


panel[,inactive_flag:=time_shutin>=15]

########################################################################
## Run Similar analysis to Utah - Calc yearly production for each well##
#########################################################################
year = panel%>%
  filter(date<=as.Date("2022-12-01"),
         date>=as.Date("2022-01-01"))%>%
  group_by(API)%>%
  summarise(BOE = sum(BOE,na.rm=T),
            Gas = sum(Gas, na.rm=T),
            Oil = sum(Oil, na.rm=T))
year_details = panel%>%
  filter(date<=as.Date("2022-12-01"),
         date>=as.Date("2021-01-01"))%>%
  group_by(API)%>%
  slice_max(n=1, order_by=date)%>%
  select(API, eff_dte, rec_termn_dte, ogrid_cde, well_typ_cde, lease_typ_cde, latitude, longitude, status, spud_dte, plug_dte, dpth_tgt_num, dpth_tvd_num, dpth_mvd_num, inactive_flag, time_shutin)

year = left_join(year, year_details, by="API")

###############################
####### see what we're missing#
###############################
# missings = panel%>%filter(!API%in%unique(year$API))
# View(missings%>%arrange(API, date))
# ####mostly looks like plugged wells but some dissapear and don't have a plugging record
# View(panel%>%filter(API==3001524212))
# View(panel%>%filter(API==3001543968))
# View(panel%>%filter(API==3004535916))
# ######find extent of the problem
# nrow(missings%>%filter(is.na(plug_dte))%>%select(API)%>%unique())
# ######not a huge deal but substantial
# nrow(unique(missings%>%select(API)))
# nrow(panel%>%select(API)%>%unique())



#####################
## Filter to only fee/state wells, impute depths
#treat J as shut-in????
year = year%>%
  filter(lease_typ_cde%in%c("P", "S"),
         !status %in% c("P", "N"))%>%
  mutate(dpth_tvd_num = replace(dpth_tvd_num, dpth_tvd_num=="99999", NA),
         dpth_tvd_num = replace(dpth_tvd_num, dpth_tvd_num=="0", NA),
         dpth_mvd_num = replace(dpth_mvd_num, dpth_mvd_num=="99999", NA),
         dpth_mvd_num = replace(dpth_mvd_num, dpth_mvd_num=="99999", NA),
         dpth_tvd_num = as.numeric(dpth_tgt_num),
         dpth_mvd_num = as.numeric(dpth_mvd_num),
         depth = coalesce(dpth_tvd_num,dpth_mvd_num))                
meandepth = mean(year$depth, na.rm=T)
year = year%>%
  mutate(depth = replace(depth, depth==0, meandepth),
         individual_bond = 25000 + 2*depth,
         plug_cost = depth*12)




################################
## Calculate Plug costs, collapse by operator
operator_summary = year%>%
  group_by(ogrid_cde, inactive_flag)%>%
  summarise(n_wells = n(),
            plug_cost = sum(plug_cost,na.rm=T),
            sum_individual_bonds = sum(individual_bond,na.rm=T))
operator_summary = pivot_wider(operator_summary, id_cols="ogrid_cde", values_from = c("n_wells", "plug_cost", "sum_individual_bonds"), names_from="inactive_flag", values_fill = 0)  

operator_summary = operator_summary%>%
  mutate(active_bond = 0,
         active_bond = replace(active_bond, n_wells_FALSE>0&n_wells_FALSE<=10, 50000),
         active_bond = replace(active_bond, n_wells_FALSE>10&n_wells_FALSE<=50, 75000),
         active_bond = replace(active_bond, n_wells_FALSE>50&n_wells_FALSE<=100, 125000),
         active_bond = replace(active_bond, n_wells_FALSE>100, 250000),
         temp_abandon_bond = 0,
         temp_abandon_bond = replace(temp_abandon_bond, n_wells_TRUE>0&n_wells_TRUE<=5, 150000),
         temp_abandon_bond = replace(temp_abandon_bond, n_wells_TRUE>5&n_wells_TRUE<=10, 300000),
         temp_abandon_bond = replace(temp_abandon_bond, n_wells_TRUE>10&n_wells_TRUE<=25, 500000),
         temp_abandon_bond = replace(temp_abandon_bond, n_wells_TRUE>25, 1000000),
         bond = pmin(active_bond, sum_individual_bonds_FALSE)+pmin(temp_abandon_bond, sum_individual_bonds_TRUE),
         hypothetical_perfoot_bond = sum_individual_bonds_FALSE+sum_individual_bonds_TRUE,
         total_plugcost = plug_cost_FALSE+plug_cost_TRUE,
         uses_active_blanket = pmin(active_bond, sum_individual_bonds_FALSE)==active_bond,
         uses_tempabandon_blanket = pmin(temp_abandon_bond, sum_individual_bonds_TRUE)==temp_abandon_bond,
         n_temp_abandon_group = "zero",
         n_temp_abandon_group = replace(n_temp_abandon_group, n_wells_TRUE>0&n_wells_TRUE<=5, "between 1 and 5"),
         n_temp_abandon_group = replace(n_temp_abandon_group, n_wells_TRUE>5&n_wells_TRUE<=10, "between 6 and 10"),
         n_temp_abandon_group = replace(n_temp_abandon_group, n_wells_TRUE>10&n_wells_TRUE<=25, "between 11 and 25"),
         n_temp_abandon_group = replace(n_temp_abandon_group, n_wells_TRUE>25, "more than 25"),
         active_bond_group = "zero",
         active_bond_group = replace(active_bond_group, n_wells_FALSE>0&n_wells_FALSE<=10, "between 1 and 10"),
         active_bond_group = replace(active_bond_group, n_wells_FALSE>10&n_wells_FALSE<=50, "between 11 and 50"),
         active_bond_group = replace(active_bond_group, n_wells_FALSE>50&n_wells_FALSE<=100, "between 51 and 100"),
         active_bond_group = replace(active_bond_group, n_wells_FALSE>100, "more than 100"))

################
## triple blanket amounts for active, double for inactive
##############

operator_summary = operator_summary%>%
  mutate(active_triple = active_bond*3,
         temp_abandon_bond_double = temp_abandon_bond*2,
         bond_2 = pmin(active_triple, sum_individual_bonds_FALSE)+pmin(temp_abandon_bond_double, sum_individual_bonds_TRUE))

####
#

ggplot(data=operator_summary)+
  geom_point(aes(x=hypothetical_perfoot_bond, y=bond, color=n_temp_abandon_group))+
  geom_abline(slope=1, intercept=0)+
  scale_x_continuous(label=dollar)+
  scale_y_continuous(label=dollar)+
  labs(caption="Plot of firm-level hypothetical single-well bonds versus actual bonds. \n A line is plotted at y=x.")+
  #ggtitle("Blanket bonds are many magnitudes lower than  individual well bonds")+
  theme_bw()

ggsave(filename=paste(codedirectory,"Figures/PerFoot_v_Actual.jpg", sep=""),
       device="jpg",
       height=5,
       width=7)

ggplot(data=operator_summary)+
  geom_point(aes(x=hypothetical_perfoot_bond, y=total_plugcost, color=n_temp_abandon_group))+
  geom_abline(slope=1, intercept=0)+
  #ggtitle("Single-well bonds are lower than estimated plugging costs.")+
  scale_x_continuous(label=dollar)+
  scale_y_continuous(label=dollar)+
  ylab("Estimated Plug Costs for Fee/State Wells by Firm")+
  xlab("Hypothetical Individual Well Bond Amounts")+
  labs(caption="Plot of firm-level total estimated plugging liabilities against required hypothetical bonds. \n A line is plotted at y=x. Plugging costs assume each well costs $12 per foot to plug.")+
  theme_bw()

ggsave(filename=paste(codedirectory,"Figures/PerFoot_v_Costs.jpg", sep=""),
       device="jpg",
       height=5,
       width=7)

ggplot(data=operator_summary)+
  geom_point(aes(x=bond, y=total_plugcost, color=n_temp_abandon_group))+
  geom_abline(slope=1, intercept=0)+
  #ggtitle("Business as usual bonds are far lower than plugging liabilities")+
  scale_x_continuous(label=dollar)+
  scale_y_continuous(label=dollar)+
  ylab("Total Plugging Liabilities for Fee/State Wells")+
  xlab("Current Bond Amounts")+
  labs(caption="Plot of firm-level total estimated plugging liabilities against required bonds. \n A line is plotted at y=x. Plugging costs assume each well costs $12 per foot to plug.")+
  theme_bw()

ggsave(filename=paste(codedirectory,"Figures/CurrentBond_v_Costs.jpg", sep=""),
       device="jpg",
       height=5,
       width=7)

ggplot(data=operator_summary)+
  geom_point(aes(x=bond, y=total_plugcost))+
  geom_abline(slope=1, intercept=0)+
  #ggtitle("Business as usual bonds are far lower than plugging liabilities")+
  scale_x_continuous(label=dollar)+
  scale_y_continuous(label=dollar)+
  ylab("Total Plugging Liabilities for Fee/State Wells")+
  xlab("Current Bond Amounts")+
  labs(caption="Plot of firm-level total estimated plugging liabilities against required bonds. \n A line is plotted at y=x. Plugging costs assume each well costs $12 per foot to plug.")+
  theme_bw()

ggsave(filename=paste(codedirectory,"Figures/CurrentBond_v_Costs_nolegend.jpg", sep=""),
       device="jpg",
       height=5,
       width=7)


ggplot(data=operator_summary)+
  geom_point(aes(x=bond_2, y=total_plugcost, color=n_temp_abandon_group))+
  geom_abline(slope=1, intercept=0)+
  #ggtitle("Bonds are still insufficient with triple/double scheme")+
  scale_x_continuous(label=dollar)+
  scale_y_continuous(label=dollar)+
  ylab("Total Plugging Liabilities for Fee/State Wells")+
  labs(caption="Plot of firm-level total estimated plugging liabilities against required bonds \n assuming active blankets triple and TA blankets double. A line is plotted at y=x. \n Plugging costs assume each well costs $12 per foot to plug.")+
  xlab("New Bond Amounts")+
  theme_bw()


ggsave(filename=paste(codedirectory,"Figures/TripleDoubleBond_v_Costs.jpg", sep=""),
       device="jpg",
       height=5,
       width=7)

ggplot(data=operator_summary)+
  geom_point(aes(x=bond_2, y=total_plugcost))+
  geom_abline(slope=1, intercept=0)+
  #ggtitle("Bonds are still insufficient with triple/double scheme")+
  scale_x_continuous(label=dollar)+
  scale_y_continuous(label=dollar)+
  ylab("Total Plugging Liabilities for Fee/State Wells")+
  labs(caption="Plot of firm-level total estimated plugging liabilities against required bonds \n assuming active blankets triple and TA blankets double. A line is plotted at y=x. \n Plugging costs assume each well costs $12 per foot to plug.")+
  xlab("New Bond Amounts")+
  theme_bw()


ggsave(filename=paste(codedirectory,"Figures/TripleDoubleBond_v_Costs_nolegend.jpg", sep=""),
       device="jpg",
       height=5,
       width=7)
#############################
## Costs for low production wells
lowprod_plugcosts = year%>%
  filter(BOE<365*2)%>%
  group_by(ogrid_cde)%>%
  summarise(plug_cost_lowprod = sum(plug_cost,na.rm=T))

operator_summary = left_join(operator_summary, lowprod_plugcosts, by="ogrid_cde")
ggplot(data=operator_summary)+
  geom_point(aes(x=bond_2, y=plug_cost_lowprod, color=n_temp_abandon_group))+
  geom_abline(slope=1, intercept=0)+
  #ggtitle("Triple/double scheme doesn't look like enough to cover \n even only fee/state wells that produce  <2BOE per day.")+
  scale_x_continuous(label=dollar)+
  scale_y_continuous(label=dollar)+
  ylab("Total Plugging Liabilities for Fee/State Marginal Wells")+
  labs(caption="Plot of firm-level total estimated plugging liabilities for marginal wells against required bonds \n assuming active blankets triple and TA blankets double. A line is plotted at y=x. \n Plugging costs assume each well costs $12 per foot to plug.")+
  xlab("New Bond Amounts")+
  theme_bw()

ggsave(filename=paste(codedirectory,"Figures/TripleDoubleBond_v_MarginalCosts.jpg", sep=""),
       device="jpg",
       height=5,
       width=7)

ggplot(data=operator_summary)+
  geom_point(aes(x=bond_2, y=plug_cost_lowprod))+
  geom_abline(slope=1, intercept=0)+
  #ggtitle("Triple/double scheme doesn't look like enough to cover \n even only fee/state wells that produce  <2BOE per day.")+
  scale_x_continuous(label=dollar)+
  scale_y_continuous(label=dollar)+
  ylab("Total Plugging Liabilities for Fee/State Marginal Wells")+
  labs(caption="Plot of firm-level total estimated plugging liabilities for marginal wells against required bonds \n assuming active blankets triple and TA blankets double. A line is plotted at y=x. \n Plugging costs assume each well costs $12 per foot to plug.")+
  xlab("New Bond Amounts")+
  theme_bw()

ggsave(filename=paste(codedirectory,"Figures/TripleDoubleBond_v_MarginalCosts_nolegend.jpg", sep=""),
       device="jpg",
       height=5,
       width=7)


##################
## Histogramms ###
##################

ggplot(data=operator_summary%>%filter(n_temp_abandon_group=="between 1 and 5"))+
  geom_histogram(aes(x=sum_individual_bonds_TRUE))+
  geom_vline(xintercept = 150000)+
  scale_x_continuous(label=dollar)+
  xlab("Sum of hypothetical individual well bonds for TA wells")+
  #ggtitle("Histogram of hypothetical individual well bonds owed for TA wells \n for firms ")+
  labs(caption="For firms that operate between 1 and 5 TA state/fee wells. \n Vertical black line drawn at the blanket amount.")+
  theme_bw()
ggsave(filename=paste(codedirectory,"Figures/Histogram_TA_1_5.jpg", sep=""),
       device="jpg",
       height=5,
       width=7)

ggplot(data=operator_summary%>%filter(n_temp_abandon_group=="between 6 and 10"))+
  geom_histogram(aes(x=sum_individual_bonds_TRUE))+
  scale_x_continuous(label=dollar)+
  xlab("Sum of hypothetical individual well bonds for TA wells")+
  #ggtitle("Histogram of hypothetical individual well bonds \n owed for TA wells")+
  labs(caption="For firms that operate between 6 and 10 TA state/fee wells. \n Vertical black line drawn at the blanket amount.")+
  geom_vline(xintercept = 300000)+
  theme_bw()

ggsave(filename=paste(codedirectory,"Figures/Histogram_TA_6_10.jpg", sep=""),
       device="jpg",
       height=5,
       width=7)

ggplot(data=operator_summary%>%filter(n_temp_abandon_group=="between 11 and 25"))+
  geom_histogram(aes(x=sum_individual_bonds_TRUE))+
  scale_x_continuous(label=dollar)+
  xlab("Sum of hypothetical individual well bonds for TA wells")+
  geom_vline(xintercept = 500000)+
  #ggtitle("Histogram of hypothetical individual well bonds \n owed for TA wells")+
  labs(caption="For firms that operate between 11 and 25 TA state/fee wells. \n Vertical black line drawn at the blanket amount.")+
  theme_bw()
ggsave(filename=paste(codedirectory,"Figures/Histogram_TA_11_25.jpg", sep=""),
       device="jpg",
       height=5,
       width=7)

ggplot(data=operator_summary%>%filter(n_temp_abandon_group=="more than 25"))+
  geom_histogram(aes(x=sum_individual_bonds_TRUE))+
  scale_x_continuous(label=dollar)+
  xlab("Sum of hypothetical individual well bonds for TA wells")+
  #ggtitle("Histogram of hypothetical individual well bonds \n owed for TA wells")+
  labs(caption="For firms that operate more than TA state/fee wells. \n Vertical black line drawn at the blanket amount.")+
  geom_vline(xintercept = 1000000)+
  theme_bw()
ggsave(filename=paste(codedirectory,"Figures/Histogram_TA_25.jpg", sep=""),
       device="jpg",
       height=5,
       width=7)

### Active

ggplot(data=operator_summary%>%filter(active_bond_group=="between 1 and 10"))+
  geom_histogram(aes(x=sum_individual_bonds_FALSE))+
  geom_vline(xintercept = 50000)+
  xlab("Sum of hypothetical individual well bonds for non-TA wells")+
  scale_x_continuous(label=dollar)+
  #ggtitle("Histogram of hypothetical individual well bonds \n owed for non-TA wells")+
  labs(caption="For firms that operate between 1 and 10 active state/fee wells. \n Vertical black line drawn at the blanket amount.")+
  theme_bw()
ggsave(filename=paste(codedirectory,"Figures/Histogram_nonTA_1_10.jpg", sep=""),
       device="jpg",
       height=5,
       width=7)

ggplot(data=operator_summary%>%filter(active_bond_group=="between 11 and 50"))+
  geom_histogram(aes(x=sum_individual_bonds_FALSE))+
  xlab("Sum of hypothetical individual well bonds for non-TA wells")+
  scale_x_continuous(label=dollar)+
  #ggtitle("Histogram of hypothetical individual well bonds \n owed for non-TA wells")+
  labs(caption="For firms that operate between 11 and 50 non-TA state/fee wells. \n Vertical black line drawn at the blanket amount.")+
  geom_vline(xintercept = 75000)+
  theme_bw()
ggsave(filename=paste(codedirectory,"Figures/Histogram_nonTA_11_50.jpg", sep=""),
       device="jpg",
       height=5,
       width=7)

ggplot(data=operator_summary%>%filter(active_bond_group=="between 51 and 100"))+
  geom_histogram(aes(x=sum_individual_bonds_FALSE))+
  xlab("Sum of hypothetical individual well bonds for non-TA wells")+
  geom_vline(xintercept = 125000)+
  scale_x_continuous(label=dollar)+
  #ggtitle("Histogram of hypothetical individual well bonds \n owed for TA wells")+
  labs(caption="For firms that operate between 11 and 25 TA state/fee wells. \n Vertical black line drawn at the blanket amount.")+
  theme_bw()
ggsave(filename=paste(codedirectory,"Figures/Histogram_nonTA_51_100.jpg", sep=""),
       device="jpg",
       height=5,
       width=7)

ggplot(data=operator_summary%>%filter(active_bond_group=="more than 100"))+
  geom_histogram(aes(x=sum_individual_bonds_FALSE))+
  xlab("Sum of hypothetical individual well bonds for non-TA wells")+
  scale_x_continuous(label=dollar)+
  #ggtitle("Histogram of hypothetical individual well bonds \n owed for TA wells")+
  labs(caption="For firms that operate more than TA state/fee wells. \n Vertical black line drawn at the blanket amount.")+
  geom_vline(xintercept = 250000)+
  theme_bw()
ggsave(filename=paste(codedirectory,"Figures/Histogram_nonTA_100.jpg", sep=""),
       device="jpg",
       height=5,
       width=7)


ggplot(data=operator_summary)+
  geom_histogram(aes(x=plug_cost_lowprod))+
  xlab("Cost")+
  scale_x_continuous(label=dollar)+
  ggtitle("Histogram firm-level plugging costs for low production wells")+
  labs(caption="Low production defined as less than 2BOE per day.  \n Vertical line drawn at $15m.")+
  geom_vline(xintercept = 15000000)+
  theme_bw()
ggsave(filename=paste(codedirectory,"Figures/Histogram_lowprod_costs.jpg", sep=""),
       device="jpg",
       height=5,
       width=7)


ggplot(data=operator_summary)+
  geom_histogram(aes(x=total_plugcost))+
  xlab("Cost")+
  scale_x_continuous(label=dollar)+
  ggtitle("Histogram firm-level plugging costs for all fee/state wells")+
  labs(caption=" Vertical line drawn at $15m.")+
  geom_vline(xintercept = 15000000)+
  theme_bw()
ggsave(filename=paste(codedirectory,"Figures/Histogram_costs.jpg", sep=""),
       device="jpg",
       height=5,
       width=7)

write.csv(panel, "DataOutput/panel.csv")
write.csv(operator_summary, paste(codedirectory, "operator_summary.csv"))

