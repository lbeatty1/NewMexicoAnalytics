rm(list=ls())

datadirectory = "C:/Users/lbeatty/OneDrive - Environmental Defense Fund - edf.org/OrphanedWells/State Advocacy/NM/Data"
codedirectory = "C:/Users/lbeatty/Documents/NewMexicoAnalytics/"

setwd(datadirectory)

library(tidyverse)
library(lubridate)
library(data.table)
library(ggplot2)
library(scales)

source(paste(codedirectory, "Utah_bond_funs.R", sep=""))

panel=fread("DataOutput/panel.csv", colClasses = "character")
wellhistory=fread("OCD_Converted/wellhistory.csv", colClasses = "character")

############
## Format ##
############

panel = panel%>%
  mutate(date=as.Date(date, "%Y-%m-%d"),
         year = year(date),
         Oil=as.numeric(Oil),
         Gas=as.numeric(Gas))

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

print(paste("Updated number of unique operators in well data:", length(unique(wellhistory$ogrid_cde))))
print(paste("Updated number of unique wells in wells data: ", length(unique(wellhistory$API))))



##### Get depth by API10
wellhistory = wellhistory%>%
  mutate(dpth_tvd_num = replace(dpth_tvd_num, dpth_tvd_num=="99999", NA),
         dpth_tvd_num = replace(dpth_tvd_num, dpth_tvd_num=="0", NA),
         dpth_mvd_num = replace(dpth_mvd_num, dpth_mvd_num=="99999", NA),
         dpth_mvd_num = replace(dpth_mvd_num, dpth_mvd_num=="99999", NA),
         dpth_tvd_num = as.numeric(dpth_tgt_num),
         dpth_mvd_num = as.numeric(dpth_mvd_num),
         depth = coalesce(dpth_mvd_num,dpth_tvd_num))

### Make a wells dataset with unique APIs
# wells dataset will have record effective at 12-01-2022
wells = wellhistory%>%
  filter(eff_dte<=as.Date("2022-12-01"),
         rec_termn_dte>as.Date("2022-12-01"))%>%
  select(API, ogrid_cde, eff_dte, rec_termn_dte, lease_typ_cde, well_typ_cde, status, spud_dte, plug_dte, depth)

meanwelldepth = mean(wells$depth[which(wells$depth!=0)])

ggplot(wells%>%filter(depth!=0, depth<30000))+
  geom_histogram(aes(x=depth))+
  theme_bw()+
  ggtitle("Histogram of Well Depths")+
  ylab("Count")+
  xlab("Depth (ft)")

wells=wells%>%
  mutate(depth=replace(depth, depth==0, meanwelldepth),
         spud_dte=replace(spud_dte, spud_dte==Inf, NA))

################################
## Aggregate production####
################################

#look from 2022-06-01 to 2023-05-01 for data availability
past_12_prod = panel%>%
  filter(date>=as.Date("2022-01-01"),
         date<as.Date("2023-01-01"))%>%
  group_by(API)%>%
  summarise(Oil=sum(Oil),
            Gas=sum(Gas))

status_122022 = panel%>%
  filter(date=="2022-12-01")%>%
  select(API, time_shutin, time_producing)  #save length of inactivity at 052023

status_max = panel%>%
  group_by(API)%>%
  filter(date<=as.Date("2023-01-01"))%>%
  arrange(date)%>%
  mutate(rownumber=row_number())%>%
  slice_max(n=1, order_by=rownumber)%>%
  select(API, time_shutin, time_producing)%>%
  rename(time_shutin_imputed = time_shutin,
         time_producing_imputed = time_producing)

past_12_prod=left_join(past_12_prod, status_122022, by="API")
past_12_prod = left_join(past_12_prod, status_max, by="API")

#replace missing with imputed values
past_12_prod = past_12_prod%>%
  mutate(time_shutin=coalesce(time_shutin, time_shutin_imputed),
         time_producing = coalesce(time_producing, time_producing_imputed))%>%
  select(-c(time_shutin_imputed, time_producing_imputed))

well_data=left_join(wells, past_12_prod, by=c("API"))

####################################
## Produce flags, figure out bonds##
####################################
#calculate BOE/day, make flags for status and depth
well_data=well_data%>%
  mutate(Oil=replace(Oil, is.na(Oil), 0),
         Gas=replace(Gas, is.na(Gas), 0),
         BOEtot=Oil+Gas*6,
         BOEperday = BOEtot/365,
         marginal_flag=BOEperday<=2,
         inactive_flag = status%in%c("S", "T", "E"),
         inactive_marginal_flag=pmax(marginal_flag, inactive_flag),
         fee_state_flag=lease_typ_cde=="S"|lease_typ_cde=="P",
         shutin12_flag = time_shutin>12,                    #saves whether a well needs to be individually bonded
         depth_1000_flag=depth<=1000,
         depth_1000_3000_flag=depth>1000&depth<=3000,
         depth_3000_6000_flag = depth>3000&depth<=6000,     #these are the new depth thresholds
         depth_6000_9000_flag = depth>6000&depth<=9000,
         depth_9000_12000_flag = depth>9000&depth<=12000,
         depth_12000_flag = depth>12000,
         shutin12_flag = replace(shutin12_flag, is.na(shutin12_flag), 0)) #bunch of missing shutin12 flags -- looks like these are mostly injection wells but I'll fill all the missings in with 0 to be conservative

well_data%>%group_by(inactive_marginal_flag)%>%summarise(n=n())

############################
#### CALCULATE OPERATOR TIER
############################
#need total production, percentage inactive wells
#also save avg depth, total inactive fee/state wells for use in bond calculation later

#Save total wells, average depth, total production (for use in calculating tier)
operator_stats = well_data%>%
  group_by(ogrid_cde)%>%
  summarise(tot_operator_wells=n(),
            avg_depth=mean(depth,na.rm=T),
            tot_BOE=sum(BOEtot))

#save number of fee/state wells (used to calculate blanket bond amount for tiered firms)
#exclude wells shutin more than 12 months since these must be bonded separately
operator_feestate = well_data%>%
  filter(fee_state_flag==1,
         shutin12_flag==0)%>%
  group_by(ogrid_cde)%>%
  summarise(tot_feestate_wells = n())

operator_stats = left_join(operator_stats, operator_feestate, by="ogrid_cde")

#figure out percentage inactive for each operator (used to calculate tier)
inactive_operator_dat=well_data%>%
  group_by(ogrid_cde, inactive_marginal_flag)%>%
  summarise(tot_inactive=n())%>%
  group_by(ogrid_cde)%>%
  mutate(tot_wells = sum(tot_inactive),
         pct_inactive=tot_inactive/tot_wells)%>%
  filter(inactive_marginal_flag==1)%>%
  select(ogrid_cde, tot_inactive, pct_inactive)

operator_stats = left_join(operator_stats, inactive_operator_dat, by="ogrid_cde")


### Calculate tier
operator_stats = operator_stats%>%
  mutate(tot_inactive = replace(tot_inactive, is.na(tot_inactive), 0),
         pct_inactive = replace(pct_inactive, is.na(pct_inactive), 0),
         BOEperday=tot_BOE/365,
         tier1 = BOEperday>=1000&pct_inactive<=0.15,
         tier2 = BOEperday>=500&pct_inactive<=0.2,
         tier3 = (BOEperday>=200&pct_inactive<=0.25)|(BOEperday>=1000),
         tier=4-tier1-tier2-tier3)

######################
## CALCULATE BONDS ##
######################

# group by operator save wells of each depth by whether well shutin>12 months and whether fee/state
operator_dat = well_data%>%
  group_by(ogrid_cde, shutin12_flag, fee_state_flag)%>%
  summarise(tot_wells=n(),
            depth_1000_wells = sum(depth_1000_flag,na.rm=T),
            depth_1000_3000_wells = sum(depth_1000_3000_flag,na.rm=T),
            depth_3000_6000_wells = sum(depth_3000_6000_flag,na.rm=T),
            depth_6000_9000_wells = sum(depth_6000_9000_flag, na.rm=T),
            depth_9000_12000_wells = sum(depth_9000_12000_flag,na.rm=T),
            depth_12000_wells = sum(depth_12000_flag,na.rm=T))


#save inactive wells on fee/state land
inactive_operator_dat=well_data%>%
  filter(shutin12_flag==0)%>%  #since these will be bonded separately, dont want them to also be bonded by tier bonds
  group_by(ogrid_cde, inactive_marginal_flag, fee_state_flag)%>%
  summarise(tot_inactive_feestate=n())%>%
  filter(inactive_marginal_flag==1,
         fee_state_flag==1)%>%
  ungroup()%>%
  select(ogrid_cde, tot_inactive_feestate)

operator_stats=left_join(operator_stats, inactive_operator_dat, by="ogrid_cde")

#some operators don't have feestate inactive wells
operator_stats = operator_stats%>%
  mutate(tot_inactive_feestate=replace(tot_inactive_feestate, is.na(tot_inactive_feestate),0),
         tot_feestate_wells = replace(tot_feestate_wells, is.na(tot_feestate_wells),0))


## Calculate new blanket bonds
operator_stats = operator_stats%>%
  mutate(tier1blanket = sapply(tot_feestate_wells, tier1_blanket),
         tier1marginal = sapply(avg_depth,tier1_marginalbond)*tot_inactive_feestate,
         tier2blanket = sapply(tot_feestate_wells, tier2_blanket),
         tier2marginal = sapply(avg_depth, tier2_marginalbond)*tot_inactive_feestate,
         tier3blanket = sapply(tot_feestate_wells, tier3_blanket),
         tier3marginal = sapply(avg_depth,tier3_marginalbond)*tot_inactive_feestate)


## Calculate per-well bonds
#per-well bond numbers come from April DOGM draft
operator_individualbonds = operator_dat%>%
  filter(fee_state_flag==1)%>%
  mutate(depth_1000_bond = depth_1000_wells*10000,
         depth_1000_3000_bond = depth_1000_3000_wells*20000,
         depth_3000_6000_bond = depth_3000_6000_wells*40000,
         depth_6000_9000_bond = depth_6000_9000_wells*65000,
         depth_9000_12000_bond = depth_9000_12000_wells*85000,
         depth_12000_bond = depth_12000_wells*110000,
         individual_well_bond = depth_1000_bond+depth_1000_3000_bond+depth_3000_6000_bond+depth_6000_9000_bond+depth_9000_12000_bond+depth_12000_bond)

#separate data for wells shutin longer than 12 months
operator_dat_shutin12 = operator_individualbonds%>%
  filter(shutin12_flag==1)%>%
  rename(shutin_well_bond = individual_well_bond,
         tot_shutin12_feestate_wells=tot_wells)%>%
  ungroup()%>%
  select(ogrid_cde, shutin_well_bond, tot_shutin12_feestate_wells)

#now just wells not shuting longer than 12 months
operator_individualbonds = operator_individualbonds%>%
  filter(shutin12_flag==0)%>%
  ungroup()%>%
  select(ogrid_cde, individual_well_bond)

operator_stats = left_join(operator_stats, operator_dat_shutin12, by="ogrid_cde")
operator_stats = left_join(operator_stats, operator_individualbonds, by="ogrid_cde")

#operator total bond will be sum of blanket bond, marginal/inactive fee/state bond, and individual well bonds for shutin wells shutin for more than 12 months
#for operators not meeting tiers, its simply the sum of individual well bonds
operator_stats = operator_stats%>%
  mutate(shutin_well_bond=replace(shutin_well_bond, is.na(shutin_well_bond), 0),
         individual_well_bond=replace(individual_well_bond, is.na(individual_well_bond), 0),
         bond = (tier1blanket+tier1marginal)*(tier==1)+(tier2blanket+tier2marginal)*(tier==2)+(tier3blanket+tier3marginal)*(tier==3)+individual_well_bond*(tier==4)+shutin_well_bond)



##########################################
#####  Calculate Plugging Liabilities ####
##########################################

#liability1 is from the spreadsheet on average decomissioning costs by state
#liability2 is the median decomissioning cost in Raimi
#liability3 assumes $6 per foot of depth
#liability4 assumes $12 per foot of depth
well_data = well_data%>%
  mutate(liability1 = 37500,
         liability2 = 75000,
         liability3 = depth*6,
         liability4 = depth*12)

operator_liabilities = well_data%>%
  group_by(ogrid_cde)%>%
  summarise(liability1=sum(liability1),
            liability2=sum(liability2),
            liability3=sum(liability3),
            liability4=sum(liability4))

operator_stats = left_join(operator_stats, operator_liabilities, by="ogrid_cde")
operator_stats=operator_stats%>%
  arrange(desc(tier))%>%
  mutate(tier=as.character(tier),
         tier=replace(tier, tier=="4", "No tier"))

#PLOT LIABILITIES AGAINST BONDS
#ASSUMPTION 4
ggplot(data=operator_stats%>%filter(bond<25000000))+
  geom_point(aes(x=bond, y=liability4, colour=tier))+
  geom_abline(slope=1, intercept=0)+
  #ggtitle("Liabilities exceed bond amounts for large firms")+
  scale_y_continuous(labels = dollar)+
  ylab("Total Plugging Liabilities")+
  scale_x_continuous(label=dollar)+
  xlab("Required Bonds")+
  labs(caption="Plot of firm-level total estimated plugging liabilities against required bonds. \n A line is plotted at y=x. Plugging costs assume each well costs $12 per foot to plug.")+
  theme_bw()

ggsave(filename=paste(codedirectory,"Figures/UtahReg/BondLiability.jpg", sep=""),
       device="jpg",
       height=5,
       width=7)

ggplot(data=operator_stats%>%filter(bond<5000000, liability4<100000000))+
  geom_point(aes(x=bond, y=liability4, colour=tier))+
  geom_abline(slope=1, intercept=0)+
  #ggtitle("If plugging costs are high then the policy still looks fairly good.")+
  scale_y_continuous(labels = dollar)+
  ylab("Total Plugging Liabilities")+
  scale_x_continuous(label=dollar)+
  xlab("Required Bonds")+
  labs(caption="Plot of firm-level total estimated plugging liabilities against required bonds. \n A line is plotted at y=x. Plugging costs assume each well costs $12 per foot to plug.")+
  theme_bw()

ggsave(filename=paste(codedirectory,"Figures/UtahReg/BondLiability_zoom.jpg", sep=""),
       device="jpg",
       height=5,
       width=7)

#####################
## What if we just consider marginal and inactive wells?
##
operator_liabilities_marginalinactive = well_data%>%
  filter(inactive_marginal_flag==1)%>%
  group_by(ogrid_cde)%>%
  summarise(liability1_marginal=sum(liability1),
            liability2_marginal=sum(liability2),
            liability3_marginal=sum(liability3),
            liability4_marginal=sum(liability4))

operator_stats = left_join(operator_stats, operator_liabilities_marginalinactive, by="ogrid_cde")

## Assumption 4
ggplot(data=operator_stats%>%filter(bond<10000000))+
  geom_point(aes(x=bond, y=liability4_marginal, colour=tier))+
  geom_abline(slope=1, intercept=0)+
  #ggtitle("If plugging costs are high, then firms which don't meet tier \n requirements (tier 4) look covered.")+
  scale_y_continuous(labels = dollar)+
  ylab("Total Plugging Liabilities for Marginal/Inactive Wells")+
  scale_x_continuous(label=dollar)+
  xlab("Required Bonds")+
  labs(caption="Plot of firm-level total estimated plugging liabilities against required bonds. \n A line is plotted at y=x. Plugging costs assume each well costs $12/foot to plug.")+
  theme_bw()
ggsave(filename=paste(codedirectory,"Figures/UtahReg/BondLiability_InactiveMarginal.jpg", sep=""),
       device="jpg",
       height=5,
       width=7)

############################
## What about only fee/state liabilities
operator_liabilities_feestate = well_data%>%
  filter(fee_state_flag==1)%>%
  group_by(ogrid_cde)%>%
  summarise(liability1_feestate=sum(liability1),
            liability2_feestate=sum(liability2),
            liability3_feestate=sum(liability3),
            liability4_feestate=sum(liability4))
operator_stats = left_join(operator_stats, operator_liabilities_feestate, by="ogrid_cde")


ggplot(data=operator_stats%>%filter(bond<10000000))+
  geom_point(aes(x=bond, y=liability4_feestate, color=tier))+
  geom_abline(slope=1, intercept=0)+
  #ggtitle("New bonds cover marginal and inactive well plugging liability")+
  scale_y_continuous(labels = dollar)+
  ylab("Total Plugging Liabilities for Fee/State Wells")+
  scale_x_continuous(label=dollar)+
  xlab("Required Bonds")+
  labs(caption="Plot of firm-level total estimated plugging liabilities against required bonds. \n A line is plotted at y=x. Plugging costs assume each well costs $12 per foot to plug.")+
  theme_bw()

ggsave(filename=paste(codedirectory,"Figures/UtahReg/BondLiability_FeeState.jpg", sep=""),
       device="jpg",
       height=5,
       width=7)



#######################
## Feestate Marginal

operator_liabilities_feestate_marginal = well_data%>%
  filter(fee_state_flag==1,
         inactive_marginal_flag==1)%>%
  group_by(ogrid_cde)%>%
  summarise(liability1_feestate_marginal=sum(liability1),
            liability2_feestate_marginal=sum(liability2),
            liability3_feestate_marginal=sum(liability3),
            liability4_feestate_marginal=sum(liability4))
operator_stats = left_join(operator_stats, operator_liabilities_feestate_marginal, by="ogrid_cde")


ggplot(data=operator_stats%>%filter(bond<10000000))+
  geom_point(aes(x=bond, y=liability4_feestate_marginal, color=tier))+
  geom_abline(slope=1, intercept=0)+
  #ggtitle("New bonds cover marginal and inactive well plugging liability")+
  scale_y_continuous(labels = dollar)+
  ylab("Total Plugging Liabilities for Fee/State Marginal/Inactive Wells")+
  scale_x_continuous(label=dollar)+
  xlab("Required Bonds")+
  labs(caption="Plot of firm-level total estimated plugging liabilities against required bonds. \n A line is plotted at y=x. Plugging costs assume each well costs $12 per foot to plug.")+
  theme_bw()
ggsave(filename=paste(codedirectory,"Figures/UtahReg/BondLiability_MarginalInactiveFeeState.jpg", sep=""),
       device="jpg",
       height=5,
       width=7)

