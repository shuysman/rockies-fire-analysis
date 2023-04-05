# > version
# _                           
# platform       x86_64-w64-mingw32          
# arch           x86_64                      
# os             mingw32                     
# system         x86_64, mingw32             
# status                                     
# major          3                           
# minor          3.1                         
# year           2016                        
# month          06                          
# day            21                          
# svn rev        70800                       
# language       R                           
# version.string R version 3.3.1 (2016-06-21)
# nickname       Bug in Your Hair  


#Script for extracting dryness indexes associated with fire from water balance
#Fire polygons are from MTBS data base
#Water balance is daily using Daymet climate input to water balance model "Daymet_Penman_Hamon_Batch.xlsm"
#Steps
#Identify ignition date in MTBS database for polygon
#Find that date in the daily water balance file associated with same polygon
#Compute backward sums or other stats for each water balance metric of interest

#set default R package library path
#.libPaths(c("C:\\Users\\dthoma\\R packages", .libPaths()))

library(xts)
library(TTR)
library(dplyr)
library(ggplot2)
library(reshape2)
library(devtools)
#library(easyGgplot2)
library(extrafont)
library(data.table)
library(lubridate)
library(lme4)
##require(randomForest)
require(MASS)#Package which contains the Boston housing dataset
library(hier.part) ## Removed from CRAN R 4.2.3
library(tidyverse)
library(MuMIn)
library(pROC)
library(ROCR)
library(plotROC)
library(colorspace)
library(here)

#loadfonts()#only have to do this once
# If you want to output to .ps files instead of .pdf, use:
# loadfonts(device="postscript")
# After the fonts are registered with R's PDF device, you can create figures with them. 
fonts()
windowsFonts(Times=windowsFont("TT Times New Roman"))

#next line is very slow, so only do once
#font_import(paths = NULL, recursive = TRUE, prompt = TRUE,pattern = NULL)


##setwd(here())

setwd("/home/steve/OneDrive/nothern-rockies-dryness")
srmaster<-read.csv("NR_master.csv",header=TRUE, stringsAsFactors=FALSE);head(srmaster)

##gye_fires <-read_csv('test-gye-fire-polys.csv') ## Need to get full GYE data from Mike, missing a few points in test file
gye_fires <-read_csv('MR_fire_points_extracted.csv')  ## Sites from middle rockies extracted in QGIS

srmaster <- srmaster %>%
    filter(Event_ID %in% gye_fires$Event_ID)  ## Filter srmaster to fires occuring in GYE, or other area of interest

#due to small shrub sample size n=10 group shrub and herb into non-forest type
unique(srmaster$maj_veg_cl)
a<-which(srmaster$maj_veg_cl=="Herb");a
srmaster$maj_veg_cl[a]<-"non-forest"
a<-which(srmaster$maj_veg_cl=="Shrub");a
srmaster$maj_veg_cl[a]<-"non-forest"
a<-which(srmaster$maj_veg_cl=="Tree");a
srmaster$maj_veg_cl[a]<-"forest"
a<-which(srmaster$maj_veg_cl=="Water");a
srmaster$maj_veg_cl[a]<-"non-forest"
a<-which(srmaster$maj_veg_cl=="Sparse");a
srmaster$maj_veg_cl[a]<-"non-forest"
a<-which(srmaster$maj_veg_cl=="Agriculture");a
srmaster$maj_veg_cl[a]<-"non-forest"
unique(srmaster$maj_veg_cl)
srmaster <- rename(srmaster, maj_class = maj_veg_cl)

srmaster <- srmaster %>%
    separate(Ig_Date, c("Year", "Month", "Day"), sep="/", remove = FALSE) %>%
    mutate_at(vars(Year,Month,Day), as.numeric)

#build some charts of fire data showing histogram of fire doy to determine fire season and histogram of size
dtx<-as.Date(srmaster$Ig_Date, format = "%Y/%m/%d");dtx#format date as POSIX so you and add and subtract number of days that bracket time period of interest

doy<- as.numeric(strftime(dtx, format = "%j"));doy
srmaster<-cbind(srmaster,doy);head(srmaster)
str(srmaster);names(srmaster)
srmaster$maj_class<-as.factor(srmaster$maj_class)#maj_class is the majority of pixels classification from Landfire 30m pixels in burned areas
str(srmaster)

#convert burned area to ha and take log so scale of burns is more legible in figures.
srmaster <- rename(srmaster, Acres = BurnBndAc)
halog<- log(srmaster$Acres * 0.404686);halog 
srmaster<-cbind(srmaster,halog);str(srmaster)

#filter srmaster to include only wildfires
srmasterwf<-subset(srmaster, srmaster$Incid_Type=="Wildfire");head(srmasterwf)



#histogram of fire size and frequency
#ggplot2.histogram(data=srmasterwf, xName="halog", groupName="maj_class",addDensityCurve=TRUE)+ ylab("Fire frequency")+xlab("Area (log ha)")
#ggplot2.histogram(data=srmasterwf, xName="halog", groupName="maj_class",addDensityCurve=FALSE)+ ylab("Fire Count")+xlab("Area (log ha)")


# Basic histogram of fires by Year.  helps identify big fire years in this case 2000, 2002 and 2012
hplot<-ggplot(srmasterwf, aes(x=Year,group=maj_class, fill=maj_class)) + geom_histogram(binwidth=1,position="dodge")+ #
   ylab("Fire frequency")  + 
  scale_x_continuous("Year", breaks=seq(1985,2015,1), limits=c(1985,2015))+ 
  theme_bw() + #theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  theme(axis.text=element_text(size=24,face="bold",family="Courier New"),
        axis.title=element_text(size=28,face="bold",family="Courier New"),
        title=element_text(size=20,face="bold",family="Courier New"))+ labs(title = "", x = NULL, y = "Area (log Ha)", color = NULL)+
  theme(legend.text=element_text(size=24),legend.position="right")+guides(color=guide_legend(ncol=2))
hplot + theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) + geom_density()


#fire size by year scatter plots
splot<- ggplot(srmasterwf, aes(x=Year, y=halog,color=maj_class)) + geom_point(size = 5) + stat_smooth(method = lm, lwd = 3)+# geom_bar(stat="identity", position=position_dodge())+#
  scale_x_continuous("Year", breaks=seq(1985,2015,1), limits=c(1985,2015))+ 
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  theme(axis.text=element_text(size=24,face="bold",family="Courier New"),
        axis.title=element_text(size=28,face="bold",family="Courier New"),
        title=element_text(size=20,face="bold",family="Courier New"))+ labs(title = "", x = NULL, y = "Area (log Ha)", color = NULL)+
  theme(legend.text=element_text(size=24),legend.position="bottom")+guides(color=guide_legend(nrow=1))
splot+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

#plot density curves for fires by year by dominant veg type
hplot<-ggplot(srmasterwf,aes(x=Year,color=maj_class)) + geom_density()
hplot+ ylab("Fire frequency")
#ggplot2.histogram(data=srmasterwf, xName="Year", groupName="maj_class",addDensityCurve=TRUE)+ ylab("Fire frequency")+xlab("Year")
ggplot(srmasterwf, aes(x=Year,fill = maj_class)) +  geom_histogram(binwidth = 1, position="dodge")+ylab("Count of Fires")+  scale_x_continuous()
ggplot(srmasterwf, aes(x = Year, fill = maj_class)) + geom_histogram(aes(y = ..density..), binwidth = 1) +  geom_density(alpha=0.2)

### aggregate variables by year month
df2 <- setDT(srmasterwf)[, lapply(.SD, sum), by=.(Year), .SDcols=c("Acres","halog")]
setDF(df2);df2#make it back into a dataframe
setDF(srmasterwf);str(srmasterwf)#setDF needed here because as.data.frame on setDT variable doesn't strip data table attributes 
df3<-df2[order(df2$Acres),];df3#order
df4<-cbind(df3,df3[,"Acres"]*0.404686);names(df4)<-c("Year","Acres","Log ha","ha");df4

#count of wildfires during period of record
nrow(srmasterwf)

# Basic histogram of fires by doy.  helps identify fire seasonality
hplot<-ggplot(srmasterwf, aes(x=doy,color=maj_class)) + geom_histogram(aes(y=..density..), colour="black", fill="white",binwidth=14)+
  geom_density(alpha=.2)+ geom_vline(aes(xintercept=mean(doy)),color="blue", linetype="dashed", size=1) + ylab("Fire frequency")+
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  theme(axis.text=element_text(size=18,face="bold",family="Courier New"),
        axis.title=element_text(size=20,face="bold",family="Courier New"),
        title=element_text(size=20,face="bold",family="Courier New"))+
  labs(title = "", x = NULL, y = "Area (log Ha)", color = NULL)+
  theme(legend.text=element_text(size=16),legend.position="right")+guides(color=guide_legend(ncol=1))
hplot+ xlab("Day of Year")

#plot density curves for fires by doy by dominant veg type. figure above split out by veg type
hplot<-ggplot(srmasterwf,aes(x=doy,color=maj_class)) + geom_density(alpha = 0.2, lwd=3)+ #geom_vline(aes(xintercept=mean(doy)), color=maj_class,size=1.5)+
  scale_x_continuous( breaks=seq(0,365,30), limits=c(0,365))+
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  theme(axis.text=element_text(size=24,face="bold",family="Courier New"),
        axis.title=element_text(size=28,face="bold",family="Courier New"),
        title=element_text(size=20,face="bold",family="Courier New"))+
  labs(title = "", x = NULL, y = "Area (log Ha)", color = NULL)+
  theme(legend.text=element_text(size=16),legend.position="right")+guides(color=guide_legend(ncol=1))
hplot+ ylab("Fire frequency") + xlab("Day of Year")
#hplot + scale_x_continuous(sec.axis = sec_axis(~.+30.4)) # need x axis as calendar month

#if using subsets according to forest vs. non forest
forest<-subset(srmasterwf, maj_class=="forest");forest#note class Tree has a space at end
cumfun_trees<-ecdf(forest$doy)#cumulative distributino function for fire start by day of year
plot(cumfun_trees, xlab="Day of Year", ylab="Cumulative Distirbution Function")
forest_q<-quantile(forest$doy,c(0,1));forest_q#90% of data fall within this range 0.05-0.95# 0.27 to 0.75 or 0,1 to determine min max
abline(v=forest_q)

nonforest<-subset(srmasterwf, maj_class=="non-forest");nonforest#note class Tree has a spac.e at end
cumfun_nonforest<-ecdf(nonforest$doy)#cumulative distributino function for fire start by day of year
plot(cumfun_nonforest, xlab="Day of Year", ylab="Cumulative Distirbution Function")
nonforest_q<-quantile(nonforest$doy,c(0,1));nonforest_q#90% of data fall within this range 0.05-0.95# 0.27 to 0.75 or 0,1 to determine min max
abline(v=nontrees_q)


# #if using subsets according to functional type
# trees<-subset(srmasterwf, maj_class=="Tree ");trees#note class Tree has a space at end
# cumfun_trees<-ecdf(trees$doy)#cumulative distributino function for fire start by day of year
# plot(cumfun_trees, xlab="Day of Year", ylab="Cumulative Distirbution Function")
# trees_q<-quantile(trees$doy,c(0,1));trees_q#90% of data fall within this range 0.05-0.95# 0.27 to 0.75 or 0,1 to determine min max
# abline(v=trees_q)
# 
# herbs<-subset(srmasterwf, maj_class=="Herb ");herbs#note class Tree has a space at end
# cumfun_herbs<-ecdf(herbs$doy)#cumulative distributino function for fire size
# hp<-plot(cumfun_herbs, xlab="Day of Year", ylab="Cumulative Distirbution Function")
# abline(hp)
# herbs_q<-quantile(herbs$doy,c(0,1));herbs_q#90% of data fall within this range 0.05-0.95# 0.27 to 0.75 or 0,1 to determine min max
# abline(v=herbs_q)
# 
# shrubs<-subset(srmasterwf, maj_class=="Shrub");shrubs#note class Tree has a space at end
# cumfun_shrubs<-ecdf(shrubs$doy)#cumulative distributino function for fire size
# sp<-plot(cumfun_shrubs, xlab="Day of Year", ylab="Cumulative Distirbution Function")
# shrubs_q<-quantile(shrubs$doy,c(0,1));shrubs_q#90% of data fall within this range 0.05-0.95# 0.27 to 0.75 or 0,1 to determine min max
# abline(v=shrubs_q)

ggplot(srmasterwf,aes(x = doy)) + stat_ecdf(aes(colour = maj_class), lwd=3)+
  theme_bw() + theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  theme(axis.text=element_text(size=24,face="bold",family="Courier New"),
        axis.title=element_text(size=28,face="bold",family="Courier New"),
        title=element_text(size=20,face="bold",family="Courier New"))+
  labs(title = "", x = "Day of Year", y = "ECDF", color = NULL)+
  theme(legend.text=element_text(size=16),legend.position="right")+guides(color=guide_legend(ncol=1))

#start and end dates of fire season by vegetation type derived from day of year in min_max table
doy_min_max<-as.data.frame(rbind(forest_q, nonforest_q));doy_min_max
forest_start<-as.Date(doy_min_max[1,1], origin = "1984-01-01");forest_start
nonforest_start<-as.Date(doy_min_max[2,1], origin = "1984-01-01");nonforest_start
forest_end<-as.Date(doy_min_max[1,2], origin = "2021-01-01");forest_end
nonforest_end<-as.Date(doy_min_max[2,2], origin = "2021-01-01");nonforest_end

# > doy_min_max
#0% 100%
# forest_q     7  303  forest start doy 7 forest end doy 303
# nonforest_q 74  301  non-forest start doy 74 non-forest end doy 301 

# #start and end dates of fire season by vegetation type derived from day of year in min_max table
# doy_min_max<-as.data.frame(rbind(trees_q, herbs_q,shrubs_q));doy_min_max
# tree_start<-as.Date(doy_min_max[1,1], origin = "1980-01-01");tree_start
# herb_start<-as.Date(doy_min_max[2,1], origin = "1980-01-01");herb_start
# shrub_start<-as.Date(doy_min_max[3,1], origin = "1980-01-01");shrub_start
# tree_end<-as.Date(doy_min_max[1,2], origin = "2017-01-01");tree_end
# herb_end<-as.Date(doy_min_max[2,2], origin = "2017-01-01");herb_end
# shrub_end<-as.Date(doy_min_max[3,2], origin = "2017-01-01");shrub_end
# 
# #doy ranges of fire season
# tree_range<-doy_min_max[1,1]:doy_min_max[1,2];tree_range
# herb_range<-doy_min_max[2,1]:doy_min_max[2,2];herb_range
# shrub_range<-doy_min_max[3,1]:doy_min_max[3,2];shrub_range
# 

#determine quartile stats for ignition doy by veg class
names(srmasterwf)
doy_stats<-tapply(srmasterwf$doy,srmasterwf$maj_class, summary);doy_stats;str(doy_stats)
doy_stats_sd<-tapply(srmasterwf$doy,srmasterwf$maj_class, sd);doy_stats_sd
doy_stats2<-do.call(rbind.data.frame, doy_stats) ;doy_stats2;str(doy_stats2)
names(doy_stats2)<-c("Min","25%","Median","Mean","75%","Max");doy_stats2

#count of fires by dominant veg type
str(srmasterwf$maj_class)
fire_count <- srmasterwf %>% group_by(maj_class) %>% summarise(no_rows = length(maj_class))   
doy_stats3<-cbind(fire_count[,2],doy_stats2);names(doy_stats3)<-c("n","Min","25%","Median","Mean","75%","Max");doy_stats3

#set up a loop to loop across the burn polygons
nrow<-nrow(srmasterwf);nrow

#create a list of site names with .csv extension that matches waterbalance outfile naming convention
sites<-unique(srmasterwf$Event_ID);sites
site_list<-NULL
igdates<-NULL
n=1
for (n in 1:length(sites)){
  site<-paste(srmasterwf[n,1],".csv",sep='');site
  site_list[n]<-site
  igdate<-srmasterwf[n,11]
  igdates[n]<-igdate
 }
igsite<-as.data.frame(cbind(site_list,igdates), stringsAsFactors = FALSE);igsite

#loop through site names
#for each site open the corresponding waterbalance file to extract dryness metrics near the ignition date
setwd("/media/steve/storage/waterbalance/fire/out/")
n= 2
##########################################################################################################################
#######percentiles analysis##############################################################################################
######build stacks of wb data corresponding to ig date, week of ignition date and across all years on doy of ignition####
##########################################################################################################################

#sum7d<-NULL
#sum7d_stack<-NULL# summary of wb 7 days prior to ignition
#doy_ann_stack<-NULL#water balance on doy of ignition and across all years prior to ignition
#burn_ranks<-NULL
igstatus_stack<-NULL# holder for water balance conditions on day of ignition
ig_status_wb<-NULL
ig_end_wb<-NULL
cur_yr_wb<-NULL
lag_one_wb<-NULL
rankstack<-NULL
rankstack_tree<-NULL
rankstack_herb<-NULL
rankstack_shrub<-NULL
rankstack_forest<-NULL
rankstack_nonforest<-NULL
sum7d<-NULL
sum7d_stack<-NULL
igvalstack<-NULL

n <- 1
##n=146#this site (117) had D=0 on day of ignition which could make it zeroth percentile.  Seems odd, but could happen 
for (n in 1:nrow(igsite)){
  site<-igsite[n,1];site
  dt<-igsite[n,2];dt
  dtx<-as.Date(dt, format = "%Y/%m/%d");dtx#format date as POSIX so you and add and subtract number of days that bracket time period of interest
  #subset out a year of data prior to ignition
  #then use rollapply functions to summarize dryness conditions at daily, weekly, monthly intervals prior to ignition
  #this way you can easily modify periods of interest in roll apply function rather than building interval specific subsets which would balloon
  #define periods of interest for extracting wb 
  start_date <- '1984-01-01'#1984 matches the beginning of MTBS data base
  ig_date <- dtx;ig_date 
  ig_cal_end<- ceiling_date(ig_date,"year") - days(1);ig_cal_end#end of calendar year date for each ignition
  ig_cal_start<- ceiling_date(ig_date,"year") - years(1);ig_cal_start
  lag1_end<- ceiling_date(ig_cal_start,"year") - days(1);lag1_end #end of prior calendar year date for each ignition
  lag1_start<-ceiling_date(lag1_end,"year") - years(1);lag1_start  
    
  sitewb<-na.omit(read.csv(site, stringsAsFactors=FALSE, blank.lines.skip = TRUE));head(sitewb)
  ##convert relative humidity to relative dryness as 100-rh
  sitewb <- sitewb %>% mutate(RH = 100 - RH)
  nrow(sitewb); names(sitewb)
  colnames(sitewb)<-c("line", "Date","P","D", "AET", "SOIL", "DRO", "RAIN", "PACK", "PET", "VPD", "RD", "Tmax", "Tmin", "Tavg", "SOIL_Delta", "GDD")
  names(sitewb)
  sitewb <- sitewb %>% separate(Date, c("year", "month", "day"), sep = "-", remove = FALSE) %>% mutate_at(vars(year,month,day), as.numeric)
  corewb<-sitewb[,c(-1,-2)];head(corewb)#drop dates and strings prior to converting to xts
  #keep any strings out of the xts matrix or it will convert everything to strings with obnoxious quotes
                                        #sitecol<-sitewb[,25]#in case you need to add the site information back into a data frame
  f2<-as.Date(sitewb[,2], format = "%Y-%m-%d");head(f2);length(f2)
  wbxts<-xts(coredata(corewb),order.by=f2);head(wbxts)#build xts data structure 
  awssm<-max(wbxts$SOIL)-wbxts$SOIL;head(awssm);names(awssm)<-"AWSSM"#  this should be added to water balance model; use max of soil moisture to estimate soil whc, if ran in really dry climate may never have maxed out soil capacity so this could be wrong
  doy<-yday(wbxts);head(doy)
  wbxts<-cbind(wbxts,awssm,doy);head(wbxts)
  names(wbxts)[ncol(wbxts)]<-"doy";head(wbxts)#rename the doy column in the xts
 
  ########daily wb summary#################
  # determine water balance conditions on date of ignition
  igstatus<-as.data.frame(wbxts[ig_date]);igstatus
  #add site attributes from master site file
  a<- which(srmasterwf$Event_ID == str_split(site, ".csv")[[1]][1]);a#identifies the row number in site file attribute file
  names(srmasterwf)
  attributes<-srmasterwf[a,c(1,8,4,33,49)];attributes#add site name (1), acres (17), fire type (19), veg class (56 - maj_class), day of year of ignition
  igstatus_att<-cbind(attributes,igstatus[,c("year","month","day","RD","VPD","Tavg","SOIL","GDD","AWSSM","RAIN","AET","D")]);igstatus_att
  igstatus_stack<-rbind(igstatus_att,igstatus_stack)
  names(igstatus_stack)
 
  #######wb summary on period of user specified days##############
  #determine average conditions for the 7 days prior to ignition
   #dtx7<-as.Date(dtx)-7;dtx7#7days prior to ignition
   # climout7<-wbxts[paste(dtx7,dtx, sep="/"), c("SVP","RH","VPD","P","TEMP","RAIN","PACK","W","PET","W_PET","SOIL","SOIL_Delta","AET","RUNOFF","D","GDD","AWSSM")];climout7
   # mean7<-colMeans(climout7[,c("SVP","RH","VPD","TEMP","SOIL","GDD","AWSSM")]);mean7
   # sum7<-colSums(climout7[,c("RAIN","AET","D")]);sum7
    #aggregate into a data frame means or sums as appropriate depending on nature of variable.  generally water should be summed, temperature and pressure averaged
    site_char<-as.data.frame(c(attributes,igstatus[1,1:3]));site_char
   # sum7d<-rbind(climbout7_agg,sum7d);sum7d
   #
 sum7d_stack<-rbind(sum7d,sum7d_stack)
   # names(sum7d_stack)
   #compute means and sums by week for daymet record 1980- until ignition plus 1 week so that ignition week is complete
   #historic<-wbxts[paste("1980-01-01",dtx, sep="/"), c("SVP","RH","VPD","P","TEMP","RAIN","PACK","W","PET","W_PET","SOIL","SOIL_Delta","AET","RUNOFF","D","GDD","AWSSM")]
   
   #Or, alternatively use a fixed period which will make it easier operationally to calculate fire danger versus a fixed period of time
                                        #historic<-wbxts[paste("1984-01-01","2015-12-31", sep="/"), c("SVP","RD","VPD","P","TEMP","RAIN","PACK","W","PET","W_PET","SOIL","SOIL_Delta","AET","RUNOFF","D","GDD","AWSSM")]
  historic<-wbxts[paste("1984-01-01","2021-12-31", sep="/"), c("RD","VPD","P","Tavg","RAIN","PACK","PET","SOIL","SOIL_Delta","AET","DRO","D","GDD","AWSSM")]
   head(historic);tail(historic)
   #f2<-as.data.frame(index(historic));#write.csv(f2,"f2.csv")
                                              
   ################calculate 14 day rolling mean or sums####################################
   ################k=?  is the number of days across which to summarize######################
                                        #ep1 <- endpoints(historic,on="days",k=14);ep1#build an index of windows over which you summarize
  ##period_means<-rollapply(historic[,c("SVP","RD","VPD","Tavg","SOIL","GDD","AWSSM")], 14, mean, by = 1, by.column = TRUE);period_means[1:16,]#rolling values start at doy 14
   period_means<-rollapply(historic[,c("RD","VPD","Tavg","SOIL","AWSSM")], 14, mean, by = 1, by.column = TRUE);period_means[1:16,]#rolling values start at doy 14
   period_sums<-rollapply(historic[,c("RAIN","AET","D", "GDD")], 14, sum, by = 1, by.column = TRUE)
   #period_means<-period.apply(historic[,c("SVP","RH","VPD","TEMP","SOIL","GDD","AWSSM")],INDEX=ep1,FUN=colMeans);tail(period_means)#summarize on those windows
   #weekly_means<-apply.weekly(historic[,c("SVP","RH","VPD","TEMP","SOIL","GDD","AWSSM")],colMeans);head(weekly_means)
   #period_sums<-period.apply(historic[,c("RAIN","AET","D")],INDEX=ep1,FUN=colSums);tail(period_sums)
   #weekly_sums<-apply.weekly(historic[,c("RAIN","AET","D")],colSums);head(weekly_sums)
   #weekly_agg<-cbind(weekly_means,weekly_sums);head(weekly_agg)#;str(weekly_agg)#these are the weekly data across all weeks
   period_agg<-cbind(period_means,period_sums);head(period_agg)#values start at row 14 or what ever the rolling mean is set to thus NA's
   #write.csv(period_agg,"period_agg.csv")
  
   #compute percentile rank of each week or peroid in the sequence of years 
   #f3<-index(weekly_agg)
   f3<-index(period_agg);#write.csv(f3,"f3.csv")
   #rankthis<-data.frame(date=index(weekly_agg), coredata(weekly_agg));head(rankthis)
   rankthis<-data.frame(date=index(period_agg), coredata(period_agg));head(rankthis);tail(rankthis)
                                        #ranks<-apply(rankthis[2:11],2,percent_rank);ranks[1:20,]#percentile ranks of every week in period of record
  ranks<-apply(rankthis[2:10],2,percent_rank);ranks[1:20,]#percentile ranks of every week in period of record
   ranksxts<-xts(coredata(ranks),order.by=f3);ranksxts[1:28,]#build xts data structure 
   periodicity(ranksxts)
   #ig_week<-floor_date(as.Date(dtx, "%Y/%m/%d"), unit="week");ig_week# a date index for week of interest when fire started
   #make a date range from ignition week that will include a date in ranksxts
   dtx
   #days padding ig_week make it a range that will vary based on ep1 k value set above.  
   #if k value positive set to set to 1/2 k value, if odd, set to 1/2 k value minus 1 
   #date_span<-paste(dtx-7,"/",dtx+7,sep="");date_span#note, must make no space before or after / for R to interpret as date range
   #igrank<-as.data.frame(ranksxts[date_span]);igrank#percentile ranks of water balance on week of ignition vs POR
   igrank<-as.data.frame(ranksxts[dtx]);igrank
   igrankatt<-cbind(attributes,igrank)
   rankstack<-rbind(igrankatt,rankstack)
   #igval<-as.data.frame(period_agg[date_span]);igval#water balance values rather than percentiles
   igval<-as.data.frame(period_agg[dtx]);igval#water balance values rather than percentiles
   
   #igval<-as.data.frame(weekly_agg[ig_week]);igval
   igvalatt<-cbind(attributes,igval);igvalatt
   igvalstack<-rbind(igvalatt,igvalstack);head(igvalstack)
  
  ####### wb summary on date of ignition##############
  #we shouldn't use summary stats, mean, min, max for these day of ingnition values so they get replaced with igstatus_stack from above
  #can comment out this section later, since it was used just to retain similarity in formatting of data compiled across longer time periods
  ig_status<-wbxts[dtx, c("RD","VPD","P","Tavg","RAIN","PACK","PET","SOIL","SOIL_Delta","AET","DRO","D","GDD","AWSSM")];head(ig_status)
  mean_ig_status<-colMeans(ig_status[,c("RD","VPD","Tavg","SOIL","D","GDD","AWSSM")]);mean_ig_status
  names(mean_ig_status)[1:7]<-c("meanRD","meanVPD","meanTEMP","meanSOIL","meanD","meanGDD","meanAWSSM");mean_ig_status#rename the columns
  sum_ig_status<-colSums(ig_status[,c("RAIN","AET","D","GDD")]);sum_ig_status
  names(sum_ig_status)[1:4]<-c("sumRAIN","sumAET","sumD","sumGDD");sum_ig_status
  max_ig_status<-apply(ig_status[,c("VPD","Tavg","GDD","AWSSM")],2,max);max_ig_status
  names(max_ig_status)[1:4]<-c("maxVPD","maxTEMP","maxGDD","maxAWSSM");max_ig_status
  min_ig_status<-apply(ig_status[,c("RD","SOIL")],2,min);min_ig_status
  names(min_ig_status)[1:2]<-c("minRD","minSOIL");min_ig_status
  #aggregate into a data frame means or sums as appropriate depending on nature of variable.  generally water should be summed, temperature and pressure averaged
  ig_status_agg<-as.data.frame(c(attributes,igstatus[1,1:3],mean_ig_status,sum_ig_status,max_ig_status,min_ig_status));ig_status_agg
  ig_status_wb<-rbind(ig_status_agg,ig_status_wb);ig_status_wb
  names(ig_status_wb)

  ####### wb summary from ig date to end of same calendar year ##############
  ig_end<-wbxts[paste(ig_date,ig_cal_end, sep="/"), c("RD","VPD","P","Tavg","RAIN","PACK","PET","SOIL","SOIL_Delta","AET","DRO","D","GDD","AWSSM")];head(ig_end)
  mean_ig_end<-colMeans(ig_end[,c("RD","VPD","Tavg","SOIL","D","GDD","AWSSM")]);mean_ig_end
  names(mean_ig_end)[1:7]<-c("meanRD","meanVPD","meanTEMP","meanSOIL","meanD","meanGDD","meanAWSSM");mean_ig_end#rename the columns
  sum_ig_end<-colSums(ig_end[,c("RAIN","AET","D","GDD")]);sum_ig_end
  names(sum_ig_end)[1:4]<-c("sumRAIN","sumAET","sumD","sumGDD");sum_ig_end
  max_ig_end<-apply(ig_end[,c("VPD","Tavg","GDD","AWSSM")],2,max);max_ig_end
  names(max_ig_end)[1:4]<-c("maxVPD","maxTEMP","maxGDD","maxAWSSM");max_ig_end
  min_ig_end<-apply(ig_end[,c("RD","SOIL")],2,min);min_ig_end
  names(min_ig_end)[1:2]<-c("minRD","minSOIL");min_ig_end
  #aggregate into a data frame means or sums as appropriate depending on nature of variable.  generally water should be summed, temperature and pressure averaged
  ig_end_agg<-as.data.frame(c(attributes,igstatus[1,1:3],mean_ig_end,sum_ig_end,max_ig_end,min_ig_end));ig_end_agg
  ig_end_wb<-rbind(ig_end_agg,ig_end_wb);ig_end_wb
  names(ig_end_wb)
    
  ####### wb summary full calendar year ##############
  cur_yr<-wbxts[paste(ig_cal_start,ig_cal_end, sep="/"), c("RD","VPD","P","Tavg","RAIN","PACK","PET","SOIL","SOIL_Delta","AET","DRO","D","GDD","AWSSM")];head(cur_yr)
  mean_cur_yr<-colMeans(cur_yr[,c("RD","VPD","Tavg","SOIL","D","GDD","AWSSM")]);mean_cur_yr
  names(mean_cur_yr)[1:7]<-c("meanRD","meanVPD","meanTEMP","meanSOIL","meanD","meanGDD","meanAWSSM");mean_cur_yr#rename the columns
  sum_cur_yr<-colSums(cur_yr[,c("RAIN","AET","D","GDD")]);sum_cur_yr
  names(sum_cur_yr)[1:4]<-c("sumRAIN","sumAET","sumD","sumGDD");sum_cur_yr
  max_cur_yr<-apply(cur_yr[,c("VPD","Tavg","GDD","AWSSM")],2,max);max_cur_yr
  names(max_cur_yr)[1:4]<-c("maxVPD","maxTEMP","maxGDD","maxAWSSM");max_cur_yr
  min_cur_yr<-apply(cur_yr[,c("RD","SOIL")],2,min);min_cur_yr
  names(min_cur_yr)[1:2]<-c("minRD","minSOIL");min_cur_yr
  #aggregate into a data frame means or sums as appropriate depending on nature of variable.  generally water should be summed, temperature and pressure averaged
  cur_yr_agg<-as.data.frame(c(attributes,igstatus[1,1:3],mean_cur_yr,sum_cur_yr,max_cur_yr,min_cur_yr));cur_yr_agg
  cur_yr_wb<-rbind(cur_yr_agg,cur_yr_wb);cur_yr_wb
  names(cur_yr_wb) 
  
  ####### wb summary from prior calendar year##############
  lag_one<-wbxts[paste(lag1_start,lag1_end, sep="/"), c("RD","VPD","P","Tavg","RAIN","PACK","PET","SOIL","SOIL_Delta","AET","DRO","D","GDD","AWSSM")];head(lag_one)
  mean_lag_one<-colMeans(lag_one[,c("RD","VPD","Tavg","SOIL","D","GDD","AWSSM")]);mean_lag_one
  names(mean_lag_one)[1:7]<-c("lag1meanRD","lag1meanVPD","lag1meanTEMP","lag1meanSOIL","lag1meanD","lag1meanGDD","lag1meanAWSSM");mean_lag_one#rename the columns
  sum_lag_one<-colSums(lag_one[,c("RAIN","AET","D","GDD")]);sum_lag_one
  names(sum_lag_one)[1:4]<-c("lag1sumRAIN","lag1sumAET","lag1sumD","lag1sumGDD");sum_lag_one
  max_lag_one<-apply(lag_one[,c("VPD","Tavg","GDD","AWSSM")],2,max);max_lag_one
  names(max_lag_one)[1:4]<-c("lag1maxVPD","lag1maxTEMP","lag1maxGDD","lag1maxAWSSM");max_lag_one
  min_lag_one<-apply(lag_one[,c("RD","SOIL")],2,min);min_lag_one
  names(min_lag_one)[1:2]<-c("lag1minRD","lag1minSOIL");min_lag_one
  #aggregate into a data frame means or sums as appropriate depending on nature of variable.  generally water should be summed, temperature and pressure averaged
  lag_one_agg<-as.data.frame(c(attributes,igstatus[1,1:3],mean_lag_one,sum_lag_one,max_lag_one,min_lag_one));lag_one_agg
  lag_one_wb<-rbind(lag_one_agg,lag_one_wb);lag_one_wb
  names(lag_one_wb)

  #compute percentile ranks within burn seasons of wb ignition values across all years 
  #determine conditions between first doy ignition and last doy ignition across all years
  #based on doy min max percentiles in doy_min_max.  this is done with doy_ann %in% function
  # however, it causes spread in the percentile rank distributions and would be more complex to implement in practice
  #easier to use range of climate data from 1980-01-01 thru ignition date
  doy_min_max#start and end of fire seasons by day of year based on ignition dates for each veg type
  #trees
 if(attributes$maj_class=="forest"){
   #forests 
   ll <- seq(forest_end, forest_start, by = "-1 day");ll#find ig doy's in years before ignition
    doy_ann<-wbxts[ll];head(doy_ann)
    #doy_ann<-wbxts[wbxts$doy %in% tree_range,];head(doy_ann);tail(doy_ann)#%in% operator used to specify elements in a vector
    #ann_index<-wbxts[ll];ann_index#ann_index<-ll[ll > st & ll < en];ann_index#xts dates in previous years
    #doy_ann<-rbind(wbxts[ig_date],doy_ann);head(doy_ann);tail(doy_ann)#append ig status conditions to fire year wb including all years of record
    #doy_ann1<-subset(doy_ann, maj_class="Tree ")
    doy_ann_sub<-doy_ann[,c("year","month","day","RD","VPD","Tavg","SOIL","GDD","AWSSM","RAIN","AET","D")];head(doy_ann_sub)
    f3<-as.Date(index(doy_ann_sub), format = "%m/%d/%Y");head(f3);length(f3)
   #compute percentile rank of each observation in the sequence of years 
    rankthis_forest<-data.frame(date=index(doy_ann_sub), coredata(doy_ann_sub));head(rankthis_forest)
    ranks<-apply(rankthis_forest[5:13],2,percent_rank)
    ranksxts<-xts(coredata(ranks),order.by=f3);head(ranksxts)#build xts data structure 
    #igstatus<-as.data.frame(wbxts[ig_date]);igstatus
    igrank<-as.data.frame(ranksxts[ig_date]);igrank
    igrankatt<-cbind(igrank, attributes)
    rankstack_forest<-rbind(igrankatt,rankstack_forest)
  } else if(attributes$maj_class=="non-forest") {
    #nonforests
    ll <- seq(nonforest_end, nonforest_start, by = "-1 day");ll#find ig doy's in years before ignition
    doy_ann<-wbxts[ll];head(doy_ann)
    #doy_ann<-wbxts[wbxts$doy %in% nonforest_range,];head(doy_ann);tail(doy_ann)#%in% operator used to specify elements in a vector
    #doy_ann<-rbind(wbxts[ig_date],doy_ann);head(doy_ann);tail(doy_ann)#append ig status conditions to fire year wb including all years of record
    #doy_ann1<-subset(doy_ann, maj_class="nonforest ")
    doy_ann_sub<-doy_ann[,c("year","month","day","RD","VPD","Tavg","SOIL","GDD","AWSSM","RAIN","AET","D")];head(doy_ann_sub)
    f3<-as.Date(index(doy_ann_sub), format = "%m/%d/%Y");head(f3);length(f3)
    #compute percentile rank of each observation in the sequence of years before and including (last line) year of fire
    rankthis_nonforest<-data.frame(date=index(doy_ann_sub), coredata(doy_ann_sub));head(rankthis_nonforest)
    ranks<-apply(rankthis_nonforest[5:13],2,percent_rank)
    ranksxts<-xts(coredata(ranks),order.by=f3);head(ranksxts)#build xts data structure 
    #igstatus<-as.data.frame(wbxts[ig_date]);igstatus
    igrank<-as.data.frame(ranksxts[ig_date]);igrank
    igrankatt<-cbind(igrank, attributes)
    rankstack_nonforest<-rbind(igrankatt,rankstack_nonforest)
  }else{
    #shrubs
    ll <- seq(shrub_end, shrub_start, by = "-1 day");ll#find ig doy's in years before ignition
    doy_ann<-wbxts[ll];head(doy_ann)
    #doy_ann<-wbxts[wbxts$doy %in% shrub_range,];head(doy_ann);tail(doy_ann)#%in% operator used to specify elements in a vector
    #doy_ann<-rbind(wbxts[ig_date],doy_ann);head(doy_ann);tail(doy_ann)#append ig status conditions to fire year wb including all years of record
    #doy_ann1<-subset(doy_ann, maj_class="Shrub");head(doy_ann1)
    doy_ann_sub<-doy_ann[,c("year","month","day","RD","VPD","Tavg","SOIL","GDD","AWSSM","RAIN","AET","D")];head(doy_ann_sub)
    f3<-as.Date(index(doy_ann_sub), format = "%m/%d/%Y");head(f3);length(f3)
    #compute percentile rank of each observation in the sequence of years before and including (last line) year of fire
    rankthis_shrub<-data.frame(date=index(doy_ann_sub), coredata(doy_ann_sub));head(rankthis_shrub)
    ranks<-apply(rankthis_shrub[5:13],2,percent_rank)
    ranksxts<-xts(coredata(ranks),order.by=f3);head(ranksxts)#build xts data structure 
    #igstatus<-as.data.frame(wbxts[ig_date]);igstatus
    igrank<-as.data.frame(ranksxts[ig_date]);igrank
    igrankatt<-cbind(igrank, attributes)
    rankstack_shrub<-rbind(igrankatt,rankstack_shrub)
  }
}

###############if not interested in ROC analysis skip to Percentiles analysis###########


###########################ROC analysis preparation steps##################################################
## make water balance an xts
## compute 14 day rolling means and sums
## compute percentile ranks of rolling means for fire season depending on veg type where site is located
## percentile rank normalizes water balance values for variation in vegetaiton structyre and type
## identify the day of year when fire started
## stack water balance files for all sites into one data frame for ROC analysis
## easier to use range of climate data from 1980-01-01 thru ignition date

## https://mlu-explain.github.io/roc-auc/
## Reading about ROC Analysis

fire_forest_long<-NULL
fire_nonforest_long<-NULL
fire_shrub_long<-NULL
fire_herb_long<-NULL

## Periods of time to calculate rolling means/sums for We want to
## evaluate what window of time to use here 14 days was chosen as the
## window for the original paper, as a period of time that would allow
## for dryness to accumulate, wouldn't get affected by small precip
## events but shorter/longer time spans were not evaluated.
windows <- c(1, 3, 7, 11, 14, 17, 21, 31)

forest_roc_table <- tribble(~window, ~var, ~auc, ~auc20, ~auc10)
nonforest_roc_table <- tribble(~window, ~var, ~auc, ~auc20, ~auc10)

## windows <- c(7) ## for testing
## k <- 7
## n <- 1

for (k in windows) {
    for (n in 1:nrow(igsite)){
        print(paste(n, "Site:", site, "Rolling mean window:", k))
        site<-igsite[n,1];site
        dt<-igsite[n,2];dt
        dtx<-as.Date(dt, format = "%Y/%m/%d");dtx#format date as POSIX so you and add and subtract number of days that bracket time period of interest
        ##subset out a year of data prior to ignition
        ##then use rollapply functions to summarize dryness conditions at daily, weekly, monthly intervals prior to ignition
        ##this way you can easily modify periods of interest in roll apply function rather than building interval specific subsets which would balloon
        ##define periods of interest for extracting wb 
        start_date <- '1984-01-01'
        ig_date <- dtx;ig_date 

        sitewb<-read.csv(site, stringsAsFactors=FALSE);head(sitewb)
                                        #convert relative humidity to relative dryness as 1-rh
        sitewb <- sitewb %>% mutate(RH = 100 - RH)
        nrow(sitewb); names(sitewb)
        ##colnames(sitewb)[1:24]<-c("date","year","month","day","SVP","RD","VPD","P","TEMP","F","RAIN","SNOW","PACK","MELT","W","PET","W_PET","SOIL","SOIL_Delta",
        ##                          "AET","RUNOFF","D","GDD","site")#rename the last 3 columns
        colnames(sitewb)<-c("line", "Date","P", "D", "AET", "SOIL", "DRO", "RAIN", "PACK", "PET", "VPD", "RD", "Tmax", "Tmin", "Tavg", "delta_soil", "GDD")
        names(sitewb)
        sitewb$Date <- as.Date(sitewb$Date, format = "%Y-%m-%d")
        sitewb <- sitewb %>% separate(Date, c("year", "month", "day"), sep = "-", remove = FALSE) %>% mutate_at(vars(year,month,day), as.numeric)
        names(sitewb)
        corewb<-sitewb[,c(-1,-2)];head(corewb)#drop dates and strings prior to converting to xts
        ##keep any strings out of the xts matrix or it will convert everything to strings with obnoxious quotes
        ## sitecol<-sitewb[,24]#in case you need to add the site information back into a data frame
        f2<-as.Date(sitewb[,2], format = "%Y-%m-%d");head(f2);length(f2)
        wbxts<-xts(coredata(corewb),order.by=f2);head(wbxts)#build xts data structure 
        awssm<-max(wbxts$SOIL)-wbxts$SOIL;head(awssm);names(awssm)<-"AWSSM"#  this should be added to water balance model
        doy<-yday(wbxts);head(doy)
        wbxts<-cbind(wbxts,awssm,doy);head(wbxts)
        names(wbxts)[ncol(wbxts)]<-"doy";head(wbxts)#rename the doy column in the xts
        
########daily wb summary#################
        ## determine water balance conditions on date of ignition
        igstatus<-as.data.frame(wbxts[ig_date]);igstatus
        ## add site attributes from master site file
        a<- which(paste(srmasterwf$Event_ID, ".csv", sep='') == site);a#identifies the row number in site file attribute file
        names(srmasterwf)
        ##attributes<-srmasterwf[a,c(1,17,19,56:58)];attributes#add site name, acres, fire type, veg class, day of year of ignition
        attributes<-srmasterwf[a,c(1,8,4,33,49)];attributes#add site name (1), acres (17), fire type (19), veg class (56 - maj_class), day of year of ignition
        ## igstatus_att<-cbind(attributes,igstatus[,c("year","month","day","SVP","RD","VPD","TEMP","SOIL","GDD","AWSSM","RAIN","AET","D")]);igstatus_att
        ## igstatus_stack<-rbind(igstatus_att,igstatus_stack)
        ## names(igstatus_stack)
        
######################calculate rolling mean or sums######################################
################k=?  is the number of days across which to summarize######################
        ##ep1 <- endpoints(historic,on="days",k=14);ep1#build an index of windows over which you summarize
        ##period_means<-rollapply(historic[,c("SVP","RD","VPD","Tavg","SOIL","GDD","AWSSM")], 14, mean, by = 1, by.column = TRUE);period_means[1:16,]#rolling values start at doy 14
        period_means<-rollapply(wbxts[,c("RD","VPD","Tavg","SOIL","AWSSM")], k, mean, by = 1, by.column = TRUE);period_means[1:16,]#rolling values start at doy 14
        period_sums<-rollapply(wbxts[,c("RAIN","AET","D", "GDD")], k, sum, by = 1, by.column = TRUE)
        ## period_means<-period.apply(historic[,c("SVP","RH","VPD","TEMP","SOIL","GDD","AWSSM")],INDEX=ep1,FUN=colMeans);tail(period_means)#summarize on those windows
        ## weekly_means<-apply.weekly(historic[,c("SVP","RH","VPD","TEMP","SOIL","GDD","AWSSM")],colMeans);head(weekly_means)
        ## period_sums<-period.apply(historic[,c("RAIN","AET","D")],INDEX=ep1,FUN=colSums);tail(period_sums)
        ## weekly_sums<-apply.weekly(historic[,c("RAIN","AET","D")],colSums);head(weekly_sums)
        ## weekly_agg<-cbind(weekly_means,weekly_sums);head(weekly_agg)#;str(weekly_agg)#these are the weekly data across all weeks
        period_agg<-cbind(period_means,period_sums);period_agg[10:20,]#values start at row 14 or what ever the rolling mean is set to thus NA's
        ## write.csv(period_agg,"period_agg.csv")
        
        doy_min_max#start and end of fire seasons by day of year based on ignition dates for each veg type
        ## trees
        if(attributes$maj_class=="forest") {
            ##forests 
            ll <- seq(forest_end, forest_start, by = "-1 day");ll#find ig doy's in years before ignition
            doy_ann<-period_agg[ll];head(doy_ann)
            f3<-as.Date(index(doy_ann), format = "%Y-%m-%d");head(f3);length(f3)
            ##compute percentile rank of each observation in the sequence of years 
            rankthis_forest<-data.frame(date=index(doy_ann), coredata(doy_ann));head(rankthis_forest)
            ranks<-apply(rankthis_forest[2:10],2,percent_rank)
            ranksxts<-xts(coredata(ranks),order.by=f3);head(ranksxts)#build xts data structure 
            
            ranksxts_ig<-cbind(ranksxts,0);head(ranksxts_ig)#add a column to hold indicator of ignition, where zero = no, 1 = ignition date
            colnames(ranksxts_ig)[10]<-"fire";head(ranksxts_ig)
            ranksxts_ig[ig_date,10]<-1;head(ranksxts_ig);ranksxts_ig[ig_date,]#change ignition date fire column value to 1
            fire_forest<-as.data.frame(ranksxts_ig)
            fire_forest<-cbind(attributes$Event_ID,attributes$maj_class,fire_forest);head(fire_forest)#add site name as a key for look up if needed later
            colnames(fire_forest)[1:2]<-c("site","maj_class");head(fire_forest)
            fire_forest_long<-rbind(fire_forest_long,fire_forest)
            

        } else if(attributes$maj_class=="non-forest") {
            ##nonforests
            ll <- seq(nonforest_end, nonforest_start, by = "-1 day");ll#find ig doy's in years before ignition
            doy_ann<-period_agg[ll];head(doy_ann)
            f3<-as.Date(index(doy_ann), format = "%Y-%m-%d");head(f3);length(f3)
            ##compute percentile rank of each observation in the sequence of years before and including (last line) year of fire
            rankthis_nonforest<-data.frame(date=index(doy_ann), coredata(doy_ann));head(rankthis_nonforest)
            ranks<-apply(rankthis_nonforest[2:10],2,percent_rank)
            ranksxts<-xts(coredata(ranks),order.by=f3);head(ranksxts)#build xts data structure 
            
            ranksxts_ig<-cbind(ranksxts,0);head(ranksxts_ig)#add a column to hold indicator of ignition, where zero = no, 1 = ignition date
            colnames(ranksxts_ig)[10]<-"fire";head(ranksxts_ig)
            ranksxts_ig[ig_date,10]<-1;head(ranksxts_ig);ranksxts_ig[ig_date,]#change ignition date fire column value to 1
            fire_nonforest<-as.data.frame(ranksxts_ig)
            fire_nonforest<-cbind(attributes$Event_ID,attributes$maj_class,fire_nonforest);head(fire_nonforest)#add site name as a key for look up if needed later
            colnames(fire_nonforest)[1:2]<-c("site","maj_class");head(fire_nonforest)
            fire_nonforest_long<-rbind(fire_nonforest_long,fire_nonforest)
            
        } else {
            ##shrubs
            ll <- seq(shrub_end, shrub_start, by = "-1 day");ll#find ig doy's in years before ignition
            doy_ann<-period_agg[ll];head(doy_ann)
            f3<-as.Date(index(doy_ann), format = "%Y-%m-%d");head(f3);length(f3)
            ##compute percentile rank of each observation in the sequence of years before and including (last line) year of fire
            rankthis_shrub<-data.frame(date=index(doy_ann), coredata(doy_ann));head(rankthis_shrub)
            ranks<-apply(rankthis_shrub[2:10],2,percent_rank)
            ranksxts<-xts(coredata(ranks),order.by=f3);head(ranksxts)#build xts data structure 
            
            ranksxts_ig<-cbind(ranksxts,0);head(ranksxts_ig)#add a column to hold indicator of ignition, where zero = no, 1 = ignition date
            colnames(ranksxts_ig)[11]<-"fire";head(ranksxts_ig)
            ranksxts_ig[ig_date,11]<-1;head(ranksxts_ig);ranksxts_ig[ig_date,]#change ignition date fire column value to 1
            fire_shrub<-as.data.frame(ranksxts_ig)
            fire_shrub<-cbind(attributes$site,attributes$maj_class,fire_shrub);head(fire_shrub)#add site name as a key for look up if needed later
            colnames(fire_shrub)[1:2]<-c("site","maj_class");head(fire_shrub)
            fire_shrub_long<-rbind(fire_shrub_long,fire_shrub)
            
        }
    }

    head(fire_forest_long)
    roc_long<-melt(fire_forest_long, id.vars = c("site", "maj_class","fire"), variable.name = "watbal",value.name = "value");head(roc_long)
    ## draw plots
                                        # basicplot <- ggplot(roc_long, aes(d = "fire", m = "value", color = model)) + geom_roc(n.cuts = 0) + 
                                        #   +   style_roc(theme = theme_bw, xlab = "1-Specificity", ylab = "Sensitivity") 
                                        # ## calculate auc
                                        # calc_auc(basicplot)

                                        # forest_svp_roc<-roc(fire_forest_long$fire,fire_forest_long$SVP)#build roc
                                        # svp.df<-data.frame(tpp=forest_svp_roc$sensitivities*100, fpp=(1-forest_svp_roc$specificities)*100, thresholds=forest_svp_roc$thresholds)
                                        # head(svp.df)#top right of curve
                                        # tail(svp.df)#bottom left
                                        # svp.df[svp.df$tpp>99 & svp.df$tpp<99.5,]#choose a range of true positive thresholds for evaluation of percentage false

                                        # glm.fit<-glm(fire_forest_long$fire~fire_forest_long$SVP, family=binomial)
                                        # plot(fire_forest_long$SVP,fire_forest_long$fire)
                                        # lines(fire_forest_long$SVP,glm.fit$fitted.values)
    forest_rd_roc<-roc(fire_forest_long$fire,fire_forest_long$RD, plot=FALSE, legacy.axes=TRUE, percent=TRUE, xlab="False Positive Percentage", ylab="True Postive Percentage", col="#377eb8", lwd=4, print.auc=TRUE)
    forest_vpd_roc<-roc(fire_forest_long$fire,fire_forest_long$VPD, plot=FALSE, legacy.axes=TRUE, percent=TRUE, xlab="False Positive Percentage", ylab="True Postive Percentage", col="#377eb8", lwd=4, print.auc=TRUE)
    forest_temp_roc<-roc(fire_forest_long$fire,fire_forest_long$Tavg, plot=FALSE, legacy.axes=TRUE, percent=TRUE, xlab="False Positive Percentage", ylab="True Postive Percentage", col="#377eb8", lwd=4, print.auc=TRUE)
    forest_soil_roc<-roc(fire_forest_long$fire,fire_forest_long$SOIL, plot=FALSE, legacy.axes=TRUE, percent=TRUE, xlab="False Positive Percentage", ylab="True Postive Percentage", col="#377eb8", lwd=4, print.auc=TRUE)
    forest_gdd_roc<-roc(fire_forest_long$fire,fire_forest_long$GDD, plot=FALSE, legacy.axes=TRUE, percent=TRUE, xlab="False Positive Percentage", ylab="True Postive Percentage", col="#377eb8", lwd=4, print.auc=TRUE)
    forest_awssm_roc<-roc(fire_forest_long$fire,fire_forest_long$AWSSM, plot=FALSE, legacy.axes=TRUE, percent=TRUE, xlab="False Positive Percentage", ylab="True Postive Percentage", col="#377eb8", lwd=4, print.auc=TRUE)
    forest_rain_roc<-roc(fire_forest_long$fire,fire_forest_long$RAIN, plot=FALSE, legacy.axes=TRUE, percent=TRUE, xlab="False Positive Percentage", ylab="True Postive Percentage", col="#377eb8", lwd=4, print.auc=TRUE)
    forest_aet_roc<-roc(fire_forest_long$fire,fire_forest_long$AET, plot=FALSE, legacy.axes=TRUE, percent=TRUE, xlab="False Positive Percentage", ylab="True Postive Percentage", col="#377eb8", lwd=4, print.auc=TRUE)
    forest_d_roc<-roc(fire_forest_long$fire,fire_forest_long$D, plot=FALSE, legacy.axes=TRUE, percent=TRUE, xlab="False Positive Percentage", ylab="True Postive Percentage", col="#377eb8", lwd=4, print.auc=TRUE)

                                        # par(pty = "m")#make square graphs
                                        # plot.roc(forest_svp_roc, legacy.axes=TRUE, percent=TRUE, xlab="False Positive Percentage",ylab="True Positive Percentage", 
                                        #          col="#377eb8",lwd=4, print.auc=TRUE)#, print.auc.x=0.4,partial.auc=c(.100,.90),auc.polygon=TRUE,auc.polygon.col="#377eb822")
                                        # plot.roc(forest_svp_roc, 0.7, 1, 2)
                                        # 
                                        # ## If you want to rename the x and y axes...
                                        # plot.roc(forest_rd_roc, legacy.axes=TRUE, percent=TRUE, xlab="False Positive Percentage", ylab="True Postive Percentage")

    ## par(pty = "s") ## pty sets the aspect ratio of the plot region. Two options:
    ## ##                "s" - creates a square plotting region
    ## ##                "m" - (the default) creates a maximal plotting region
    ## par(bg="white")
    ## plot.roc(forest_rd_roc, xlab="False positive percentage",ylab="True positive percentage",legacy.axes = TRUE, lwd=4, print.auc=FALSE,cex.lab=2.5,cex.axis=2, main="Forest")
    ##                                     #plot.roc(add=TRUE,col="blue",forest_svp_roc, xlab="fraction of burned identified correctly",ylab="fraction of unburned correctly identified",main="vpd",lwd=4, print.auc=FALSE)
    ## plot.roc(add=TRUE,col="pink",forest_rd_roc, xlab="fraction of burned identified correctly",ylab="fraction of unburned correctly identified",main="rd",lwd=4, print.auc=FALSE)
    ## plot.roc(add=TRUE,col="dark blue",forest_vpd_roc, xlab="fraction of burned identified correctly",ylab="fraction of unburned correctly identified",main="vpd",lwd=4, print.auc=FALSE)
    ## plot.roc(add=TRUE,col="green",forest_temp_roc, xlab="fraction of burned identified correctly",ylab="fraction of unburned correctly identified",main="temp",lwd=4, print.auc=FALSE)
    ## plot.roc(add=TRUE,col="red",forest_soil_roc, xlab="fraction of burned identified correctly",ylab="fraction of unburned correctly identified",main="soil",lwd=4, print.auc=FALSE)
    ## plot.roc(add=TRUE,col="brown",forest_gdd_roc, xlab="fraction of burned identified correctly",ylab="fraction of unburned correctly identified",main="gdd",lwd=4, print.auc=FALSE)
    ## plot.roc(add=TRUE,col="magenta",forest_awssm_roc, xlab="fraction of burned identified correctly",ylab="fraction of unburned correctly identified",main="awssm",lwd=4, print.auc=FALSE)
    ## plot.roc(add=TRUE,col="purple",forest_rain_roc, xlab="fraction of burned identified correctly",ylab="fraction of unburned correctly identified",main="rain",lwd=4, print.auc=FALSE)
    ## plot.roc(add=TRUE,col="dark green",forest_aet_roc, xlab="fraction of burned identified correctly",ylab="fraction of unburned correctly identified",main="aet",lwd=4, print.auc=FALSE)
    ## plot.roc(add=TRUE,col="orange",forest_d_roc, xlab="fraction of burned identified correctly",ylab="fraction of unburned correctly identified",main="d",lwd=4, print.auc=FALSE)
    ##                                     # Add a legend
    ## legend(35, 75, legend=c(paste("RD ",round(auc(forest_rd_roc),1)),
    ##                                     #paste("SVP ",round(auc(forest_svp_roc),1)),
    ##                         paste("VPD ",round(auc(forest_vpd_roc),1)),
    ##                         paste("TEMP ",round(auc(forest_temp_roc),1)),
    ##                         paste("SOIL ",round(auc(forest_soil_roc),1)),
    ##                         paste("GDD ",round(auc(forest_gdd_roc),1)),
    ##                         paste("SWD",round(auc(forest_awssm_roc),1)),
    ##                         paste("RAIN ",round(auc(forest_rain_roc),1)),
    ##                         paste("AET ",round(auc(forest_aet_roc),1)),
    ##                         paste("CWD ",round(auc(forest_d_roc),1))),
    ##        bty="n", col=c("pink", "dark blue","green","red","brown","magenta","purple","dark green","orange"), #col=c("black","blue", "dark blue","green","red","brown","magenta","purple","dark green","orange")
    ##        lty=1, lwd=4, text.font=2)#cex=0.9,
    ## text(1,75,"AUC")


    ## auc(forest_rd_roc)
    ## ##auc(forest_svp_roc)
    ## auc(forest_vpd_roc)
    ## auc(forest_temp_roc)
    ## auc(forest_soil_roc)
    ## auc(forest_gdd_roc)
    ## auc(forest_awssm_roc)
    ## auc(forest_rain_roc)
    ## auc(forest_aet_roc)
    ## auc(forest_d_roc)

    auc0 <- function(var) {
        roc <- paste0("forest_", var, "_roc")
        return(as.numeric(auc(get(roc))))
    }
    auc10 <- function(var) {
        roc <- paste0("forest_", var, "_roc")
        return(as.numeric(auc(get(roc),  partial.auc=c(100, 90))))
    }
    auc20 <- function(var) {
        roc <- paste0("forest_", var, "_roc")
        return(as.numeric(auc(get(roc),  partial.auc=c(100, 80))))
    }

    vars <- c("RD", "VPD", "TEMP", "SOIL", "GDD", "AWSSM", "RAIN", "AET", "D")
    for (var in vars) {
        forest_roc_table <- forest_roc_table %>% add_row(window = k, var = var, auc = auc0(tolower(var)), auc20 = auc20(tolower(var)), auc10 = auc10(tolower(var)))
    }
    


    nonforest_rd_roc<-roc(fire_nonforest_long$fire,fire_nonforest_long$RD, plot=FALSE, legacy.axes=TRUE, percent=TRUE, xlab="False Positive Percentage", ylab="True Postive Percentage", col="#377eb8", lwd=4, print.auc=TRUE)
    nonforest_vpd_roc<-roc(fire_nonforest_long$fire,fire_nonforest_long$VPD, plot=FALSE, legacy.axes=TRUE, percent=TRUE, xlab="False Positive Percentage", ylab="True Postive Percentage", col="#377eb8", lwd=4, print.auc=TRUE)
    nonforest_temp_roc<-roc(fire_nonforest_long$fire,fire_nonforest_long$Tavg, plot=FALSE, legacy.axes=TRUE, percent=TRUE, xlab="False Positive Percentage", ylab="True Postive Percentage", col="#377eb8", lwd=4, print.auc=TRUE)
    nonforest_soil_roc<-roc(fire_nonforest_long$fire,fire_nonforest_long$SOIL, plot=FALSE, legacy.axes=TRUE, percent=TRUE, xlab="False Positive Percentage", ylab="True Postive Percentage", col="#377eb8", lwd=4, print.auc=TRUE)
    nonforest_gdd_roc<-roc(fire_nonforest_long$fire,fire_nonforest_long$GDD, plot=FALSE, legacy.axes=TRUE, percent=TRUE, xlab="False Positive Percentage", ylab="True Postive Percentage", col="#377eb8", lwd=4, print.auc=TRUE)
    nonforest_awssm_roc<-roc(fire_nonforest_long$fire,fire_nonforest_long$AWSSM, plot=FALSE, legacy.axes=TRUE, percent=TRUE, xlab="False Positive Percentage", ylab="True Postive Percentage", col="#377eb8", lwd=4, print.auc=TRUE)
    nonforest_rain_roc<-roc(fire_nonforest_long$fire,fire_nonforest_long$RAIN, plot=FALSE, legacy.axes=TRUE, percent=TRUE, xlab="False Positive Percentage", ylab="True Postive Percentage", col="#377eb8", lwd=4, print.auc=TRUE)
    nonforest_aet_roc<-roc(fire_nonforest_long$fire,fire_nonforest_long$AET, plot=FALSE, legacy.axes=TRUE, percent=TRUE, xlab="False Positive Percentage", ylab="True Postive Percentage", col="#377eb8", lwd=4, print.auc=TRUE)
    nonforest_d_roc<-roc(fire_nonforest_long$fire,fire_nonforest_long$D, plot=FALSE, legacy.axes=TRUE, percent=TRUE, xlab="False Positive Percentage", ylab="True Postive Percentage", col="#377eb8", lwd=4, print.auc=TRUE)

                                        # par(pty = "m")#make square graphs
                                        # plot.roc(nonforest_svp_roc, legacy.axes=TRUE, percent=TRUE, xlab="False Positive Percentage",ylab="True Positive Percentage", 
                                        #          col="#377eb8",lwd=4, print.auc=TRUE)#, print.auc.x=0.4,partial.auc=c(.100,.90),auc.polygon=TRUE,auc.polygon.col="#377eb822")
                                        # plot.roc(nonforest_svp_roc, 0.7, 1, 2)
                                        # 
                                        # ## If you want to rename the x and y axes...
                                        # plot.roc(nonforest_rd_roc, legacy.axes=TRUE, percent=TRUE, xlab="False Positive Percentage", ylab="True Postive Percentage")

                                        #par(pty = "s") ## pty sets the aspect ratio of the plot region. Two options:
    ##                "s" - creates a square plotting region
    ##                "m" - (the default) creates a maximal plotting region

    ## plot.roc(nonforest_rd_roc, xlab="False positive percentage",ylab="True positive percentage",legacy.axes = TRUE, lwd=4, print.auc=FALSE,cex.lab=2.5,cex.axis=2, main="Non-forest")
    ##                                     #plot.roc(add=TRUE,col="blue",nonforest_svp_roc, xlab="fraction of burned identified correctly",ylab="fraction of unburned correctly identified",main="vpd",lwd=4, print.auc=FALSE)
    ## plot.roc(add=TRUE,col="pink",nonforest_rd_roc, xlab="fraction of burned identified correctly",ylab="fraction of unburned correctly identified",main="rd",lwd=4, print.auc=FALSE)
    ## plot.roc(add=TRUE,col="dark blue",nonforest_vpd_roc, xlab="fraction of burned identified correctly",ylab="fraction of unburned correctly identified",main="vpd",lwd=4, print.auc=FALSE)
    ## plot.roc(add=TRUE,col="green",nonforest_temp_roc, xlab="fraction of burned identified correctly",ylab="fraction of unburned correctly identified",main="temp",lwd=4, print.auc=FALSE)
    ## plot.roc(add=TRUE,col="red",nonforest_soil_roc, xlab="fraction of burned identified correctly",ylab="fraction of unburned correctly identified",main="soil",lwd=4, print.auc=FALSE)
    ## plot.roc(add=TRUE,col="brown",nonforest_gdd_roc, xlab="fraction of burned identified correctly",ylab="fraction of unburned correctly identified",main="gdd",lwd=4, print.auc=FALSE)
    ## plot.roc(add=TRUE,col="magenta",nonforest_awssm_roc, xlab="fraction of burned identified correctly",ylab="fraction of unburned correctly identified",main="awssm",lwd=4, print.auc=FALSE)
    ## plot.roc(add=TRUE,col="purple",nonforest_rain_roc, xlab="fraction of burned identified correctly",ylab="fraction of unburned correctly identified",main="rain",lwd=4, print.auc=FALSE)
    ## plot.roc(add=TRUE,col="dark green",nonforest_aet_roc, xlab="fraction of burned identified correctly",ylab="fraction of unburned correctly identified",main="aet",lwd=4, print.auc=FALSE)
    ## plot.roc(add=TRUE,col="orange",nonforest_d_roc, xlab="fraction of burned identified correctly",ylab="fraction of unburned correctly identified",main="d",lwd=4, print.auc=FALSE)
    ##                                     # Add a legend
    ## legend(35, 75, legend=c(paste("RD ",round(auc(nonforest_rd_roc),1)),
    ##                                     #paste("SVP ",round(auc(nonforest_svp_roc),1)),
    ##                         paste("VPD ",round(auc(nonforest_vpd_roc),1)),
    ##                         paste("TEMP ",round(auc(nonforest_temp_roc),1)),
    ##                         paste("SOIL ",round(auc(nonforest_soil_roc),1)),
    ##                         paste("GDD ",round(auc(nonforest_gdd_roc),1)),
    ##                         paste("SWD",round(auc(nonforest_awssm_roc),1)),
    ##                         paste("RAIN ",round(auc(nonforest_rain_roc),1)),
    ##                         paste("AET ",round(auc(nonforest_aet_roc),1)),
    ##                         paste("CWD ",round(auc(nonforest_d_roc),1))),
    ##        bty="n", col=c("pink", "dark blue","green","red","brown","magenta", "purple","dark green","orange"), #col=c("black","blue", "dark blue","green","red","brown","magenta","purple","dark green","orange")
    ##        lty=1, lwd=4, text.font=2)#cex=0.9,
    ## text(1,75,"AUC")


    ## auc(nonforest_rd_roc)
    ## ##auc(nonforest_svp_roc)
    ## auc(nonforest_vpd_roc)
    ## auc(nonforest_temp_roc)
    ## auc(nonforest_soil_roc)
    ## auc(nonforest_gdd_roc)
    ## auc(nonforest_awssm_roc)
    ## auc(nonforest_rain_roc)
    ## auc(nonforest_aet_roc)
    ## auc(nonforest_d_roc)

    auc0 <- function(var) {
        roc <- paste0("nonforest_", var, "_roc")
        return(as.numeric(auc(get(roc))))
    }
    auc10 <- function(var) {
        roc <- paste0("nonforest_", var, "_roc")
        return(as.numeric(auc(get(roc),  partial.auc=c(100, 90))))
    }
    auc20 <- function(var) {
        roc <- paste0("nonforest_", var, "_roc")
        return(as.numeric(auc(get(roc),  partial.auc=c(100, 80))))
    }
    
    vars <- c("RD", "VPD", "TEMP", "SOIL", "GDD", "AWSSM", "RAIN", "AET", "D")
    for (var in vars) {
        nonforest_roc_table <- nonforest_roc_table %>% add_row(window = k, var = var, auc = auc0(tolower(var)), auc20 = auc20(tolower(var)), auc10 = auc10(tolower(var)))
    }
}

## We can calculate the area under the curve...
##roc(fire_nonforest_long$fire,fire_nonforest_long$SVP, plot=TRUE, legacy.axes=TRUE, percent=TRUE, xlab="False Positive Percentage", ylab="True Postive Percentage", col="#377eb8", lwd=4, print.auc=TRUE)

## ...test for different area sizes under the partial area under the curve
#do this for levels of false positive percentage representing high levels of dryness
# partial.auc=c(100, 80) in this variable "80" is 100-80=20% false positive rate which is in the dry region of the curve
# test also 100-90 = 10% to test the driest region. 
roc(fire_nonforest_long$fire,fire_nonforest_long$VPD, main="VPD",plot=TRUE, legacy.axes=TRUE, percent=TRUE,
    xlab="False Positive Percentage", ylab="True Postive Percentage", col="#377eb8", lwd=4, print.auc=TRUE,
    print.auc.x=95, partial.auc=c(100, 80), auc.polygon = TRUE, auc.polygon.col = "#377eb822")

roc(fire_nonforest_long$fire,fire_nonforest_long$RD, main="RD",plot=TRUE, legacy.axes=TRUE, percent=TRUE,
    xlab="False Positive Percentage", ylab="True Postive Percentage", col="#377eb8", lwd=4, print.auc=TRUE,
    print.auc.x=95, partial.auc=c(100, 80), auc.polygon = TRUE, auc.polygon.col = "#377eb822")

roc(fire_nonforest_long$fire,fire_nonforest_long$D, main="D",plot=TRUE, legacy.axes=TRUE, percent=TRUE,
    xlab="False Positive Percentage", ylab="True Postive Percentage", col="#377eb8", lwd=4, print.auc=TRUE,
    print.auc.x=95, partial.auc=c(100, 80), auc.polygon = TRUE, auc.polygon.col = "#377eb822")

roc(fire_nonforest_long$fire,fire_nonforest_long$Tavg, main="TEMP",plot=TRUE, legacy.axes=TRUE, percent=TRUE,
    xlab="False Positive Percentage", ylab="True Postive Percentage", col="#377eb8", lwd=4, print.auc=TRUE,
    print.auc.x=95, partial.auc=c(100, 80), auc.polygon = TRUE, auc.polygon.col = "#377eb822")

roc(fire_nonforest_long$fire,fire_nonforest_long$GDD, main="GDD",plot=TRUE, legacy.axes=TRUE, percent=TRUE,
    xlab="False Positive Percentage", ylab="True Postive Percentage", col="#377eb8", lwd=4, print.auc=TRUE,
    print.auc.x=95, partial.auc=c(100, 80), auc.polygon = TRUE, auc.polygon.col = "#377eb822")

roc(fire_nonforest_long$fire,fire_nonforest_long$SOIL, main="SOIL",plot=TRUE, legacy.axes=TRUE, percent=TRUE,
    xlab="False Positive Percentage", ylab="True Postive Percentage", col="#377eb8", lwd=4, print.auc=TRUE,
    print.auc.x=95, partial.auc=c(100, 80), auc.polygon = TRUE, auc.polygon.col = "#377eb822")

roc(fire_nonforest_long$fire,fire_nonforest_long$AET, main="AET",plot=TRUE, legacy.axes=TRUE, percent=TRUE,
    xlab="False Positive Percentage", ylab="True Postive Percentage", col="#377eb8", lwd=4, print.auc=TRUE,
    print.auc.x=95, partial.auc=c(100, 80), auc.polygon = TRUE, auc.polygon.col = "#377eb822")

roc(fire_nonforest_long$fire,fire_nonforest_long$RAIN, main="RAIN",plot=TRUE, legacy.axes=TRUE, percent=TRUE,
    xlab="False Positive Percentage", ylab="True Postive Percentage", col="#377eb8", lwd=4, print.auc=TRUE,
    print.auc.x=95, partial.auc=c(100, 80), auc.polygon = TRUE, auc.polygon.col = "#377eb822")

############forest
## ...and the partial area under the curve.
roc(fire_forest_long$fire,fire_forest_long$VPD, main="VPD",plot=TRUE, legacy.axes=TRUE, percent=TRUE,
    xlab="False Positive Percentage", ylab="True Postive Percentage", col="#377eb8", lwd=4, print.auc=TRUE,
    print.auc.x=95, partial.auc=c(100, 80), auc.polygon = TRUE, auc.polygon.col = "#377eb822")

roc(fire_forest_long$fire,fire_forest_long$RD, main="RD",plot=TRUE, legacy.axes=TRUE, percent=TRUE,
    xlab="False Positive Percentage", ylab="True Postive Percentage", col="#377eb8", lwd=4, print.auc=TRUE,
    print.auc.x=95, partial.auc=c(100, 80), auc.polygon = TRUE, auc.polygon.col = "#377eb822")

roc(fire_forest_long$fire,fire_forest_long$D, main="D",plot=TRUE, legacy.axes=TRUE, percent=TRUE,
    xlab="False Positive Percentage", ylab="True Postive Percentage", col="#377eb8", lwd=4, print.auc=TRUE,
    print.auc.x=95, partial.auc=c(100, 80), auc.polygon = TRUE, auc.polygon.col = "#377eb822")

roc(fire_forest_long$fire,fire_forest_long$Tavg, main="TEMP",plot=TRUE, legacy.axes=TRUE, percent=TRUE,
    xlab="False Positive Percentage", ylab="True Postive Percentage", col="#377eb8", lwd=4, print.auc=TRUE,
    print.auc.x=95, partial.auc=c(100, 80), auc.polygon = TRUE, auc.polygon.col = "#377eb822")

roc(fire_forest_long$fire,fire_forest_long$GDD, main="GDD",plot=TRUE, legacy.axes=TRUE, percent=TRUE,
    xlab="False Positive Percentage", ylab="True Postive Percentage", col="#377eb8", lwd=4, print.auc=TRUE,
    print.auc.x=95, partial.auc=c(100, 80), auc.polygon = TRUE, auc.polygon.col = "#377eb822")

roc(fire_forest_long$fire,fire_forest_long$SOIL, main="SOIL",plot=TRUE, legacy.axes=TRUE, percent=TRUE,
    xlab="False Positive Percentage", ylab="True Postive Percentage", col="#377eb8", lwd=4, print.auc=TRUE,
    print.auc.x=95, partial.auc=c(100, 80), auc.polygon = TRUE, auc.polygon.col = "#377eb822")

roc(fire_forest_long$fire,fire_forest_long$AET, main="AET",plot=TRUE, legacy.axes=TRUE, percent=TRUE,
    xlab="False Positive Percentage", ylab="True Postive Percentage", col="#377eb8", lwd=4, print.auc=TRUE,
    print.auc.x=95, partial.auc=c(100, 80), auc.polygon = TRUE, auc.polygon.col = "#377eb822")

roc(fire_forest_long$fire,fire_forest_long$RAIN, main="RAIN",plot=TRUE, legacy.axes=TRUE, percent=TRUE,
    xlab="False Positive Percentage", ylab="True Postive Percentage", col="#377eb8", lwd=4, print.auc=TRUE,
    print.auc.x=95, partial.auc=c(100, 80), auc.polygon = TRUE, auc.polygon.col = "#377eb822")


########################## testing the driest region 
roc(fire_nonforest_long$fire,fire_nonforest_long$VPD, main="VPD",plot=TRUE, legacy.axes=TRUE, percent=TRUE,
    xlab="False Positive Percentage", ylab="True Postive Percentage", col="#377eb8", lwd=4, print.auc=TRUE,
    print.auc.x=95, partial.auc=c(100, 90), auc.polygon = TRUE, auc.polygon.col = "#377eb822")

roc(fire_nonforest_long$fire,fire_nonforest_long$RD, main="RD",plot=TRUE, legacy.axes=TRUE, percent=TRUE,
    xlab="False Positive Percentage", ylab="True Postive Percentage", col="#377eb8", lwd=4, print.auc=TRUE,
    print.auc.x=95, partial.auc=c(100, 90), auc.polygon = TRUE, auc.polygon.col = "#377eb822")

roc(fire_nonforest_long$fire,fire_nonforest_long$D, main="D",plot=TRUE, legacy.axes=TRUE, percent=TRUE,
    xlab="False Positive Percentage", ylab="True Postive Percentage", col="#377eb8", lwd=4, print.auc=TRUE,
    print.auc.x=95, partial.auc=c(100, 90), auc.polygon = TRUE, auc.polygon.col = "#377eb822")

roc(fire_nonforest_long$fire,fire_nonforest_long$Tavg, main="TEMP",plot=TRUE, legacy.axes=TRUE, percent=TRUE,
    xlab="False Positive Percentage", ylab="True Postive Percentage", col="#377eb8", lwd=4, print.auc=TRUE,
    print.auc.x=95, partial.auc=c(100, 90), auc.polygon = TRUE, auc.polygon.col = "#377eb822")

roc(fire_nonforest_long$fire,fire_nonforest_long$GDD, main="GDD",plot=TRUE, legacy.axes=TRUE, percent=TRUE,
    xlab="False Positive Percentage", ylab="True Postive Percentage", col="#377eb8", lwd=4, print.auc=TRUE,
    print.auc.x=95, partial.auc=c(100, 90), auc.polygon = TRUE, auc.polygon.col = "#377eb822")

roc(fire_nonforest_long$fire,fire_nonforest_long$SOIL, main="SOIL",plot=TRUE, legacy.axes=TRUE, percent=TRUE,
    xlab="False Positive Percentage", ylab="True Postive Percentage", col="#377eb8", lwd=4, print.auc=TRUE,
    print.auc.x=95, partial.auc=c(100, 90), auc.polygon = TRUE, auc.polygon.col = "#377eb822")

roc(fire_nonforest_long$fire,fire_nonforest_long$AET, main="AET",plot=TRUE, legacy.axes=TRUE, percent=TRUE,
    xlab="False Positive Percentage", ylab="True Postive Percentage", col="#377eb8", lwd=4, print.auc=TRUE,
    print.auc.x=95, partial.auc=c(100, 90), auc.polygon = TRUE, auc.polygon.col = "#377eb822")

roc(fire_nonforest_long$fire,fire_nonforest_long$RAIN, main="RAIN",plot=TRUE, legacy.axes=TRUE, percent=TRUE,
    xlab="False Positive Percentage", ylab="True Postive Percentage", col="#377eb8", lwd=4, print.auc=TRUE,
    print.auc.x=95, partial.auc=c(100, 90), auc.polygon = TRUE, auc.polygon.col = "#377eb822")

############forest
## ...and the partial area under the curve.
roc(fire_forest_long$fire,fire_forest_long$VPD, main="VPD",plot=TRUE, legacy.axes=TRUE, percent=TRUE,
    xlab="False Positive Percentage", ylab="True Postive Percentage", col="#377eb8", lwd=4, print.auc=TRUE,
    print.auc.x=95, partial.auc=c(100, 90), auc.polygon = TRUE, auc.polygon.col = "#377eb822")

roc(fire_forest_long$fire,fire_forest_long$RD, main="RD",plot=TRUE, legacy.axes=TRUE, percent=TRUE,
    xlab="False Positive Percentage", ylab="True Postive Percentage", col="#377eb8", lwd=4, print.auc=TRUE,
    print.auc.x=95, partial.auc=c(100, 90), auc.polygon = TRUE, auc.polygon.col = "#377eb822")

roc(fire_forest_long$fire,fire_forest_long$D, main="D",plot=TRUE, legacy.axes=TRUE, percent=TRUE,
    xlab="False Positive Percentage", ylab="True Postive Percentage", col="#377eb8", lwd=4, print.auc=TRUE,
    print.auc.x=95, partial.auc=c(100, 90), auc.polygon = TRUE, auc.polygon.col = "#377eb822")

roc(fire_forest_long$fire,fire_forest_long$Tavg, main="Tavg",plot=TRUE, legacy.axes=TRUE, percent=TRUE,
    xlab="False Positive Percentage", ylab="True Postive Percentage", col="#377eb8", lwd=4, print.auc=TRUE,
    print.auc.x=95, partial.auc=c(100, 90), auc.polygon = TRUE, auc.polygon.col = "#377eb822")

roc(fire_forest_long$fire,fire_forest_long$GDD, main="GDD",plot=TRUE, legacy.axes=TRUE, percent=TRUE,
    xlab="False Positive Percentage", ylab="True Postive Percentage", col="#377eb8", lwd=4, print.auc=TRUE,
    print.auc.x=95, partial.auc=c(100, 90), auc.polygon = TRUE, auc.polygon.col = "#377eb822")

roc(fire_forest_long$fire,fire_forest_long$SOIL, main="SOIL",plot=TRUE, legacy.axes=TRUE, percent=TRUE,
    xlab="False Positive Percentage", ylab="True Postive Percentage", col="#377eb8", lwd=4, print.auc=TRUE,
    print.auc.x=95, partial.auc=c(100, 90), auc.polygon = TRUE, auc.polygon.col = "#377eb822")

roc(fire_forest_long$fire,fire_forest_long$AET, main="AET",plot=TRUE, legacy.axes=TRUE, percent=TRUE,
    xlab="False Positive Percentage", ylab="True Postive Percentage", col="#377eb8", lwd=4, print.auc=TRUE,
    print.auc.x=95, partial.auc=c(100, 90), auc.polygon = TRUE, auc.polygon.col = "#377eb822")

roc(fire_forest_long$fire,fire_forest_long$RAIN, main="RAIN",plot=TRUE, legacy.axes=TRUE, percent=TRUE,
    xlab="False Positive Percentage", ylab="True Postive Percentage", col="#377eb8", lwd=4, print.auc=TRUE,
    print.auc.x=95, partial.auc=c(100, 90), auc.polygon = TRUE, auc.polygon.col = "#377eb822")



mylogit <- glm(fire ~ D, data = fire_forest_long, family = "binomial")
summary(mylogit)


#demonstrate histogram of conditions associated with fire or no fire to see how much overlap there is.  This helps interpret the ROC
head(fire_forest_long)
fire<-subset(fire_forest_long, fire==1)
nofire<-subset(fire_forest_long, fire==0)
plot(density(fire$D), col="red")
lines(density(nofire$D), col="blue")

plot(density(fire$VPD), col="red")
lines(density(nofire$VPD), col="blue")



########################################################################
#################Percentiles analysis####################################
########################################################################

#note: rankstack and igvalstack are same data, one as percentile, the other as wb magnitudes, both at user specified interval

#explore distribution of wb metrics associated with these different time intervals
ig_status_wb<-igstatus_stack#water balance conditions on day of ignition

#Just use reclass() to make it xts again: reclass(apply(X = xts_object,MARGIN = 2,FUN = cumsum), match.to=xts_object)

#build table of ignition week water balance values, median etc from igvalstack
#determine quartile stats for ignition doy by veg class
names(igvalstack)
vpd_stats<-tapply(igvalstack$VPD,igvalstack$maj_class, summary);vpd_stats;str(vpd_stats)
vpd_stats_sd<-tapply(igvalstack$VPD,igvalstack$maj_class, sd);vpd_stats_sd
vpd_stats2<-do.call(rbind.data.frame, vpd_stats) ;vpd_stats2;str(vpd_stats2)
names(vpd_stats2)<-c("Min","25%","Median","Mean","75%","Max");vpd_stats2

soil_stats<-tapply(igvalstack$SOIL,igvalstack$maj_class, summary);soil_stats;str(soil_stats)
soil_stats_sd<-tapply(igvalstack$SOIL,igvalstack$maj_class, sd);soil_stats_sd
soil_stats2<-do.call(rbind.data.frame, soil_stats) ;soil_stats2;str(soil_stats2)
names(soil_stats2)<-c("Min","25%","Median","Mean","75%","Max");soil_stats2

awssm_stats<-tapply(igvalstack$AWSSM,igvalstack$maj_class, summary);awssm_stats;str(awssm_stats)
awssm_stats_sd<-tapply(igvalstack$AWSSM,igvalstack$maj_class, sd);awssm_stats_sd
awssm_stats2<-do.call(rbind.data.frame, awssm_stats) ;awssm_stats2;str(awssm_stats2)
names(awssm_stats2)<-c("Min","25%","Median","Mean","75%","Max");awssm_stats2

RD_stats<-tapply(igvalstack$RD,igvalstack$maj_class, summary);RD_stats;str(RD_stats)
RD_stats_sd<-tapply(igvalstack$RD,igvalstack$maj_class, sd);RD_stats_sd
RD_stats2<-do.call(rbind.data.frame, RD_stats) ;RD_stats2;str(RD_stats2)
names(RD_stats2)<-c("Min","25%","Median","Mean","75%","Max");RD_stats2

D_stats<-tapply(igvalstack$D,igvalstack$maj_class, summary);D_stats;str(D_stats)
D_stats_sd<-tapply(igvalstack$D,igvalstack$maj_class, sd);D_stats_sd
D_stats2<-do.call(rbind.data.frame, D_stats) ;D_stats2;str(D_stats2)
names(D_stats2)<-c("Min","25%","Median","Mean","75%","Max");D_stats2


#build table of ignition week water balance values, median etc from rankstack, which is table of igvalstack as percentiles
#determine quartile stats for ignition doy by veg class
names(rankstack)
vpd_stats<-tapply(rankstack$VPD,rankstack$maj_class, summary);vpd_stats;str(vpd_stats)
vpd_stats_sd<-tapply(rankstack$VPD,rankstack$maj_class, sd);vpd_stats_sd
vpd_stats2<-do.call(rbind.data.frame, vpd_stats) ;vpd_stats2;str(vpd_stats2)
names(vpd_stats2)<-c("Min","25%","Median","Mean","75%","Max");vpd_stats2

soil_stats<-tapply(rankstack$SOIL,rankstack$maj_class, summary);soil_stats;str(soil_stats)
soil_stats_sd<-tapply(rankstack$SOIL,rankstack$maj_class, sd);soil_stats_sd
soil_stats2<-do.call(rbind.data.frame, soil_stats) ;soil_stats2;str(soil_stats2)
names(soil_stats2)<-c("Min","25%","Median","Mean","75%","Max");soil_stats2

awssm_stats<-tapply(rankstack$AWSSM,rankstack$maj_class, summary);awssm_stats;str(awssm_stats)
awssm_stats_sd<-tapply(rankstack$AWSSM,rankstack$maj_class, sd);awssm_stats_sd
awssm_stats2<-do.call(rbind.data.frame, awssm_stats) ;awssm_stats2;str(awssm_stats2)
names(awssm_stats2)<-c("Min","25%","Median","Mean","75%","Max");awssm_stats2

RD_stats<-tapply(rankstack$RD,rankstack$maj_class, summary);RD_stats;str(RD_stats)
RD_stats_sd<-tapply(rankstack$RD,rankstack$maj_class, sd);RD_stats_sd
RD_stats2<-do.call(rbind.data.frame, RD_stats) ;RD_stats2;str(RD_stats2)
names(RD_stats2)<-c("Min","25%","Median","Mean","75%","Max");RD_stats2

D_stats<-tapply(rankstack$D,rankstack$maj_class, summary);D_stats;str(D_stats)
D_stats_sd<-tapply(rankstack$D,rankstack$maj_class, sd);D_stats_sd
D_stats2<-do.call(rbind.data.frame, D_stats) ;D_stats2;str(D_stats2)
names(D_stats2)<-c("Min","25%","Median","Mean","75%","Max");D_stats2


#Determine contribution of various parameters to burned area
head(rankstack)
ranksnon<- subset(rankstack, maj_class=="non-forest");head(rankstack);names(rankstack)
y<-as.vector(ranksnon[,"Acres"]);y
xcann<-subset(ranksnon,select=c("Tavg","SOIL","GDD","AWSSM","AET","D","RD","VPD"));head(xcann)#
hier.part(y,xcann,barplot=TRUE)

ranksforest<- subset(rankstack, maj_class=="forest");head(rankstack);names(rankstack)
y<-as.vector(ranksforest[,"Acres"]);y
xcann<-subset(ranksforest,select=c("Tavg","SOIL","GDD","AWSSM","AET","D","RD","VPD"));head(xcann)#
hier.part(y,xcann,barplot=TRUE)

##############ROC###################################
#take the percentiles at ignition in rankstack and fit them into an ordered rank between 0 and 1
#where each ordered rank value has associated with it a zero for not burned and 1 for burned.
#start by defining all rows in rank stack as burned since these are precentiles when fires ignited,
#then pad all other possible values between zero and 1 with zero indicating not burned.

head(rankstack)
#add burn indicator column = 1 to rankstack
rankstack2<-cbind(rankstack,1);head(rankstack2)
ncol(rankstack2)
colnames(rankstack2)[15]<-"burned";head(rankstack2)
# for (n in 7:ncol(rankstack2-1)){
#   sub<-rankstack2[,c(1:6,n,17)];head(sub)#extract meta data and one column of percentiles
#   padding<-seq(0.01, 1, by=0.01 );head(padding)
#   matrix("nan", nrow = 15, ncol = 3)
#   subord<- sub[order,)
# 
#   #sort by mpg (ascending) and cyl (descending)
#   newdata <- mtcars[order(mpg, -cyl),] 
rankstack2$halog <- log(rankstack$Acres/.404686)

#plot cum dist functions of wb vars at week of each ignition
dec<- seq(from = 0.1, to = 0.9, by = 0.1);dec
head(rankstack)#percentile rank of wb conditions at week of burn relative to all weeks prior to burn
forest<-subset(rankstack, maj_class=="forest");forest#note class Tree has a space at end
cumfun_forest<-ecdf(forest$D)#cumulative distributinon function for fire start by day of year
plot(cumfun_forest, xlab="Deficit Percentiles at Ignition", ylab="Cumulative Distirbution Function % Fires")
forest_q<-quantile(forest$D,dec);forest_q#90% of data fall within this range 0.05-0.95# 0.27 to 0.75 or 0,1 to determine min max
abline(v=forest_q)#set percentiles of dryness
abline(h=dec)#quantile of burns at each of the set percentiles of dryness
points(forest_q,dec,col="red", pch=19)# intersection of burn quantile at dryness percentile defined by dec

#plot cum dist functions of wb vars at week of each ignition
dec<- seq(from = 0.1, to = 0.9, by = 0.1);dec
head(rankstack)#percentile rank of wb conditions at week of burn relative to all weeks prior to burn
forest<-subset(rankstack, maj_class=="forest");forest#note class Tree has a space at end
cumfun_forest<-ecdf(forest$VPD)#cumulative distributinon function for fire start by day of year
plot(cumfun_forest, xlab="Deficit Percentiles at Ignition", ylab="Cumulative Distirbution Function % Fires")
forest_q<-quantile(forest$VPD,dec);forest_q#90% of data fall within this range 0.05-0.95# 0.27 to 0.75 or 0,1 to determine min max
abline(v=forest_q)#set percentiles of dryness
abline(h=dec)#quantile of burns at each of the set percentiles of dryness
points(forest_q,dec,col="red", pch=19)# intersection of burn quantile at dryness percentile defined by dec



head(rankstack)#percentile rank of wb conditions at week of burn relative to all weeks prior to burn
nonforest<-subset(rankstack, maj_class=="non-forest");forest#note class Tree has a space at end
cumfun_nonforest<-ecdf(nonforest$VPD)#cumulative distribution function for fire start by day of year
plot(cumfun_nonforest, xlab="VPD Percentiles at Ignition", ylab="Cumulative Distirbution Function % Fires")
nonforest_q<-quantile(nonforest$VPD,dec);nonforest_q#90% of data fall within this range 0.05-0.95# 0.27 to 0.75 or 0,1 to determine min max
abline(v=nonforest_q)


#determine which variable best related with % of fires burned
head(rankstack)
write.csv(rankstack,"rankstack.csv")
s="SVP"
ecdfquants<-NULL
#dec controls the range of values used to calc ECDF. Can be used to truncate small values that are infrequent say below 0.1
dec<- seq(from = 0.01, to = 0.99, by = 0.01);dec
names(rankstack)
#select variables based on results from hierarchical partitioning
#c("SVP","RH","VPD","Tavg","SOIL","GDD","AWSSM","AET","D")
for (s in c("RD","VPD","AWSSM","D")){#
  #toggle next 2 lines to test forest vs. non forest relations
  nonforest<-subset(rankstack, maj_class=="non-forest");nonforest#note class Tree has a space at end
  x<-nonforest[,s];x#these are water balance percentiles for user defined period when fires burned 
 #forest<-subset(rankstack, maj_class=="forest");forest#note class Tree has a space at end
#x<-forest[,s];x
cumfun<-ecdf(x);cumfun#determine the ecdf
quants<-quantile(cumfun,dec);quants#think of quants as x-axis percentile at ignition
ecdfquants<-cbind(ecdfquants,quants)
}
head(ecdfquants);str(ecdfquants)
ecdfquants<-cbind(dec,ecdfquants)
head(ecdfquants)
colnames(ecdfquants)[1:5]<-c("dec","RD","VPD","SWD","CWD")
head(ecdfquants)#table of wb quantiles below which X% of all fires burned.  At high fire count want to high percentile of dryness
#this reflects a right shifted ecdf tightly grouped at high values when fire burned.  Can be left shifted for soil moisture
ecdfquants<-as.data.frame(ecdfquants)
ecdfquants<-cbind("nonforest",ecdfquants)
colnames(ecdfquants)[1]<-"maj_class"
nonforest_ecdfquants<-ecdfquants;nonforest_ecdfquants
#########################repeat for forest types
ecdfquants<-NULL
#dec<- seq(from = 0.1, to = 0.9, by = 0.01);dec
names(rankstack)
for (s in c("RD","VPD","AWSSM","D")){#
  #toggle next 2 lines to test forest vs. non forest relations
  forest<-subset(rankstack, maj_class=="forest");head(forest)#note class Tree has a space at end
  x<-forest[,s];x#these are water balance percentiles for user defined period when fires burned 
  
  #forest<-subset(rankstack, maj_class=="forest");forest#note class Tree has a space at end
  #x<-forest[,s];x
  cumfun<-ecdf(x);cumfun#determine the ecdf
  quants<-quantile(cumfun,dec);quants
    #quants2<-quantile(x,dec);quants2#this is identical to quantile(cumfun,dec)
  ecdfquants<-cbind(ecdfquants,quants)
}
head(ecdfquants);str(ecdfquants)
ecdfquants<-cbind(dec,ecdfquants)
head(ecdfquants)
colnames(ecdfquants)[1:5]<-c("dec","RD","VPD","SWD","CWD")
head(ecdfquants)#table of wb quantiles below which X% of all fires burned.  At low fire count want to high percentile of dryness
#this reflects a right shifted ecdf tightly grouped at high values when fire burned.  Can be left shifted for soil moisture
ecdfquants<-as.data.frame(ecdfquants)
ecdfquants<-cbind("forest",ecdfquants)
colnames(ecdfquants)[1]<-"maj_class"
forest_ecdfquants<-ecdfquants;forest_ecdfquants
#fit models to each ecdf by wb variable.  Use regression relationship, intercept and slope to determine best variable for 
#determining conditions dry enough to burn
#larger intercepts indicate higher x-axis value where ecdf crosses x-axis


##############################################################
####Models of ecdf for use in predicting fire danger############
# note, since these are models of cumulative distribution functions that only can increase they should not be used
# as determinants of goodness of fit
# however the equations for line fits can be used for estimating proportion of fires that burned historically below a given
# water balance percentile
head(nonforest_ecdfquants)
#convert to percentile by multiplying by 100
nonforest_ecdfquants2<-cbind(nonforest_ecdfquants[,1],nonforest_ecdfquants[,c(2:6)]*100)
colnames(nonforest_ecdfquants2)[1]<-"maj_class"
head(nonforest_ecdfquants2)
write.csv(nonforest_ecdfquants2,"nonforest_ecdfquants2.csv")


models <- vector(mode = "list", length = ncol(nonforest_ecdfquants)-2)#set up empty matrix
test_cols<-seq(from = 3, to = ncol(nonforest_ecdfquants), by = 1);test_cols
var<-NULL
coeffs_table_nonforest<-NULL
for (j in test_cols){#note model number reflects panel order in ggplot figures following the model comparison where 
  #model 1 is upper left panel.#multiply dec by 100 to convert percentile to percent
models[[j-2]] <- nls(data = nonforest_ecdfquants2, dec*100 ~ a*exp(nonforest_ecdfquants2[,j]*b), start=list(a=0.1,b=0.1))#linear model as exponential fuction
var[j-2]<-colnames(nonforest_ecdfquants2)[j]
coeffs<-as.data.frame(summary(models[[j-2]])[10], stringsAsFactors = FALSE)
coeffs2<-cbind("nonforest",var[j-2],coeffs[1,1],coeffs[1,2])
coeffs_table_nonforest<-rbind(coeffs_table_nonforest,coeffs2)
}
colnames(coeffs_table_nonforest)[1:4]<-c("maj_class","variable","a","b");coeffs_table_nonforest
#nonforest_model_rank<-model.sel(models);nonforest_model_rank;var

head(forest_ecdfquants)
models <- vector(mode = "list", length = ncol(forest_ecdfquants)-2)
test_cols<-seq(from = 3, to = ncol(forest_ecdfquants), by = 1);test_cols
var<-NULL
coeffs_table_forest<-NULL
#convert to percentile by multiplying by 100
forest_ecdfquants2<-cbind(forest_ecdfquants[,1],forest_ecdfquants[,c(2:6)]*100)
colnames(forest_ecdfquants2)[1]<-"maj_class"
head(forest_ecdfquants2)
write.csv(forest_ecdfquants2,"forest_ecdfquants2.csv")

for (j in test_cols){#multiply dec by 100 to convert percentile to percent
  models[[j-2]] <- nls(data = forest_ecdfquants2, dec*100 ~ a*exp(forest_ecdfquants2[,j]*b), start=list(a=0.1,b=0.1))#linear model as exponential fuction
  var[j-2]<-colnames(forest_ecdfquants2)[j]
  coeffs<-as.data.frame(summary(models[[j-2]])[10], stringsAsFactors = FALSE)
  coeffs2<-cbind("forest",var[j-2],coeffs[1,1],coeffs[1,2])
  coeffs_table_forest<-rbind(coeffs_table_forest,coeffs2)
}
colnames(coeffs_table_forest)[1:4]<-c("maj_class","variable","a","b");coeffs_table_forest
#forest_model_rank<-model.sel(models);forest_model_rank;var

coeff_table<-rbind(coeffs_table_nonforest,coeffs_table_forest);coeff_table

#model_rank<-cbind(match(var),model_rank[1]);model_rank
head(nonforest_ecdfquants)
all_quants<-rbind(forest_ecdfquants,nonforest_ecdfquants)
head(all_quants);tail(all_quants)

mquant<-melt(nonforest_ecdfquants[,1:6],id=c("maj_class","dec"));head(mquant)

unloadNamespace("pROC")#pROC interferes with ggplot
library(ggplot2)

ggplot(mquant, aes(x=value, y=dec)) +geom_point()+ ##geom_vline(xintercept = 75, linetype="solid",color = "blue", size=1)+
  geom_smooth(method="nls", formula = 'y ~ a*exp(b*x)',method.args = list(start=list(a=.1,b=.5)),se = FALSE)+
  facet_wrap(~variable, scales="fixed") + theme_bw() + theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  theme(axis.text=element_text(size=12,face="bold",family="Courier New"),
        axis.title=element_text(size=28,face="bold",family="Courier New"),
        title=element_text(size=20,face="bold",family="Courier New"))+ 
  labs(title = "", x = "14-day Percentile at Ignition", y = "Percentage of Fires (ECDF)", color = NULL)+
  scale_color_manual(values=c("#CC6666", "#9999CC","red","black","green","blue","orange","magenta","purple" ))

#combine forest and nonforest on one plot
#Create a custom color scale
comb_quants<-rbind(forest_ecdfquants,nonforest_ecdfquants)
head(comb_quants)
#comb_quants[,3]<-1/comb_quants[,3]
#multiply by 100 to convert to percentage
comb_quants[,c(2:6)]<-comb_quants[,c(2:6)]*100
head(comb_quants)
#drop RD and SWD as they are not as good as VPD and CWD as predictors of ignition based on ROC analyis
comb_quants2<-subset(comb_quants, select=c(maj_class, dec, VPD, CWD));head(comb_quants2)
#comb_quants2[,c(2:4)]<-comb_quants2[,c(2:4)]*100;head(comb_quants2)
write.csv(comb_quants2,"comb_quants2.csv")
mquant<-melt(comb_quants2[,1:4],id=c("maj_class","dec"));head(mquant)


# library(RColorBrewer)
# myColors <- brewer.pal(2,"Set1")
# names(myColors) <- levels(mquant$maj_class)
# colScale <- scale_colour_manual(name = "maj_class",values = myColors)

ggplot(mquant, aes(x=value, y=dec,color=maj_class)) + geom_point(size=3)+ 
  #geom_vline(xintercept = c(0.9), linetype="dotted",color = "black", size=0.5)+
  #geom_hline(yintercept = c(0.25), linetype="dotted",color = "black", size=0.5)+
  geom_smooth(method="nls", formula = 'y ~ a*exp(b*x)',method.args = list(start=list(a=0.5,b=0.05)),se = FALSE)+
  facet_wrap(~variable, scales = "fixed") +#  geom_blank(aes(y = 0)) +geom_blank(aes(y = 1)) +
  theme_bw() + theme(legend.text=element_text(size=20))+
  theme(strip.text.x = element_text(size = 18, colour = "black", angle = 0))+
  theme(panel.grid.major.x =  element_blank(),panel.grid.major.y =  element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  theme(axis.text=element_text(size=18,face="bold",family="Courier New"),
        axis.title=element_text(size=34,face="bold",family="Courier New"),
        title=element_text(size=22,face="bold",family="Courier New"))+ 
  labs(title = "", x = "Percentile at Ignition", y = "Proportion of Historical Fires", color = NULL)+
  scale_color_manual(values=c("red","black"))#,"red","black","green","blue","orange","magenta","purple"

ggplot(mquant, aes(x=value, y=dec,color=variable)) + geom_point(size=3)+ 
  #geom_vline(xintercept = c(0.9), linetype="dotted",color = "black", size=0.5)+
  #geom_hline(yintercept = c(0.25), linetype="dotted",color = "black", size=0.5)+
  facet_wrap(~maj_class, scales = "fixed") +
  geom_smooth(aes(color=variable),method="nls", formula = 'y ~ a*exp(b*x)',method.args = list(start=list(a=0.5,b=0.05)),se = FALSE)+
  #  geom_blank(aes(y = 0)) +geom_blank(aes(y = 1)) +
  theme_bw() + theme(legend.text=element_text(size=20))+
  theme(strip.text.x = element_text(size = 24, colour = "black", angle = 0))+
  theme(panel.grid.major.x =  element_blank(),panel.grid.major.y =  element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  theme(axis.text=element_text(size=24,face="bold",family="Courier New"),
        axis.title=element_text(size=34,face="bold",family="Courier New"),
        title=element_text(size=22,face="bold",family="Courier New"))+ 
  labs(title = "", x = "Percentile at Ignition", y = "Proportion of Historical Fires", color = NULL)+
  scale_color_manual(values=c("red","black"))#,"red","black","green","blue","orange","magenta","purple"


# the following is a way to quantify AUC of ROC using simple percentages of fires that burned vs. percentile of climvar when burns happen
#right shifted curves are better for estimating percentage of fires burned in the past
# It computes ther percent of fires that occurred when percentiles of water balance were <0.75 
wb_threshold<-0.995#percentile of wb values you want to use to determine % of fires that occur above that value
head(nonforest_ecdfquants);nrow(nonforest_ecdfquants)
nonforest_rtshift<-NULL
for(v in c(3:6)){
cnt<-subset(nonforest_ecdfquants[,v], nonforest_ecdfquants[,v]<=wb_threshold);head(cnt);length(cnt)
pct<-length(cnt)/nrow(nonforest_ecdfquants)
hold<-cbind("nonforest",colnames(nonforest_ecdfquants[v]),pct)
nonforest_rtshift<-rbind(nonforest_rtshift,hold)
}
colnames(nonforest_rtshift)[]<-c("maj_class","variable","percent")#table of percent of fires below user specified water balance percentile
nonforest_rtshift

head(forest_ecdfquants);nrow(forest_ecdfquants)
forest_rtshift<-NULL
for(v in c(3:6)){
  cnt<-subset(forest_ecdfquants[,v], forest_ecdfquants[,v]<=wb_threshold);head(cnt);length(cnt)
  pct<-length(cnt)/nrow(forest_ecdfquants)
  hold<-cbind("forest",colnames(forest_ecdfquants[v]),pct)
  forest_rtshift<-rbind(forest_rtshift,hold)
}
colnames(forest_rtshift)[]<-c("maj_class","variable","percent")#table of percent of fires below user specified water balance percentile
forest_rtshift

right_shift<-rbind(nonforest_rtshift,forest_rtshift);right_shift





#plots of wb percentile vs. area burned show weak relationships
head(rankstack)#, maj_class=="forest"
rankforest<-subset(rankstack, select=c("Event_ID", "maj_class", "doy","Acres", "RD", "VPD", "SOIL", "GDD", "Tavg","AWSSM", "AET", "D"));head(rankforest)
mquant<-melt(rankforest,id=c("maj_class","Event_ID","doy","Acres"));head(mquant)
ggplot(mquant, aes(x=value, y=Acres, colour=factor(maj_class))) +geom_point(size=3)+ #geom_vline(xintercept = 0.6, linetype="solid",color = "blue", size=0.5)+
  #geom_smooth(method="nls", formula = 'y ~ a*exp(b*x)',method.args = list(start=list(a=.1,b=.5)),se = FALSE)+
  geom_smooth(method="lm", se=FALSE)+
  facet_wrap(~variable, scales="fixed") + theme_bw() + theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  theme(axis.text=element_text(size=12,face="bold",family="Courier New"),
        axis.title=element_text(size=28,face="bold",family="Courier New"),
        title=element_text(size=20,face="bold",family="Courier New"))+ 
  labs(title = "", x = "Percentile at Week of Ignition", y = "Area burned (ln Ha)", color = NULL)+
  scale_color_manual(values=c("red","black"))


#compare different model fits visually in ggplot
testPlot <- ggplot(mquant, aes(x=value, y=dec)) + geom_point() + geom_vline(xintercept = 0.8, linetype="solid",color = "blue", size=1)+ 
  facet_wrap(~variable, scales="fixed") + 
  geom_smooth(method="lm", formula= (y ~ exp(x)), se=FALSE, color=1) +
  geom_smooth(method="nls", formula = 'y ~ a*exp(b*x)',method.args = list(start=list(a=.1,b=.5)), se=FALSE, color=2)
testPlot

models[[11]] <- lm(data = ecdfquants, dec ~ pivot + I(pivot^2)+ depth:pct_clay)


write.csv(model_rank,"model_rank.csv")

# trees<-subset(rankstack, maj_class=="Tree ");trees#note class Tree has a space at end
# cumfun_trees<-ecdf(trees$D)#cumulative distributino function for fire start by day of year
# plot(cumfun_trees, xlab="Deficit Percentiles at Ignition", ylab="Cumulative Distirbution Function % Fires")
# trees_q<-quantile(trees$D,c(0,.75,0.9, 0.95, 0.99));trees_q#90% of data fall within this range 0.05-0.95# 0.27 to 0.75 or 0,1 to determine min max
# abline(v=trees_q)

#rearrainge for plotting in ggplot
mranks<-melt(ranks2, id=c("doy","Event_ID","maj_class","Incid_Type","Acres"));head(mranks)
# Multiple ECDFs
ggplot(mranks, aes(x=value, colour = maj_class)) +stat_ecdf(size = 2)+facet_wrap(~variable, scales="fixed") + theme_bw() + theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  theme(axis.text=element_text(size=12,face="bold",family="Courier New"),
        axis.title=element_text(size=28,face="bold",family="Courier New"),
        title=element_text(size=20,face="bold",family="Courier New"))+ 
  labs(title = "", x = "Percentile at Week of Ignition", y = "Percentage of Fires (ECDF)", color = NULL)+
  scale_color_manual(values=c("#CC6666", "#9999CC","red","black","green","blue","orange","magenta","purple" ))

ggplot(mranks, aes(x=value, colour = variable)) + stat_ecdf(size = 2)+facet_wrap(~maj_class, scales="fixed") + theme_bw() + theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  theme(axis.text=element_text(size=12,face="bold",family="Courier New"),
        axis.title=element_text(size=28,face="bold",family="Courier New"),
        title=element_text(size=20,face="bold",family="Courier New"))+ 
  labs(title = "", x = "Percentile at Week of Ignition", y = "Percentage of Fires (ECDF)", color = NULL)+
  scale_color_manual(values=c("#CC6666", "#9999CC","red","black","green","blue","orange","magenta","purple" ))

# # Split the data by group and calculate the smoothed cumulative density for each group
# dens = split(mranks, mranks$maj_class) %>%  map_df(function(d) {
#     dens = density(d$x, adjust=0.1, from=min(dat$x) - 0.05*diff(range(dat$x)), 
#                    to=max(dat$x) + 0.05*diff(range(dat$x)))
#     data.frame(x=dens$x, y=dens$y, cd=cumsum(dens$y)/sum(dens$y), group=d$group[1])
#   })
# 
# #Now we can plot each smoothed cumulative density. In the plot below, I've included a call to stat_ecdf with the original data for comparison.
# 
# ggplot() +
# stat_ecdf(data=dat, aes(x, colour=group), alpha=0.8, lty="11") +
# geom_line(data=dens, aes(x, cd, colour=group)) +
# theme_classic()

#https://stackoverflow.com/questions/27611438/density-curve-overlay-on-histogram-where-vertical-axis-is-frequency-aka-count
hplot<-ggplot(mranks, aes(x=value)) + geom_histogram(aes(fill=maj_class, position="dodge", binwidth=50), alpha=0.3)+ 
  geom_density(aes(y=0.075 * ..count..,fill=maj_class), alpha=0.3)+
  facet_wrap(~variable, scales="fixed") + theme_bw() + theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  theme(axis.text=element_text(size=12,face="bold",family="Courier New"),
        axis.title=element_text(size=28,face="bold",family="Courier New"),
        title=element_text(size=20,face="bold",family="Courier New"))+ 
  labs(title = "", x = "Percentile at Week of Ignition", y = "Fire Count", color = NULL)
hplot 

#comparison of linear models of burned area vs. percentile rank climate variables
sig_test<-NULL
head(ranks2)
classes<-unique(ranks2$maj_class);classes[2]
for(j in c(1:length(classes))){
  grp<-classes[j]
  testset<-subset(ranks2,maj_class==classes[j]);head(testset) 
  
  for(i in c(6:14)){
    var<-colnames(testset[i]);var
    y=testset$Acres
    x=testset[,i]
    lmfit<-lm(y~x)
    cor_p<-cor.test(x,y,method="pearson");cor_p
    names(lmfit) #output names of lmfit
    int<-as.numeric(lmfit$coefficients[[1]])#this is intercept
    slope<-as.numeric(lmfit$coefficients[[2]])#this is slope
    adjrsq<-as.numeric(summary(lmfit)$adj.r.squared)#this is adjusted rsquared value
    aic<-AIC(lmfit)#double brackets pull numeric value rather than list value
    #cbind.data.frame allows joining different data types ie. character and numeric into columns of dataframe
    variable_test<-cbind.data.frame(classes[j],var,slope,int,adjrsq, cor_p[[3]],cor_p[[4]],aic);variable_test
    #str(variable_test)
    sig_test<- rbind(sig_test,variable_test)
  }
}#next class of vegetation
colnames(sig_test)[]<-c("Vegetation","Variable","Slope","Intercept","Adjrsq","P-val","Correlation","AIC")
sig_test


qs<-c(0.99)#quantiles for quantile regression
splot<- ggplot(mranks, aes(x=value, y=halog,color=maj_class)) + geom_point(size = 5) + # geom_bar(stat="identity", position=position_dodge())+#
  facet_wrap(~variable, scales="fixed")+#scale_x_continuous("Year", breaks=seq(1985,2015,1), limits=c(1985,2015))+ 
  theme_bw() + theme( panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  theme(axis.text=element_text(size=24,face="bold",family="Courier New"),
        axis.title=element_text(size=28,face="bold",family="Courier New"),
        title=element_text(size=20,face="bold",family="Courier New"))+ labs(title = "", x = NULL, y = "Area (log Ha)", color = NULL)+
  theme(legend.text=element_text(size=24),legend.position="bottom")+guides(color=guide_legend(nrow=1))

splot + geom_smooth(method="lm",formula=y~x,se=FALSE)
#splot + geom_smooth(method="nls",formula=nls(y ~   a * exp(b * x), start = list(a = 0.12345, b = 0.54321)),se=FALSE)



splot + geom_quantile(formula=y~x+exp(x),quantiles =qs,linetype=c("solid"),size=2, aes(colour = maj_class))
splot + geom_quantile(formula=y~x,quantiles =qs,linetype=c("solid"),size=2, aes(colour = maj_class))

qs<-c(0.05,0.5, 0.95)#quantiles for quantile regression
# splot<- ggplot(mranks) + geom_point(aes(x=value, y=halog,color=maj_class),size = 5) + # geom_bar(stat="identity", position=position_dodge())+#
#   facet_wrap(~variable, scales="fixed")+#scale_x_continuous("Year", breaks=seq(1985,2015,1), limits=c(1985,2015))+ 
#   theme_bw() + theme( panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
#   theme(axis.text=element_text(size=24,face="bold",family="Courier New"),
#         axis.title=element_text(size=28,face="bold",family="Courier New"),
#         title=element_text(size=20,face="bold",family="Courier New"))+ labs(title = "", x = NULL, y = "Area (log Ha)", color = NULL)+
#   theme(legend.text=element_text(size=24),legend.position="bottom")+guides(color=guide_legend(nrow=1))
# 
# my.equation<-halog~a*exp(b*value)# where exp is 2.781 
# splot + geom_smooth(method = 'nls', formula = halog ~ exp(b * value), method.args = list(start = c(a = 0.01, b = 1)), se = FALSE)
# #nls.fit <- nls(my.equation,data = subclim_climvar,start = list(a=0.01,b = 0.01), model=TRUE)
# start = list(a=0.1,b = 0.01)
# splot + geom_quantile(aes(x=value, y=halog),formula=halog~0.01*exp(0.1*value),quantiles =qs,linetype=c("solid"),size=2)

splot + geom_quantile(aes(x=value, y=halog),formula=y~x+exp(x),quantiles =qs,linetype=c("solid"),size=2)
splot + geom_quantile(aes(x=value, y=halog),formula=y~x,quantiles=0.5,linetype=c("dashed"),size=2, color="black") +
  geom_quantile(aes(x=value, y=halog),formula=y~x,quantiles=0.95,linetype=c("solid"),size=2, color="black")

head(ranks2)
fit1<-lm(halog~AET+D, ranks2);summary(fit1)
 #######################################################################
  #non-linear quantile regression across veg types
  #######################################################################
  #adapted from code below that tests response and pivot vs. soil properties in qreg context ~line 3850
  t="SOIL"
head(rankstack)
  rqtable<-NULL# holder for quantile regression coefficients
  full_climvars<-c("SVP","RD","VPD","Tavg","SOIL","GDD","AWSSM","AET","D")
  climvars<- c("VPD","SOIL","AWSSM","D") 
  for (t in c("VPD","SOIL","AWSSM","D")){
    a<-which(mranks$variable==t);a
    subclim_climvar<-mranks[a,];head(subclim_climvar) ;str(subclim_climvar);nrow(subclim_climvar)# ;test_data<-subclim_cp_climvar
    #write.csv(subclim_climvar,"subclim_climvar.csv")#write out for plotting pivot vs. resp in excel
    
    # tradeoff=ggplot(data=subclim_climvar,aes(x = value,y = halog)) + #geom_point(data=subclim_climvar,aes(x=value,y=valuehalog, color=cp_vt))+
    #   geom_point(aes(colour = maj_class))+ xlab(paste(t,"value")) +  ylab(paste("iNDVI Response * 1000")) + theme_bw()+
    #   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) #remove background grid
    # tradeoff + geom_quantile(quantiles = qs,aes(colour = maj_class))#linear quantile regression lines
    # 
    head(subclim_climvar)#set up a blank plot with x,y axis scales appropriate for the the climate metric 
    #identify the weird sagebrush plots
    #sub_sage<-subset(subclim_climvar, cp_vt=="all_sage");sub_sage
    if (t=="D"){#create empty charts
          plot(subclim_climvar$value,subclim_climvar$halog, xlab=paste(t,"(percentile)"), ylab=paste("Log Area (ha)"), type="n", 
           cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5, ylim=c(6,12), xlim=c(0,1))#type n makes empty plot, main=paste("h_cheat cutoff = ", high_cht)
    }else if (t=="AWSSM"){
      plot(subclim_climvar$value,subclim_climvar$halog, xlab=paste(t,"(percentile)"), ylab=paste("Log Area (ha)"), type="n", 
           cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5, ylim=c(6,12), xlim=c(0,1))#type n makes empty plot, main=paste("h_cheat cutoff = ", high_cht)
    }else if (t=="SOIL"){
      plot(subclim_climvar$value,subclim_climvar$halog,xlab=paste(t,"(percentile)"), ylab=paste("Log Area (ha)"), type="n", 
           cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5, ylim=c(6,12), xlim=c(0,1))#type n makes empty plot, main=paste("h_cheat cutoff = ", high_cht)
    }else if (t=="VPD"){ 
      plot(subclim_climvar$value,subclim_climvar$halog, xlab=paste(t,"(percentile)"), ylab=paste("Log Area (ha)"), type="n", 
           cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5, ylim=c(6,12), xlim=c(0,1))#type n makes empty plot, main=paste("h_cheat cutoff = ", high_cht)
    }
    
    #vary color by factor in a basic R plot
    #http://stackoverflow.com/questions/7466023/how-to-give-color-to-each-class-in-scatter-plot-in-r
    #subset(subclim_climvar, subclim_climvar$cp_vt3=="Exotic_annuals")
    #order of plotting for color match = zion_pj, 
    clr=c("blue","red");clr
    i=0#plot counter
    climvars<-unique(subclim_climvar$maj_class);climvars
    #legend_matrix<-cbind(climvars,color);names(legend_matrix)<-c("cp_vt3","color");legend_matrix
    p=9
    for(p in c(1:length(climvars))){#unique(subclim_climvar$cp_vt3)){print(p)}
      #i=i+1
      a<- which(subclim_climvar$maj_class==climvars[p]);a
      b<-subclim_climvar[a,];head(b)
      points(b$value,b$halog, pch=16, col=clr) #
      #text(b$value,b$halog, labels =paste((b$vt_pol)))#round(b$h_years,1),round(b$t_years,1)label by site to track down geography of outliers
      #points(b$value,b$halog, col=c("red","blue","green", "black","orange","brown","purple")[b$cp_vt],bg=c("red","blue","green", "black","orange","brown","purple")[b$cp_vt], pch=16)
    }
    
    if (t=="SOIL"){#t=="P"|t=="ET"|#puts legend in only the soil panel
      #P,ET,SOIL
      legend("topleft",inset = c(0.5,9),cex = 1,  bty = "n", legend = c("forest","herbaceous","shrub"),text.col = c("black"),#,"exotic_annuals"
             col = c("red","blue","green", "black"),  pch = c(16))#,"orange","brown","purple"
      # }else if (t=="D"){#D
      # legend("bottomright",inset = c(0.02,0.1),cex = 1,  bty = "n", legend = c("blackbrush","grass","pj","sage"),text.col = c("black"),#"bbpj","blca_pj","sage_shrub","dino_pj","zion_pj","exotic_annuals"
      #        col = c("red","blue","green", "black","orange","brown","purple"),  pch = c(16))
    }
    
    #myequation for P, ET and soil is a power function with different constants
    #my.equation<-halog~a*value^(b)# where ex is a variable exponent start = list(a=1,b = 2)
    if (t=="P"|t=="ET"|t=="SOIL"||t=="D"|t=="SVP"|t=="VPD"|t=="GDD"||t=="AWSSM"){#"SVP","RD","VPD","Tavg","SOIL","GDD","AWSSM","AET","D"
      my.equation<-halog~a*exp(b*value)# where exp is 2.781 
    }else  if (t=="dog"){#dummy variable for odd fits like deficit in landscape pivot points paper
      #my equaiton for D is log
      my.equation<- halog~a*log(value)-b
    };my.equation
    
    nls.fit<-NULL
    
    if (t=="P"|t=="ET"|t=="SOIL"||t=="D"|t=="SVP"|t=="VPD"|t=="GDD"||t=="AWSSM"){
      #For ET and P
      # fit the equation to the data via "non-linear least squares"
      nls.fit <- nls(my.equation,data = subclim_climvar,start = list(a=1,b = 1), model=TRUE)
    }else if (t=="dog"){#for SOIL
      nls.fit<- nls(my.equation, data=subclim_climvar, start=list(a=10, b=-0.55),model=TRUE)
    }else if (t=="dog"){
      #for D
      nls.fit<- nls(my.equation, data=subclim_climvar, start=list(a=0.4, b=-3),model=TRUE)
    };summary(nls.fit)
    
    # create a dummy range that we use to predict response from our fitted model
    predict_range <- data.frame(value= seq(0.001,1, length = nrow(subclim_climvar)));head(predict_range)#67 is number of rows in test_data
    # calculate for each x-range value the corresponding y-range
    my.line <- within(predict_range, Response <- predict(nls.fit, newdata = predict_range))
    # add the line to the existing graph
    # This line represents the "mean" fit, no quantile regression involved
    #lines(my.line, col = "black",lty=2,lwd=3 )#toggle off if ploting 50th quantile below
    
    
    # Non-linear quantile regression
    # aiming for the upper 95% quantile and bottom 5%
    my.rq <- nlrq(my.equation,data = subclim_climvar,start = list(a=1,b = 0.01), tau = .95)
    rq95<-summary(my.rq)
    rq95sum<-data.frame(cbind(rq95$coefficients,rq95$tau,t));rq95sum
    # calculating the values from the model
    my.line95 <- within(predict_range,Cond <- predict(my.rq,newdata = predict_range))
    lines(my.line95, col = "black",lty=1, lwd=3)
    
    my.rq <- nlrq(my.equation,data = subclim_climvar,start = list(a=0.1,b = 0.01), tau = .5)#a=0.01,b=0.01
    rq50<-summary(my.rq)
    rq50sum<-data.frame(cbind(rq50$coefficients,rq50$tau,t));rq50sum
    # calculating the values from the model
    my.line50 <- within(predict_range,Cond <- predict(my.rq,newdata = predict_range))
    lines(my.line50, col = "black",lty=2,lwd=3)
    
    my.rq <- nlrq(my.equation,data = subclim_climvar,start = list(a=0.1,b = 0.01), tau = .05)#a=0.01,b=0.01
    rq05<-summary(my.rq)
    rq05sum<-data.frame(cbind(rq05$coefficients,rq05$tau,t));rq05sum
    # calculating the values from the model
    my.line5 <- within(predict_range,Cond <- predict(my.rq,newdata = predict_range))
    lines(my.line5, col = "black",lty=3,lwd=3)
    
    rqtable<-rbind(rqtable,rq95sum, rq50sum,rq05sum);rqtable
    
    if (t=="P"){#P legend
      #legend(350,4, c("95%","Mean","5%"),bty="n",lty = c(1,2,3),lwd=2, col = c("black"),cex = 1) # make the characters slightly smaller than standard text
    }else if (t=="ET"){#ETlegend
      #legend(280,3, c("95%","Mean","5%"),bty="n",lty = c(1,2,3),lwd=2, col = c("black"),cex = 1) # make the characters slightly smaller than standard text
    }else if (t=="SOIL"){#Soil legend
      legend(40,170, c("95%","Median","5%"),bty="n",lty = c(1,2,3),lwd=2, col = c("black"),cex = 1) # make the characters slightly smaller than standard text
    }else if (t=="D"){#D legend
      #legend(280,-3, c("95%","Mean","5%"),bty="n",lty = c(1,2,3),lwd=2, col = c("black"),cex = 1) # make the characters slightly smaller than standard text
    }
    
    # ## grab the scene as a grid object and save it with a variable name
    # a<-paste("plot_",i,sep="");a#create a variable file name
    # grid.echo()
    # assign(a, grid.grab())
    # 
    # ## draw it, changes optional
    # grid.newpage()
    # plot_6 <- editGrob(plot_6, vp=viewport(width=unit(2,"in")), gp=gpar(fontsize=10))
    # grid.draw(plot_6)
    
    i=i+1
    #plots[[i]]<-tradeoff_scatter#add each plot into the plot list
    
    #linear quantile regression for each vegetation type
    ##for(vt in (unique(subclim_climvar$cp_vt3))){
    #subsub<-subset(subclim_climvar, cp_vt3==vt);subsub
    qs<-c(0.05,0.5,0.9)#define quantiles
    lt=c("solid", "dashed", "dotted")#define linetype, "dotdash", 
    #if (t=="D"){
    #   tradeoff=ggplot(data=subclim_climvar,aes(x = value,y = (halog))) + #geom_point(data=subclim_climvar,aes(x=value,y=valuehalog, color=cp_vt))+
    #     geom_point(aes(colour = maj_class))+ xlab(paste("Ln(",t, "value)")) +  ylab(paste("ln Area (ha)")) + theme_bw((base_size = 24))+
    #     theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+scale_color_manual(values=color)+
    #     #facet_wrap(~maj_class)+ #uncomment this line to plot each veg type in its own panel
    #     geom_quantile(quantiles =qs,linetype=c("solid"), aes(colour = maj_class))#+ geom_quantile(quantiles = 0.5,linetype="b",aes(colour = maj_class))+ geom_quantile(quantiles = 0.9,linetype="c",aes(colour = maj_class)) #remove background grid
    #   print(tradeoff)
    # }else if (t=="SOIL"){
    #   tradeoff=ggplot(data=subclim_climvar,aes(x = log(value),y = log(halog))) + #geom_point(data=subclim_climvar,aes(x=value,y=valuehalog, color=cp_vt))+
    #     geom_point(aes(colour = maj_class))+ xlab(paste("Ln(SM value)")) +  ylab(paste("ln Area (ha)")) + theme_bw((base_size = 24))+
    #     theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+scale_color_manual(values=color)+
    #     #facet_wrap(~maj_class)+ #uncomment this line to plot each veg type in its own panel
    #     geom_quantile(formula=y~x+I(x^2),quantiles =qs,linetype=c("solid"), aes(colour = maj_class)) #remove background grid
    #   print(tradeoff)
    #   # print(tradeoff + geom_quantile(quantiles = qs,aes(colour = maj_class)))#linear quantile regression lines
    #   #}
    # }else {
    #   
    #   #change "P" to "PRCP" for x-axis 
    #   if (t=="P"){
    #     t="PRCP"
    #   }
    #   tradeoff=ggplot(data=subclim_climvar,aes(x = (value),y = (halog))) + #geom_point(data=subclim_climvar,aes(x=value,y=valuehalog, color=cp_vt))+
    #     geom_point(aes(colour = maj_class))+ xlab(paste(t,"value")) +  ylab(paste("ln Area (ha)")) + theme_bw((base_size = 24))+
    #     theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+scale_color_manual(values=color)+
    #     #facet_wrap(~maj_class)+ #uncomment this line to plot each veg type in its own panel 
    #     #stat_quantile(method="lm",formula=y~poly(x,2)) #lm(formula = noisy.y ~ poly(q, 3))
    #     #stat_quantile(method="lm",formula=y ~ poly(x, 2), quantiles = c(0.25,0.75)) 
    #     geom_quantile(quantiles =qs,linetype=c("solid"), aes(colour = maj_class)) #remove background grid
    #   print(tradeoff)
    # }
    
  }#next climate variable starts at 3863
  #multiplot(plotlist=plots,cols=2)
  rqtable
############################################################################################################################  

#water balance values for each fire WITHOUT any data for other years when fires did not burn at each fire location
#without context of conditions in non fire years these distribution are flat and broad and not very useful
head(ig_status_wb)# holder for water balance conditions on day of ignition for each fire
head(ig_end_wb)# holder for water balance conditions on day of ignition to end of ignition calendar year for each fire
head(cur_yr_wb)# holder for water balance conditions for whole year in years when ignitions occur for each fire
head(lag_one_wb)# holder for water balance conditions in year prior to ignition for each fire

names(ig_status_wb);ncol(ig_status_wb)
#ranks<-cbind(ig_status_wb[,1:9],apply(ig_status_wb[10:19],2,percent_rank));head(ranks)
##ranks2<- subset(ig_end_wb, select=-c(Acres));names(ranks2)
ranks2<- subset(ig_end_wb);names(ranks2)
##mranks<-melt(ranks2, id=c("year","month","day","doy","site","maj_class","Fire_Type","halog"));head(mranks)
mranks<-melt(ranks2, id=c("year","month","day","doy","Event_ID","maj_class","Incid_Type"));head(mranks)

hplot<-ggplot(mranks, aes(x=value)) + geom_density(aes(fill=maj_class), alpha=0.3)+ 
  facet_wrap(~variable, scales="free") + theme_bw() + #theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  theme(axis.text=element_text(size=12,face="bold",family="Courier New"),
        axis.title=element_text(size=28,face="bold",family="Courier New"),
        title=element_text(size=20,face="bold",family="Courier New"))+ labs(title = "", x = "Conditions at Ignition to End of Year", y = "", color = NULL)
hplot 

qs<-c(0.95)#quantiles for quantile regression
splot<- ggplot(mranks, aes(x=value, y=log(Acres),color=maj_class)) + geom_point(size = 5) + # geom_bar(stat="identity", position=position_dodge())+#
  facet_wrap(~variable, scales="free")+#scale_x_continuous("Year", breaks=seq(1985,2015,1), limits=c(1985,2015))+ 
  theme_bw() + theme( panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  theme(axis.text=element_text(size=24,face="bold",family="Courier New"),
        axis.title=element_text(size=28,face="bold",family="Courier New"),
        title=element_text(size=20,face="bold",family="Courier New"))+ labs(title = "", x = NULL, y = "Area (log Ha)", color = NULL)+
  theme(legend.text=element_text(size=24),legend.position="bottom")+guides(color=guide_legend(nrow=1))
splot + geom_quantile(formula=y~x+exp(x),quantiles =qs,linetype=c("solid"), aes(colour = maj_class))


#percentile ranks within fire season
head(rankstack_tree)#percentile rank of ig condition within fire season across all years
head(rankstack_herb)#percentile rank of ig condition within fire season across all years
head(rankstack_shrub)#percentile rank of ig condition within fire season across all years
rankstack_all<-rbind(rankstack_tree,rankstack_herb,rankstack_shrub)
names(rankstack_all);head(rankstack_all)


#for plotting purposes swap out subset file name with above data frames
#drop acres and use log of hectares
rankstack2<- subset(rankstack_all, select=-c(Acres,RAIN));names(rankstack_all)
#for testing in Excel
write.csv(rankstack2,"rankstack2.csv")
mrankstack<-melt(rankstack2, id=c("doy","Event_ID","maj_class","Incid_Type","Acres"));head(mrankstack)

hplot<-ggplot(mrankstack, aes(x=value)) + geom_density(aes(fill=maj_class), alpha=0.3)+ 
  facet_wrap(~variable, scales="fixed") + theme_bw() + #theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  theme(axis.text=element_text(size=12,face="bold",family="Courier New"),
        axis.title=element_text(size=28,face="bold",family="Courier New"),
        title=element_text(size=20,face="bold",family="Courier New"))+ labs(title = "", x = "Percentiles at Ignition", y = "", color = NULL)
        #theme(legend.text=element_text(size=24),legend.position="bottom")+
        #guides(color=guide_legend(nrow=1))
hplot 

qs<-c(0.99)#quantiles for quantile regression
#fire size by percentile of variable scatter plots
splot<- ggplot(mrankstack, aes(x=value, y=halog,color=maj_class)) + geom_point(size = 5) + # geom_bar(stat="identity", position=position_dodge())+#
  facet_wrap(~variable, scales="free")+#scale_x_continuous("Year", breaks=seq(1985,2015,1), limits=c(1985,2015))+ 
  theme_bw() + theme( panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  theme(axis.text=element_text(size=24,face="bold",family="Courier New"),
        axis.title=element_text(size=28,face="bold",family="Courier New"),
        title=element_text(size=20,face="bold",family="Courier New"))+ labs(title = "", x = NULL, y = "Area (log Ha)", color = NULL)+
  theme(legend.text=element_text(size=24),legend.position="bottom")+guides(color=guide_legend(nrow=1))
splot+ stat_smooth(method="lm")+theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
splot + geom_smooth(method = 'nls', formula = y ~ exp(a * x + b), method.args = list(start = c(a = 0.001, b = 3)), se = FALSE)

#my.equation<-slope~a*exp(b*pivot)# where exp is 2.781 
splot + geom_quantile(formula=y~x+I(x^2),quantiles =qs,linetype=c("solid"), aes(colour = maj_class))
splot + geom_quantile(formula=y~x,quantiles =qs,linetype=c("solid"), aes(colour = maj_class))

splot + geom_quantile(formula=y~x+exp(x),quantiles =qs,linetype=c("solid"), aes(colour = maj_class))

#plot density curves for fires by doy by dominant veg type. figure above split out by veg type
hplot<-ggplot(rankstack2,aes(x=doy,color=maj_class)) + geom_density(alpha = 0.2, lwd=3)+ #geom_vline(aes(xintercept=mean(doy)), color=maj_class,size=1.5)+
  scale_x_continuous( breaks=seq(0,365,30), limits=c(0,365))+
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  theme(axis.text=element_text(size=24,face="bold",family="Courier New"),
        axis.title=element_text(size=28,face="bold",family="Courier New"),
        title=element_text(size=20,face="bold",family="Courier New"))+
  labs(title = "", x = NULL, y = "Area (log Ha)", color = NULL)+
  theme(legend.text=element_text(size=16),legend.position="right")+guides(color=guide_legend(ncol=1))
hplot+ ylab("Fire frequency") + xlab("Day of Year")
#hplot + scale_x_continuous(sec.axis = sec_axis(~.+30.4)) # need x axis as calendar month



boxplot(halog ~ maj_class,col=c("white","lightgray"),ig_status_wb)
burn.model = lmer(halog ~ AET + D + (1|maj_class), data=ig_status_wb)
summary(burn.model)

#combine lag one data frame with other three to test how previous year affects current
names(lag_one_wb)
ig_lag<-cbind(ig_status_wb,lag_one_wb[,10:28]);head(ig_lag);names(ig_lag)#wb on ignition date with extra cols as previous year summary
rankstack2<-cbind(ig_end_wb,lag_one_wb[,10:28]);head(rankstack2);names(rankstack2)#wb from ignition date to end of burn year with previous year summary
cur_lag<-cbind(cur_yr_wb,lag_one_wb[,10:28]);head(cur_lag);names(cur_lag)#wb from entire ignition year with previous year

#plot density curves for fires by doy by dominant veg type. figure above split out by veg type
hplot<-ggplot(ig_status_wb,aes(x=meanRD,color=maj_class)) + geom_histogram(aes(y=..density..))
hplot  #+ geom_vline(aes(xintercept=mean(doy)), color=maj_class,size=1.5)+
  #scale_x_continuous( breaks=seq(0,365,30), limits=c(0,365))+
  + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  theme(axis.text=element_text(size=24,face="bold",family="Courier New"),
        axis.title=element_text(size=28,face="bold",family="Courier New"),
        title=element_text(size=20,face="bold",family="Courier New"))+
  labs(title = "", x = NULL, y = "Area (log Ha)", color = NULL)+
  theme(legend.text=element_text(size=16),legend.position="right")+guides(color=guide_legend(ncol=1))
hplot+ ylab("Fire frequency") + xlab("Water Balance")

##############
#https://www.r-bloggers.com/part-4a-modelling-predicting-the-amount-of-rain/
#http://blog.revolutionanalytics.com/2016/03/confidence-intervals-for-random-forest.html
#####################################################
library(randomForest)

#Randomforest models
#search and replace ig_end_wb, cur_yr_wb, ig_lag, rankstack2, cur_lag to test different preiods of summarized water balance that may affect fire size
head(rankstack2)
names(rankstack2);str(rankstack2)
levels(rankstack2$maj_class)#identify factor levels
endcol<-ncol(rankstack2)
##ranknum<-subset(rankstack2, maj_class=="Tree ")
ranknum<-subset(rankstack2, maj_class=="forest")
##ranknum<-rankstack2[,c(1:9,13:14)]#pull just numeric values
ranknum<-ranknum[,c(6:14,16)]#pull just numeric values
str(ranknum)
dim(ranknum)
head(ranknum)
vars<-ncol(ranknum)-1;vars

#training Sample with 70% of observations
# randomly pick 70% of the number of observations (365)
index <- sample(1:nrow(ranknum)-1,size = 0.7*nrow(ranknum)) #
# subset to include only the elements in the index
train <- ranknum[index,] 
# subset to include all but the r
test <- ranknum[-index,] 

set.seed(1)#set random number start to stablilize results
ranknum.rf=randomForest(halog ~ . , data =  train, ntree=1000, importance=TRUE)
ranknum.rf
plot(ranknum.rf)
which.min(ranknum.rf$mse)
varImpPlot(ranknum.rf)
# Using the importance()  function to calculate the importance of each variable
imp <- as.data.frame(sort(importance(ranknum.rf)[,1],decreasing = TRUE),optional = T)
names(imp) <- "% Inc MSE"
imp#it is estimated that, in the absence of that variable, the error would increase by this amount
# As usual, predict and evaluate on the test set
test.pred.forest <- predict(ranknum.rf,test)
RMSE.forest <- sqrt(mean((test.pred.forest-test$halog)^2))
RMSE.forest
MAE.forest <- mean(abs(test.pred.forest-test$halog))
MAE.forest

names(test)
obs_wb<-test[,c(1:vars)]#an observation data set for making predictions
obs_burned<-test[,vars + 1]
pred_halog<-predict(ranknum.rf,obs_wb)
plot(obs_burned,pred_halog)
lmfit<-lm(obs_burned~pred_halog);summary(lmfit)
abline(lmfit)


#######################################################
#model results for burned area vs. wb on ignition date######### all veg classes
#######################################################
###relations on date of ignition
stor_ig_status_sum <- list()
stor_ig_status_aic <- list()
for(i in names(ig_status_wb)[c(9:17)]){
  lmfit<-lm(get(i) ~ halog, ig_status_wb)
  lmsum<-summary(lm(get(i) ~ halog, ig_status_wb))
  aiclmfit<-AIC(lmfit)
  stor_ig_status_sum[[i]] <- lmsum
  stor_ig_status_aic[[i]] <-aiclmfit
}
stor_ig_status_sum
stor_ig_status_aic

###relations for ignition to end of year
stor_ig_end_sum <- list()
stor_ig_end_aic <- list()
for(i in names(ig_end_wb)[c(9:25)]){
  lmfit<-lm(get(i) ~ halog, ig_end_wb)
  lmsum<-summary(lm(get(i) ~ halog, ig_end_wb))
  aiclmfit<-AIC(lmfit)
  stor_ig_end_sum[[i]] <- lmsum
  stor_ig_end_aic[[i]] <-aiclmfit
}
stor_ig_end_sum 
stor_ig_end_aic 

###relations for whole year when fire burned
stor_cur_yr_sum <- list()
stor_cur_yr_aic <- list()
for(i in names(cur_yr_wb)[c(9:25)]){
  lmfit<-lm(get(i) ~ halog, cur_yr_wb)
  lmsum<-summary(lm(get(i) ~ halog, cur_yr_wb))
  aiclmfit<-AIC(lmfit)
  stor_cur_yr_sum[[i]] <- lmsum
  stor_cur_yr_aic[[i]] <-aiclmfit
}
stor_cur_yr_sum 
stor_cur_yr_aic


###relations for lag one year
stor_lag_one_sum <- list()
stor_lag_one_aic <- list()
for(i in names(lag_one_wb)[c(9:25)]){
  lmfit<-lm(get(i) ~ halog, lag_one_wb)
  lmsum<-summary(lm(get(i) ~ halog, lag_one_wb))
  aiclmfit<-AIC(lmfit)
  stor_lag_one_sum[[i]] <- lmsum
  stor_lag_one_aic[[i]] <-aiclmfit
}
stor_lag_one_sum 
stor_lag_one_aic 

###relations for ig date + lag one year
#test for indicators of positive growth lag 1 plus dryness current year
#test for indicators of positive growth lag 1 (AET) plus growth current year (AET) plus dryness current year (D)
head(ig_lag)
head(rankstack2)
head(cur_lag)


stor_ig_date_lag_one_sum <- list()
stor_ig_date_lag_one_aic <- list()
for(i in names(lag_one_wb)[c(9:25)]){
  y=lag_one_wb$halog#this can be from either data frame since they'er the same in this value
  x=get(i,ig_status_wb)
  z=
  lmfit<-lm(get(i) ~ halog, lag_one_wb)
  lmsum<-summary(lm(get(i) ~ halog, lag_one_wb))
  aiclmfit<-AIC(lmfit)
  stor_lag_one_sum[[i]] <- lmsum
  stor_lag_one_aic[[i]] <-aiclmfit
}
stor_lag_one_sum 
stor_lag_one_aic 





#are fires occurring earlier in the year? NOPE!
hplot<-ggplot(ig_status_wb, aes(x=year, y=doy,color=maj_class)) + geom_point() + geom_smooth(method=lm)
hplot
lmfit<-lm(ig_status_wb$doy~ig_status_wb$year)#for all veg classes
summary(lmfit)

igstatus_herb<-subset(ig_status_wb, maj_class="Herb");igstatus_herb
lmfit_herb<-lm(igstatus_herb$doy~igstatus_herb$year)#for all herb class
summary(lmfit_herb)

igstatus_shrub<-subset(ig_status_wb, maj_class="non-forest");igstatus_shrub
lmfit_shrub<-lm(igstatus_shrub$doy~igstatus_shrub$year)#for all shrub class
summary(lmfit_shrub)

igstatus_tree<-subset(ig_status_wb, maj_class="forest");igstatus_tree
lmfit_tree<-lm(igstatus_tree$doy~igstatus_tree$year)#for all tree class
summary(lmfit_tree)

####explore condition distritutions for day of ignition
#subset out just the water balance variables of interest
names(ig_status_wb)
#drop acres and use log of hectares
ig_status_wb2<- subset(ig_status_wb, select=-c(Acres));names(ig_status_wb2)
mstack<-melt(ig_status_wb2, id=c("year","month","day","Event_ID","maj_class","Incid_Type"));head(mstack)

hplot<-ggplot(mstack, aes(x=value, color=maj_class)) + geom_density(aes(group=maj_class, fill=maj_class), alpha=0.3)+ facet_wrap(~variable, scales="free")
#geom_vline(aes(xintercept=mean(VPD)),color="blue", linetype="dashed", size=1)
hplot

#subset by fire size
wfmstack<-subset(mstack, halog > 7);head(wfmstack)
hplot<-ggplot(wfmstack, aes(x=value, color=maj_class)) + geom_density(aes(group=maj_class, fill=maj_class), alpha=0.3)+ facet_wrap(~variable, scales="free")
#geom_vline(aes(xintercept=mean(VPD)),color="blue", linetype="dashed", size=1)
hplot



#scatter plots to demonstrate water balance relationships with fire size
#scale to make z scores for direct comparison across wb variables that have different units
#this will allow you to determine which variables burned area is most sensitive to.
names(ig_status_wb2)
igscale<-scale(ig_status_wb2[,c(4,9:18)], center=FALSE, scale=FALSE);head(igscale)
igscale<-cbind(ig_status_wb2[,c(1:3,5:8)],igscale);head(igscale)
#make scatter plots of wb variable vs area but first need to reorganize data frame
#mstack<-melt(igstatus_stack2, id=c("year","month","day","site","maj_class","Fire_Type","halog"));head(mstack)#for unscaled applications
mstack<-melt(igscale, id=c("year","month","day","site","maj_class","Fire_Type","halog"));head(mstack)#for scaled applications
#subset by fire type and or size
wfmstack<-subset(mstack, Fire_Type =="WF" & halog > 0);head(wfmstack)
splot<-ggplot(mstack, aes(x=value, y=halog, shape=maj_class,color=maj_class)) + geom_point() +facet_wrap(~variable, scales="free")#fix scales if scaling and centering
splot +  geom_smooth(method=lm, se=TRUE)#slopes of these regression lines tell us which variable fire size is most responsive to

######################Percentile ranks of ignition conditions ##############################
#compute percentile rank of each observation in the sequence of years before and including (last line) year of fire
head(ig_status_wb2)
df_rank<-as.data.frame(lapply(ig_status_wb2[,c(8:ncol(ig_status_wb2))],cume_dist));head(df_rank)#Proportion of all values less than or equal to the current rank.
df_rank2<-cbind(ig_status_wb2[,"halog"],df_rank);head(df_rank2)
plot(df_rank2$RD)
##########################################################################################################################
######END build stacks of wb data corresponding to ig date, ig year and prior year of ignition####
##########################################################################################################################




#stopped here

#compute mean, max, min 
mean_rank<-apply(burn_ranks[,c("SVP","RH","VPD","Tavg","SOIL")],2,mean);mean_rank
max_rank<-apply(burn_ranks[,c("SVP","RH","VPD","Tavg","SOIL")],2,max);max_rank
min_rank<-apply(burn_ranks[,c("SVP","RH","VPD","Tavg","SOIL")],2,min);min_rank#need to include data outside of ig date or this can be zero
median_rank<-apply(burn_ranks[,c("SVP","RH","VPD","Tavg","SOIL")],2,median);median_rank
  
#burned area by year and veg type
x<-srmasterwf$Year;x
y<-srmasterwf$Acres
y<-srmasterwf$halog

Data<-data.frame(x,y);Data
plot(y ~ x, Data)
# fit a loess line
loess_fit <- loess(y ~ x, Data)
lines(Data$x, predict(loess_fit), col = "blue")

ggplot(Data,aes(x=x,y=y)) + geom_point() + geom_smooth()

# fit a non-linear regression
nls_fit <- nls(y ~ a + b * x^(c), Data, start = c(b = 1), alg = "plinear")
lines(Data$x, predict(nls_fit), col = "red")

plot(srmasterwf$doy,srmasterwf$Acres)
# fit a non-linear regression
nls_fit <- nls(y ~ a + b * x^(-c), Data, start = list(a = 80, b = 20, c = 0.2))
lines(Data$x, predict(nls_fit), col = "red")
  df <- percent_rank(as.data.frame(doy_ann$VPD));df
  df_cum<-cume_dist(as.data.frame(doy_ann$VPD));df_cum
  
 
   
  x <- timeBasedSeq('2010-01-01/2011-01-02 ')
  ep<-endpoints(wbxts, on="days", k=ig_date);ep

  wbxts[ep]
  
  
  x <- xts(1:length(x), x)
  x[.indexday(x) %in% "10"]
  
  
  ddat<-wbxts[indexday(wbxts) %in% 10];ddat
  ddat=wbxts[.indexday(wbxts) %in% c(5)];ddat
  mdat=wbxts[.indexday(wbxts) %in% c(5,6,7)];mdat
  
  rollapply(wbxts, width=365, sum, ., by = 1, by.column = TRUE) 
  
  ep1 <- endpoints(wbxts,on="weeks",k=2);ep1
  apply.monthly(wbxts,sum,1)
  
  sub<-wbxts[paste(start_date, end_date, sep="/"), ];head(sub)#water balance from ignition date backward in time
  
  

 
  
  
   dtx365<-as.Date(dtx)-365;dtx365#365days prior to ignition
  climout365<-wbxts[paste(dtx365,dtx, sep="/"), c("SVP","RH","VPD","P","Tavg","RAIN","PACK","W","PET","W_PET","SOIL","SOIL_Delta",
                                              "AET","RUNOFF","D","GDD")];head(climout365)
 
  
  dtx7<-as.Date(dtx)-7;dtx7#7days prior to ignition
  climout7<-wbxts[paste(dtx7,dtx, sep="/"), c("SVP","RH","VPD","P","Tavg","RAIN","PACK","W","PET","W_PET","SOIL","SOIL_Delta",
                                            "AET","RUNOFF","D","GDD")];climout7
  #rollapply(climout7, 2, align='left',cumsum)
  runSum(climout365[,1], n = 1, cumulative = TRUE)
  runMean(climout365[,1], n = 1, cumulative = TRUE)
  
  
  dtx14<-as.Date(dtx)-14;dtx14#14 days prior to ignition
  names(corewb)
  climout14<-wbxts[paste(dtx14,dtx, sep="/"), c("SVP","RH","VPD","P","Tavg","RAIN","PACK","W","PET","W_PET","SOIL","SOIL_Delta",
                                              "AET","RUNOFF","D","GDD")];climout14
  
  #apply means or sums as appropriate depending on metric.
  mean7<-apply(climout7[,c("SVP","RH","VPD","Tavg","SOIL")],2,mean)
  sum7<-apply(climout7[,c(c("P","RAIN","PACK","W","PET","W_PET","SOIL_Delta",
                            "AET","RUNOFF","D","GDD"))],2,sum)
  
  
  climsum7<-ddply(climout7,.(Coarse_Veg2,poly_num,year),plyr::summarize,sum_p=sum(P),mean_t=mean(T),se_p=se(P),se_t=se(T))
  wb_ann_summary<-ddply(wb,.(Coarse_Veg2,poly_num,year),plyr::summarize,sum_p=sum(P),mean_t=mean(T),se_p=se(P),se_t=se(T))
  head(wb_ann_summary)#area_summary<-ddply(pivot_by_poly3,.(veg_type),plyr::summarize,areasum=sum(ha));area_summary
  #then compute mean across polygons in a coarse veg type of the earlier sum Ps and mean of mean T
  cv_ann_summary<-ddply(wb_ann_summary,.(Coarse_Veg2,year),plyr::summarize,sum_p=mean(sum_p),mean_t=mean(mean_t),se_p=se(se_p),se_t=se(se_t))
  head(cv_ann_summary)
  
  
  oob.err<-NULL
  test.err<-NULL
  #mtry is no of Variables randomly chosen at each split
  for(mtry in (vars/3)) 
  {
    rf=randomForest(halog ~ . , data = train,mtry=mtry,ntree=1000) #in regression mtry is number of variables / 3
    oob.err[mtry] = rf$mse[1000] #Error of all Trees fitted
    
    pred<-predict(rf,test) #Predictions on Test Set for each Tree
    test.err[mtry]= with(test, mean( (halog - pred)^2)) #Mean Squared Test Error
    
    cat(mtry," ") #printing the output to the console
    
  }
  test.err
  oob.err
  matplot(cbind(oob.err,test.err), pch=19 , col=c("red","blue"),type="b",ylab="Mean Squared Error",xlab="Number of Predictors Considered at each Split")
  legend("topright",legend=c("Out of Bag Error","Test Error"),pch=19, col=c("red","blue"))
  
