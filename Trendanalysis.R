#install.packages("here")
#install.packages("dplyr")
#install.packages("reshape2")
#install.packages("ggplot2")
#install.packages("lubridate")

library(here)
library(dplyr)
library(reshape2) 
library (ggplot2)
library(lubridate)
library(SWMPr)
library(ggpubr)

#Set your working directory

##All files saved in the same github folder, including data downloaded from box
here()
wq_pdb<-read.csv(here("Data", "PDB/wq_pdb.csv"))
met_pdb<-read.csv(here("Data","PDB/met_pdb.csv"))
nut_pdb<-read.csv(here("Data","PDB/nut_pdb.csv"))

wq_pdb_mean<-select(wq_pdb,contains(c("station","year","month","mean")))
met_pdb_mean<-select(met_pdb,contains(c("station","year","month","mean")))


by(wq_pdb_mean, wq_pdb_mean$station, summary)
by(met_pdb_mean, met_pdb_mean$station, summary)
by(nut_pdb, nut_pdb$station, summary)



##change it from this icky wide format to long

wq_pdb_mean_long<-melt(wq_pdb_mean, na.rm = FALSE, value.name = "value",
                       id = c("station","year","month"))

##Select the parameters of interest, let's do it differently than above

wq_pdb_mean_subset<-dplyr::filter(wq_pdb_mean_long, 
                                  grepl('turb', variable))

unique(wq_pdb_mean_subset$variable)

##And let's make a date we can get more granular with
wq_pdb_mean_subset$Date <- make_date(year = wq_pdb_mean_subset$year, 
                                     month = wq_pdb_mean_subset$month)
unique(wq_pdb_mean_subset$station)

wq_pdb_mean_subset<- wq_pdb_mean_subset%>%
  mutate(
    station = 
      ifelse(station == "pdbbpwq", 
             "Ploeng channel", station ))

wq_pdb_mean_subset<- wq_pdb_mean_subset%>%
  mutate(
    station = 
      ifelse(station == "pdbbywq", 
             "Bayview", station ))
wq_pdb_mean_subset<- wq_pdb_mean_subset%>%
  mutate(
    station = 
      ifelse(station == "pdbgdwq" , 
             "Gong deep", station ))

wq_pdb_mean_subset<- wq_pdb_mean_subset%>%
  mutate(
    station = 
      ifelse(station == "pdbgswq"  , 
             "Gong surface", station ))
wq_pdb_mean_subset<- wq_pdb_mean_subset%>%
  mutate(
    station = 
      ifelse(station == "pdbjewq"  , 
             "Joe Leary Estuary", station ))
wq_pdb_mean_subset<- wq_pdb_mean_subset%>%
  mutate(
    station = 
      ifelse(station == "pdbjlwq"  , 
             "Joe Leary slough", station ))
wq_pdb_mean_subset<- wq_pdb_mean_subset%>%
  mutate(
    station = 
      ifelse(station == "pdbjtwq"  , 
             "Joe Leary Tidegate", station ))


##Okay let's look at some more data now
ggplot(wq_pdb_mean_subset,aes(x=Date,y=value))+
  geom_point()+
  geom_line()+
  geom_smooth()+
  scale_x_date(date_breaks = "12 months", date_labels =  "%b %Y") +ylab("")+xlab("")+
  facet_wrap(station~variable,scales="free_y")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  theme_pubr()

##Change the labels to the actual station names for PDB - can be found on CDMO
wq_pdb_mean_subset$station<-factor(wq_pdb_mean_subset$station, 
                                   labels=c("Joe Leary Tidegate","Joe Leary Estuary", "Gong Deep",
                                            "Ploeg Channel", "Bayview Channel", "Gong Surface",
                                            "Padilla Bay Farm")) #"Joe Leary Slough" 

ggplot(wq_pdb_mean_subset,aes(x=Date,y=value))+
  geom_point()+
  geom_line()+
  geom_smooth()+
  scale_x_date(date_breaks = "12 months", date_labels =  "%b %Y") +ylab("")+xlab("")+
  facet_wrap(station~variable,scales="free_y")+
  theme_pubr()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

##More plotting
fixed_year <- 2018
fixed_month<-10
wq_pdb_mean_subset$Date_month <- make_date(year=fixed_year, month = wq_pdb_mean_subset$month)
wq_pdb_mean_subset$Date_year <- make_date(year=wq_pdb_mean_subset$year, month = fixed_month)


ggplot(wq_pdb_mean_subset,aes(x=Date_month,y=value,group=month))+
  geom_boxplot()+
  ylab("")+xlab("")+
  scale_x_date(date_breaks = "1 month", date_labels =  "%b") +
  facet_wrap(station~variable,scales="free_y")+
  theme_pubr()
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


ggplot(wq_pdb_mean_subset)+
  geom_boxplot(aes(x=Date_month,y=value,group=month))+
  geom_smooth(aes(x=Date_month,y=value))+
  ylab("")+xlab("")+
  scale_x_date(date_breaks = "1 month", date_labels =  "%b") +
  facet_wrap(station~variable,scales="free_y")+
  theme_pubr()
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

##Across years 2002-2024
  ggplot(wq_pdb_mean_subset,aes(x=Date_year,y=value,group=year))+
  geom_boxplot()+
  ylab("")+xlab("")+
  scale_x_date(date_breaks = "1 year", date_labels =  "%Y") +
  facet_wrap(station~variable,scales="free_y")+
    theme_pubr()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggplot(wq_pdb_mean_subset)+
  geom_boxplot(aes(x=Date_year,y=value,group=year))+
  geom_smooth(aes(x=Date_year,y=value))+
  ylab("")+xlab("")+
  scale_x_date(date_breaks = "1 year", date_labels =  "%Y") +
  facet_wrap(station~variable,scales="free_y")+
  theme_pubr()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


####
##Nutrient Data
nut_pdb_clean<-nut_pdb[,c(2:10)]
nut_pdb_long<-melt(nut_pdb_clean, na.rm = FALSE, value.name = "value",
                   id = c("station","year","month"))

nut_pdb_long_subset<-dplyr::filter(nut_pdb_long, 
                                   grepl('nh4', variable))

unique(nut_pdb_long_subset$variable)


nut_pdb_long_subset$Date <- make_date(year = nut_pdb_long_subset$year, 
                                      month = nut_pdb_long_subset$month)


unique(nut_pdb_long_subset$station)

nut_pdb_long_subset<- nut_pdb_long_subset%>%
  mutate(
    station = 
      ifelse(station == "pdbbpnut", 
             "Ploeng channel", station ))

nut_pdb_long_subset<- nut_pdb_long_subset%>%
  mutate(
    station = 
      ifelse(station == "pdbbynut", 
             "Bayview", station ))
nut_pdb_long_subset<- nut_pdb_long_subset%>%
  mutate(
    station = 
      ifelse(station == "pdbgdnut" , 
             "Gong deep", station ))

nut_pdb_long_subset<- nut_pdb_long_subset%>%
  mutate(
    station = 
      ifelse(station == "pdbgsnut"  , 
             "Gong surface", station ))
nut_pdb_long_subset<- nut_pdb_long_subset%>%
  mutate(
    station = 
      ifelse(station == "pdbjenut"  , 
             "Joe Leary Estuary", station ))
nut_pdb_long_subset<- nut_pdb_long_subset%>%
  mutate(
    station = 
      ifelse(station == "pdbjlnut"  , 
             "Joe Leary slough", station ))
nut_pdb_long_subset<- nut_pdb_long_subset%>%
  mutate(
    station = 
      ifelse(station == "pdbjtnut"  , 
             "Joe Leary Tidegate", station ))

nut_pdb_long_subset<- nut_pdb_long_subset%>%
  mutate(
    variable = 
      ifelse(variable == "nh4f"  , 
             "Ammonium(mg/L)", station ))




ggplot(nut_pdb_long_subset,aes(x=Date,y=value))+
  geom_point()+
  geom_smooth()+
  scale_x_date(date_breaks = "12 months", date_labels =  "%b %Y") +  ylab("")+xlab("")+
  facet_wrap(station~variable,scales="free_y")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

nut_pdb_long_subset$station<-factor(nut_pdb_long_subset$station, 
                                    labels=c("Joe Leary Tidegate","Joe Leary Estuary", "Gong Deep",
                                             "Ploeg Channel", "Bayview Channel", "Gong Surface",
                                             "Padilla Bay Farm"))
#"Joe Leary Slough",  "Padilla Bay Farm"))

nut_pdb_long_subset$Date <- make_date(year = nut_pdb_long_subset$year, 
                                      month = nut_pdb_long_subset$month)


ggplot(nut_pdb_long_subset,aes(x=Date,y=value))+
  geom_point()+
  geom_smooth()+
  scale_x_date(date_breaks = "12 months", date_labels =  "%b %Y") +  ylab("")+xlab("")+
  facet_wrap(station~variable,scales="free_y")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

##More plotting-MONTHS
fixed_year <- 2020
fixed_month<-10
nut_pdb_long_subset$Date_month <- make_date(year=fixed_year, month = nut_pdb_long_subset$month)
nut_pdb_long_subset$Date_year <- make_date(year=nut_pdb_long_subset$year, month = fixed_month)


ggplot(nut_pdb_long_subset,aes(x=Date_month,y=value,group=month))+
  geom_boxplot()+
  ylab("")+xlab("")+
  scale_x_date(date_breaks = "1 month", date_labels =  "%b") +
  facet_wrap(station~variable,scales="free_y")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggplot(nut_pdb_long_subset)+
  geom_boxplot(aes(x=Date_month,y=value,group=month))+
  geom_smooth(aes(x=Date_month,y=value))+
  ylab("")+xlab("")+
  scale_x_date(date_breaks = "1 month", date_labels =  "%b") +
  facet_wrap(station~variable,scales="free_y")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

#Across years
ggplot(nut_pdb_long_subset,aes(x=Date_year,y=value,group=year))+
  geom_boxplot()+
  ylab("")+xlab("")+
  scale_x_date(date_breaks = "1 year", date_labels =  "%Y") +
  facet_wrap(station~variable,scales="free_y")+
  theme_pubr()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggplot(nut_pdb_long_subset)+
  geom_boxplot(aes(x=Date_year,y=value,group=year))+
  geom_smooth(aes(x=Date_year,y=value))+
  ylab("")+xlab("")+
  scale_x_date(date_breaks = "1 year", date_labels =  "%Y") +
  facet_wrap(station~variable,scales="free_y")+
  theme_pubr()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))



