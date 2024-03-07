# Script to explore your reserve's data
# Zoo 955, Spring 2024

###########################
# Begin user input section
library(ggpubr)
# Set the working directory to wherever this script is located
setwd("/Users/hitelab/OneDrive/Master/R_projects/SWMPProject")

# Identify the meteorological (met), nutrient (nut), and water quality (WQ) data files for your reserve
metFile <- read.csv("Data/PDB/met_pdb.csv")
nutFile = read.csv("Data/PDB/nut_pdb.csv")
wqFile  = read.csv("Data/PDB/wq_pdb.csv")
# End user input section
###########################

# Load the data into separate data frames
datMet = read.csv("Data/PDB/met_pdb.csv")
datNut =  read.csv("Data/PDB/nut_pdb.csv")
datWQ  = read.csv("Data/PDB/wq_pdb.csv")


# Create a handy year fraction variable for each data frame for plotting
datMet$YearFrac = datMet$year + datMet$month/12
datNut$YearFrac = datNut$year + datNut$month/12
datWQ$YearFrac  = datWQ$year + datWQ$month/12

# Each reserve can have multiple sampling stations
# Determine the number of stations for each data frame
uMet = unique(datMet$station)
uNut = unique(datNut$station)
uWQ  = unique(datWQ$station)

# Print out the unique stations for each data frame
cat('Unique met stations: ',uMet,'\n')
cat('Unique nut stations: ',uNut,'\n')
cat('Unique WQ  stations: ',uWQ,'\n')

# Cycle through the met data and plot in natural order
par(mfrow=c(3,1),lend=2,mai = c(0.25,0.75, 0.08, 0.05),oma = c(2,1,0.2,0.2), cex = 0.8)

print(paste('Generating ', (dim(datMet)[2]-4-1)/3, ' plots...',sep=""))
# Note that the first 4 columns do not contain observational data
for (i in 5:dim(datMet)[2]){
  thisVar = colnames(datMet[i])
  if (thisVar != 'YearFrac'){ # Only plot if it's actually data
    plot(datMet$YearFrac,datMet[,i],type='l',
         xlab="Year",ylab=paste("Met: ", thisVar,sep=""))
  }
}

# For nutrient and water quality data, there can be several sites
# Setup color scheme for plotting multiple sites per panel
myCol = c('black','red','blue','green','orchid','orange','hotpink','forestgreen')

# Cycle through the nutrient data and plot
par(mfrow=c(3,1),lend=2,mai = c(0.25,0.75, 0.08, 0.05),oma = c(2,1,0.2,0.2), cex = 0.8)

print(paste('Generating ', (dim(datNut)[2]-4-1)/3, ' plots...',sep=""))
# Cycle through the variables
for (i in 5:dim(datNut)[2]){
  thisVar = colnames(datNut[i])
  if (thisVar != 'YearFrac'){ # Only plot if it's actually data
    # Cycle through the sites
    myXLim = c(min(datNut$YearFrac,na.rm=TRUE),max(datNut$YearFrac,na.rm=TRUE))
    myYLim = c(min(datNut[,i],na.rm=TRUE),max(datNut[,i],na.rm=TRUE))
    if (any(is.infinite(myYLim))){ # In this case, there are no data and min/max produces Inf
      myYLim = c(0,0)
    }
    for (j in 1:length(uNut)){
      #thisNut = uNut[i]
      whichRows = which(datNut$station==uNut[j])
      if (j==1){
        plot(datNut$YearFrac[whichRows],datNut[whichRows,i],type='l',col=myCol[j],
             xlab="Year",ylab=paste("Nut: ", thisVar,sep=""))#,main=paste('Site ',siteName))
      }else{
        lines(datNut$YearFrac[whichRows],datNut[whichRows,i],col=myCol[j])
      }
    }
    legend('topleft',legend=uNut,lty=c(1,1,1,1,1),col=myCol)
  }
}


# Cycle through the water quality (WQ) data and plot
par(mfrow=c(3,1),lend=2,mai = c(0.25,0.75, 0.08, 0.05),oma = c(2,1,0.2,0.2), cex = 0.8)

print(paste('Generating ', (dim(datWQ)[2]-4-1)/3, ' plots...',sep=""))
# Cycle through the variables
for (i in 5:dim(datWQ)[2]){
  thisVar = colnames(datWQ[i])
  if (thisVar != 'YearFrac'){ # Only plot if it's actually data
    # Cycle through the sites
    # Set the overall X and Y limits
    myXLim = c(min(datWQ$YearFrac,na.rm=TRUE),max(datWQ$YearFrac,na.rm=TRUE))
    myYLim = c(min(datWQ[,i],na.rm=TRUE),max(datWQ[,i],na.rm=TRUE))
    if (any(is.infinite(myYLim))){ # In this case, there are no data and min/max produces Inf
      myYLim = c(0,0)
    }
    for (j in 1:length(uWQ)){ # For each unique site (uWQ), plot the water quality line
      whichRows = which(datWQ$station==uWQ[j])
      if (j==1){
        plot(datWQ$YearFrac[whichRows],datWQ[whichRows,i],xlim=myXLim,ylim=myYLim,type='l',col=myCol[j],
             xlab="Year",ylab=paste("WQ: ", thisVar,sep=""))#,main=paste('Site ',siteName))
      }else{
        lines(datWQ$YearFrac[whichRows],datWQ[whichRows,i],col=myCol[j])
      }
    }
    legend('topleft',legend=uWQ,lty=c(1,1,1,1,1),col=myCol)
  }
}

######## Emily's data visulization #####



wq <-  datWQ %>%
  dplyr::select("station", "year", "month", "sal_mean", "turb_mean", "sal_min", "sal_median", "sal_max")
wq$YearFrac <-wq$year + wq$month/12

wq$sta <- substr(wq$station, 1,5)

nut <- datNut %>%
  dplyr::select("station", "year", "month", "chla_n", "nh4f", "no3f")
nut$YearFrac  = nut$year + nut$month/12

nut$sta <- substr(nut$station, 1,5)

d <-full_join(wq, nut, by = c("sta", "year", "month"))
d$YearFrac  = d$year + d$month/12

ggplot(d, aes(log(nh4f+1), log(turb_mean+1), color = as.factor(year)))+
  geom_point()+
  geom_smooth(method = lm, se = F)+
  facet_wrap(~sta, scales = 'free')

nut %>%
  filter(station == "pdbjenut" | station == "pdbbpnut" |station == "pdbgsnut" | station == "pdbgdnut") %>%
  ggplot(aes(x = YearFrac, y = (nh4f), color = station)) + 
   geom_line()

##### Is there a gradient of ammounnium from JL slough out to the Gong sampling site  
#### time series of ammonium at the 3 gradient sites
d %>%
  filter(sta == "pdbje" | sta == "pdbbp" |sta == "pdbgs" | sta== "pdbgd") %>%
  ggplot(aes(x =YearFrac, y = nh4f))+
  geom_line(color = "orange") +
  facet_wrap(~sta, labeller = labeller(sta = c("pdbje" = "Joe Leary Estuary", "pdbbp" = "Ploeg Channel", "pdbgs" = "Gong Shallow", "pdbgd" = "Gong Deep")))


#### does the influence of water releases from the tide gate at JL reach the gong site?
##Monthly mean for salinity 
wq %>%
  filter(station == "pdbjewq" | station == "pdbbpwq" |station == "pdbgswq" | station == "pdbgdwq") %>%
  ggplot(aes(x = YearFrac, y = (sal_mean), color = station)) + 
  geom_line()
#### hard to interperate, maybe looking at the min will be more informative to tell us the magnitude of freshwater that entered the system 
wq %>%
  filter(station == "pdbjewq" | station == "pdbbpwq" |station == "pdbgswq" | station == "pdbgdwq") %>%
  ggplot(aes(x =YearFrac, y = (sal_min), color = station))+
  geom_line()
  
##### minimum salinity is lowest at the JE site... much more freshwater influence, spikes when water is more influenced by ocean tides?

wq %>%
  filter(sta == "pdbje" | sta == "pdbbp" | sta == "pdbgs" | sta == "pdbgd") %>%
  ggplot(aes(x = YearFrac, y = sal_median)) +
  geom_line(color = "gold") +
  geom_line(aes(x = YearFrac, y = sal_min), color = "green") +
  geom_line(aes(x = YearFrac, y = sal_max), color = "blue") +
  facet_wrap(~sta, scales = "free", labeller = labeller(sta = c("pdbje" = "Joe Leary Estuary", "pdbbp" = "Ploeg Channel", "pdbgs" = "Gong Shallow", "pdbgd" = "Gong Deep"))) +
  labs(x = "Year", y = "Salinity")



###### is there a correlation between ammonium and turb/chl in the slough?
##### Only looking at the two eelgras sites
### time vs chl 
d %>%
  filter(sta == "pdbbp" | sta == "pdbby") %>%
  ggplot(aes(x = YearFrac, y = chla_n, color = sta)) +
  geom_line() +
  geom_smooth(se = F)+
  ylim(0,35) +
  scale_color_manual(values = c("pdbbp" = "blue", "pdbby" = "red"),
                     labels = c("Ploeg Channel", "Bayview Channel")) +
  labs(x = "Year Fraction", y = "Chlorophyll-a")

#### time vs ammonium #### 
d %>%
  filter(sta == "pdbbp") %>%
  ggplot(aes(x = YearFrac, y = scale(nh4f), color = "red")) +
  geom_line() +
  geom_line(aes(x = YearFrac, y = scale(chla_n)), color = "blue") 

###Ammonium vs chlorophyll across time for pdbbp (Ploeg chanell)
p1 <- d %>%
  filter(sta == "pdbbp") %>%
  ggplot(aes(x = YearFrac)) +
  geom_line(aes(y = scale(nh4f), color = "Ammonia")) +
  geom_line(aes(y = scale(chla_n), color = "Chlorophyll")) +
  geom_smooth(aes(y = scale(nh4f)), method = "lm", se = FALSE, color = "red") +  # Adding trendline for Ammonia
  geom_smooth(aes(y = scale(chla_n)), method = "lm", se = FALSE, color = "blue") +  # Adding trendline for Chlorophyll
  scale_color_manual(values = c("red", "blue"), labels = c("Ammonia", "Chlorophyll")) +
  labs(y = "Scaled values", x = "Year", color = "Variable",subtitle = "Ploeg channel") +
  theme(legend.position = "bottom") +
  theme_pubr()


###Ammonium vs chlorophyll across time for pdbby (Bayview chanell)
p2 <- d %>%
  filter(sta == "pdbby") %>%
  ggplot(aes(x = YearFrac)) +
  geom_line(aes(y = scale(nh4f), color = "Ammonia")) +
  geom_line(aes(y = scale(chla_n), color = "Chlorophyll")) +
  geom_smooth(aes(y = scale(nh4f)), method = "lm", se = FALSE, color = "red") +  # Adding trendline for Ammonia
  geom_smooth(aes(y = scale(chla_n)), method = "lm", se = FALSE, color = "blue") +  # Adding trendline for Chlorophyll
  scale_color_manual(values = c("red", "blue"), labels = c("Ammonia", "Chlorophyll")) +
  labs(y = "Scaled values", x = "Year", color = "Variable",subtitle = "Bayview channel") +
  theme(legend.position = "bottom") +
  theme_pubr()




###Ammonium vs Nitrate across time for pdbbp (Ploeg chanell)
p3 <- d %>%
  filter(sta == "pdbbp") %>%
  ggplot(aes(x = YearFrac)) +
  geom_line(aes(y = scale(nh4f), color = "Ammonia")) +
  geom_line(aes(y = scale(no3f), color = "Nitrate")) +
  geom_smooth(aes(y = scale(nh4f)), method = "lm", se = FALSE, color = "red") +  # Adding trendline for Ammonia
  geom_smooth(aes(y = scale(no3f)), method = "lm", se = FALSE, color = "blue") +  # Adding trendline for Nitrate
  scale_color_manual(values = c("red", "blue"), labels = c("Ammonia", "Nitrate")) +
  labs(y = "Scaled values", x = "Year", color = "Variable",subtitle = "Ploeg channel") +
  theme(legend.position = "bottom") +
  theme_pubr()



###Ammonium vs chlorophyll across time for pdbby (Bayview chanell)

p4 <- d %>%
  filter(sta == "pdbby") %>%
  ggplot(aes(x = YearFrac)) +
  geom_line(aes(y = scale(nh4f), color = "Ammonia")) +
  geom_line(aes(y = scale(no3f), color = "Nitrate")) +
  geom_smooth(aes(y = scale(nh4f)), method = "lm", se = FALSE, color = "red") +  # Adding trendline for Ammonia
  geom_smooth(aes(y = scale(no3f)), method = "lm", se = FALSE, color = "blue") +  # Adding trendline for Nitrate
  scale_color_manual(values = c("red", "blue"), labels = c("Ammonia", "Nitrate")) +
  labs(y = "Scaled values", x = "Year", color = "Variable",subtitle = "Bayview channel") +
  theme(legend.position = "bottom") +
  theme_pubr()


library(patchwork)

# Check data structure and dimensions
#str(d)

# Check for non-finite values
#summary(is.na(d))
#summary(is.infinite(d))

# Remove non-finite values
#d <- na.omit(d)


library(gridExtra)

# Assuming your plots are named p1, p2, p3, and p4
grid.arrange(p1, p2, p3, p4, nrow = 2, ncol = 2)
