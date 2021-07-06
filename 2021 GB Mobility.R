initial_mob_2020=read.csv("2020_GB_Region_Mobility_Report.csv")
initial_mob_2020_1=initial_mob_2020
initial_mob_2021=read.csv("2021_GB_Region_Mobility_Report.csv")
initial_mob_2021_1=initial_mob_2021

#The last 6 are all '% change from baseline'
names(initial_mob_2020_1)=c("CountryRegionCode","CountryRegion","SubRegion1","SubRegion2","MetroArea","iso_3166_2_code","CensusFipsCode","PlaceID","Date","RetailRec",
                        "GroceryPharmacy","Parks","TransitStation","Workplaces","Residential")
names(initial_mob_2021_1)=c("CountryRegionCode","CountryRegion","SubRegion1","SubRegion2","MetroArea","iso_3166_2_code","CensusFipsCode","PlaceID","Date","RetailRec",
                            "GroceryPharmacy","Parks","TransitStation","Workplaces","Residential")
mob_2020_1=initial_mob_2020_1
mob_2020_1$Date=as.Date(mob_2020_1$Date)
mob_2021_1=initial_mob_2021_1
mob_2021_1$Date=as.Date(mob_2021_1$Date)

length(unique(mob_2020_1$PlaceID)) #419 unique places

mob_comb=rbind(mob_2020_1,mob_2021_1)

#### Separate into those with data for each of the 6 reasons to leave house ####

# 1. Retail and Recreation

retail_rec=mob_comb[is.na(mob_comb$RetailRec)==FALSE,]

# 2. Grocery and Pharmacy

grocery_pharm=mob_comb[is.na(mob_comb$GroceryPharmacy)==FALSE,]

# 3. Parks

parks=mob_comb[is.na(mob_comb$Parks)==FALSE,]

# 4. Transit Stations

transit=mob_comb[is.na(mob_comb$TransitStation)==FALSE,]

# 5. Workplaces

workplace=mob_comb[is.na(mob_comb$Workplaces)==FALSE,]

# 6. Residential

residential=mob_comb[is.na(mob_comb$Residential)==FALSE,]

# Take mean over all counties
retail_rec_mean=aggregate(retail_rec[, 10], list(retail_rec$Date), mean)
names(retail_rec_mean)=c("Date","RetailRec")

grocery_pharm_mean=aggregate(grocery_pharm[, 11], list(grocery_pharm$Date), mean)
names(grocery_pharm_mean)=c("Date","GroceryPharmacy")

parks_mean=aggregate(parks[, 12], list(parks$Date), mean)
names(parks_mean)=c("Date","Parks")

transit_mean=aggregate(transit[, 13], list(transit$Date), mean)
names(transit_mean)=c("Date","TransitStation")

workplace_mean=aggregate(workplace[, 14], list(workplace$Date), mean)
names(workplace_mean)=c("Date","Workplaces")

residential_mean=aggregate(residential[, 15], list(residential$Date), mean)
names(residential_mean)=c("Date","Residential")


# Example Codes
plot(GroceryPharmacy~Date,grocery_pharm[grocery_pharm$PlaceID=="ChIJ_dJhnG_h2UcRwOLBu6gcDgQ",],ylim=c(-100,100))
plot(GroceryPharmacy~Date,grocery_pharm_mean,ylim=c(-100,100))
grocery_pharm[grocery_pharm$PlaceID=="ChIJ_dJhnG_h2UcRwOLBu6gcDgQ" & grocery_pharm$Date=="2020-06-20",]

plot(Workplaces~Date,workplace[workplace$PlaceID=="ChIJ_dJhnG_h2UcRwOLBu6gcDgQ",],ylim=c(-100,100))
workplace[workplace$PlaceID=="ChIJ_dJhnG_h2UcRwOLBu6gcDgQ" & workplace$Date=="2020-06-20",]

plot(Residential~Date,residential[residential$PlaceID=="ChIJ_dJhnG_h2UcRwOLBu6gcDgQ",],ylim=c(-100,100))
plot(Residential~Date,residential_mean,ylim=c(-100,100))
residential[residential$PlaceID=="ChIJ_dJhnG_h2UcRwOLBu6gcDgQ" & residential$Date=="2020-06-20",]

# Example plots
Mobplot_R=ggplot(residential_mean)
Mobplot_R=Mobplot_R + geom_point(aes(x=Date,y=Residential),color="magenta",size=1)
Mobplot_R=Mobplot_R+labs(title="Residential")
Mobplot_R=Mobplot_R+ylab("% Change from Baseline")
Mobplot_R

Mobplot_WP=ggplot(workplace_mean)
Mobplot_WP=Mobplot_WP + geom_point(aes(x=Date,y=Workplaces),color="magenta",size=1)
Mobplot_WP=Mobplot_WP+labs(title="Workplace")
Mobplot_WP=Mobplot_WP+ylab("% Change from Baseline")
Mobplot_WP

Mobplot_T=ggplot(transit_mean)
Mobplot_T=Mobplot_T + geom_point(aes(x=Date,y=TransitStation),color="magenta",size=1)
Mobplot_T=Mobplot_T+labs(title="Transit Stations")
Mobplot_T=Mobplot_T+ylab("% Change from Baseline")
Mobplot_T

Mobplot_P=ggplot(parks_mean)
Mobplot_P=Mobplot_P + geom_point(aes(x=Date,y=Parks),color="magenta",size=1)
Mobplot_P=Mobplot_P+labs(title="Parks")
Mobplot_P=Mobplot_P+ylab("% Change from Baseline")
Mobplot_P

Mobplot_GP=ggplot(grocery_pharm_mean)
Mobplot_GP=Mobplot_GP + geom_point(aes(x=Date,y=GroceryPharmacy),color="magenta",size=1)
Mobplot_GP=Mobplot_GP+labs(title="Grocery and Pharmacy")
Mobplot_GP=Mobplot_GP+ylab("% Change from Baseline")
Mobplot_GP

Mobplot_RR=ggplot(retail_rec_mean)
Mobplot_RR=Mobplot_RR + geom_point(aes(x=Date,y=RetailRec),color="magenta",size=1)
Mobplot_RR=Mobplot_RR+labs(title="Retail and Recreation")
Mobplot_RR=Mobplot_RR+ylab("% Change from Baseline")
Mobplot_RR

### The combo and average over all ###########
mob_comb_needed=mob_comb[,c(9:15)]
mob_comb_rem=na.omit(mob_comb_needed)
mob_comb_rem$average=(mob_comb_rem$RetailRec+mob_comb_rem$GroceryPharmacy +mob_comb_rem$Parks +
                        mob_comb_rem$TransitStation +mob_comb_rem$Workplaces +mob_comb_rem$Residential)/6
mobility_average=mob_comb_rem[,c(1,8)]

mobility_average=aggregate(mob_comb_rem$average, list(mob_comb_rem$Date), mean)
names(mobility_average)=c("Date","Average")

Mobplot_Average=ggplot(mobility_average)
Mobplot_Average=Mobplot_Average + geom_point(aes(x=Date,y=Average),color="magenta",size=1)
Mobplot_Average=Mobplot_Average+labs(title="Average % Change in Mobility Across all Factors")
Mobplot_Average=Mobplot_Average+ylab("% Change from Baseline")
Mobplot_Average

