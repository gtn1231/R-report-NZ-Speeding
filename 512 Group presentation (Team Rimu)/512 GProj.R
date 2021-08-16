library(tidyverse)
library(readxl)
Copy_of_road_policing_driver_offence_data_1jan2009_31dec2020 <- read_excel("Copy of road-policing-driver-offence-data-1jan2009-31dec2020.xlsx", 
                                                                           sheet = "Mobile Speed Camera")


####No. of offences (mobile)####
#removing 2009-2014 data
mcamspeed <- Copy_of_road_policing_driver_offence_data_1jan2009_31dec2020[,-c(3:80)]
#focus on number of offences
mcamspeedoffence <- mcamspeed[1:40,]

#rename first rows
colnames(mcamspeedoffence) = mcamspeedoffence[2, ] # the first row will be the header
mcamspeedoffence = mcamspeedoffence[-1, ]
mcamspeedoffence = mcamspeedoffence[-1, ] 
colnames(mcamspeedoffence)[1] <- "Mobile Camera-issued speed offences"
colnames(mcamspeedoffence)[2] <- "Province"

#manually changing all rows
colnames(mcamspeedoffence)[3:15] <-paste("2015",colnames(mcamspeedoffence[3:15]),sep="_")
colnames(mcamspeedoffence)[16:28] <-paste("2016",colnames(mcamspeedoffence[16:28]),sep="_")
colnames(mcamspeedoffence)[29:41] <-paste("2017",colnames(mcamspeedoffence[29:41]),sep="_")
colnames(mcamspeedoffence)[42:54] <-paste("2018",colnames(mcamspeedoffence[42:54]),sep="_")
colnames(mcamspeedoffence)[55:67] <-paste("2019",colnames(mcamspeedoffence[55:67]),sep="_")
colnames(mcamspeedoffence)[68:80] <-paste("2020",colnames(mcamspeedoffence[68:80]),sep="_")

#replenish as dataframe
mcamspeedoffence<- as.data.frame(mcamspeedoffence)

#replace NA with 0
mcamspeedoffence[is.na(mcamspeedoffence)] <- 0

#changing data to factors and numeric
mcamspeedoffence[,1:2] <- lapply(mcamspeedoffence[,1:2], as.factor)
mcamspeedoffence[,3:80] <- lapply(mcamspeedoffence[,3:80], as.numeric)

#total yearly offences
mcamspeedtotal <- mcamspeedoffence %>% select(ends_with("Total"))

mcamspeedtotal$`Mobile Camera-issued speed offences` <- mcamspeedoffence$`Mobile Camera-issued speed offences`

#sum of offences by region
mcamspeedtotalagg <- aggregate(. ~ mcamspeedtotal$`Mobile Camera-issued speed offences`, mcamspeedtotal, FUN = sum)

#remove redundant data
colnames(mcamspeedtotalagg)[1] <- "Mobile Camera-issued speed offences"
mcamspeedtotalagg = mcamspeedtotalagg[,-8]

#remove suffix
colnames(mcamspeedtotalagg)<-gsub("_Total","",colnames(mcamspeedtotalagg))

#change to long
data_long <- gather(mcamspeedtotalagg, year, count, '2015':'2020', factor_key=TRUE)

#change year to numeric for geom_line
data_long$year <- as.numeric(as.character(data_long$year))

#year,offences,region
ggplot(data_long, aes(`year`,`count`, colour = `Mobile Camera-issued speed offences`)) +
  geom_point() +
  geom_line() +
  scale_y_continuous(breaks = scales::breaks_width(10000)) +
  labs(y = "No. of infringement", x = "Year", colour = "Regions", title =  "No. of speeding infringements") +
  theme(plot.title = element_text(hjust = 0.5))
  

#####cost of speed (mobile camera)####
mcamspeedmon <- mcamspeed[47:86,]

#rename first rows
colnames(mcamspeedmon) = mcamspeedmon[2, ] # the first row will be the header
mcamspeedmon = mcamspeedmon[-1, ]
mcamspeedmon = mcamspeedmon[-1, ] 
colnames(mcamspeedmon)[1] <- "Mobile Camera-issued speed monetary amount"
colnames(mcamspeedmon)[2] <- "Province"

colnames(mcamspeedmon)[3:15] <-paste("2015",colnames(mcamspeedmon[3:15]),sep="_")
colnames(mcamspeedmon)[16:28] <-paste("2016",colnames(mcamspeedmon[16:28]),sep="_")
colnames(mcamspeedmon)[29:41] <-paste("2017",colnames(mcamspeedmon[29:41]),sep="_")
colnames(mcamspeedmon)[42:54] <-paste("2018",colnames(mcamspeedmon[42:54]),sep="_")
colnames(mcamspeedmon)[55:67] <-paste("2019",colnames(mcamspeedmon[55:67]),sep="_")
colnames(mcamspeedmon)[68:80] <-paste("2020",colnames(mcamspeedmon[68:80]),sep="_")

mcamspeedmon <- as.data.frame(mcamspeedmon)

mcamspeedmon[is.na(mcamspeedmon)] <- 0

#changing data to factors and numeric
mcamspeedmon[,1:2] <- lapply(mcamspeedmon[,1:2], as.factor)
mcamspeedmon[,3:80] <- lapply(mcamspeedmon[,3:80], as.numeric)

mcamspeedmoney <- mcamspeedmon %>% select(ends_with("Total"))

mcamspeedmoney$`Mobile Camera-issued speed monetary amount` <- mcamspeedmon$`Mobile Camera-issued speed monetary amount`

#sum of offences by region
mcamspeedmoneyagg <- aggregate(. ~ mcamspeedmoney$`Mobile Camera-issued speed monetary amount`, mcamspeedmoney, FUN = sum)

#remove redundant data
colnames(mcamspeedmoneyagg)[1] <- "Mobile Camera-issued speed monetary amount"
mcamspeedmoneyagg = mcamspeedmoneyagg[,-8]

#remove suffix
colnames(mcamspeedmoneyagg)<-gsub("_Total","",colnames(mcamspeedmoneyagg))

#change to long
money_data_long <- gather(mcamspeedmoneyagg, year, cost, '2015':'2020', factor_key=TRUE)

#change year to numeric for geom_line
money_data_long$year <- as.numeric(as.character(money_data_long$year))

#year,offences,region
ggplot(money_data_long, aes(`year`,`cost`, colour = `Mobile Camera-issued speed monetary amount`)) +
  geom_point() +
  geom_line() +
  scale_y_continuous(breaks = scales::breaks_width(1500000)) +
  labs(y = "Cost of infringement", x = "Year", colour = "Regions", title = "Total cost of infringement") +
  theme(plot.title = element_text(hjust = 0.5))

#####number of cars passed (mobile camera)####
mcamspeeddetect <- mcamspeed[94:133,]

colnames(mcamspeeddetect) = mcamspeeddetect[2, ] # the first row will be the header
mcamspeeddetect = mcamspeeddetect[-1, ]
mcamspeeddetect = mcamspeeddetect[-1, ] 
colnames(mcamspeeddetect)[1] <- "Number of vehicles detected at mobile camera sites"
colnames(mcamspeeddetect)[2] <- "Province"

colnames(mcamspeeddetect)[3:15] <-paste("2015",colnames(mcamspeeddetect[3:15]),sep="_")
colnames(mcamspeeddetect)[16:28] <-paste("2016",colnames(mcamspeeddetect[16:28]),sep="_")
colnames(mcamspeeddetect)[29:41] <-paste("2017",colnames(mcamspeeddetect[29:41]),sep="_")
colnames(mcamspeeddetect)[42:54] <-paste("2018",colnames(mcamspeeddetect[42:54]),sep="_")
colnames(mcamspeeddetect)[55:67] <-paste("2019",colnames(mcamspeeddetect[55:67]),sep="_")
colnames(mcamspeeddetect)[68:80] <-paste("2020",colnames(mcamspeeddetect[68:80]),sep="_")

mcamspeeddetect <- as.data.frame(mcamspeeddetect)

mcamspeeddetect[is.na(mcamspeeddetect)] <- 0

#changing data to factors and numeric
mcamspeeddetect[,1:2] <- lapply(mcamspeeddetect[,1:2], as.factor)
mcamspeeddetect[,3:80] <- lapply(mcamspeeddetect[,3:80], as.numeric)

#detected cars by year
mcamspeeddetyear <- mcamspeeddetect %>% select(ends_with("Total"))

mcamspeeddetyear$`Number of vehicles detected at mobile camera sites` <- mcamspeeddetect$`Number of vehicles detected at mobile camera sites`

#sum of cars by region
mcamspeeddetyearaggr <- aggregate(. ~ mcamspeeddetyear$`Number of vehicles detected at mobile camera sites`, mcamspeeddetyear, FUN = sum)

#remove redundant data
colnames(mcamspeeddetyearaggr)[1] <- "Number of vehicles detected at mobile camera sites"
mcamspeeddetyearaggr = mcamspeeddetyearaggr[,-8]

#remove suffix
colnames(mcamspeeddetyearaggr)<-gsub("_Total","",colnames(mcamspeeddetyearaggr))

#change to long
det_data_long <- gather(mcamspeeddetyearaggr, year, total_cars, '2015':'2020', factor_key=TRUE)

#change year to numeric for geom_line
det_data_long$year <- as.numeric(as.character(det_data_long$year))

#year,offences,region
ggplot(det_data_long, aes(`year`,`total_cars`, colour = `Number of vehicles detected at mobile camera sites`)) +
  geom_point() +
  geom_line() +
  scale_y_continuous(breaks = scales::breaks_width(1500000)) +
  labs(y = "No. of cars", x = "Year", color = 'Regions', title = "No. cars Detected") +
  theme(plot.title = element_text(hjust = 0.5))
  #geom_text(aes(label = `Number of vehicles detected at mobile camera sites`), check_overlap = TRUE) 
  

####joined data####
dlt <- data_long
colnames(dlt)[1] <- "Region"
mlt <- money_data_long
colnames(mlt)[1] <- "Region"
dtlt <- det_data_long
colnames(dtlt)[1] <- "Region"

#Joining all 3 data
dmt <- inner_join(dlt,mlt)
naturaljoined <- inner_join(dmt,dtlt)

colnames(naturaljoined)[3] <- "offences"

#per car caught
naturaljoined <- naturaljoined %>%
  group_by(Region,year) %>% 
  mutate(per_cars_caught = (offences/total_cars) * 100)

ggplot(naturaljoined, aes(year,per_cars_caught, colour = Region)) +
  geom_point() +
  geom_line() +
  scale_y_continuous(breaks = scales::breaks_width(0.2)) +
  labs(y = "% of cars caught", x = "Year", title = "Percentage of cars caught to cars detected")+
theme(plot.title = element_text(hjust = 0.5))

#cost per offence
naturaljoined <- naturaljoined %>%
  group_by(Region,year) %>% 
  mutate(per_cost_offence = (cost/offences))

ggplot(naturaljoined, aes(year,per_cost_offence, colour = Region)) +
  geom_point() +
  geom_line() +
  scale_y_continuous(breaks = scales::breaks_width(10)) +
  labs(y = "Cost per Infringement", x = "Year", title = "Average cost per infringement") +
theme(plot.title = element_text(hjust = 0.5))

plot(naturaljoined$Region,naturaljoined$per_cost_offence, xlab = "Region", ylab = "Cost per Infringement", main = "Cost of Infringement per Region (2015-2020)")

####Archive####
#removing 2009-2010 data
mcamspeed <- Copy_of_road_policing_driver_offence_data_1jan2009_31dec2020[,-c(3:28)]
#manually changing all rows (2011-2020)
colnames(mcamspeedoffence)[3:15] <-paste("2011",colnames(mcamspeedoffence[3:15]),sep="_")
colnames(mcamspeedoffence)[16:28] <-paste("2012",colnames(mcamspeedoffence[16:28]),sep="_")
colnames(mcamspeedoffence)[29:41] <-paste("2013",colnames(mcamspeedoffence[29:41]),sep="_")
colnames(mcamspeedoffence)[42:54] <-paste("2014",colnames(mcamspeedoffence[42:54]),sep="_")
colnames(mcamspeedoffence)[55:67] <-paste("2015",colnames(mcamspeedoffence[55:67]),sep="_")
colnames(mcamspeedoffence)[68:80] <-paste("2016",colnames(mcamspeedoffence[68:80]),sep="_")
colnames(mcamspeedoffence)[81:93] <-paste("2017",colnames(mcamspeedoffence[81:93]),sep="_")
colnames(mcamspeedoffence)[94:106] <-paste("2018",colnames(mcamspeedoffence[94:106]),sep="_")
colnames(mcamspeedoffence)[107:119] <-paste("2019",colnames(mcamspeedoffence[107:119]),sep="_")
colnames(mcamspeedoffence)[120:132] <-paste("2020",colnames(mcamspeedoffence[120:132]),sep="_")

####test test####

mcamspeedt <- mcamspeed[1:40,]

colnames(mcamspeedt) = mcamspeedt[2, ] # the first row will be the header
mcamspeedt = mcamspeedt[-1, ]
mcamspeedt = mcamspeedt[-1, ] 
mcamspeedt = mcamspeedt[,-2 ] 
colnames(mcamspeedt)[1] <- "Mobile Camera-issued speed offences"

mcamspeedt <- mcamspeedt[ , -which(names(mcamspeedt) %in% c("Total"))]
mcamspeedt<- as.data.frame(mcamspeedt)
mcamspeedt[is.na(mcamspeedt)] <- 0

mcamspeedt[2:49] <- as.numeric(mcamspeedt[2:49])

mcamspeedtaggr <- aggregate(. ~mcamspeedt$`Mobile Camera-issued speed offences`,mcamspeedt, FUN = sum)


crazy <- naturaljoined

crazy <- crazy %>%
  group_by(Region,year) %>% 
  mutate(pct_change = (offences/total_cars) * 100)


ggplot(naturaljoined, aes(x="", y=offences, fill= Region)) + 
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0)

ggplot(naturaljoined, aes(year,per_cost_offence, colour = Region)) +
  geom_density_2d()
  labs(y = "Cost per Offence", x = "Year")
