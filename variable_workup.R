library(lubridate)
library(imputeTS)
library(tseries)
library(forecast)
library(dplyr)
library(tidyr)
library(brms)
library(TSA)
library(piecewiseSEM)


## source the R code that makes the plot of the data used
source("/Users/laura.ganley001/Documents/R_Projects/SEM/plotting_sem_stations.R")


##******************************LOCAL WIND DATA**************************************
## I need to check that the various sources of wind data are correlated with 
## eachother in order to justify using multiple sources

## read in wind speed and direction data
boston_buoy_98 <- read.table("/Users/laura.ganley001/Documents/R_Projects/SEM/raw_data/local_wind_data/boston_buoy/wind_boston_buoy_1998.txt", header = TRUE)
boston_buoy_99 <-  read.table("/Users/laura.ganley001/Documents/R_Projects/SEM/raw_data/local_wind_data/boston_buoy/wind_boston_buoy_1999.txt", header = TRUE)
boston_buoy_00 <-  read.table("/Users/laura.ganley001/Documents/R_Projects/SEM/raw_data/local_wind_data/boston_buoy/wind_boston_buoy_2000.txt", header = TRUE)
boston_buoy_01 <-  read.table("/Users/laura.ganley001/Documents/R_Projects/SEM/raw_data/local_wind_data/boston_buoy/wind_boston_buoy_2001.txt", header = TRUE)
boston_buoy_02 <-  read.table("/Users/laura.ganley001/Documents/R_Projects/SEM/raw_data/local_wind_data/boston_buoy/wind_boston_buoy_2002.txt", header = TRUE)
boston_buoy_03 <-  read.table("/Users/laura.ganley001/Documents/R_Projects/SEM/raw_data/local_wind_data/boston_buoy/wind_boston_buoy_2003.txt", header = TRUE)
boston_buoy_04 <-  read.table("/Users/laura.ganley001/Documents/R_Projects/SEM/raw_data/local_wind_data/boston_buoy/wind_boston_buoy_2004.txt", header = TRUE)
boston_buoy_05 <-  read.table("/Users/laura.ganley001/Documents/R_Projects/SEM/raw_data/local_wind_data/boston_buoy/wind_boston_buoy_2005.txt", header = TRUE)
boston_buoy_06 <-  read.table("/Users/laura.ganley001/Documents/R_Projects/SEM/raw_data/local_wind_data/boston_buoy/wind_boston_buoy_2006.txt", header = TRUE)
boston_buoy_07 <-  read.table("/Users/laura.ganley001/Documents/R_Projects/SEM/raw_data/local_wind_data/boston_buoy/wind_boston_buoy_2007.txt", header = TRUE)
boston_buoy_08 <-  read.table("/Users/laura.ganley001/Documents/R_Projects/SEM/raw_data/local_wind_data/boston_buoy/wind_boston_buoy_2008.txt", header = TRUE)
boston_buoy_09 <-  read.table("/Users/laura.ganley001/Documents/R_Projects/SEM/raw_data/local_wind_data/boston_buoy/wind_boston_buoy_2009.txt", header = TRUE)
boston_buoy_10 <-  read.table("/Users/laura.ganley001/Documents/R_Projects/SEM/raw_data/local_wind_data/boston_buoy/wind_boston_buoy_2010.txt", header = TRUE)
boston_buoy_11 <-  read.table("/Users/laura.ganley001/Documents/R_Projects/SEM/raw_data/local_wind_data/boston_buoy/wind_boston_buoy_2011.txt", header = TRUE)
boston_buoy_13 <-  read.table("/Users/laura.ganley001/Documents/R_Projects/SEM/raw_data/local_wind_data/boston_buoy/wind_boston_buoy_2013.txt", header = TRUE)
boston_buoy_14 <-  read.table("/Users/laura.ganley001/Documents/R_Projects/SEM/raw_data/local_wind_data/boston_buoy/wind_boston_buoy_2014.txt", header = TRUE)
boston_buoy_15 <-  read.table("/Users/laura.ganley001/Documents/R_Projects/SEM/raw_data/local_wind_data/boston_buoy/wind_boston_buoy_2015.txt", header = TRUE)
boston_buoy_16 <-  read.table("/Users/laura.ganley001/Documents/R_Projects/SEM/raw_data/local_wind_data/boston_buoy/wind_boston_buoy_2016.txt", header = TRUE)
boston_buoy_april17 <-  read.table("/Users/laura.ganley001/Documents/R_Projects/SEM/raw_data/local_wind_data/boston_buoy/wind_boston_buoy_april2017.txt", header = TRUE)
boston_buoy_march17 <- read.table("/Users/laura.ganley001/Documents/R_Projects/SEM/raw_data/local_wind_data/boston_buoy/wind_boston_buoy_march2017.txt", header = TRUE)
boston_buoy_may17 <-  read.table("/Users/laura.ganley001/Documents/R_Projects/SEM/raw_data/local_wind_data/boston_buoy/wind_boston_buoy_may2017.txt", header = TRUE)
boston_buoy_jan17 <- read.table("/Users/laura.ganley001/Documents/R_Projects/SEM/raw_data/local_wind_data/boston_buoy/wind_boston_buoy_jan2017.txt", header = TRUE)
boston_buoy_feb17 <-  read.table("/Users/laura.ganley001/Documents/R_Projects/SEM/raw_data/local_wind_data/boston_buoy/wind_boston_buoy_feb2017.txt", header = TRUE)

gloucester_buoy_02 <- read.csv("/Users/laura.ganley001/Documents/R_Projects/SEM/raw_data/local_wind_data/gloucester_buoy/wind_spd_gloucester_buoy_2002.csv")
gloucester_buoy_03 <- read.csv("/Users/laura.ganley001/Documents/R_Projects/SEM/raw_data/local_wind_data/gloucester_buoy/wind_spd_gloucester_buoy_2003.csv")
gloucester_buoy_04 <- read.csv("/Users/laura.ganley001/Documents/R_Projects/SEM/raw_data/local_wind_data/gloucester_buoy/wind_spd_gloucester_buoy_2004.csv")
gloucester_buoy_05 <- read.csv("/Users/laura.ganley001/Documents/R_Projects/SEM/raw_data/local_wind_data/gloucester_buoy/wind_spd_gloucester_buoy_2005.csv")
gloucester_buoy_06 <- read.csv("/Users/laura.ganley001/Documents/R_Projects/SEM/raw_data/local_wind_data/gloucester_buoy/wind_spd_gloucester_buoy_2006.csv")
gloucester_buoy_07 <- read.csv("/Users/laura.ganley001/Documents/R_Projects/SEM/raw_data/local_wind_data/gloucester_buoy/wind_spd_gloucester_buoy_2007.csv")
gloucester_buoy_08 <- read.csv("/Users/laura.ganley001/Documents/R_Projects/SEM/raw_data/local_wind_data/gloucester_buoy/wind_spd_gloucester_buoy_2008.csv")
gloucester_buoy_09 <- read.csv("/Users/laura.ganley001/Documents/R_Projects/SEM/raw_data/local_wind_data/gloucester_buoy/wind_spd_gloucester_buoy_2009.csv")
gloucester_buoy_10 <- read.csv("/Users/laura.ganley001/Documents/R_Projects/SEM/raw_data/local_wind_data/gloucester_buoy/wind_spd_gloucester_buoy_2010.csv")
gloucester_buoy_11 <- read.csv("/Users/laura.ganley001/Documents/R_Projects/SEM/raw_data/local_wind_data/gloucester_buoy/wind_spd_gloucester_buoy_2011.csv")
gloucester_buoy_12 <- read.csv("/Users/laura.ganley001/Documents/R_Projects/SEM/raw_data/local_wind_data/gloucester_buoy/monthly_avg_wind_spd_direction_Gloucester_buoy_2012.csv")
gloucester_buoy_13 <- read.csv("/Users/laura.ganley001/Documents/R_Projects/SEM/raw_data/local_wind_data/gloucester_buoy/wind_spd_gloucester_buoy_2013.csv")
gloucester_buoy_14 <- read.csv("/Users/laura.ganley001/Documents/R_Projects/SEM/raw_data/local_wind_data/gloucester_buoy/wind_spd_gloucester_buoy_2014.csv")
gloucester_buoy_15 <- read.csv("/Users/laura.ganley001/Documents/R_Projects/SEM/raw_data/local_wind_data/gloucester_buoy/wind_spd_gloucester_buoy_2015.csv")
gloucester_buoy_16 <- read.csv("/Users/laura.ganley001/Documents/R_Projects/SEM/raw_data/local_wind_data/gloucester_buoy/wind_spd_gloucester_buoy_2016.csv")
gloucester_buoy_17 <- read.csv("/Users/laura.ganley001/Documents/R_Projects/SEM/raw_data/local_wind_data/gloucester_buoy/wind_spd_gloucester_buoy_2017.csv")
gloucester_wind_direc <- read.csv("/Users/laura.ganley001/Documents/R_Projects/SEM/raw_data/local_wind_data/gloucester_buoy/wind_direction_gloucester_buoy.csv")
hourly_gloucester_wind_direc <- read.csv("/Users/laura.ganley001/Documents/R_Projects/SEM/raw_data/local_wind_data/gloucester_buoy/hourly_avg_gloucester_buoy.csv", header = TRUE)

ptwn_airprt_feb01 <- read.csv("/Users/laura.ganley001/Documents/R_Projects/SEM/raw_data/local_wind_data/ptwn_airport/wind_spd_Ptwn_airport_feb_2001.csv", header = TRUE)
ptwn_airprt_april01 <- read.csv("/Users/laura.ganley001/Documents/R_Projects/SEM/raw_data/local_wind_data/ptwn_airport/wind_spd_Ptwn_airprt_april_2001.csv", header = TRUE)
ptwn_airprt_march01 <- read.csv("/Users/laura.ganley001/Documents/R_Projects/SEM/raw_data/local_wind_data/ptwn_airport/wind_spd_Ptwn_airprt_march_2001.csv", header = TRUE)
ptwn_airprt_jan12 <- read.csv("/Users/laura.ganley001/Documents/R_Projects/SEM/raw_data/local_wind_data/ptwn_airport/wind_spd_Ptwn_airport_jan_2012.csv", header = TRUE)
ptwn_airprt_feb12 <- read.csv("/Users/laura.ganley001/Documents/R_Projects/SEM/raw_data/local_wind_data/ptwn_airport/wind_spd_Ptwn_airport_feb_2012.csv", header = TRUE)
ptwn_airprt_march12 <- read.csv("/Users/laura.ganley001/Documents/R_Projects/SEM/raw_data/local_wind_data/ptwn_airport/wind_spd_Ptwn_airport_march_2012.csv", header = TRUE)
ptwn_airprt_april12 <- read.csv("/Users/laura.ganley001/Documents/R_Projects/SEM/raw_data/local_wind_data/ptwn_airport/wind_spd_Ptwn_airport_april_2012.csv", header = TRUE)
ptwn_airprt_jan05 <- read.csv("/Users/laura.ganley001/Documents/R_Projects/SEM/raw_data/local_wind_data/ptwn_airport/wind_spd_Ptwn_airport_jan_2005.csv", header = TRUE)
ptwn_airprt_feb05 <- read.csv("/Users/laura.ganley001/Documents/R_Projects/SEM/raw_data/local_wind_data/ptwn_airport/wind_spd_Ptwn_airport_feb_2005.csv", header = TRUE)
ptwn_airprt_march05 <- read.csv("/Users/laura.ganley001/Documents/R_Projects/SEM/raw_data/local_wind_data/ptwn_airport/wind_spd_Ptwn_airport_march_2005.csv", header = TRUE)
ptwn_airprt_may05 <- read.csv("/Users/laura.ganley001/Documents/R_Projects/SEM/raw_data/local_wind_data/ptwn_airport/wind_spd_Ptwn_airport_may_2005.csv", header = TRUE)
ptwn_airprt_jan06 <- read.csv("/Users/laura.ganley001/Documents/R_Projects/SEM/raw_data/local_wind_data/ptwn_airport/wind_spd_Ptwn_airport_jan_2006.csv", header = TRUE)
ptwn_airprt_feb06 <- read.csv("/Users/laura.ganley001/Documents/R_Projects/SEM/raw_data/local_wind_data/ptwn_airport/wind_spd_Ptwn_airport_feb_2006.csv", header = TRUE)
ptwn_airprt_march06 <- read.csv("/Users/laura.ganley001/Documents/R_Projects/SEM/raw_data/local_wind_data/ptwn_airport/wind_spd_Ptwn_airport_march_2006.csv", header = TRUE)
ptwn_airprt_april06 <- read.csv("/Users/laura.ganley001/Documents/R_Projects/SEM/raw_data/local_wind_data/ptwn_airport/wind_spd_Ptwn_airport_april_2006.csv", header = TRUE)
ptwn_airprt_may06 <- read.csv("/Users/laura.ganley001/Documents/R_Projects/SEM/raw_data/local_wind_data/ptwn_airport/wind_spd_Ptwn_airport_may_2006.csv", header = TRUE)
ptwn_airprt_jan07 <- read.csv("/Users/laura.ganley001/Documents/R_Projects/SEM/raw_data/local_wind_data/ptwn_airport/wind_spd_Ptwn_airport_jan_2007.csv", header = TRUE)
ptwn_airprt_feb07 <- read.csv("/Users/laura.ganley001/Documents/R_Projects/SEM/raw_data/local_wind_data/ptwn_airport/wind_spd_Ptwn_airport_feb_2007.csv", header = TRUE)
ptwn_airprt_march07 <- read.csv("/Users/laura.ganley001/Documents/R_Projects/SEM/raw_data/local_wind_data/ptwn_airport/wind_spd_Ptwn_airport_march_2007.csv", header = TRUE)
ptwn_airprt_april07 <- read.csv("/Users/laura.ganley001/Documents/R_Projects/SEM/raw_data/local_wind_data/ptwn_airport/wind_spd_Ptwn_airport_april_2007.csv", header = TRUE)


## change colnames for boston buoy data to keep consistent between years
names(boston_buoy_05) <- c("YYYY", "MM", "DD", "hh", "mm", "DIR", "SPD", "GDR", "GSP", "GMN")
names(boston_buoy_06) <- c("YYYY", "MM", "DD", "hh", "mm", "DIR", "SPD", "GDR", "GSP", "GMN")
names(boston_buoy_07) <- c("YYYY", "MM", "DD", "hh", "mm", "DIR", "SPD", "GDR", "GSP", "GMN")
names(boston_buoy_08) <- c("YYYY", "MM", "DD", "hh", "mm", "DIR", "SPD", "GDR", "GSP", "GMN")
names(boston_buoy_09) <- c("YYYY", "MM", "DD", "hh", "mm", "DIR", "SPD", "GDR", "GSP", "GMN")
names(boston_buoy_10) <- c("YYYY", "MM", "DD", "hh", "mm", "DIR", "SPD", "GDR", "GSP", "GMN")
names(boston_buoy_11) <- c("YYYY", "MM", "DD", "hh", "mm", "DIR", "SPD", "GDR", "GSP", "GMN")
names(boston_buoy_13) <- c("YYYY", "MM", "DD", "hh", "mm", "DIR", "SPD", "GDR", "GSP", "GMN")
names(boston_buoy_14) <- c("YYYY", "MM", "DD", "hh", "mm", "DIR", "SPD", "GDR", "GSP", "GMN")
names(boston_buoy_15) <- c("YYYY", "MM", "DD", "hh", "mm", "DIR", "SPD", "GDR", "GSP", "GMN")
names(boston_buoy_16) <- c("YYYY", "MM", "DD", "hh", "mm", "DIR", "SPD", "GDR", "GSP", "GMN")
names(boston_buoy_april17) <- c("YYYY", "MM", "DD", "hh", "mm", "DIR", "SPD", "GDR", "GSP", "GMN")
names(boston_buoy_march17) <- c("YYYY", "MM", "DD", "hh", "mm", "DIR", "SPD", "GDR", "GSP", "GMN")
names(boston_buoy_may17) <- c("YYYY", "MM", "DD", "hh", "mm", "DIR", "SPD", "GDR", "GSP", "GMN")
names(boston_buoy_jan17) <- c("YYYY", "MM", "DD", "hh", "mm", "DIR", "SPD", "GDR", "GSP", "GMN")
names(boston_buoy_feb17) <- c("YYYY", "MM", "DD", "hh", "mm", "DIR", "SPD", "GDR", "GSP", "GMN")


##bind together all of the hourly boston data 
## 1999 was not hourly. 
boston_buoy <- rbind(boston_buoy_98, boston_buoy_99, 
                     boston_buoy_00, boston_buoy_01, 
                     boston_buoy_02, boston_buoy_03, boston_buoy_04,
                     boston_buoy_05, boston_buoy_06, boston_buoy_07,
                     boston_buoy_08, boston_buoy_09, boston_buoy_10,
                     boston_buoy_11, 
                     boston_buoy_13, boston_buoy_14,
                     boston_buoy_15, boston_buoy_16, boston_buoy_jan17,
                     boston_buoy_feb17, boston_buoy_march17,
                     boston_buoy_april17, boston_buoy_may17)

## to get the average wind direction you need to calculate u and v components 
## i got most of htis code from Grange 2014 (averaging wind speed and direction)
boston_buoy$u.wind <- - boston_buoy$SPD * sin(2 * pi * boston_buoy$DIR/360)
boston_buoy$v.wind <- - boston_buoy$SPD * cos(2 * pi * boston_buoy$DIR/360)

## group by year and month to calculate the average vectors for each month
boston_buoy_direc_group <- as.data.frame(boston_buoy %>%
                                   dplyr::group_by(YYYY, MM) %>%
                                  dplyr::summarise(mean.u = mean(u.wind))) # calc u vector

boston_buoy_direc_group_v <- as.data.frame(boston_buoy %>%
                                            dplyr::group_by(YYYY, MM) %>%
                                             dplyr::summarise(mean.v = mean(v.wind))) #calc v vector


# Calculate the scalar average wind speed, the standard mean for boston data and put it in 
## the column ws.scalar.average
boston_buoy_scalar_avg_spd <- as.data.frame(boston_buoy %>%
                                             dplyr::group_by(YYYY, MM) %>%
                                              dplyr::summarise(ws.scalar.average.boston = mean(SPD)))


## combine data frame back together with monthly average u and v vectors
boston_buoy_vec <- dplyr::full_join(boston_buoy_direc_group, boston_buoy_direc_group_v)

## Calculate the resultant vector average wind direction with atan2.  This will give 
## us the average wind direction for each month (wd.average)
boston_buoy_vec$wd.average.boston <- (atan2(boston_buoy_vec$mean.u, boston_buoy_vec$mean.v) * 360/2/pi) + 180

ggplot(boston_buoy_vec, aes(x = MM, y = wd.average.boston)) +
  geom_point()


## Calculate the vector average wind speed for boston data and put it in the column
## ws.vector.average
boston_buoy_vec$ws.vector.average.boston <- ((boston_buoy_vec$mean.u^2 + boston_buoy_vec$mean.v^2)^0.5)

boston_wind_full <- dplyr::full_join(boston_buoy_vec, boston_buoy_scalar_avg_spd)

boston_wind_full$YYYY <- gsub("1999", "99", boston_wind_full$YYYY)
boston_wind_full$YYYY <- gsub("1998", "98", boston_wind_full$YYYY)
boston_wind_full$YYYY <- gsub("2000", "00", boston_wind_full$YYYY)
boston_wind_full$YYYY <- gsub("2001", "01", boston_wind_full$YYYY)
boston_wind_full$YYYY <- gsub("2002", "02", boston_wind_full$YYYY)
boston_wind_full$YYYY <- gsub("2003", "03", boston_wind_full$YYYY)
boston_wind_full$YYYY <- gsub("2004", "04", boston_wind_full$YYYY)
boston_wind_full$YYYY <- gsub("2005", "05", boston_wind_full$YYYY)
boston_wind_full$YYYY <- gsub("2006", "06", boston_wind_full$YYYY)
boston_wind_full$YYYY <- gsub("2007", "07", boston_wind_full$YYYY)
boston_wind_full$YYYY <- gsub("2008", "08", boston_wind_full$YYYY)
boston_wind_full$YYYY <- gsub("2009", "09", boston_wind_full$YYYY)
boston_wind_full$YYYY <- gsub("2010", "10", boston_wind_full$YYYY)
boston_wind_full$YYYY <- gsub("2011", "11", boston_wind_full$YYYY)
boston_wind_full$YYYY <- gsub("2012", "12", boston_wind_full$YYYY)
boston_wind_full$YYYY <- gsub("2013", "13", boston_wind_full$YYYY)
boston_wind_full$YYYY <- gsub("2014", "14", boston_wind_full$YYYY)
boston_wind_full$YYYY <- gsub("2015", "15", boston_wind_full$YYYY)
boston_wind_full$YYYY <- gsub("2016", "16", boston_wind_full$YYYY)
boston_wind_full$YYYY <- gsub("2017", "17", boston_wind_full$YYYY)

boston_wind_full <- filter(boston_wind_full, MM == "1" | MM == "5" |
                             MM == "2" | MM == "3"| MM == "4")


ggplot(boston_wind_full, aes(x = MM, y = wd.average.boston)) +
  geom_point() +
  facet_wrap(~YYYY)
##******************GLOUCESTER BUOY DATA**********************************************
## gloucester hourly wind direction and speed
## separate gloucester buoy TIME.UTC into year month and day 
gloucester_wind_direc_spd <- hourly_gloucester_wind_direc %>%
  separate(col = "Time.UTC", into = c("YYYY", "MM", "dd"), sep = "-") %>%
  dplyr::select(-dd) %>%
  separate(col = "MM", into=c("MM", "time"), sep = " ") %>%
  dplyr::select(-time) 

gloucester_wind_direc_spd$YYYY <- gsub("2001", "01", gloucester_wind_direc_spd$YYYY)
gloucester_wind_direc_spd$YYYY <- gsub("2002", "02", gloucester_wind_direc_spd$YYYY)
gloucester_wind_direc_spd$YYYY <- gsub("2003", "03", gloucester_wind_direc_spd$YYYY)
gloucester_wind_direc_spd$YYYY <- gsub("2004", "04", gloucester_wind_direc_spd$YYYY)
gloucester_wind_direc_spd$YYYY <- gsub("2005", "05", gloucester_wind_direc_spd$YYYY)
gloucester_wind_direc_spd$YYYY <- gsub("2006", "06", gloucester_wind_direc_spd$YYYY)
gloucester_wind_direc_spd$YYYY <- gsub("2007", "07", gloucester_wind_direc_spd$YYYY)
gloucester_wind_direc_spd$YYYY <- gsub("2008", "08", gloucester_wind_direc_spd$YYYY)
gloucester_wind_direc_spd$YYYY <- gsub("2009", "09", gloucester_wind_direc_spd$YYYY)
gloucester_wind_direc_spd$YYYY <- gsub("2010", "10", gloucester_wind_direc_spd$YYYY)
gloucester_wind_direc_spd$YYYY <- gsub("2011", "11", gloucester_wind_direc_spd$YYYY)
gloucester_wind_direc_spd$YYYY <- gsub("2012", "12", gloucester_wind_direc_spd$YYYY)
gloucester_wind_direc_spd$YYYY <- gsub("2013", "13", gloucester_wind_direc_spd$YYYY)
gloucester_wind_direc_spd$YYYY <- gsub("2014", "14", gloucester_wind_direc_spd$YYYY)
gloucester_wind_direc_spd$YYYY <- gsub("2015", "15", gloucester_wind_direc_spd$YYYY)
gloucester_wind_direc_spd$YYYY <- gsub("2016", "16", gloucester_wind_direc_spd$YYYY)
gloucester_wind_direc_spd$YYYY <- gsub("2017", "17", gloucester_wind_direc_spd$YYYY)

gloucester_wind_direc_spd$MM <- gsub("01", "1", gloucester_wind_direc_spd$MM)
gloucester_wind_direc_spd$MM <- gsub("02", "2", gloucester_wind_direc_spd$MM)
gloucester_wind_direc_spd$MM <- gsub("03", "3", gloucester_wind_direc_spd$MM)
gloucester_wind_direc_spd$MM <- gsub("04", "4", gloucester_wind_direc_spd$MM)
gloucester_wind_direc_spd$MM <- gsub("05", "5", gloucester_wind_direc_spd$MM)


gloucester_wind_direc_spd$A01.Hourly.Wind_Speed_m.s <- as.numeric(as.character(gloucester_wind_direc_spd$A01.Hourly.Wind_Speed_m.s))
gloucester_wind_direc_spd$A01.Hourly.Wind_Direction_degrees <- as.numeric(as.character(gloucester_wind_direc_spd$A01.Hourly.Wind_Direction_degrees))
## to get the average wind direction you need to calculate u and v components 
## i got most of this code from Grange 2014 (averaging wind speed and direction)
gloucester_wind_direc_spd$u.wind.gloucester <- - gloucester_wind_direc_spd$A01.Hourly.Wind_Speed_m.s * sin(2 * pi * gloucester_wind_direc_spd$A01.Hourly.Wind_Direction_degrees/360)
gloucester_wind_direc_spd$v.wind.gloucester <- - gloucester_wind_direc_spd$A01.Hourly.Wind_Speed_m.s * cos(2 * pi * gloucester_wind_direc_spd$A01.Hourly.Wind_Direction_degrees/360)

## group by year and month to calculate the average vectors for each month
gloucester_wind_direc_spd_group <- as.data.frame(gloucester_wind_direc_spd %>%
                                        dplyr::group_by(YYYY, MM) %>%
                                        dplyr::summarise(mean.u.gloucester = mean(u.wind.gloucester))) # calc u vector

gloucester_wind_direc_spd_group_v <- as.data.frame(gloucester_wind_direc_spd %>%
                                          dplyr::group_by(YYYY, MM) %>%
                                            dplyr::summarise(mean.v.gloucester = mean(v.wind.gloucester))) #calc v vector


# Calculate the scalar average wind speed, the standard mean for gom data and put it in 
## the column ws.scalar.average
gloucester_scalar_avg_spd <- as.data.frame(gloucester_wind_direc_spd %>%
                                             dplyr::group_by(YYYY, MM) %>%
                                             dplyr::summarise(ws.scalar.average.gloucester = mean(A01.Hourly.Wind_Speed_m.s)))


## combine data frame back together with monthly average u and v vectors
gloucester_buoy_vec <- dplyr::full_join(gloucester_wind_direc_spd_group, gloucester_wind_direc_spd_group_v)

## Calculate the resultant vector average wind direction with atan2.  This will give 
## us the average wind direction for each month (wd.average)
gloucester_buoy_vec$wd.average.gloucester <- (atan2(gloucester_buoy_vec$mean.u.gloucester, gloucester_buoy_vec$mean.v.gloucester) * 360/2/pi) + 180

ggplot(gloucester_buoy_vec, aes(x = MM, y = wd.average.gloucester)) +
  geom_point() +
  facet_wrap(~YYYY)


## Calculate the vector average wind speed for gloucester data and put it in the column
## ws.vector.average
gloucester_buoy_vec$ws.vector.average.gloucester <- ((gloucester_buoy_vec$mean.u.gloucester^2 + gloucester_buoy_vec$mean.v.gloucester^2)^0.5)

gloucester_wind_full <- dplyr::full_join(gloucester_buoy_vec, gloucester_scalar_avg_spd)

## remove last row
gloucester_wind_full <- gloucester_wind_full[1:85, ]

## bind boston and gloucester data
boston_wind_full$MM <- as.character(boston_wind_full$MM)
boston_gloucester <- dplyr::full_join(gloucester_wind_full, boston_wind_full)

## determine if the two wind direction datasets are correlated
wind_direc_local_lm <- lm(wd.average.gloucester ~ wd.average.boston, 
                             boston_gloucester)
plot(wind_direc_local_lm)
summary(wind_direc_local_lm)
##Determine if the slope is really different then 1
confint(wind_direc_local_lm)
## the confidence intervals include 1 (2.5% = .61, 97.5% = 1.03)
## so our slope is not different than 1 and therefore we can assume
## a 1 to 1 relationship between wind direction at the GLoucester buoy and
## wind direction at the Boston buoy

## pull out coefs to put on ggplot
a <- signif(coef(wind_direc_local_lm)[1], digits = 2)
b <- signif(coef(wind_direc_local_lm)[2], digits = 2)
c <- signif(summary(wind_direc_local_lm)$adj.r.squared, digits = 4)
textlab <- paste("y = ",b,"x + ",a, sep="")
textlab2 <- paste("adj. r^2 = ",c, sep = "")

local_wind_direc <- ggplot(boston_gloucester, 
                           aes(y = wd.average.gloucester, x = wd.average.boston)) +
  geom_point() +
  stat_smooth(method = "lm") +
  annotate("text", x = 245, y = 30, label = textlab, color="black", 
           size = 5, parse=FALSE) +
  annotate("text", x = 280, y = 13, label = textlab2, color = "black",
           size = 5, parse = FALSE) +
  labs(x = "Boston wind direction", y = "Gloucester wind direction")



wind_spd_local_lm <- lm(ws.scalar.average.gloucester ~ ws.scalar.average.boston,
                        boston_gloucester)
plot(wind_spd_local_lm)
summary(wind_spd_local_lm)
## Determine if the slope is really different then 1
confint(wind_spd_local_lm)
## the confidence intervals include 1 (2.5% = .86, 97.5% = .98)
## so our slope is different than 1 and therefore we can't assume
## a 1 to 1 relationship between wind spd at the Gloucester buoy and wind speed
## at the boston buoy


## pull out coefs to put on ggplot
a <- signif(coef(wind_spd_local_lm)[1], digits = 2)
b <- signif(coef(wind_spd_local_lm)[2], digits = 2)
c <- signif(summary(wind_spd_local_lm)$adj.r.squared, digits = 4)
textlab <- paste("y = ",b,"x + ",a, sep="")
textlab2 <- paste("adj. r^2 = ",c, sep = "")

local_wind_spd <- ggplot(boston_gloucester,
                            aes(y = ws.scalar.average.gloucester, 
                                x = ws.scalar.average.boston)) +
  geom_point() +
  stat_smooth(method = "lm") +
  annotate("text", x = 5, y = 7, label = textlab, color="black", 
           size = 5, parse=FALSE) +
  annotate("text", x = 5, y = 6.7, label = textlab2, color = "black",
           size = 5, parse = FALSE) +
  labs(x = "Boston scalar average wind speed", 
       y = "Gloucester scalar average wind speed") 



local_wind_spd_vec <- ggplot(boston_gloucester,
                                aes(y=ws.vector.average.boston, x = ws.vector.average.gloucester)) +
  geom_point() +
  stat_smooth(method = "lm") +
  labs(x = "Gloucester vector average spd", y = "Boston vector average spd")


##wind_spd_local_vector_avg_lm <- lm( ws.vector.average.gloucester ~ ws.vector.average.boston,
                               #       boston_gloucester)
#plot(wind_spd_local_vector_avg_lm)
#summary(wind_spd_local_vector_avg_lm)

## Determine if the slope is really different then 1
#confint(wind_spd_local_vector_avg_lm)
## the confidence intervals include 1 (2.5% = .89, 97.5% = 1.91)
## so our slope is not different than 1 and therefore we can assume
## a 1 to 1 relationship between wind spd at the Gloucester buoy and wind speed
## at the boston buoy



##**************************Provincetown airport wind data*******************
##combine february, march and april 2001 ptown airport data
colnames(ptwn_airprt_april01) <-c("day", "temphigh", "tempavg", "templow", "dewpointhigh", 
                                  "dewpointavg", "dewpointlow", "humidityhigh", "humidityavg",
                                  "humiditylow", "sealevelpressurehigh", "sealevelpressureavg",
                                  "sealevelpressurelow", "visbilityhigh","visibilityavg", 
                                  "visibilitylow", "wind_high_mph", "wind_avg_mph", 
                                  "wind_highmph", "precip_sum", "events", "wind_direction")

colnames(ptwn_airprt_feb01) <-c("day", "temphigh", "tempavg", "templow", "dewpointhigh", 
                                  "dewpointavg", "dewpointlow", "humidityhigh", "humidityavg",
                                  "humiditylow", "sealevelpressurehigh", "sealevelpressureavg",
                                  "sealevelpressurelow", "visbilityhigh","visibilityavg", 
                                  "visibilitylow", "wind_high_mph", "wind_avg_mph", 
                                  "wind_highmph", "precip_sum", "events", "wind_direction")

colnames(ptwn_airprt_march01) <-c("day", "temphigh", "tempavg", "templow", "dewpointhigh", 
                                  "dewpointavg", "dewpointlow", "humidityhigh", "humidityavg",
                                  "humiditylow", "sealevelpressurehigh", "sealevelpressureavg",
                                  "sealevelpressurelow", "visbilityhigh","visibilityavg", 
                                  "visibilitylow", "wind_high_mph", "wind_avg_mph", 
                                  "wind_highmph", "precip_sum", "events", "wind_direction")

colnames(ptwn_airprt_jan12) <-c("day", "temphigh", "tempavg", "templow", "dewpointhigh", 
                                  "dewpointavg", "dewpointlow", "humidityhigh", "humidityavg",
                                  "humiditylow", "sealevelpressurehigh", "sealevelpressureavg",
                                  "sealevelpressurelow", "visbilityhigh","visibilityavg", 
                                  "visibilitylow", "wind_high_mph", "wind_avg_mph", 
                                  "wind_highmph", "precip_sum", "events", "wind_direction")

colnames(ptwn_airprt_feb12) <-c("day", "temphigh", "tempavg", "templow", "dewpointhigh", 
                                  "dewpointavg", "dewpointlow", "humidityhigh", "humidityavg",
                                  "humiditylow", "sealevelpressurehigh", "sealevelpressureavg",
                                  "sealevelpressurelow", "visbilityhigh","visibilityavg", 
                                  "visibilitylow", "wind_high_mph", "wind_avg_mph", 
                                  "wind_highmph", "precip_sum", "events", "wind_direction")

colnames(ptwn_airprt_march12) <-c("day", "temphigh", "tempavg", "templow", "dewpointhigh", 
                                  "dewpointavg", "dewpointlow", "humidityhigh", "humidityavg",
                                  "humiditylow", "sealevelpressurehigh", "sealevelpressureavg",
                                  "sealevelpressurelow", "visbilityhigh","visibilityavg", 
                                  "visibilitylow", "wind_high_mph", "wind_avg_mph", 
                                  "wind_highmph", "precip_sum", "events", "wind_direction")


colnames(ptwn_airprt_april12) <-c("day", "temphigh", "tempavg", "templow", "dewpointhigh", 
                                  "dewpointavg", "dewpointlow", "humidityhigh", "humidityavg",
                                  "humiditylow", "sealevelpressurehigh", "sealevelpressureavg",
                                  "sealevelpressurelow", "visbilityhigh","visibilityavg", 
                                  "visibilitylow", "wind_high_mph", "wind_avg_mph", 
                                  "wind_highmph", "precip_sum", "events", "wind_direction")

colnames(ptwn_airprt_jan05) <-c("day", "temphigh", "tempavg", "templow", "dewpointhigh", 
                                  "dewpointavg", "dewpointlow", "humidityhigh", "humidityavg",
                                  "humiditylow", "sealevelpressurehigh", "sealevelpressureavg",
                                  "sealevelpressurelow", "visbilityhigh","visibilityavg", 
                                  "visibilitylow", "wind_high_mph", "wind_avg_mph", 
                                  "wind_highmph", "precip_sum", "events", "wind_direction")

colnames(ptwn_airprt_feb05) <-c("day", "temphigh", "tempavg", "templow", "dewpointhigh", 
                                  "dewpointavg", "dewpointlow", "humidityhigh", "humidityavg",
                                  "humiditylow", "sealevelpressurehigh", "sealevelpressureavg",
                                  "sealevelpressurelow", "visbilityhigh","visibilityavg", 
                                  "visibilitylow", "wind_high_mph", "wind_avg_mph", 
                                  "wind_highmph", "precip_sum", "events", "wind_direction")

colnames(ptwn_airprt_march05) <-c("day", "temphigh", "tempavg", "templow", "dewpointhigh", 
                                  "dewpointavg", "dewpointlow", "humidityhigh", "humidityavg",
                                  "humiditylow", "sealevelpressurehigh", "sealevelpressureavg",
                                  "sealevelpressurelow", "visbilityhigh","visibilityavg", 
                                  "visibilitylow", "wind_high_mph", "wind_avg_mph", 
                                  "wind_highmph", "precip_sum", "events", "wind_direction")

colnames(ptwn_airprt_may05) <-c("day", "temphigh", "tempavg", "templow", "dewpointhigh", 
                                  "dewpointavg", "dewpointlow", "humidityhigh", "humidityavg",
                                  "humiditylow", "sealevelpressurehigh", "sealevelpressureavg",
                                  "sealevelpressurelow", "visbilityhigh","visibilityavg", 
                                  "visibilitylow", "wind_high_mph", "wind_avg_mph", 
                                  "wind_highmph", "precip_sum", "events", "wind_direction")

colnames(ptwn_airprt_jan06) <-c("day", "temphigh", "tempavg", "templow", "dewpointhigh", 
                                  "dewpointavg", "dewpointlow", "humidityhigh", "humidityavg",
                                  "humiditylow", "sealevelpressurehigh", "sealevelpressureavg",
                                  "sealevelpressurelow", "visbilityhigh","visibilityavg", 
                                  "visibilitylow", "wind_high_mph", "wind_avg_mph", 
                                  "wind_highmph", "precip_sum", "events", "wind_direction")

colnames(ptwn_airprt_feb06) <-c("day", "temphigh", "tempavg", "templow", "dewpointhigh", 
                                  "dewpointavg", "dewpointlow", "humidityhigh", "humidityavg",
                                  "humiditylow", "sealevelpressurehigh", "sealevelpressureavg",
                                  "sealevelpressurelow", "visbilityhigh","visibilityavg", 
                                  "visibilitylow", "wind_high_mph", "wind_avg_mph", 
                                  "wind_highmph", "precip_sum", "events", "wind_direction")

colnames(ptwn_airprt_march06) <-c("day", "temphigh", "tempavg", "templow", "dewpointhigh", 
                                  "dewpointavg", "dewpointlow", "humidityhigh", "humidityavg",
                                  "humiditylow", "sealevelpressurehigh", "sealevelpressureavg",
                                  "sealevelpressurelow", "visbilityhigh","visibilityavg", 
                                  "visibilitylow", "wind_high_mph", "wind_avg_mph", 
                                  "wind_highmph", "precip_sum", "events", "wind_direction")

colnames(ptwn_airprt_april06) <-c("day", "temphigh", "tempavg", "templow", "dewpointhigh", 
                                  "dewpointavg", "dewpointlow", "humidityhigh", "humidityavg",
                                  "humiditylow", "sealevelpressurehigh", "sealevelpressureavg",
                                  "sealevelpressurelow", "visbilityhigh","visibilityavg", 
                                  "visibilitylow", "wind_high_mph", "wind_avg_mph", 
                                  "wind_highmph", "precip_sum", "events", "wind_direction")

colnames(ptwn_airprt_may06) <-c("day", "temphigh", "tempavg", "templow", "dewpointhigh", 
                                  "dewpointavg", "dewpointlow", "humidityhigh", "humidityavg",
                                  "humiditylow", "sealevelpressurehigh", "sealevelpressureavg",
                                  "sealevelpressurelow", "visbilityhigh","visibilityavg", 
                                  "visibilitylow", "wind_high_mph", "wind_avg_mph", 
                                  "wind_highmph", "precip_sum", "events", "wind_direction")

colnames(ptwn_airprt_jan07) <-c("day", "temphigh", "tempavg", "templow", "dewpointhigh", 
                                  "dewpointavg", "dewpointlow", "humidityhigh", "humidityavg",
                                  "humiditylow", "sealevelpressurehigh", "sealevelpressureavg",
                                  "sealevelpressurelow", "visbilityhigh","visibilityavg", 
                                  "visibilitylow", "wind_high_mph", "wind_avg_mph", 
                                  "wind_highmph", "precip_sum", "events", "wind_direction")

colnames(ptwn_airprt_feb07) <-c("day", "temphigh", "tempavg", "templow", "dewpointhigh", 
                                  "dewpointavg", "dewpointlow", "humidityhigh", "humidityavg",
                                  "humiditylow", "sealevelpressurehigh", "sealevelpressureavg",
                                  "sealevelpressurelow", "visbilityhigh","visibilityavg", 
                                  "visibilitylow", "wind_high_mph", "wind_avg_mph", 
                                  "wind_highmph", "precip_sum", "events", "wind_direction")

colnames(ptwn_airprt_march07) <-c("day", "temphigh", "tempavg", "templow", "dewpointhigh", 
                                  "dewpointavg", "dewpointlow", "humidityhigh", "humidityavg",
                                  "humiditylow", "sealevelpressurehigh", "sealevelpressureavg",
                                  "sealevelpressurelow", "visbilityhigh","visibilityavg", 
                                  "visibilitylow", "wind_high_mph", "wind_avg_mph", 
                                  "wind_highmph", "precip_sum", "events", "wind_direction")

colnames(ptwn_airprt_april07) <-c("day", "temphigh", "tempavg", "templow", "dewpointhigh", 
                                  "dewpointavg", "dewpointlow", "humidityhigh", "humidityavg",
                                  "humiditylow", "sealevelpressurehigh", "sealevelpressureavg",
                                  "sealevelpressurelow", "visbilityhigh","visibilityavg", 
                                  "visibilitylow", "wind_high_mph", "wind_avg_mph", 
                                  "wind_highmph", "precip_sum", "events", "wind_direction")

## remove first row of crap
ptwn_airprt_april01 <- ptwn_airprt_april01[2:31, ]
ptwn_airprt_march01 <- ptwn_airprt_march01[2:32, ]
ptwn_airprt_feb01 <- ptwn_airprt_feb01[2:29, ]
ptwn_airprt_jan12 <- ptwn_airprt_jan12[2:32, ]
ptwn_airprt_feb12 <- ptwn_airprt_feb12[2:30, ]
ptwn_airprt_march12 <- ptwn_airprt_march12[2:32, ]
ptwn_airprt_april12<- ptwn_airprt_april12[2:31, ]
ptwn_airprt_jan05 <- ptwn_airprt_jan05[2:32, ]
ptwn_airprt_feb05 <- ptwn_airprt_feb05[2:32, ]
ptwn_airprt_march05 <- ptwn_airprt_march05[2:32, ]
ptwn_airprt_may05 <- ptwn_airprt_may05[2:32, ]
ptwn_airprt_jan06 <- ptwn_airprt_jan06[2:32, ]
ptwn_airprt_feb06 <- ptwn_airprt_feb06[2:29, ]
ptwn_airprt_march06 <- ptwn_airprt_march06[2:32, ]
ptwn_airprt_april06 <- ptwn_airprt_april06[2:31, ]
ptwn_airprt_may06 <- ptwn_airprt_may06[2:32, ]
ptwn_airprt_jan07 <- ptwn_airprt_jan07[2:32, ]
ptwn_airprt_feb07 <- ptwn_airprt_feb07[2:29, ]
ptwn_airprt_march07 <- ptwn_airprt_march07[2:32, ]
ptwn_airprt_april07 <- ptwn_airprt_april07[2:31, ]


## remove columns i don't need
ptwn_aprt_april01select <- ptwn_airprt_april01 %>%
  dplyr::select(-temphigh, -tempavg, -templow,- dewpointhigh, -dewpointavg,- dewpointlow,- humidityhigh,
                -humidityavg,- humiditylow, -sealevelpressurehigh, -sealevelpressureavg, -sealevelpressurelow,
                -visbilityhigh, -visibilityavg, -visibilitylow, -wind_high_mph,- wind_highmph,
               -precip_sum, -events)
MM <- rep("4", length = 30)
ptwn_aprt_april01month <- cbind(MM, ptwn_aprt_april01select)

ptwn_aprt_feb01select <- ptwn_airprt_feb01 %>%
  dplyr::select(-temphigh, -tempavg, -templow,- dewpointhigh, -dewpointavg,- dewpointlow,- humidityhigh,
                -humidityavg,- humiditylow, -sealevelpressurehigh, -sealevelpressureavg, -sealevelpressurelow,
                -visbilityhigh, -visibilityavg, -visibilitylow, -wind_high_mph,- wind_highmph,
                -precip_sum, -events)
MM <- rep("2", length = 28)
ptwn_aprt_feb01month <- cbind(MM, ptwn_aprt_feb01select)

ptwn_aprt_march01select <- ptwn_airprt_march01 %>%
  dplyr::select(-temphigh, -tempavg, -templow,- dewpointhigh, -dewpointavg,- dewpointlow,- humidityhigh,
                -humidityavg,- humiditylow, -sealevelpressurehigh, -sealevelpressureavg, -sealevelpressurelow,
                -visbilityhigh, -visibilityavg, -visibilitylow, -wind_high_mph,- wind_highmph,
                -precip_sum, -events)
MM <- rep("3", length = 31)
ptwn_aprt_march01month <- cbind(MM, ptwn_aprt_march01select)

ptwn_aprt_jan12select <- ptwn_airprt_jan12 %>%
  dplyr::select(-temphigh, -tempavg, -templow,- dewpointhigh, -dewpointavg,- dewpointlow,- humidityhigh,
                -humidityavg,- humiditylow, -sealevelpressurehigh, -sealevelpressureavg, -sealevelpressurelow,
                -visbilityhigh, -visibilityavg, -visibilitylow, -wind_high_mph,- wind_highmph,
                -precip_sum, -events)
MM <- rep("1", length = 31)
ptwn_aprt_jan12month <- cbind(MM, ptwn_aprt_jan12select)

ptwn_aprt_feb12select <- ptwn_airprt_feb12 %>%
  dplyr::select(-temphigh, -tempavg, -templow,- dewpointhigh, -dewpointavg,- dewpointlow,- humidityhigh,
                -humidityavg,- humiditylow, -sealevelpressurehigh, -sealevelpressureavg, -sealevelpressurelow,
                -visbilityhigh, -visibilityavg, -visibilitylow, -wind_high_mph,- wind_highmph,
                -precip_sum, -events)
MM <- rep("2", length = 29)
ptwn_aprt_feb12month <- cbind(MM, ptwn_aprt_feb12select)

ptwn_aprt_march12select <- ptwn_airprt_march12 %>%
  dplyr::select(-temphigh, -tempavg, -templow,- dewpointhigh, -dewpointavg,- dewpointlow,- humidityhigh,
                -humidityavg,- humiditylow, -sealevelpressurehigh, -sealevelpressureavg, -sealevelpressurelow,
                -visbilityhigh, -visibilityavg, -visibilitylow, -wind_high_mph,- wind_highmph,
                -precip_sum, -events)
MM <- rep("3", length = 31)
ptwn_aprt_march12month <- cbind(MM, ptwn_aprt_march12select)

ptwn_aprt_april12select <- ptwn_airprt_april12 %>%
  dplyr::select(-temphigh, -tempavg, -templow,- dewpointhigh, -dewpointavg,- dewpointlow,- humidityhigh,
                -humidityavg,- humiditylow, -sealevelpressurehigh, -sealevelpressureavg, -sealevelpressurelow,
                -visbilityhigh, -visibilityavg, -visibilitylow, -wind_high_mph,- wind_highmph,
                -precip_sum, -events)
MM <- rep("4", length = 30)
ptwn_aprt_april12month <- cbind(MM, ptwn_aprt_april12select)

ptwn_aprt_jan05select <- ptwn_airprt_jan05 %>%
  dplyr::select(-temphigh, -tempavg, -templow,- dewpointhigh, -dewpointavg,- dewpointlow,- humidityhigh,
                -humidityavg,- humiditylow, -sealevelpressurehigh, -sealevelpressureavg, -sealevelpressurelow,
                -visbilityhigh, -visibilityavg, -visibilitylow, -wind_high_mph,- wind_highmph,
                -precip_sum, -events)
MM <- rep("1", length = 31)
ptwn_aprt_jan05month <- cbind(MM, ptwn_aprt_jan05select)

ptwn_aprt_feb05select <- ptwn_airprt_feb05 %>%
  dplyr::select(-temphigh, -tempavg, -templow,- dewpointhigh, -dewpointavg,- dewpointlow,- humidityhigh,
                -humidityavg,- humiditylow, -sealevelpressurehigh, -sealevelpressureavg, -sealevelpressurelow,
                -visbilityhigh, -visibilityavg, -visibilitylow, -wind_high_mph,- wind_highmph,
                -precip_sum, -events)
MM <- rep("2", length = 31)
ptwn_aprt_feb05month <- cbind(MM, ptwn_aprt_feb05select)

ptwn_aprt_march05select <- ptwn_airprt_march05 %>%
  dplyr::select(-temphigh, -tempavg, -templow,- dewpointhigh, -dewpointavg,- dewpointlow,- humidityhigh,
                -humidityavg,- humiditylow, -sealevelpressurehigh, -sealevelpressureavg, -sealevelpressurelow,
                -visbilityhigh, -visibilityavg, -visibilitylow, -wind_high_mph,- wind_highmph,
                -precip_sum, -events)
MM <- rep("3", length = 31)
ptwn_aprt_march05month <- cbind(MM, ptwn_aprt_march05select)

ptwn_aprt_may05select <- ptwn_airprt_may05 %>%
  dplyr::select(-temphigh, -tempavg, -templow,- dewpointhigh, -dewpointavg,- dewpointlow,- humidityhigh,
                -humidityavg,- humiditylow, -sealevelpressurehigh, -sealevelpressureavg, -sealevelpressurelow,
                -visbilityhigh, -visibilityavg, -visibilitylow, -wind_high_mph,- wind_highmph,
                -precip_sum, -events)
MM <- rep("5", length = 31)
ptwn_aprt_may05month <- cbind(MM, ptwn_aprt_may05select)

ptwn_aprt_jan06select <- ptwn_airprt_jan06 %>%
  dplyr::select(-temphigh, -tempavg, -templow,- dewpointhigh, -dewpointavg,- dewpointlow,- humidityhigh,
                -humidityavg,- humiditylow, -sealevelpressurehigh, -sealevelpressureavg, -sealevelpressurelow,
                -visbilityhigh, -visibilityavg, -visibilitylow, -wind_high_mph,- wind_highmph,
                -precip_sum, -events)
MM <- rep("1", length = 31)
ptwn_aprt_jan06month <- cbind(MM, ptwn_aprt_jan06select)

ptwn_aprt_feb06select <- ptwn_airprt_feb06 %>%
  dplyr::select(-temphigh, -tempavg, -templow,- dewpointhigh, -dewpointavg,- dewpointlow,- humidityhigh,
                -humidityavg,- humiditylow, -sealevelpressurehigh, -sealevelpressureavg, -sealevelpressurelow,
                -visbilityhigh, -visibilityavg, -visibilitylow, -wind_high_mph,- wind_highmph,
                -precip_sum, -events)
MM <- rep("2", length = 28)
ptwn_aprt_feb06month <- cbind(MM, ptwn_aprt_feb06select)

ptwn_aprt_march06select <- ptwn_airprt_march06 %>%
  dplyr::select(-temphigh, -tempavg, -templow,- dewpointhigh, -dewpointavg,- dewpointlow,- humidityhigh,
                -humidityavg,- humiditylow, -sealevelpressurehigh, -sealevelpressureavg, -sealevelpressurelow,
                -visbilityhigh, -visibilityavg, -visibilitylow, -wind_high_mph,- wind_highmph,
                -precip_sum, -events)
MM <- rep("3", length = 31)
ptwn_aprt_march06month <- cbind(MM, ptwn_aprt_march06select)

ptwn_aprt_april06select <- ptwn_airprt_april06 %>%
  dplyr::select(-temphigh, -tempavg, -templow,- dewpointhigh, -dewpointavg,- dewpointlow,- humidityhigh,
                -humidityavg,- humiditylow, -sealevelpressurehigh, -sealevelpressureavg, -sealevelpressurelow,
                -visbilityhigh, -visibilityavg, -visibilitylow, -wind_high_mph,- wind_highmph,
                -precip_sum, -events)
MM <- rep("4", length = 30)
ptwn_aprt_april06month <- cbind(MM, ptwn_aprt_april06select)

ptwn_aprt_may06select <- ptwn_airprt_may06 %>%
  dplyr::select(-temphigh, -tempavg, -templow,- dewpointhigh, -dewpointavg,- dewpointlow,- humidityhigh,
                -humidityavg,- humiditylow, -sealevelpressurehigh, -sealevelpressureavg, -sealevelpressurelow,
                -visbilityhigh, -visibilityavg, -visibilitylow, -wind_high_mph,- wind_highmph,
                -precip_sum, -events)
MM <- rep("5", length = 31)
ptwn_aprt_may06month <- cbind(MM, ptwn_aprt_may06select)

ptwn_aprt_jan07select <- ptwn_airprt_jan07 %>%
  dplyr::select(-temphigh, -tempavg, -templow,- dewpointhigh, -dewpointavg,- dewpointlow,- humidityhigh,
                -humidityavg,- humiditylow, -sealevelpressurehigh, -sealevelpressureavg, -sealevelpressurelow,
                -visbilityhigh, -visibilityavg, -visibilitylow, -wind_high_mph,- wind_highmph,
                -precip_sum, -events)
MM <- rep("1", length = 31)
ptwn_aprt_jan07month <- cbind(MM, ptwn_aprt_jan07select)

ptwn_aprt_feb07select <- ptwn_airprt_feb07 %>%
  dplyr::select(-temphigh, -tempavg, -templow,- dewpointhigh, -dewpointavg,- dewpointlow,- humidityhigh,
                -humidityavg,- humiditylow, -sealevelpressurehigh, -sealevelpressureavg, -sealevelpressurelow,
                -visbilityhigh, -visibilityavg, -visibilitylow, -wind_high_mph,- wind_highmph,
                -precip_sum, -events)
MM <- rep("2", length = 28)
ptwn_aprt_feb07month <- cbind(MM, ptwn_aprt_feb07select)

ptwn_aprt_march07select <- ptwn_airprt_march07 %>%
  dplyr::select(-temphigh, -tempavg, -templow,- dewpointhigh, -dewpointavg,- dewpointlow,- humidityhigh,
                -humidityavg,- humiditylow, -sealevelpressurehigh, -sealevelpressureavg, -sealevelpressurelow,
                -visbilityhigh, -visibilityavg, -visibilitylow, -wind_high_mph,- wind_highmph,
                -precip_sum, -events)
MM <- rep("3", length = 31)
ptwn_aprt_march07month <- cbind(MM, ptwn_aprt_march07select)

ptwn_aprt_april07select <- ptwn_airprt_april07 %>%
  dplyr::select(-temphigh, -tempavg, -templow,- dewpointhigh, -dewpointavg,- dewpointlow,- humidityhigh,
                -humidityavg,- humiditylow, -sealevelpressurehigh, -sealevelpressureavg, -sealevelpressurelow,
                -visbilityhigh, -visibilityavg, -visibilitylow, -wind_high_mph,- wind_highmph,
                -precip_sum, -events)
MM <- rep("4", length = 30)
ptwn_aprt_april07month <- cbind(MM, ptwn_aprt_april07select)


cqx_wind_2001 <- rbind(ptwn_aprt_april01month, ptwn_aprt_feb01month, ptwn_aprt_march01month)
YYYY <- rep("01", length = 89)
cqx_wind_2001 <- cbind(YYYY, cqx_wind_2001)

pvt_wind_2012 <- rbind(ptwn_aprt_jan12month, ptwn_aprt_feb12month, ptwn_aprt_march12month, ptwn_aprt_april12month)
YYYY <- rep("12", length = 121)
pvt_wind_2012 <- cbind(YYYY, pvt_wind_2012)

pvt_wind_2005 <- rbind(ptwn_aprt_jan05month, ptwn_aprt_feb05month, ptwn_aprt_march05month, ptwn_aprt_may05month)
YYYY <- rep("05", length = 124)
pvt_wind_2005 <- cbind(YYYY, pvt_wind_2005)

pvt_wind_2006 <- rbind(ptwn_aprt_jan06month, ptwn_aprt_feb06month, ptwn_aprt_march06month, ptwn_aprt_may06month)
YYYY <- rep("06", length = 121)
pvt_wind_2006 <- cbind(YYYY, pvt_wind_2006)

pvt_wind_2007 <- rbind(ptwn_aprt_jan07month, ptwn_aprt_feb07month, ptwn_aprt_march07month)
YYYY <- rep("07", length = 90)
pvt_wind_2007 <- cbind(YYYY, pvt_wind_2007)

## bind 2012 ptwon airprort data wiht 2001 ptown airport data, 2005, 2006, and 2007
cqx_pvt_wind_2001_2012 <- rbind(cqx_wind_2001, pvt_wind_2005, pvt_wind_2006, pvt_wind_2007, pvt_wind_2012)

## convert directions to numbers not cardinal points
cqx_pvt_wind_2001_2012$wind_direction <- gsub("\\bN\\b", 349, cqx_pvt_wind_2001_2012$wind_direction)
cqx_pvt_wind_2001_2012$wind_direction <- gsub("\\bNNE\\b", 12, cqx_pvt_wind_2001_2012$wind_direction)
cqx_pvt_wind_2001_2012$wind_direction <- gsub("\\bNE\\b", 34, cqx_pvt_wind_2001_2012$wind_direction)
cqx_pvt_wind_2001_2012$wind_direction <- gsub("\\bENE\\b", 57, cqx_pvt_wind_2001_2012$wind_direction)
cqx_pvt_wind_2001_2012$wind_direction <- gsub("\\bE\\b", 79, cqx_pvt_wind_2001_2012$wind_direction)
cqx_pvt_wind_2001_2012$wind_direction <- gsub("\\bESE\\b", 102, cqx_pvt_wind_2001_2012$wind_direction)
cqx_pvt_wind_2001_2012$wind_direction <- gsub("\\bSE\\b", 124, cqx_pvt_wind_2001_2012$wind_direction)
cqx_pvt_wind_2001_2012$wind_direction <- gsub("\\bSSE\\b", 147, cqx_pvt_wind_2001_2012$wind_direction)
cqx_pvt_wind_2001_2012$wind_direction <- gsub("\\bS\\b", 169, cqx_pvt_wind_2001_2012$wind_direction)
cqx_pvt_wind_2001_2012$wind_direction <- gsub("\\bSSW\\b", 192, cqx_pvt_wind_2001_2012$wind_direction)
cqx_pvt_wind_2001_2012$wind_direction <- gsub("\\bSW\\b", 214, cqx_pvt_wind_2001_2012$wind_direction)
cqx_pvt_wind_2001_2012$wind_direction <- gsub("\\bWSW\\b", 237, cqx_pvt_wind_2001_2012$wind_direction)
cqx_pvt_wind_2001_2012$wind_direction <- gsub("\\bW\\b", 259, cqx_pvt_wind_2001_2012$wind_direction)
cqx_pvt_wind_2001_2012$wind_direction <- gsub("\\bWNW\\b", 282, cqx_pvt_wind_2001_2012$wind_direction)
cqx_pvt_wind_2001_2012$wind_direction <- gsub("\\bNW\\b", 304, cqx_pvt_wind_2001_2012$wind_direction)
cqx_pvt_wind_2001_2012$wind_direction <- gsub("\\bNNW\\b", 327, cqx_pvt_wind_2001_2012$wind_direction)

cqx_pvt_wind_2001_2012$wind_direction <- as.numeric(cqx_pvt_wind_2001_2012$wind_direction)
cqx_pvt_wind_2001_2012$wind_avg_mph <- as.numeric(as.character(cqx_pvt_wind_2001_2012$wind_avg_mph))
## make sure spd is in correct units
cqx_wind_2001_2012_ms <- cqx_pvt_wind_2001_2012 %>%
  na.omit() %>%
  dplyr::mutate(wind_spd_ms = wind_avg_mph *(.44)) 

## to get the average wind direction you need to calculate u and v components 
## i got most of this code from Grange 2014 (averaging wind speed and direction)
cqx_wind_2001_2012_ms$u.wind.cqx <- - cqx_wind_2001_2012_ms$wind_spd_ms * sin(2 * pi * cqx_wind_2001_2012_ms$wind_direction/360)
cqx_wind_2001_2012_ms$v.wind.cqx <- - cqx_wind_2001_2012_ms$wind_spd_ms * cos(2 * pi * cqx_wind_2001_2012_ms$wind_direction/360)

## group by year and month to calculate the average vectors for each month
cqx_wind_direc_spd_group <- as.data.frame(cqx_wind_2001_2012_ms %>%
                                            dplyr::group_by(YYYY, MM) %>%
                                            dplyr::summarise(mean.u.cqx = mean(u.wind.cqx))) # calc u vector

cqx_wind_direc_spd_group_v <- as.data.frame(cqx_wind_2001_2012_ms %>%
                                              dplyr::group_by(YYYY, MM) %>%
                                              dplyr::summarise(mean.v.cqx = mean(v.wind.cqx))) #calc v vector


# Calculate the scalar average wind speed, the standard mean for gom data and 
##put it in the column ws.scalar.average
cqx_scalar_avg_spd <- as.data.frame(cqx_wind_2001_2012_ms %>%
                                      dplyr::group_by(YYYY, MM) %>%
                                      dplyr::summarise(ws.scalar.average.cqx = mean(wind_spd_ms)))

## combine data frame back together with monthly average u and v vectors
cqx_vec <- dplyr::full_join(cqx_wind_direc_spd_group, cqx_wind_direc_spd_group_v)

## Calculate the resultant vector average wind direction with atan2.  This will give 
## us the average wind direction for each month (wd.average)
cqx_vec$wd.average.cqx <- (atan2(cqx_vec$mean.u.cqx, cqx_vec$mean.v.cqx) * 360/2/pi) + 180


## Calculate the vector average wind speed for cqx data and put it in the column
## ws.vector.average
cqx_vec$ws.vector.average.cqx <- ((cqx_vec$mean.u.cqx^2 + cqx_vec$mean.v.cqx^2)^0.5)

cqx_wind_full <- dplyr::full_join(cqx_vec, cqx_scalar_avg_spd)

## join with the boston gloucester data
cqx_boston_gloucester<- dplyr::full_join(cqx_wind_full, boston_gloucester)


## plot local wind data
all_local_wind_direc <- ggplot(cqx_boston_gloucester, aes(x = MM, y = wd.average.boston)) +
  geom_point() +
  geom_point(aes(x=MM, y = wd.average.gloucester)) +
  geom_point(aes(x=MM, y = wd.average.cqx)) +
  facet_wrap(~YYYY) +
  ylab("Average wind direction") +
  xlab("Month")

all_local_wind_spd <- ggplot(cqx_boston_gloucester, aes(x = MM, y = ws.scalar.average.boston)) +
  geom_point() +
  geom_point(aes(x = MM, y = ws.scalar.average.gloucester)) +
  geom_point(aes(x=MM, y = ws.scalar.average.cqx)) +
  facet_wrap(~YYYY) +
  ylab("Average scalar wind speed (m/s)") +
  xlab("Month")

## compare ptwn airport wiht Boston
boston_ptwn_spd <- ggplot(cqx_boston_gloucester, 
                          aes(x = ws.scalar.average.boston, y = ws.scalar.average.cqx)) +
                          geom_point()+
                          geom_smooth(method = "lm")

boston_ptwn_spdlm <- lm(ws.scalar.average.cqx ~ ws.scalar.average.boston, 
                        data = cqx_boston_gloucester)
plot(boston_ptwn_spdlm)
summary(boston_ptwn_spdlm)

gloucester_ptwn_spdlm <- lm(ws.scalar.average.cqx ~ ws.scalar.average.gloucester, 
                            data = cqx_boston_gloucester)
plot(gloucester_ptwn_spdlm)
summary(gloucester_ptwn_spdlm)

boston_ptwn_direc <- ggplot(cqx_boston_gloucester, 
                            aes(x = wd.average.boston, y = wd.average.cqx)) +
  geom_point() +
  geom_smooth(method = "lm")

boston_ptwn_direclm <- lm(wd.average.cqx ~ wd.average.boston, 
                          data = cqx_boston_gloucester)
plot(boston_ptwn_direclm)
summary(boston_ptwn_direclm)

gloucester_ptwn_direc <- ggplot(cqx_boston_gloucester,
                                aes(x= wd.average.gloucester, y = wd.average.cqx)) +
                                 geom_point() +
                                  geom_smooth(method = "lm")


## change years to four digit format
cqx_boston_gloucester$YYYY <- gsub("99", "1999", cqx_boston_gloucester$YYYY)
cqx_boston_gloucester$YYYY <- gsub("98", "1998", cqx_boston_gloucester$YYYY)
cqx_boston_gloucester$YYYY <- gsub("00", "2000", cqx_boston_gloucester$YYYY)
cqx_boston_gloucester$YYYY <- gsub("01", "2001", cqx_boston_gloucester$YYYY)
cqx_boston_gloucester$YYYY <- gsub("02", "2002", cqx_boston_gloucester$YYYY)
cqx_boston_gloucester$YYYY <- gsub("03", "2003", cqx_boston_gloucester$YYYY)
cqx_boston_gloucester$YYYY <- gsub("04", "2004", cqx_boston_gloucester$YYYY)
cqx_boston_gloucester$YYYY <- gsub("05", "2005", cqx_boston_gloucester$YYYY)
cqx_boston_gloucester$YYYY <- gsub("06", "2006", cqx_boston_gloucester$YYYY)
cqx_boston_gloucester$YYYY <- gsub("07", "2007", cqx_boston_gloucester$YYYY)
cqx_boston_gloucester$YYYY <- gsub("08", "2008", cqx_boston_gloucester$YYYY)
cqx_boston_gloucester$YYYY <- gsub("09", "2009", cqx_boston_gloucester$YYYY)
cqx_boston_gloucester$YYYY <- gsub("10", "2010", cqx_boston_gloucester$YYYY)
cqx_boston_gloucester$YYYY <- gsub("11", "2011", cqx_boston_gloucester$YYYY)
cqx_boston_gloucester$YYYY <- gsub("12", "2012", cqx_boston_gloucester$YYYY)
cqx_boston_gloucester$YYYY <- gsub("13", "2013", cqx_boston_gloucester$YYYY)
cqx_boston_gloucester$YYYY <- gsub("14", "2014", cqx_boston_gloucester$YYYY)
cqx_boston_gloucester$YYYY <- gsub("15", "2015", cqx_boston_gloucester$YYYY)
cqx_boston_gloucester$YYYY <- gsub("16", "2016", cqx_boston_gloucester$YYYY)
cqx_boston_gloucester$YYYY <- gsub("17", "2017", cqx_boston_gloucester$YYYY)




##**************************************REGIONAL_WIND_DATA***********************************
## Now I want to do the same as above but comparing the Mattinicus Rock buoy with the mattinicus buoy

## read in the Mattinicus rock data
mattinicus_98 <- read.table("/Users/laura.ganley001/Documents/R_Projects/SEM/raw_data/regional_wind_data/wind_mattinicus_buoy_1998.txt", header = TRUE)
mattinicus_99 <- read.table("/Users/laura.ganley001/Documents/R_Projects/SEM/raw_data/regional_wind_data/wind_mattinicus_buoy_1999.txt", header = TRUE)
mattinicus_00 <- read.table("/Users/laura.ganley001/Documents/R_Projects/SEM/raw_data/regional_wind_data/wind_mattinicus_buoy_2000.txt", header = TRUE)
mattinicus_01 <- read.table("/Users/laura.ganley001/Documents/R_Projects/SEM/raw_data/regional_wind_data/wind_mattinicus_buoy_2001.txt", header = TRUE)
mattinicus_02 <- read.table("/Users/laura.ganley001/Documents/R_Projects/SEM/raw_data/regional_wind_data/wind_mattinicus_buoy_2002.txt", header = TRUE)
mattinicus_03 <- read.table("/Users/laura.ganley001/Documents/R_Projects/SEM/raw_data/regional_wind_data/wind_mattinicus_buoy_2003.txt", header = TRUE)
mattinicus_04 <- read.table("/Users/laura.ganley001/Documents/R_Projects/SEM/raw_data/regional_wind_data/wind_mattinicus_buoy_2004.txt", header = TRUE)
mattinicus_05 <- read.table("/Users/laura.ganley001/Documents/R_Projects/SEM/raw_data/regional_wind_data/wind_mattinicus_buoy_2005.txt", header = TRUE)
mattinicus_06 <- read.table("/Users/laura.ganley001/Documents/R_Projects/SEM/raw_data/regional_wind_data/wind_mattinicus_buoy_2006.txt", header = TRUE)
mattinicus_07 <- read.table("/Users/laura.ganley001/Documents/R_Projects/SEM/raw_data/regional_wind_data/wind_mattinicus_buoy_2007.txt", header = TRUE)
mattinicus_08 <- read.table("/Users/laura.ganley001/Documents/R_Projects/SEM/raw_data/regional_wind_data/wind_mattinicus_buoy_2008.txt", header = TRUE)
mattinicus_jan_09 <- read.table("/Users/laura.ganley001/Documents/R_Projects/SEM/raw_data/regional_wind_data/wind_mattinicus_buoy_january_2009.txt", header = TRUE)
mattinicus_mar_10 <- read.table("/Users/laura.ganley001/Documents/R_Projects/SEM/raw_data/regional_wind_data/wind_mattinicus_buoy_march_2010.txt", header = TRUE)
mattinicus_11 <- read.table("/Users/laura.ganley001/Documents/R_Projects/SEM/raw_data/regional_wind_data/wind_mattinicus_buoy_2011.txt", header = TRUE)
mattinicus_12 <- read.table("/Users/laura.ganley001/Documents/R_Projects/SEM/raw_data/regional_wind_data/wind_mattinicus_buoy_2012.txt", header = TRUE)
mattinicus_13 <- read.table("/Users/laura.ganley001/Documents/R_Projects/SEM/raw_data/regional_wind_data/wind_mattinicus_buoy_2013.txt", header = TRUE)
mattinicus_14 <- read.table("/Users/laura.ganley001/Documents/R_Projects/SEM/raw_data/regional_wind_data/wind_mattinicus_buoy_2014.txt", header = TRUE)
mattinicus_15 <- read.table("/Users/laura.ganley001/Documents/R_Projects/SEM/raw_data/regional_wind_data/wind_mattinicus_buoy_2015.txt", header = TRUE)
mattinicus_16 <- read.table("/Users/laura.ganley001/Documents/R_Projects/SEM/raw_data/regional_wind_data/wind_mattinicus_buoy_2016.txt", header = TRUE)
mattinicus_jan_17 <- read.table("/Users/laura.ganley001/Documents/R_Projects/SEM/raw_data/regional_wind_data/wind_mattinicus_buoy_january_2017.txt", header = TRUE)
mattinicus_feb_17 <- read.table("/Users/laura.ganley001/Documents/R_Projects/SEM/raw_data/regional_wind_data/wind_mattinicus_buoy_february_2017.txt", header = TRUE)
mattinicus_mar_17 <- read.table("/Users/laura.ganley001/Documents/R_Projects/SEM/raw_data/regional_wind_data/wind_mattinicus_buoy_march_2017.txt", header = TRUE)
mattinicus_apr_17 <- read.table("/Users/laura.ganley001/Documents/R_Projects/SEM/raw_data/regional_wind_data/wind_mattinicus_buoy_april_2017.txt", header = TRUE)
mattinicus_may_17 <- read.table("/Users/laura.ganley001/Documents/R_Projects/SEM/raw_data/regional_wind_data/wind_mattinicus_buoy_may_2017.txt", header = TRUE)

gom_98 <- read.table("/Users/laura.ganley001/Documents/R_Projects/SEM/raw_data/regional_wind_data/wind_GOM_buoy_1998.txt", header = TRUE)
gom_99 <- read.table("/Users/laura.ganley001/Documents/R_Projects/SEM/raw_data/regional_wind_data/wind_GOM_buoy_1999.txt", header = TRUE)
gom_00 <- read.table("/Users/laura.ganley001/Documents/R_Projects/SEM/raw_data/regional_wind_data/wind_GOM_buoy_2000.txt", header = TRUE)
gom_01 <- read.table("/Users/laura.ganley001/Documents/R_Projects/SEM/raw_data/regional_wind_data/wind_GOM_buoy_2001.txt", header = TRUE)
gom_02 <- read.table("/Users/laura.ganley001/Documents/R_Projects/SEM/raw_data/regional_wind_data/wind_GOM_buoy_2002.txt", header = TRUE)
gom_04 <- read.table("/Users/laura.ganley001/Documents/R_Projects/SEM/raw_data/regional_wind_data/wind_GOM_buoy_2004.txt", header = TRUE)
gom_05 <- read.table("/Users/laura.ganley001/Documents/R_Projects/SEM/raw_data/regional_wind_data/wind_GOM_buoy_2005.txt", header = TRUE)
gom_06 <- read.table("/Users/laura.ganley001/Documents/R_Projects/SEM/raw_data/regional_wind_data/wind_GOM_buoy_2006.txt", header = TRUE)
gom_07 <- read.table("/Users/laura.ganley001/Documents/R_Projects/SEM/raw_data/regional_wind_data/wind_GOM_buoy_2007.txt", header = TRUE)
gom_08 <- read.table("/Users/laura.ganley001/Documents/R_Projects/SEM/raw_data/regional_wind_data/wind_GOM_buoy_2008.txt", header = TRUE)
gom_09 <- read.table("/Users/laura.ganley001/Documents/R_Projects/SEM/raw_data/regional_wind_data/wind_GOM_buoy_2009.txt", header = TRUE)
gom_10 <- read.table("/Users/laura.ganley001/Documents/R_Projects/SEM/raw_data/regional_wind_data/wind_GOM_buoy_2010.txt", header = TRUE)
gom_11 <- read.table("/Users/laura.ganley001/Documents/R_Projects/SEM/raw_data/regional_wind_data/wind_GOM_buoy_2011.txt", header = TRUE)
gom_12 <- read.table("/Users/laura.ganley001/Documents/R_Projects/SEM/raw_data/regional_wind_data/wind_GOM_buoy_2012.txt", header = TRUE)
gom_14 <- read.table("/Users/laura.ganley001/Documents/R_Projects/SEM/raw_data/regional_wind_data/wind_GOM_buoy_2014.txt", header = TRUE)
gom_15 <- read.table("/Users/laura.ganley001/Documents/R_Projects/SEM/raw_data/regional_wind_data/wind_GOM_buoy_2015.txt", header = TRUE)
gom_16 <- read.table("/Users/laura.ganley001/Documents/R_Projects/SEM/raw_data/regional_wind_data/wind_GOM_buoy_2016.txt", header = TRUE)


##standardize columns across years
colnames(gom_98) <- c("YYYY", "MM", "DD", "hh", "mm", "DIR", "SPD", "GDR", "GSP", "GMN")
colnames(gom_99) <- c("YYYY", "MM", "DD", "hh", "mm", "DIR", "SPD", "GDR", "GSP", "GMN")
colnames(gom_00) <- c("YYYY", "MM", "DD", "hh", "mm", "DIR", "SPD", "GDR", "GSP", "GMN")
colnames(gom_01) <- c("YYYY", "MM", "DD", "hh", "mm", "DIR", "SPD", "GDR", "GSP", "GMN")
colnames(gom_02) <- c("YYYY", "MM", "DD", "hh", "mm", "DIR", "SPD", "GDR", "GSP", "GMN")
colnames(gom_04) <- c("YYYY", "MM", "DD", "hh", "mm", "DIR", "SPD", "GDR", "GSP", "GMN")
colnames(gom_05) <- c("YYYY", "MM", "DD", "hh", "mm", "DIR", "SPD", "GDR", "GSP", "GMN")
colnames(gom_06) <- c("YYYY", "MM", "DD", "hh", "mm", "DIR", "SPD", "GDR", "GSP", "GMN")
colnames(gom_07) <- c("YYYY", "MM", "DD", "hh", "mm", "DIR", "SPD", "GDR", "GSP", "GMN")
colnames(gom_08) <- c("YYYY", "MM", "DD", "hh", "mm", "DIR", "SPD", "GDR", "GSP", "GMN")
colnames(gom_09) <- c("YYYY", "MM", "DD", "hh", "mm", "DIR", "SPD", "GDR", "GSP", "GMN")
colnames(gom_10) <- c("YYYY", "MM", "DD", "hh", "mm", "DIR", "SPD", "GDR", "GSP", "GMN")
colnames(gom_11) <- c("YYYY", "MM", "DD", "hh", "mm", "DIR", "SPD", "GDR", "GSP", "GMN")
colnames(gom_12) <- c("YYYY", "MM", "DD", "hh", "mm", "DIR", "SPD", "GDR", "GSP", "GMN")
colnames(gom_14) <- c("YYYY", "MM", "DD", "hh", "mm", "DIR", "SPD", "GDR", "GSP", "GMN")
colnames(gom_15) <- c("YYYY", "MM", "DD", "hh", "mm", "DIR", "SPD", "GDR", "GSP", "GMN")
colnames(gom_16) <- c("YYYY", "MM", "DD", "hh", "mm", "DIR", "SPD", "GDR", "GSP", "GMN")


## GOM BUOY DATA*************************
## bind together all of the hourly gom buoy data
gom_buoy <- rbind(gom_98, gom_99, gom_00, gom_01, gom_02, gom_04, gom_05, gom_06,
                  gom_07, gom_08, gom_09, gom_10, gom_11, gom_12, gom_14, gom_15,
                  gom_16)

## to get the average wind direction you need to calculate u and v components 
## i got most of this code from Grange 2014 (averaging wind speed and direction)
gom_buoy$u.wind <- - gom_buoy$SPD * sin(2 * pi * gom_buoy$DIR/360)
gom_buoy$v.wind <- - gom_buoy$SPD * cos(2 * pi * gom_buoy$DIR/360)

## group by year and month to calculate the average vectors for each month
gom_buoy_direc_group <- as.data.frame(gom_buoy %>%
                                        dplyr::group_by(YYYY, MM) %>%
                                        dplyr::summarise(mean.u = mean(u.wind))) # calc u vector

gom_buoy_direc_group_v <- as.data.frame(gom_buoy %>%
                                          dplyr::group_by(YYYY, MM) %>%
                                          dplyr::summarise(mean.v = mean(v.wind))) #calc v vector


# Calculate the scalar average wind speed, the standard mean for gom data and put it in 
## the column ws.scalar.average
gom_buoy_scalar_avg_spd <- as.data.frame(gom_buoy %>%
                                           dplyr::group_by(YYYY, MM) %>%
                                           dplyr::summarise(ws.scalar.average.gom = mean(SPD)))


## combine data frame back together with monthly average u and v vectors
gom_buoy_vec <- dplyr::full_join(gom_buoy_direc_group, gom_buoy_direc_group_v)

## Calculate the resultant vector average wind direction with atan2.  This will give 
## us the average wind direction for each month (wd.average)
gom_buoy_vec$wd.average.gom <- (atan2(gom_buoy_vec$mean.u, gom_buoy_vec$mean.v) * 360/2/pi) + 180

ggplot(gom_buoy_vec, aes(x = MM, y = wd.average.gom)) +
  geom_point() +
  facet_wrap(~YYYY)


## Calculate the vector average wind speed for GOM data and put it in the column
## ws.vector.average
gom_buoy_vec$ws.vector.average.gom <- ((gom_buoy_vec$mean.u^2 + gom_buoy_vec$mean.v^2)^0.5)

gom_wind_full <- dplyr::full_join(gom_buoy_vec, gom_buoy_scalar_avg_spd)

gom_wind_full$YYYY <- gsub("98", "1998", gom_wind_full$YYYY)

## *****************************MATTINICUS ROCK BUOY DATA****************************************

## format columns across years
colnames(mattinicus_98) <- c("YYYY", "MM", "DD", "hh", "mm", "DIR", "SPD", "GDR", "GSP", "GMN")
colnames(mattinicus_99) <- c("YYYY", "MM", "DD", "hh", "mm", "DIR", "SPD", "GDR", "GSP", "GMN")
colnames(mattinicus_00) <- c("YYYY", "MM", "DD", "hh", "mm", "DIR", "SPD", "GDR", "GSP", "GMN")
colnames(mattinicus_01) <- c("YYYY", "MM", "DD", "hh", "mm", "DIR", "SPD", "GDR", "GSP", "GMN")
colnames(mattinicus_02) <- c("YYYY", "MM", "DD", "hh", "mm", "DIR", "SPD", "GDR", "GSP", "GMN")
colnames(mattinicus_03) <- c("YYYY", "MM", "DD", "hh", "mm", "DIR", "SPD", "GDR", "GSP", "GMN")
colnames(mattinicus_04) <- c("YYYY", "MM", "DD", "hh", "mm", "DIR", "SPD", "GDR", "GSP", "GMN")
colnames(mattinicus_05) <- c("YYYY", "MM", "DD", "hh", "mm", "DIR", "SPD", "GDR", "GSP", "GMN")
colnames(mattinicus_06) <- c("YYYY", "MM", "DD", "hh", "mm", "DIR", "SPD", "GDR", "GSP", "GMN")
colnames(mattinicus_07) <- c("YYYY", "MM", "DD", "hh", "mm", "DIR", "SPD", "GDR", "GSP", "GMN")
colnames(mattinicus_08) <- c("YYYY", "MM", "DD", "hh", "mm", "DIR", "SPD", "GDR", "GSP", "GMN")
colnames(mattinicus_jan_09) <- c("YYYY", "MM", "DD", "hh", "mm", "DIR", "SPD", "GDR", "GSP", "GMN")
colnames(mattinicus_mar_10) <- c("YYYY", "MM", "DD", "hh", "mm", "DIR", "SPD", "GDR", "GSP", "GMN")
colnames(mattinicus_11) <- c("YYYY", "MM", "DD", "hh", "mm", "DIR", "SPD", "GDR", "GSP", "GMN")
colnames(mattinicus_12) <- c("YYYY", "MM", "DD", "hh", "mm", "DIR", "SPD", "GDR", "GSP", "GMN")
colnames(mattinicus_13) <- c("YYYY", "MM", "DD", "hh", "mm", "DIR", "SPD", "GDR", "GSP", "GMN")
colnames(mattinicus_14) <- c("YYYY", "MM", "DD", "hh", "mm", "DIR", "SPD", "GDR", "GSP", "GMN")
colnames(mattinicus_15) <- c("YYYY", "MM", "DD", "hh", "mm", "DIR", "SPD", "GDR", "GSP", "GMN")
colnames(mattinicus_16) <- c("YYYY", "MM", "DD", "hh", "mm", "DIR", "SPD", "GDR", "GSP", "GMN")
colnames(mattinicus_jan_17) <- c("YYYY", "MM", "DD", "hh", "mm", "DIR", "SPD", "GDR", "GSP", "GMN")
colnames(mattinicus_feb_17) <- c("YYYY", "MM", "DD", "hh", "mm", "DIR", "SPD", "GDR", "GSP", "GMN")
colnames(mattinicus_mar_17) <- c("YYYY", "MM", "DD", "hh", "mm", "DIR", "SPD", "GDR", "GSP", "GMN")
colnames(mattinicus_apr_17) <- c("YYYY", "MM", "DD", "hh", "mm", "DIR", "SPD", "GDR", "GSP", "GMN")
colnames(mattinicus_may_17) <- c("YYYY", "MM", "DD", "hh", "mm", "DIR", "SPD", "GDR", "GSP", "GMN")


## bind together all of the hourly gom buoy data
mattinicus_buoy <- rbind(mattinicus_98, mattinicus_99, mattinicus_00, mattinicus_01, 
                         mattinicus_02, mattinicus_03, mattinicus_04, mattinicus_05, 
                         mattinicus_06, mattinicus_07, mattinicus_08, mattinicus_jan_09, 
                         mattinicus_mar_10, mattinicus_11, mattinicus_12, mattinicus_13, 
                         mattinicus_14, mattinicus_15, mattinicus_16, mattinicus_jan_17, 
                         mattinicus_feb_17, mattinicus_mar_17, mattinicus_apr_17, 
                         mattinicus_may_17)

mattinicus_buoy$YYYY <- gsub("99", "1999", mattinicus_buoy$YYYY)
mattinicus_buoy$YYYY <- gsub("119999", "1999", mattinicus_buoy$YYYY)
mattinicus_buoy$YYYY <- gsub("98", "1998", mattinicus_buoy$YYYY)

## to get the average wind direction you need to calculate u and v components 
## i got most of this code from Grange 2014 (averaging wind speed and direction)
mattinicus_buoy$u.wind <- - mattinicus_buoy$SPD * sin(2 * pi * mattinicus_buoy$DIR/360)
mattinicus_buoy$v.wind <- - mattinicus_buoy$SPD * cos(2 * pi * mattinicus_buoy$DIR/360)

## group by year and month to calculate the average vectors for each month
mattinicus_buoy_direc_group <- as.data.frame(mattinicus_buoy %>%
                                               dplyr::group_by(YYYY, MM) %>%
                                               dplyr::summarise(mean.u = mean(u.wind))) # calc u vector

mattinicus_buoy_direc_group_v <- as.data.frame(mattinicus_buoy %>%
                                                 dplyr::group_by(YYYY, MM) %>%
                                                 dplyr::summarise(mean.v = mean(v.wind))) #calc v vector


# Calculate the scalar average wind speed, the standard mean for gom data and put it in 
## the column ws.scalar.average
mattinicus_buoy_scalar_avg_spd <- as.data.frame(mattinicus_buoy %>%
                                                  dplyr::group_by(YYYY, MM) %>%
                                                  dplyr::summarise(ws.scalar.average.mattinicus = mean(SPD)))


## combine data frame back together with monthly average u and v vectors
mattinicus_buoy_vec <- dplyr::full_join(mattinicus_buoy_direc_group, mattinicus_buoy_direc_group_v)

## Calculate the resultant vector average wind direction with atan2.  This will give 
## us the average wind direction for each month (wd.average)
mattinicus_buoy_vec$wd.average.mattinicus <- (atan2(mattinicus_buoy_vec$mean.u, mattinicus_buoy_vec$mean.v) * 360/2/pi) + 180

ggplot(mattinicus_buoy_vec, aes(x = MM, y = wd.average.mattinicus)) +
  geom_point()


## Calculate the vector average wind speed for GOM data and put it in the column
## ws.vector.average
mattinicus_buoy_vec$ws.vector.average.mattinicus <- ((mattinicus_buoy_vec$mean.u^2 + mattinicus_buoy_vec$mean.v^2)^0.5)

mattinicus_wind_full <- dplyr::full_join(mattinicus_buoy_vec, mattinicus_buoy_scalar_avg_spd)

## join mattinicus and gom buoy data
mattinicus_gom  <- dplyr::full_join(mattinicus_wind_full, gom_wind_full, by = c("YYYY", "MM"))

## remove data outside of jan- may
mattinicus_gom_fil <- dplyr::filter(mattinicus_gom,
                                 MM == "1" | MM == "2" | MM == "3" | 
                                   MM == "4" | MM == "5" )


## determine if the two wind direction datasets are correlated
wind_direc_regional_lm <- lm(wd.average.gom ~ wd.average.mattinicus, 
                             mattinicus_gom_fil)
plot(wind_direc_regional_lm)
summary(wind_direc_regional_lm)
confint(wind_direc_regional_lm)

## point 45 shows up as an outlier (May 2006) remove it and re run wind direction lm
mattinicus_gom_nooutlier <- mattinicus_gom_fil[-45, ]
wind_direc_regional_lm_nooutlier <- lm(wd.average.gom ~ wd.average.mattinicus,
                                       mattinicus_gom_nooutlier)
plot(wind_direc_regional_lm_nooutlier)
summary(wind_direc_regional_lm_nooutlier)
## Determine if the slope is really different then 1
confint(wind_direc_regional_lm_nooutlier)
## the confidence intervals include 1 (2.5% = .70, 97.5% = 1.05)
## so our slope is not different than 1 and therefore we can assume
## a 1 to 1 relationship between wind direction at mattinicus and wind 
## direction at the GOM buoy.


## plot wind direc with out outlier
regional_wind_direc_nooutlier <- ggplot(mattinicus_gom_nooutlier, 
                                        aes(y = wd.average.gom, x = wd.average.mattinicus)) +
  geom_point() +
  stat_smooth(method = "lm") +
  labs(x = "Mattinicus wind direction", y = "GOM wind direction")

## plot wind direc with the outlier
## pull out coefs to put on ggplot
a <- signif(coef(wind_direc_regional_lm)[1], digits = 2)
b <- signif(coef(wind_direc_regional_lm)[2], digits = 2)
c <- signif(summary(wind_direc_regional_lm)$adj.r.squared, digits = 4)
textlab <- paste("y = ",b,"x + ",a, sep="")
textlab2 <- paste("adj. r^2 = ",c, sep = "")

regional_wind_direc <- ggplot(mattinicus_gom_fil, 
                           aes(y = wd.average.gom, x = wd.average.mattinicus)) +
  geom_point() +
  stat_smooth(method = "lm") +
  annotate("text", x = 245, y = 30, label = textlab, color="black", 
           size = 5, parse=FALSE) +
  annotate("text", x = 280, y = 13, label = textlab2, color = "black",
           size = 5, parse = FALSE) +
  labs(x = "Mattinicus wind direction", y = "GOM wind direction")




## linear model and plot of regional wind speed
wind_spd_regional_lm <- lm(ws.scalar.average.gom ~ ws.scalar.average.mattinicus,
                           mattinicus_gom_fil)
plot(wind_spd_regional_lm)
summary(wind_spd_regional_lm)
## Determine if the slope is really different then 1
confint(wind_spd_regional_lm)
## the confidence intervals include 1 (2.5% = .97, 97.5% = 1.34)
## so our slope is not different than 1 and therefore we can assume
## a 1 to 1 relationship between wind spd at mattinicus and wind 
## spd at the GOM buoy.

## plot wind direc with the outlier
## pull out coefs to put on ggplot
a <- signif(coef(wind_spd_regional_lm)[1], digits = 2)
b <- signif(coef(wind_spd_regional_lm)[2], digits = 2)
c <- signif(summary(wind_spd_regional_lm)$adj.r.squared, digits = 4)
textlab <- paste("y = ",b,"x + ",a, sep="")
textlab2 <- paste("adj. r^2 = ",c, sep = "")

regional_wind_spd <- ggplot(mattinicus_gom_fil,
                         aes(y = ws.scalar.average.gom, x = ws.scalar.average.mattinicus)) +
  geom_point() +
  stat_smooth(method = "lm") +
  annotate("text", x = 14, y = 7, label = textlab, color="black", 
           size = 5, parse=FALSE) +
  annotate("text", x = 14, y = 6, label = textlab2, color = "black",
           size = 5, parse = FALSE) +
  labs(x = "Mattinicus scalar average spd", y = "GOM scalar average spd")



regional_wind_spd_vec <- ggplot(mattinicus_gom_fil,
                                aes(y=ws.vector.average.gom, x = ws.vector.average.mattinicus)) +
  geom_point() +
  stat_smooth(method = "lm") +
  labs(x = "Mattinicus vector average spd", y = "GOM vector average spd")




wind_spd_regional_vector_avg_lm <- lm(ws.vector.average.gom ~ ws.vector.average.mattinicus,
                    mattinicus_gom_fil)
plot(wind_spd_regional_vector_avg_lm)
summary(wind_spd_regional_vector_avg_lm)

## Determine if the slope is really different then 1
confint(wind_spd_regional_vector_avg_lm)
## the confidence intervals include 1 (2.5% = .86, 97.5% = 1.18)
## so our slope is not different than 1 and therefore we can assume
## a 1 to 1 relationship between wind spd at mattinicus and wind 
## spd at the GOM buoy.


## plot all of the regional wind direction data
mattinicus_gom_fil$MM <- as.character(mattinicus_gom_fil$MM)
all_regional_wind_direc <- ggplot(mattinicus_gom_fil, aes(x= MM, 
                                        y = wd.average.gom)) +
  geom_point() +
  geom_point(aes(x= MM, y = wd.average.mattinicus)) +
  facet_wrap(~YYYY)

## plot all of the regional wind speed data
all_regional_wind_spd <- ggplot(mattinicus_gom_fil, aes(x = MM, y = ws.scalar.average.gom)) +
  geom_point() +
  geom_point(aes(x = MM, y = ws.scalar.average.mattinicus)) +
  facet_wrap(~YYYY)
##******************************REGIONAL STRATIFICATION DATA*************************************
## used the Bedford institute data that was already filtered for month 
##and year in the plotting_sem_stations.R file

## filter by station
bedford_strat_station <- dplyr::filter(bedford_reg_strat, AREA_NAME == "SS14"|
                                  AREA_NAME == "SS29" | AREA_NAME == "SS52"|
                                  AREA_NAME == "SS55")


## at each station for within each month of each year I want to subtract the maximum and 
## minimum sigma-t values as an index of the degree of stratification.

sigma_t_reg <- as.data.frame(bedford_strat_station %>%
                               dplyr::group_by(YEAR, MONTH, AREA_NAME) %>%
                               dplyr:: summarise(strat_index = max(AVG_SIGMAT) - min(AVG_SIGMAT)))

sigma_t_reg_allstations <- as.data.frame(bedford_reg_strat %>%
                                           dplyr::group_by(YEAR, MONTH, AREA_NAME) %>%
                                           dplyr::summarise(strat_index = max(AVG_SIGMAT) - min(AVG_SIGMAT)))

ggplot(sigma_t_reg_allstations, aes(x = MONTH, y = strat_index)) +
  geom_point() +
  facet_wrap(~YEAR)


## determine if the sigma t at each station is correlated with teh other stations
## first make the data wide not long
sigma_t_wide <- spread(sigma_t_reg, AREA_NAME, strat_index)
cor(sigma_t_wide[3:6])
library(psych)
pairs.panels(sigma_t_wide[3:6])

## try the same thing with a different set of stations

bedford_strat_station_2 <- dplyr::filter(bedford_reg_strat, AREA_NAME == "SS12"|
                                  AREA_NAME == "SS21" | AREA_NAME == "SS22"|
                                  AREA_NAME == "SS23" | AREA_NAME == "SS24" |
                                  AREA_NAME == "SS25" | AREA_NAME == "SS26" |
                                  AREA_NAME == "SS27")

## at each station for within each month of each year I want to subtract the maximum and 
## minimum sigma-t values as an index of the degree of stratification.

sigma_t_reg_2 <- as.data.frame(bedford_strat_station_2 %>%
                                 dplyr::group_by(YEAR, MONTH, AREA_NAME) %>%
                                 dplyr::summarise(strat_index = max(AVG_SIGMAT) - min(AVG_SIGMAT)))


## determine if the sigma t at each station is correlated with the
##other stations
## first make the data wide not long
sigma_t_wide_2 <- spread(sigma_t_reg_2, AREA_NAME, strat_index)
cor(sigma_t_wide_2[3:10])
library(psych)
pairs.panels(sigma_t_wide_2[3:10])


## ********************************CCB Stratification Data*****************************
library(oce)
ctd_amy <- read.csv("/Users/laura.ganley001/Documents/R_Projects/SEM/raw_data/CTD_local/CTD_data_from_amy_costa_24Feb2018.csv")
ctd_christy <- read.csv("/Users/laura.ganley001/Documents/R_Projects/SEM/raw_data/CTD_local/CTD2_2018updated_from_christy.csv")
ctd_mwra <- read.csv("/Users/laura.ganley001/Documents/R_Projects/SEM/raw_data/CTD_local/CTD_MWRA_CCB.csv")

## calculate the difference in sigma - t at each station within each month of each year for Amy's data
sigma_t_amy <-  dplyr::mutate(ctd_amy, month = month.abb[Month]) %>%
  dplyr::group_by(Year, month, YrDay) %>%
  dplyr::summarise(sigmat = max(Density..kg.m3.) - min(Density..kg.m3.)) %>%
  dplyr::ungroup()

colnames(sigma_t_amy) <- c("Year", "Month", "YrDay", "sigmat")

## get a look 
ggplot(sigma_t_amy, aes(x = Month, y = sigmat)) +
  geom_point() +
  facet_wrap(~ Year)

## remove notes column from Christy's data
ctd_christy <- dplyr::select(ctd_christy, -Notes)

## this is the data with no temp90 data so we need to use the temp68
temp68 <- dplyr::filter(ctd_christy, T090C == "") %>%
  dplyr::filter(T068C != "T068C")

temp68$PrSE <- as.numeric(as.character(temp68$PrSE))
temp68$Sal00 <- as.numeric(as.character(temp68$Sal00))
temp68$DepSM <- as.numeric(as.character(temp68$DepSM))
temp68$T068C <- as.numeric(as.character(temp68$T068C))

ctd_christy$T090C <- as.numeric(as.character(ctd_christy$T090C))
ctd_christy$PrSE <- as.numeric(as.character(ctd_christy$PrSE))
ctd_christy$Sal00 <- as.numeric(as.character(ctd_christy$Sal00))
ctd_christy$DepSM <- as.numeric(as.character(ctd_christy$DepSM))
ctd_christy$T068C <- as.numeric(as.character(ctd_christy$T068C))

## remove soaks by removing data that was taken above .5 m
christy_soak <- dplyr::filter(ctd_christy, DepSM>1)
christy_soak68 <- dplyr::filter(temp68, DepSM >1) 
 ## mutate(PrSE = PrSE*14.5037738) ## convert pressure from db to psi, when I do 
## this the stratification no longer looks right

## calculate density for Christy's data
christy_soak$Density <- swRho(salinity = christy_soak$Sal00, 
                                             temperature=christy_soak$T090C, 
                                             pressure = christy_soak$PrSE, 
                                             eos=c("unesco"))

christy_soak68$Density <- swRho(salinity = christy_soak68$Sal00, 
                                temperature=christy_soak68$T068C, 
                                pressure = christy_soak68$PrSE, 
                                eos=c("unesco"))

## bind back together
christy_soak_final <- rbind(christy_soak, christy_soak68)

## calculate the difference in sigma-t at each station for each date
sigma_t_christy <- christy_soak_final%>%
  na.omit()%>%
  dplyr::group_by(date) %>%
  dplyr::summarise(sigmat = max(Density) - min(Density)) 

ggplot(sigma_t_christy, aes(x = date, y = sigmat)) +
  geom_point()


## combine Christy's data with Amy's data
## first separate Christy date into year month day
christy_combine <- separate(sigma_t_christy, date, c("YrDay", "Month", "Year")) %>%
                    dplyr::select(-YrDay)

## there might be more than one cast occurring within each month of each year
## so I want to average the densities if this is the case
christy_sigmat_avg <- as.data.frame(christy_combine)
colnames(christy_sigmat_avg) <- c("Month", "Year", "christy_sigmat")


amy <- dplyr::select(sigma_t_amy, -YrDay) 
amy$Year <- as.character(amy$Year)
colnames(amy)<-c("Year", "Month", "sigmat.amy")

amy$Year <- gsub("1998", "98", amy$Year)
amy$Year <- gsub("1999", "99", amy$Year)
amy$Year <- gsub("2000", "00", amy$Year)
amy$Year <- gsub("2001", "01", amy$Year)
amy$Year <- gsub("2002", "02", amy$Year)
amy$Year <- gsub("2003", "03", amy$Year)
amy$Year <- gsub("2004", "04", amy$Year)
amy$Year <- gsub("2005", "05", amy$Year)
amy$Year <- gsub("2006", "06", amy$Year)
amy$Year <- gsub("2007", "07", amy$Year)
amy$Year <- gsub("2008", "08", amy$Year)
amy$Year <- gsub("2009", "09", amy$Year)
amy$Year <- gsub("2010", "10", amy$Year)
amy$Year <- gsub("2011", "11", amy$Year)
amy$Year <- gsub("2012", "12", amy$Year)
amy$Year <- gsub("2013", "13", amy$Year)
amy$Year <- gsub("2014", "14", amy$Year)
amy$Year <- gsub("2015", "15", amy$Year)
amy$Year <- gsub("2016", "16", amy$Year)
amy$Year <- gsub("2017", "17", amy$Year)


amy <- as.data.frame(amy)

## combine Amy's and Christy's data
amy_christy_sigmat <- dplyr::full_join(amy, christy_sigmat_avg)

## spread the mwra data
## remove blank rows
mwra_clean <- dplyr::distinct(ctd_mwra) %>%
  dplyr::filter(DEPTH != "NA") %>%
  dplyr::group_by(PARAM_CODE) %>%
  spread(PARAM_CODE, VALUE) %>%
  dplyr::filter(SIGMA_T != "NA")

sigma_t_mwra <- dplyr::group_by(mwra_clean, STAT_ARRIV) %>%
  dplyr::summarise(sigmat = max(SIGMA_T) - min(SIGMA_T)) %>%
  separate(STAT_ARRIV, c("Month", "Day", "Year", "TIME")) %>%
  dplyr::select(-Day, -TIME) %>%
  dplyr::ungroup()

sigma_t_mwra$Month <- as.numeric(sigma_t_mwra$Month)
sigma_t_mwra_month <- dplyr::mutate(sigma_t_mwra, month = month.abb[Month]) %>%
  dplyr::select(-Month)
colnames(sigma_t_mwra_month) <- c("Year", "mwra.sigmat", "Month")  


## bind the mwra data with amy's and christy's data 
ccb_strat <- dplyr::full_join(amy_christy_sigmat, sigma_t_mwra_month) %>%
  gather("index", "value", 3:5) %>%
  dplyr::group_by(Year, Month) %>%
  na.omit() %>%
  dplyr::summarise(mean.sigmat = mean(value))

ggplot(ccb_strat, aes(x = Month, y = mean.sigmat)) +
  geom_point() +
  facet_wrap(~Year) +
  scale_x_discrete(limits = c("Jan", "Feb", "Mar", "Apr", "May"))

## put in rows for missing values at 3/03, 1/05, 1/06, 1/07, 5/98
Year <- c("03", "05", "06", "07", "98")
Month <- c("Mar", "Jan", "Jan", "Jan", "May")
mean.sigmat <-(rep(NA, 5))

Year_name <- "Year"
Month_name <- "Month"
sigmat_name <- "mean.sigmat"
## missing vals data frame
df <- data.frame(Year, Month, mean.sigmat)
names(df) <- c(Year_name, Month_name, sigmat_name)

ccb_strat_missing <- rbind(df, as.data.frame(ccb_strat))
ccb_strat_missing$Year <- gsub("99", "1999", ccb_strat_missing$Year)
ccb_strat_missing$Year <- gsub("98", "1998", ccb_strat_missing$Year)
ccb_strat_missing$Year <- gsub("00", "2000", ccb_strat_missing$Year)
ccb_strat_missing$Year <- gsub("01", "2001", ccb_strat_missing$Year)
ccb_strat_missing$Year <- gsub("02", "2002", ccb_strat_missing$Year)
ccb_strat_missing$Year <- gsub("03", "2003", ccb_strat_missing$Year)
ccb_strat_missing$Year <- gsub("04", "2004", ccb_strat_missing$Year)
ccb_strat_missing$Year <- gsub("05", "2005", ccb_strat_missing$Year)
ccb_strat_missing$Year <- gsub("06", "2006", ccb_strat_missing$Year)
ccb_strat_missing$Year <- gsub("07", "2007", ccb_strat_missing$Year)
ccb_strat_missing$Year <- gsub("08", "2008", ccb_strat_missing$Year)
ccb_strat_missing$Year <- gsub("09", "2009", ccb_strat_missing$Year)
ccb_strat_missing$Year <- gsub("10", "2010", ccb_strat_missing$Year)
ccb_strat_missing$Year <- gsub("11", "2011", ccb_strat_missing$Year)
ccb_strat_missing$Year <- gsub("12", "2012", ccb_strat_missing$Year)
ccb_strat_missing$Year <- gsub("13", "2013", ccb_strat_missing$Year)
ccb_strat_missing$Year <- gsub("14", "2014", ccb_strat_missing$Year)
ccb_strat_missing$Year <- gsub("15", "2015", ccb_strat_missing$Year)
ccb_strat_missing$Year <- gsub("16", "2016", ccb_strat_missing$Year)
ccb_strat_missing$Year <- gsub("17", "2017", ccb_strat_missing$Year)

## arrange by year and month
ccb_strat_missing$Month <- gsub("Jan", 1, ccb_strat_missing$Month) 
ccb_strat_missing$Month <- gsub("Feb", 2, ccb_strat_missing$Month) 
ccb_strat_missing$Month <- gsub("Mar", 3, ccb_strat_missing$Month) 
ccb_strat_missing$Month <- gsub("Apr", 4, ccb_strat_missing$Month) 
ccb_strat_missing$Month <- gsub("May", 5, ccb_strat_missing$Month) 

ccb_arran <- dplyr::arrange(ccb_strat_missing, Year, Month)

## convert this to a timeseries in order to interpolate
ccb.strat.ts <- ts(ccb_arran)

## first difference the data
ccb.strat.ts[,3] %>%
  diff(lag = 5) %>%
  ggtsdisplay

## fit an arima model with the differenced data
##to then be smoothed with a kalman smoother
fit.ccb.strat <- auto.arima(ccb.strat.ts[,3], d = 1, approximation=F,stepwise=F) 
summary(fit.ccb.strat)
## check residuals
checkresiduals(fit.ccb.strat) ## acf looks good, Ljung-Box not so much

# With model 1
ccb.strat.interpolate <- na.kalman(ccb.strat.ts[,3], fit.ccb.strat$model)

## bind the interpolate data back with the time data 
ccb.strat.interpolate <- cbind(ccb.strat.interpolate, ccb.strat.ts)
colnames(ccb.strat.interpolate) <- c("strat.interpolate", "Year", "Month", "ccb.strat.mean.sigmat")

autoplot(ccb.strat.interpolate[,1], series="Interpolated") +
  autolayer(ccb.strat.interpolate[,4], series= "Original") +
  scale_color_manual(values=c(`Interpolated`="red",`Original`="gray"))

ccb.strat.interpolate <- as.data.frame(ccb.strat.interpolate)

local_strat <- ggplot(ccb.strat.interpolate, aes(x=Month, y = ccb.strat.mean.sigmat, color = "Original"))+
  geom_point(aes(x=Month, y = strat.interpolate, color = "Interpolated")) +
  geom_point() +
  facet_wrap(~Year) +
  theme(legend.position = c(0.06, 0.93), legend.title = element_blank())

##**********************************LOCAL SST**********************************
## I should be able to use the SST from the ctd data I received from Amy, Christy,
## and the MWRA.

## make Christy's data Year, Month, SST.  Right now, this takes
## the maximum temperature each day but I need to make sure that The max temp
## is what I want I would rather have the depth associated so I could tell
## it was at the surface, sometimes the max temp is not the first reading.
sst_christy <- dplyr::select(christy_soak_final, -cruise.name, -station, -PrSE, 
                      -Sal00, -Density) %>%
                      separate(date, c("dd", "mm", "yy")) %>%
  dplyr::group_by(yy, mm, dd) %>%
  dplyr::top_n(n = -1, wt = DepSM) %>%## get the most shallow depth for each day
  dplyr::group_by(yy, mm) %>% ## group by year and month
  dplyr::summarise(SST = mean(T068C)) ## get the mean sst for each month
colnames(sst_christy) <- c("Year", "month", "SST")
                

sst_amy <-  dplyr::mutate(ctd_amy, month = month.abb[Month]) %>%
  dplyr::group_by(Year, month, YrDay) %>%
  dplyr::filter(Depth..m. == max(Depth..m.)) %>% ## get the data at hte surface (amy's depths have negatives in fron tof them so I really want the maximum value)
  dplyr::ungroup()%>%
  dplyr::select(-YrDay, -Stn._6M, -Descent.Rate, -Salinity..PSU., 
                -Temperature..C..1, -Density..kg.m3., -PAR..µE.m2.s., 
                -Fluorescence..mg.m3., -Dissolved.Oxygen..mg.L.,
         -Dissolved.Oxygen....saturation., -Month, -Depth..m.)
colnames(sst_amy) <- c("Year", "SST", "month")

sst_amy$Year <- gsub("1998", "98", sst_amy$Year)
sst_amy$Year <- gsub("1999", "99", sst_amy$Year)
sst_amy$Year <- gsub("2000", "00", sst_amy$Year)
sst_amy$Year <- gsub("2001", "01", sst_amy$Year)
sst_amy$Year <- gsub("2002", "02", sst_amy$Year)
sst_amy$Year <- gsub("2003", "03", sst_amy$Year)
sst_amy$Year <- gsub("2004", "04", sst_amy$Year)
sst_amy$Year <- gsub("2005", "05", sst_amy$Year)
sst_amy$Year <- gsub("2006", "06", sst_amy$Year)
sst_amy$Year <- gsub("2007", "07", sst_amy$Year)
sst_amy$Year <- gsub("2008", "08", sst_amy$Year)
sst_amy$Year <- gsub("2009", "09", sst_amy$Year)
sst_amy$Year <- gsub("2010", "10", sst_amy$Year)
sst_amy$Year <- gsub("2011", "11", sst_amy$Year)
sst_amy$Year <- gsub("2012", "12", sst_amy$Year)
sst_amy$Year <- gsub("2013", "13", sst_amy$Year)
sst_amy$Year <- gsub("2014", "14", sst_amy$Year)
sst_amy$Year <- gsub("2015", "15", sst_amy$Year)
sst_amy$Year <- gsub("2016", "16", sst_amy$Year)
sst_amy$Year <- gsub("2017", "17", sst_amy$Year)

  
sst_mwra <- dplyr::distinct(ctd_mwra) %>%
  dplyr::filter(DEPTH != "NA") %>% ## get rid of data that has depth = NA
  dplyr::group_by(PARAM_CODE) %>% 
  spread(PARAM_CODE, VALUE) %>% ## make the data wide not long
  dplyr::filter(TEMP != "NA") %>% ## only keep rows that have a value for temperature
  separate(STAT_ARRIV, c("Month", "Day", "Year", "TIME")) %>% ## get the date values separated
  dplyr::select(-STUDY_ID, -EVENT_ID, - STAT_ID, -PROF_DATE_TIME,
                -VAL_QUAL, -UNIT_CODE, -METH_CODE, -INSTR_CODE,
                -FILENAME, -LOGIN_ID, -DATE_TIME_STAMP, -CDOM, -CONDTVY,
                -DISS_OXYGEN, -DO_RAW, -FLU_OLD, -FLU_RAW, -FLUORESCENCE,
                -LIGHT, -PCT_SAT, -SAL, -SIGMA_T, -SURFACE_IRRAD, -TRANS) %>% ## remove crap
  dplyr::mutate(month = month.abb[as.numeric(Month)]) %>% ## convert months from numeric to abbrevs
  dplyr::select(-Month, -LATITUDE, -LONGITUDE, -TIME, -COMMENTS) %>% ## get rid of more crap
  dplyr::group_by(Year, month, Day) %>%
  dplyr::filter(DEPTH == min(DEPTH)) %>% ## get the surface values (mwra doesn't have negatives in front of depth data so I can take the minimum value)
  dplyr::group_by(Year, month)%>%
  dplyr::summarise(meansst = mean(TEMP))## if there is more than one value temperature value per month I want the mean value
colnames(sst_mwra) <- c("Year", "month", "SST")

## bind amy's sst data with the mwra sst data
sst_mwra <- as.data.frame(sst_mwra)
sst_amy <- as.data.frame(sst_amy)
sst_christy <- as.data.frame(sst_christy)
amy_mwra_christy_sst <- rbind(sst_mwra, sst_amy, sst_christy)

ggplot(amy_mwra_christy_sst, aes(x = month, y = SST)) +
  geom_point() +
  facet_wrap(~Year) +
  scale_x_discrete(limits = c("Jan", "Feb", "Mar", "Apr", "May"))

##*******************************REGIONAL CALANUS DATA**************************
## I need to see if the calanus data from ecomon in southern new england and
## the mid atlantic is a good proxy for what is going on in northern GOM.  I dont
## have data from northern gom for every month of every year which is ideally where
## I would like the calanus data to come from.  So, I need to determine if the
## southern data is a reasonable stand in.  I also need to ensure that CPR data
## are correlated with ecomon data since they were collected by two different
## devices.

## take the spatial data from "plotting_sem_stations.R" and convert ot data frame
southern_inside.df <- as.data.frame(southern_inside)
gom_ideal.df <- as.data.frame(gom_ideal_outside)

## remove some columns I don't need
southern_inside.select <- southern_inside.df %>%
                  dplyr::select(Yearmonth, calfin_100m3) %>%
  separate(Yearmonth, c("year", "month"))
names(southern_inside.select) <- c("Year", "month", "calfin_100m3_southern")

gom_ideal.select <- gom_ideal.df %>%
  dplyr::select(Yearmonth, calfin_100m3) %>%
  separate(Yearmonth, c("year", "month"))
names(gom_ideal.select) <- c("Year", "month", "calfin_100m3_northern")

## adjust year column
southern_inside.select$Year <- gsub("99", "1999", southern_inside.select$Year)
southern_inside.select$Year <- gsub("98", "1998", southern_inside.select$Year)
southern_inside.select$Year <- gsub("00", "2000", southern_inside.select$Year)
southern_inside.select$Year <- gsub("01", "2001", southern_inside.select$Year)
southern_inside.select$Year <- gsub("02", "2002", southern_inside.select$Year)
southern_inside.select$Year <- gsub("03", "2003", southern_inside.select$Year)
southern_inside.select$Year <- gsub("04", "2004", southern_inside.select$Year)
southern_inside.select$Year <- gsub("05", "2005", southern_inside.select$Year)
southern_inside.select$Year <- gsub("06", "2006", southern_inside.select$Year)
southern_inside.select$Year <- gsub("07", "2007", southern_inside.select$Year)
southern_inside.select$Year <- gsub("08", "2008", southern_inside.select$Year)
southern_inside.select$Year <- gsub("09", "2009", southern_inside.select$Year)
southern_inside.select$Year <- gsub("10", "2010", southern_inside.select$Year)
southern_inside.select$Year <- gsub("11", "2011", southern_inside.select$Year)
southern_inside.select$Year <- gsub("12", "2012", southern_inside.select$Year)
southern_inside.select$Year <- gsub("13", "2013", southern_inside.select$Year)
southern_inside.select$Year <- gsub("14", "2014", southern_inside.select$Year)
southern_inside.select$Year <- gsub("15", "2015", southern_inside.select$Year)
southern_inside.select$Year <- gsub("16", "2016", southern_inside.select$Year)
southern_inside.select$Year <- gsub("17", "2017", southern_inside.select$Year)

## adjust year column
gom_ideal.select$Year <- gsub("99", "1999", gom_ideal.select$Year)
gom_ideal.select$Year <- gsub("98", "1998", gom_ideal.select$Year)
gom_ideal.select$Year <- gsub("00", "2000", gom_ideal.select$Year)
gom_ideal.select$Year <- gsub("01", "2001", gom_ideal.select$Year)
gom_ideal.select$Year <- gsub("02", "2002", gom_ideal.select$Year)
gom_ideal.select$Year <- gsub("03", "2003", gom_ideal.select$Year)
gom_ideal.select$Year <- gsub("04", "2004", gom_ideal.select$Year)
gom_ideal.select$Year <- gsub("05", "2005", gom_ideal.select$Year)
gom_ideal.select$Year <- gsub("06", "2006", gom_ideal.select$Year)
gom_ideal.select$Year <- gsub("07", "2007", gom_ideal.select$Year)
gom_ideal.select$Year <- gsub("08", "2008", gom_ideal.select$Year)
gom_ideal.select$Year <- gsub("09", "2009", gom_ideal.select$Year)
gom_ideal.select$Year <- gsub("10", "2010", gom_ideal.select$Year)
gom_ideal.select$Year <- gsub("11", "2011", gom_ideal.select$Year)
gom_ideal.select$Year <- gsub("12", "2012", gom_ideal.select$Year)
gom_ideal.select$Year <- gsub("13", "2013", gom_ideal.select$Year)
gom_ideal.select$Year <- gsub("14", "2014", gom_ideal.select$Year)
gom_ideal.select$Year <- gsub("15", "2015", gom_ideal.select$Year)
gom_ideal.select$Year <- gsub("16", "2016", gom_ideal.select$Year)
gom_ideal.select$Year <- gsub("17", "2017", gom_ideal.select$Year)

## get the means within each month and year for north and south data
gom_ideal_mean <- dplyr::group_by(gom_ideal.select, Year, month) %>%
  na.omit()%>%
  dplyr::summarise(mean_calfin_north = mean(calfin_100m3_northern))

south_mean <- dplyr::group_by(southern_inside.select, Year, month)%>%
  na.omit()%>%
  dplyr::summarise(mean_calfin_south = mean(calfin_100m3_southern))
## combine the data sets so i have one table with the mean for north
## and mean for south for each month of each year
south_north_mean <- dplyr::full_join(gom_ideal_mean, south_mean)

ggplot(as.data.frame(south_north_mean), aes(x=mean_calfin_north, y = mean_calfin_south)) +
  geom_point() +
  geom_smooth(method = "lm")

south_north_meanlm <- lm(mean_calfin_south ~ mean_calfin_north, south_north_mean)
plot(south_north_meanlm)
summary(south_north_meanlm) ## solange said to try a curve fitting
## to this like a polynomial. she said this is what they do for
## fish age at certain lengths.


## are the CPR data correlated with the calanus north and calanus south data?
## cpr_unite is the dataframe I worked up in plotting_sem_stations.R to 
## only keep data from 1998 - 2017

cpr_reduced <- cpr_unite %>%
  dplyr::select(Yearmonth, X.Calanus.finmarchicus..7) %>%
  tidyr::separate(Yearmonth, c("YYYY", "mm"))
cpr_reduced$mm <- as.numeric(as.character(cpr_reduced$mm))

cpr_reduced <- cpr_reduced%>%
  dplyr::mutate(month = month.abb[mm]) %>%
  dplyr::select(-mm) 
cpr_reduced$X.Calanus.finmarchicus..7 <- as.numeric(as.character(cpr_reduced$X.Calanus.finmarchicus..7))

names(cpr_reduced) <- c("Year", "calfin_cpr", "month")

cpr_monthly_means <- cpr_reduced %>%
  dplyr::group_by(Year, month) %>%
  dplyr::summarise(mean_cpr = mean(calfin_cpr))

cpr_ecomon <- dplyr::full_join(south_north_mean, cpr_monthly_means)
  

## check to see if the cpr data are a good indicator of the ecomon north data
cpr_ecomon_north_lm <- lm(mean_calfin_north ~ mean_cpr, cpr_ecomon)
plot(cpr_ecomon_north_lm) ## goodness of fit is better for this model
summary(cpr_ecomon_north_lm) ## cpr no longer significant indicator of ecomon

## pull out the coefs to put on a plot
a <- signif(coef(cpr_ecomon_north_lm)[1], digits = 2)
b <- signif(coef(cpr_ecomon_north_lm)[2], digits = 2)
c <- signif(summary(cpr_ecomon_north_lm)$adj.r.squared, digits = 4)
textlab <- paste("y = ",b,"x + ",a, sep="")
textlab2 <- paste("adj. r^2 = ",c, sep = "")

## plot the cpr data vs the ecomon data north
cpr_ecomon_north <- ggplot(cpr_ecomon, aes(y = mean_calfin_north, x = mean_cpr)) +
  geom_point() +
  annotate("text", x = 10000, y = 15000, label = textlab, color="black", 
           size = 5, parse=FALSE) +
  annotate("text", x = 10000, y = 10000, label = textlab2, color = "black",
           size = 5, parse = FALSE) +
  stat_smooth(method = "lm") +
  xlab("Monthly means of Calanus finmarchicus/100m3 from CPR") +
  ylab("Monthly means of Calanus finmarchicus/100m3 from EcoMon")

## the data are not normally distributed.  So, i thought of doing a glm with poisson
## distribution.  but since these are means they are not discrete values and R doens't
## like having a poisson distribution with non-discrete data.
##cpr_ecomon_north_glm <- glm(mean_calfin_north ~ mean_cpr, cpr_ecomon, family = "poisson")
##plot(cpr_ecomon_north_lm)
##summary(cpr_ecomon_north_lm) ## The cpr data is a significant indicator of the
## ecomon north data but the adjusted r squared is low .3

## remove the data above 10000 and see if the relationship still sticks?
cpr_ecomon_noout <- dplyr::filter(cpr_ecomon, mean_cpr <10000)

ggplot(cpr_ecomon_noout, aes(y = mean_calfin_north, x = mean_cpr)) +
  geom_point() +
  stat_smooth(method = "lm")

cpr_ecomon_north_lm_noout <- lm(mean_calfin_north ~ mean_cpr, cpr_ecomon_noout)
plot(cpr_ecomon_north_lm_noout) ## goodness of fit is better for this model
summary(cpr_ecomon_north_lm_noout) ## cpr no longer significant indicator of ecomon


##plot the cpr data vs the ecomon south 
ggplot(cpr_ecomon, aes(y = mean_calfin_south, x = mean_cpr)) +
  geom_point() +
  stat_smooth(method = "lm")

cpr_ecomon_south_lm <- lm(mean_calfin_south ~ mean_cpr, 
                          cpr_ecomon)
plot(cpr_ecomon_south_lm)
summary(cpr_ecomon_south_lm) ## The cpr data is not a significant indicator
## of the southern data but it has a major outlier

## remove the outlier (row 2) and try again
cpr_ecomon_nooutlier <- cpr_ecomon[-c(2), ]   
ggplot(cpr_ecomon_nooutlier, aes(y = mean_calfin_south, x = mean_cpr)) +
  geom_point() +
  stat_smooth(method = "lm")

cpr_ecomon_south_noout_lm <- lm(mean_calfin_south ~ mean_cpr, 
                          cpr_ecomon_nooutlier)
plot(cpr_ecomon_south_noout_lm)
summary(cpr_ecomon_south_noout_lm) ## with out the outlier the cpr data
## is a significant indicator of the southern ecomon data with an 
## R2 of .1181.
  

## the means are not normally distributed which maybe creating some problems.  Maybe
## trying the raw (not means) data will help?
gom_north_cpr_raw <- dplyr::full_join(gom_ideal.select, cpr_reduced)
ggplot(gom_north_cpr_raw, aes(y = calfin_100m3_northern, x = calfin_cpr)) +
  geom_point() +
  stat_smooth(method = "glm")

cpr_ecomon_north_glm_raw <- glm(calfin_100m3_northern ~ calfin_cpr, gom_north_cpr_raw,
                                family = "poisson")
## the residual deviance is much greater than the df (overdispersion)
##try a different error structure

cpr_ecomon_north_glm_raw_quasi <- glm(calfin_100m3_northern ~ calfin_cpr, gom_north_cpr_raw,
                                      family = "quasipoisson")
## we need to do an f test to determine relatinoships
## calfin_cpr is still a significant indicator of ecomon north
anova(cpr_ecomon_north_glm_raw_quasi, test = "F")


##*****************************REGIONAL SFC TEMP DATA***********************
## the ecomon data has surface temp at each station
## isolate the columns I need
southern_inside.select.sst <- southern_inside.df %>%
  dplyr::select(Yearmonth, sfc_temp) %>%
  separate(Yearmonth, c("year", "month"))
names(southern_inside.select.sst) <- c("Year", "month", "sfc_temp")

gom_ideal.select.sst <- gom_ideal.df %>%
  dplyr::select(Yearmonth, sfc_temp) %>%
  separate(Yearmonth, c("year", "month"))
names(gom_ideal.select.sst) <- c("Year", "month", "sfc_temp")

## get the mean sst within each month of each year for both north and south 
gom_ideal_meansst <- dplyr::group_by(gom_ideal.select.sst, Year, month) %>%
  na.omit()%>%
  dplyr::summarise(mean_sst.north = mean(sfc_temp))

south_meansst <- dplyr::group_by(southern_inside.select.sst, Year, month)%>%
  na.omit()%>%
  dplyr::summarise(mean_sst.south = mean(sfc_temp))

## combine the data sets so i have one table with the mean sst for north
## and mean for south for each month of each year
south_north_meansst <- dplyr::full_join(gom_ideal_meansst, south_meansst)

ggplot(south_north_meansst, aes(x=mean_sst.north, y = mean_sst.south)) +
  geom_point() +
  geom_smooth(method = "lm")

south_north_mean.sst.lm <- lm(mean_sst.south ~ mean_sst.north, south_north_meansst)
plot(south_north_mean.sst.lm)
summary(south_north_mean.sst.lm)

## adjust year column
south_north_meansst$Year <- gsub("99", "1999", south_north_meansst$Year)
south_north_meansst$Year <- gsub("98", "1998", south_north_meansst$Year)
south_north_meansst$Year <- gsub("00", "2000", south_north_meansst$Year)
south_north_meansst$Year <- gsub("01", "2001", south_north_meansst$Year)
south_north_meansst$Year <- gsub("02", "2002", south_north_meansst$Year)
south_north_meansst$Year <- gsub("03", "2003", south_north_meansst$Year)
south_north_meansst$Year <- gsub("04", "2004", south_north_meansst$Year)
south_north_meansst$Year <- gsub("05", "2005", south_north_meansst$Year)
south_north_meansst$Year <- gsub("06", "2006", south_north_meansst$Year)
south_north_meansst$Year <- gsub("07", "2007", south_north_meansst$Year)
south_north_meansst$Year <- gsub("08", "2008", south_north_meansst$Year)
south_north_meansst$Year <- gsub("09", "2009", south_north_meansst$Year)
south_north_meansst$Year <- gsub("10", "2010", south_north_meansst$Year)
south_north_meansst$Year <- gsub("11", "2011", south_north_meansst$Year)
south_north_meansst$Year <- gsub("12", "2012", south_north_meansst$Year)
south_north_meansst$Year <- gsub("13", "2013", south_north_meansst$Year)
south_north_meansst$Year <- gsub("14", "2014", south_north_meansst$Year)
south_north_meansst$Year <- gsub("15", "2015", south_north_meansst$Year)
south_north_meansst$Year <- gsub("16", "2016", south_north_meansst$Year)
south_north_meansst$Year <- gsub("17", "2017", south_north_meansst$Year)


mean_reg_sst <- gather(south_north_meansst, "region", "value", 3:4)
ggplot(mean_reg_sst, aes(x=month, y = value)) +
  geom_point()+
  facet_wrap(~Year)


## using the bedford institute data might be better for this metric.  I could
## average over all of the stations.

sst_bed_inst <- dplyr::select(bedford_reg_strat, 
                              YEAR, MONTH, DEPTH, AVG_TEMPERATURE) %>%
  dplyr::group_by(YEAR, MONTH) %>%
  dplyr::top_n(n = -1, wt = DEPTH) %>%## get the most shallow depth for each day
  dplyr::summarise(SST = mean(AVG_TEMPERATURE)) %>% ## get the mean sst for each month
  dplyr::mutate(month = month.abb[MONTH])  %>%
  dplyr::select(-MONTH)

colnames(sst_bed_inst) <- c("Year", "mean_sst.bed.inst", "month")
sst_bed_inst$Year <- as.character(sst_bed_inst$Year)
sst_bed_inst$month <- as.character(sst_bed_inst$month)

ecomon_bedinst.sst <- dplyr::full_join(south_north_meansst, sst_bed_inst)

ggplot(ecomon_bedinst.sst, aes(x=mean_sst.north, y = mean_sst.bed.inst)) +
  geom_point() +
  geom_smooth(method = "lm")

ecomon_bedinst_gather <- gather(ecomon_bedinst.sst, "source", "value", 3:5)
ggplot(ecomon_bedinst_gather, aes(x=month, y=value, colour = source)) +
  geom_point() +
  facet_wrap(~Year)


##*******************************CCB ZPL DATA********************************
## read in surface tows from regular stations
sfc_stations <- read.csv("/Users/laura.ganley001/Documents/R_Projects/SEM/raw_data/local_zpl/surface_ccb_regular_stations.csv")
## read in the oblique tows from regular stations
oblq_stations <- read.csv("/Users/laura.ganley001/Documents/R_Projects/SEM/raw_data/local_zpl/oblique_ccb_regular_stations.csv")

## convert factors to numerics
sfc_stations$AvgOftotal_zpl.m3 <- as.numeric(as.character(sfc_stations$AvgOftotal_zpl.m3))
sfc_stations$Avg_Of_calanus_total.m3 <- as.numeric(as.character(sfc_stations$Avg_Of_calanus_total.m3))
sfc_stations$Avg_Of_pseudo_spp_total.m3 <- as.numeric(as.character(sfc_stations$Avg_Of_pseudo_spp_total.m3))
sfc_stations$Avg_Of_centropages_spp_total.m3 <- as.numeric(as.character(sfc_stations$Avg_Of_centropages_spp_total.m3))
oblq_stations$Avg_Of_total_zpl.m3 <- as.numeric(as.character(oblq_stations$Avg_Of_total_zpl.m3))
oblq_stations$Avg_Of_calanus_total.m3 <- as.numeric(as.character(oblq_stations$Avg_Of_calanus_total.m3))
oblq_stations$Avg_Of_pseudo_spp_total.m3 <- as.numeric(as.character(oblq_stations$Avg_Of_pseudo_spp_total.m3))
oblq_stations$Avg_Of_centropages_spp_total.m3 <- as.numeric(as.character(oblq_stations$Avg_Of_centropages_spp_total.m3))

colnames(sfc_stations) <- c("Year", "Month", "MeanTotalZplm3sfc", "MeanCalanusm3sfc", "MeanPseudom3sfc", "MeanCentrom3sfc")
colnames(oblq_stations) <- c("Year", "Month", "MeanTotalZplm3oblq", "MeanCalanusm3oblq", "MeanPseudom3oblq", "MeanCentrom3oblq")
oblq_sfc <- dplyr::full_join(oblq_stations, sfc_stations)
oblq_sfc$Month <- as.character(oblq_sfc$Month)

## Determine if there is a relationship between surface tows and oblique tows
## for each zpl measure.
ggplot(oblq_sfc, aes(x=MeanTotalZplm3sfc, y = MeanTotalZplm3oblq)) +
  geom_point() + 
  geom_smooth(method = "lm") 

mtz_sfc_oblq_lm <- lm(MeanTotalZplm3oblq ~ MeanTotalZplm3sfc, oblq_sfc)

  
ggplot(oblq_sfc, aes(x=MeanCalanusm3sfc, y = MeanCalanusm3oblq)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(~Month)

calanus_sfc_oblq_lm <- lm(MeanCalanusm3oblq ~ MeanCalanusm3sfc, oblq_sfc)

ggplot(oblq_sfc, aes(x=MeanPseudom3sfc, y = MeanPseudom3oblq)) +
  geom_point() +
  geom_smooth(method = "lm") 

pseudo_sfc_oblq_lm <- lm(MeanPseudom3oblq ~ MeanPseudom3sfc, oblq_sfc)

ggplot(oblq_sfc, aes(x=MeanCentrom3sfc, y =  MeanCentrom3oblq)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(~Month)

centro_sfc_oblq_lm <- lm(MeanCentrom3oblq ~ MeanCentrom3sfc, oblq_sfc)

## plot monthly calanus
ggplot(oblq_sfc, aes(x=Month, y = MeanCalanusm3sfc)) +
  geom_point() +
  facet_wrap(~Year) +
  scale_x_discrete(limits=c(1,2,3, 4, 5, 6, 7, 8, 9, 10, 11, 12))


##***************************DETERMINE LAG BETWEEN CPR AND CCB DATA********
## filter cpr data for just post 1998
cpr_year <-  dplyr::filter(cpr, Year >= "1998") %>%
  dplyr:: mutate(longitude = Longitude*(-1))
## get rid of extra columns
cpr_select <- cpr_year[ , 1:33]
## remove a few more extra columns
cpr_select_more <- dplyr::select(cpr_select, -Cruise, -Station, -Day, -Hour,
                                 -Minute, -Latitude, -Longitude, -X.Phytoplankton.Color.Index.,
                                 -X.Unidentified.plankton.and.fragments.)


## get the mean of all the cpr stations per month
cpr_select_more$X.Calanus.finmarchicus..7 <- as.numeric(as.character(cpr_select_more$X.Calanus.finmarchicus..7))
## remove -9999 which are counts for taxa that are observed in sample but not quantified during
## standard analysis see Richardson et al 2006 for a detailed description of CPR
## methods
cpr_means <- cpr_select_more %>%
  dplyr::filter(X.Calanus.finmarchicus..7 != -9999) %>%
  dplyr::group_by(Year, Month) %>% 
  dplyr::summarise(mean_calanus_cpr = mean(X.Calanus.finmarchicus..7)) ## get the mean 
## calanus abundance for each month of each year

## combine CPR and christy's data into one dataframe
cpr_christy <- as.data.frame(dplyr::full_join(cpr_means, sfc_stations) %>%
  dplyr::select(Year, Month, mean_calanus_cpr, MeanCalanusm3sfc)) %>%
  dplyr::mutate(MeanCalanus100m3sfc = MeanCalanusm3sfc*10) ## convert Christy's data
## so that it is calanus abundance per 100 m3 to match the CPR data

cpr_christy_order <- cpr_christy[with(cpr_christy, order(Year, Month)), ]
cpr_christy_order <- dplyr::arrange(cpr_christy, Year, Month)
index <- seq(1:240)
cpr_christy_index_order <- cbind(cpr_christy_order, index)

ggplot(cpr_christy_index_order, aes(x = index, y = mean_calanus_cpr)) +
  geom_line() +
  geom_line(aes(x = index, y = MeanCalanus100m3sfc), color = "red") +
  xlab("Year") + ylab("") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

## impute missing values for CPR data.  first convert to a ts object
cpr_ts <- dplyr::select(cpr_christy_index_order, -Year, -Month, -index, -MeanCalanusm3sfc, -MeanCalanus100m3sfc)
cpr.ts <- ts(cpr_ts)
ccb.calanus.ts <- dplyr::select(cpr_christy_index_order, -Year, -Month, -index, -mean_calanus_cpr, -MeanCalanusm3sfc)
ccb.calanus.ts <- ts(ccb.calanus.ts)

## interpolate CPR data using a Kalman filter.  StrucTS is using a structural
## model fit by maximum likelihood (the other option is to fit an arima first
## and use that arima model).  smooth = true is used for interpolation, smooth =
## false is used for extrapolation.
## david helped me with the arima model (see timeseries_help_from_david.R for more
## details). 

## I also tried with the non-log transformed data
## first difference the data

cpr.ts %>%
  diff(lag = 12) %>%
  ggtsdisplay

## fit an arima model 
##to then be smoothed with a kalman smoother
fit.cpr <- auto.arima(cpr.ts, D = 1, approximation=F,stepwise=F) 
summary(fit.cpr)
## check residuals
checkresiduals(fit.cpr) ## acf looks good Ljung-Box is alright (.045)

# With model 1
cpr.interpolate <- na.kalman(cpr.ts, fit.cpr$model, smooth = TRUE)

autoplot(cpr.interpolate, series="Interpolated") +
  autolayer(cpr.ts, series= "Original") +
  scale_color_manual(values=c(`Interpolated`="red",`Original`="gray"))


## interpolate CCB data using Kalman filter
fitccbarima <- auto.arima(ccb.calanus.ts, d = 1, approximation = F, stepwise = F)
## Inspect residuals
checkresiduals(fitccbarima)
##plot(fitccbarima) # no significant trend
##qqnorm(fitccbarima) # not great, log transform?
##acf(fitccbarima,na.action = na.pass) 
##pacf(fitccbarima,na.action = na.pass) 
#partial autocorrelations at lag 4

ccb_interpolation <- na.kalman(ccb.calanus.ts, model = fitccbarima$model, smooth = TRUE)

autoplot(ccb_interpolation, series = "Interpolated") +
  autolayer(ccb.calanus.ts, series = "Original") +
  scale_color_manual(values = c('Interpolated' = "red", 'Original' = "gray"))

## bind together ccb original, ccb interpolated, and date info
ccb.calanus.orig.interp <- as.data.frame(cbind(ccb_interpolation, ccb.calanus.ts, cpr_christy_index_order$Year,
                                               cpr_christy_index_order$Month))
colnames(ccb.calanus.orig.interp) <-c("ccb_calanus_interpolation", "ccb_calanus_orig", "Year", "Month")
ccb.calanus.orig.interp$Month <- as.character(ccb.calanus.orig.interp$Month)


## plot the original and interpolated (non log transformed) ccb calanus data
ccb.calanus.orig.interp.plot.non <- ggplot(ccb.calanus.orig.interp, aes(x=Month, y=ccb_calanus_interpolation, color = "interpolated"))+
  geom_point() +
  geom_point(aes(x=Month, y = ccb_calanus_orig, color = "original")) +
  facet_wrap(~Year) +
  xlab("Month") + ylab("CCB monthly means of Calanus finmarchicus/100m3") +
  scale_x_discrete(limits = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12")) +
  theme(legend.position = c(0.95, 0.14), legend.title = element_blank())


## look for lags.  
## exclude after May 2013 because that is when the CPR data ends
cpr_interp_2013 <- ts(cpr.interpolate[1:185, ])
ccb_interp_2013 <- ts(ccb_interpolation[1:185, ])


## there is seasonality in the data that needs to be differenced.  First differencing
## didn't fix the problem for ccb data.  So, I seasonally differenced the 
## ccb data (lag of 12).
ccb_interp_2013 %>%
  diff(12) %>%
  ggtsdisplay

ccb_interp_2013_diff <- diff(ccb_interp_2013, 12)
stats::acf(ccb_interp_2013_diff)  ## i think this needs to be taken with a grain of salt
adf.test(ccb_interp_2013_diff) ## p-value less than .05 indicates data are differenced properly
Box.test(ccb_interp_2013_diff, type="Ljung-Box") ## pvalue more than .05 also indicates data are differeneced properly
kpss.test(ccb_interp_2013_diff) ## p-value more than .05 also indicates data are differenced properly

## difference the cpr data
cpr_interp_2013 %>%
  diff(12) %>%
  ggtsdisplay

cpr_interp_2013_diff <- diff(cpr_interp_2013, 12)
stats::acf(cpr_interp_2013_diff)  
adf.test(cpr_interp_2013_diff) ## p-value less than .05 indicates data are differenced properly
Box.test(cpr_interp_2013_diff, type="Ljung-Box") ## pvalue less than .05 indicates data are not differeneced properly
## Solange and Jarrett thinks that this is good enough.
kpss.test(cpr_interp_2013_diff) ## p-value greater than .05 indicates data are differenced properly

## bind the two differenced ts objects together
ccb_cpr_interp_2013_diff <- cbind(ccb_interp_2013_diff, cpr_interp_2013_diff)
## bind the non-differenced ts objects together 
ccb_cpr_interp_2013 <- cbind(ccb_interp_2013, cpr_interp_2013)


############################
##LOG TRANSFORMED CALANUS DATA (LOCAL AND REGIONAL)
############################

## try transforming the data and re-doing the above (interpolating and search
## for lags).
# Find best Box-Cox transformation of cpr data 
BoxCox.lambda(ccb.calanus.ts)
# Best lambda is almost zero --> logarithmic transform
logccb = log(ccb.calanus.ts)
logccb[is.infinite(logccb)] <- NA
stats::acf(logccb, na.action = na.pass)


## Find best ARIMA model for cpr log-transformed ts (w.r.t. AICc)
fitlogccb <- auto.arima(logccb,approximation=F,stepwise=F)
summary(fitlogccb)
# Best model = ARIMA(5,0,0)

## Inspect residuals 
elogccb = residuals(fitlogccb)
plot(elogccb) # no trend
qqnorm(elogccb) # normality assumption ok
stats::acf(elogccb,na.action = na.pass) 
# Mildly significant autocorrelation at lags 3 & 15. Not too worrisome
pacf(elogccb,na.action = na.pass) 
# Significant partial autocorrelations at lags 3. 
# Note: ACF and PACF must be taken with a grain of salt b/c 
# of the many missing values

## Model accuracy
accuracy(fitlogccb) # in-sample
f = function(x,h) forecast(Arima(x,c(5,0,0)),h=h)
e = tsCV(logccb,f) # 1-step ahead forecasting errors (CV)
mean(e,na.rm=T) # mean error (ME) 
mean(abs(e),na.rm=T) # mean absolute error (MAE)
mean(abs(e/logccb),na.rm=T) # mean absolute percentage error (MAPE)
# ?????

##impute missing CCB data with log transformed data 
# With model 1
x.imputeccb = exp(na.kalman(logccb, fitlogccb$model))

# Plot CCB  (linear scale)
autoplot(ts(x.imputeccb), series="Interpolated") +
  autolayer(ccb.calanus.ts, series= "Original") +
  scale_color_manual(values=c(`Interpolated`="red",`Original`="gray")) +
  ylab("CCB data (log transformed before interpolation)")


## try a forecast for fun
x.forecast <- auto.arima(x.imputeccb)
x.forecast %>%
  forecast(h=5) %>%
  autoplot


## bind the interpolated data back to the original dataframe so the month 
##and year data are there
x.imputeccb <- as.data.frame(x.imputeccb)
colnames(x.imputeccb) <- c("InterpMeanCalanus100m3sfc")
log.interp.orig <- cbind(cpr_christy_index_order, x.imputeccb)


## plot log transformed interpolated ccb calanus by month and year
ccb.calanus.orig.interp.plot <- ggplot(log.interp.orig, 
                                       aes(x=Month,
                                           y= InterpMeanCalanus100m3sfc,
                                           color = "Interpolated"))+
  geom_point() +
  geom_point(aes(x=Month, y = MeanCalanus100m3sfc, color = "Original")) +
  facet_wrap(~Year) +
  xlab("Month") + ylab("CCB monthly means of Calanus finmarchicus/100m3") +
  scale_x_discrete(limits = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12")) +
  theme(legend.position = c(0.95, 0.14), legend.title = element_blank())



log.interp.orig <- as.data.frame(log.interp.orig)

## Log transform the CPR data
# Find best Box-Cox transformation of cpr data 
BoxCox.lambda(cpr.ts)
# Best lambda is almost zero --> logarithmic transform
logx = log(cpr.ts)
logx[is.infinite(logx)] = NA

## Find best ARIMA model for cpr log-transformed ts (w.r.t. AICc)
fit1 <- auto.arima(logx,approximation=F,stepwise=F)
summary(fit1)
# Best model = ARIMA(4,1,1)

## Inspect residuals 
e = residuals(fit1)
plot(e) # no trend
qqnorm(e) # normality assumption ok
stats::acf(e,na.action = na.pass) 
# Mildly significant autocorrelation at lags 5 & 13. Not too worrisome
pacf(e,na.action = na.pass) 
# Significant partial autocorrelations at lags 3 & 13. 
# Note: ACF and PACF must be taken with a grain of salt b/c 
# of the many missing values

## Model accuracy
accuracy(fit1) # in-sample
f = function(x,h) forecast(Arima(x,c(4,1,1)),h=h)
e = tsCV(logx,f) # 1-step ahead forecasting errors (CV)
mean(e,na.rm=T) # mean error (ME) 
mean(abs(e),na.rm=T) # mean absolute error (MAE)
mean(abs(e/logx),na.rm=T) # mean absolute percentage error (MAPE)
# No big degradation from in-sample to out-of-sample accuracy: good

##impute missing cpr data with log transformed data 
# With model 1
x.impute1 = exp(na.kalman(logx,fit1$model))

# Plot cpr  (linear scale)
autoplot(ts(x.impute1), series="Interpolated") +
  autolayer(cpr.ts, series= "Original") +
  scale_color_manual(values=c(`Interpolated`="red",`Original`="gray")) +
  ylab("CPR data (log transformed before interpolation)")

autoplot(ts(x.impute1), series="Regional Calanus finmarchicus") +
  ylab("Regional Calanus finmarchicus abundance/100m^3") +
  theme(legend.position="none") +
  coord_cartesian(xlim=c(0,200))


## bind the interpolated data back to the original dataframe so the month 
##and year data are there
x.impute1 <- as.data.frame(x.impute1)
log.interp.orig.cpr <- cbind(log.interp.orig, x.impute1)
colnames(log.interp.orig.cpr) <- c("Year", "Month", "mean_calanus_cpr", "MeanCalanusm3sfc",
                                   "MeanCalanus100m3sfc", "index", "logtransMeanCalanus100m3sfcinterp", "logtranscpr_interp")

## plot log transformed interpolated cpr calanus by month and year
cpr.calanus.orig.interp.plot <- ggplot(log.interp.orig.cpr, 
                                       aes(x=Month,
                                           y= logtranscpr_interp,
                                           color = "Interpolated"))+
  geom_point() +
  geom_point(aes(x=Month, y = mean_calanus_cpr, color = "Original")) +
  facet_wrap(~Year) +
  xlab("Month") + ylab("CPR monthly means of Calanus finmarchicus/100m3") +
  scale_x_discrete(limits = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12")) +
  theme(legend.position = c(0.07, 0.92))

cpr.ccb.plot <- ggplot(log.interp.orig.cpr, aes(x = Month, y = logtranscpr_interp,
                                                color = "CPR")) +
  geom_point() +
  geom_point(aes(x = Month, y = logtransMeanCalanus100m3sfcinterp, color = "CCB")) +
  facet_wrap(~Year) +
  xlab("Month") + ylab("Monthly means of Calanus finmarchicus/100m3") +
  scale_x_discrete(limits = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12")) +
  theme(legend.position = c(0.07, 0.92))


## exclude after May 2013 because that is when the CPR data ends
log.cpr_interp_2013 <- ts(x.impute1[1:185, ])
log.ccb_interp_2013 <- ts(x.imputeccb[1:185, ])

## bind the log transformed ts objects together 
log.ccb.cpr.2013 <- cbind(log.cpr_interp_2013, log.ccb_interp_2013)

## differnce the log transformed interpolated CCB and 
##log transformed interpolated CPR data
diff.log.cpr <- diff(log.ccb.cpr.2013[,1])
diff.log.ccb <- diff(log.ccb.cpr.2013[,2])

## Are CPR data differenced enough?
adf.test(diff.log.cpr, alternative = "stationary") ## this p-value is small 
##and suggests differencing is not required the data are stationary
kpss.test(diff.log.cpr) ## this p-value is large and suggests differencing is 
##not required the data are stationary
Box.test(diff.log.cpr, type="Ljung-Box") ## p-value greater than .05 and 
##suggests differencing is not necessary
stats::acf(diff.log.cpr, na.action = na.pass) ## shows some autocorr at lag 1 and 12

## Are CCB data differenced enough?
adf.test(diff.log.ccb, alternative = "stationary") ## this p-value is small 
##and suggests differencing is not required the data are stationary
kpss.test(diff.log.ccb) ## this p-value is large and suggests differencing is 
##not required the data are stationary
Box.test(diff.log.ccb, type="Ljung-Box") ## p-value less than .05 and 
##suggests more differencing is necessary
stats::acf(diff.log.ccb, na.action = na.pass) ## shows some autocorr at lag 1, 6,and 12


## bind the differenced data back to original dataframe
log.ccb.cpr.2013 <- cbind(log.ccb.cpr.2013, diff.log.cpr, diff.log.ccb)
colnames(log.ccb.cpr.2013) <- c("CPR_interp_log", "CCB_interp_log", 
                                "diff_interp_CPR", "diff_interp_CCB")


#########################
##########CROSS CORR FOR CPR AND CCB CALANUS
#########################
##to determine the lagged relationship between cpr data and ccb data. 
## The CPR data is the "input" variable (the input variable is the influencer). 
## The CCB data is the "output" variable (the output variable is the influenced variable).
## To do a cross correlation you must...
##1.) difference the data in the arima
##2.) fit an arima for the input variable (CPR)
cpr.arima <- auto.arima(log.ccb.cpr.2013[,1], xreg =  time(log.ccb.cpr.2013[,1]), D = 1,
                        stepwise = FALSE, approximation = FALSE)
## residuals don't look good 
##3.) use the arima model to prewhiten and do the cross correlation on the differenced
## data
detach(package:dplyr, unload = TRUE)
ccb_cpr_cross_corr <- TSA::prewhiten(log.ccb.cpr.2013[2:185, 3], 
                                    log.ccb.cpr.2013[2:185, 4],
                                     x.model = cpr.arima, 
                                     main = "CCB Calanus Lags CPR Calanus")

##The cross correlation shows that the most 
## dominant cross correlations occur at +2.  So, above average 
## cpr abundance will likely lead to above average values of CCB calanus 2
## months later.  



## David DeGras said that cross correlation will flag where there may be lags
## but if you have more than one spike (which we do) you can't take it at face value
## and need to fit an arima model as well.  He said try doing an arima with your
## longest lag first.  See if that gets rid of the residuals.  If it doesn't fully
## get rid of residuals than add in a second lag and see what remains.  

## Determine the lag between the log interpolated 
##CPR data and log interpolated CCB
## Lag 0 is our largest lag so we'll start with that
logcpr_lag <- cbind(
  AdLag0 = log.ccb.cpr.2013[,1],
  AdLag1 = stats::lag(log.ccb.cpr.2013[,1],-1),
  AdLag2 = stats::lag(log.ccb.cpr.2013[,1],-2),
  AdLag3 = stats::lag(log.ccb.cpr.2013[,1],-3),
  AdLag4 = stats::lag(log.ccb.cpr.2013[,1],-4),
  AdLag5 = stats::lag(log.ccb.cpr.2013[,1],-5),
  AdLag6 = stats::lag(log.ccb.cpr.2013[,1],-6),
  AdLag7 = stats::lag(log.ccb.cpr.2013[,1],-7),
  AdLag8 = stats::lag(log.ccb.cpr.2013[,1],-8),
  AdLag9 = stats::lag(log.ccb.cpr.2013[,1],-9),
  AdLag10 = stats::lag(log.ccb.cpr.2013[,1],-10),
  AdLag11 = stats::lag(log.ccb.cpr.2013[,1],-11),
  AdLag12 = stats::lag(log.ccb.cpr.2013[,1],-12),
  AdLag13 = stats::lag(log.ccb.cpr.2013[,1],-13))[1:NROW(log.ccb.cpr.2013),]


# Restrict data so models use same fitting period.  I did not include a 
## difference in these models because the CPR data were already differenced
## prior to interpolation.  And when I include D = 1 it does not change the outcome
## auto.arima still picks models with a d = 0 as the best models. 
## no lag
fit1 <- auto.arima(log.ccb.cpr.2013[,2], xreg=logcpr_lag[,1], stepwise = F, 
                   approximation = F) ## fits

## lag of 1 month
fit2 <- auto.arima(log.ccb.cpr.2013[,2], xreg=logcpr_lag[,2], stepwise = F, 
                   approximation = F)  ## fits

## lag of 2 months
fit3 <- auto.arima(log.ccb.cpr.2013[,2], xreg=logcpr_lag[,3], stepwise = F,
                   approximation = F) ##fits

## lag of 3 months
fit5 <- auto.arima(log.ccb.cpr.2013[,2], xreg=logcpr_lag[,4], stepwise = F, 
                   approximation = F) ##fits

## lag of 4 months
fit6 <- auto.arima(log.ccb.cpr.2013[,2], xreg=logcpr_lag[,5], stepwise = F, 
                   approximation = F) ##fits

## lag of 5 months
fit7 <- auto.arima(log.ccb.cpr.2013[,2], xreg=logcpr_lag[,6], stepwise = F, 
                   approximation = F) ##fits

## lag of 6 months
fit8 <- auto.arima(log.ccb.cpr.2013[,2], xreg=logcpr_lag[,7], stepwise = F,
                   approximation = F) ## fits

## lag of 7 months
fit9 <- auto.arima(log.ccb.cpr.2013[,2], xreg=logcpr_lag[,8], stepwise = F,
                   approximation = F) ## fits

## lag of 12 months 
fit12 <- auto.arima(log.ccb.cpr.2013[,2], xreg=logcpr_lag[,13],  stepwise = F,
                    approximation = F) ## fits

## lag of 13 months
fit13 <- auto.arima(log.ccb.cpr.2013[,2], xreg=logcpr_lag[,14], stepwise = F,
                    approximation = F) ## fits

## lag of 0 - 2 months
fit14 <- auto.arima(log.ccb.cpr.2013[,2], xreg=logcpr_lag[,1:3],  stepwise = F,
                    approximation = F) ## fits


## make a table of results for models that fit
CCB_CPR_lag_aic <- c(fit1$aicc, fit2$aicc, fit3$aicc, fit5$aicc, fit6$aicc, fit7$aicc,
                     fit8$aicc, fit9$aicc, fit12$aicc, fit13$aicc, fit14$aicc)
Lag_time <- c("0 months", "1 month", "2 months", "3 months", "4 months", 
              "5 months", "6 months", "7 months", "12 months", "13 months", "0:2 months")
CCB_CPR_lag <- cbind(Lag_time, CCB_CPR_lag_aic)
knitr::kable(CCB_CPR_lag, col.names = c("Lag time", "AICc"), caption = "AICc scores
             for ARIMA models with adaquete goodness-of-fit for CCB Calanus finmarchicus abundance
              lagging the interpolated CPR data. The best model appears to be with a lag 
              of 1 year.")




##**********************************Determine lag for AO to NAO******************
## read in Arctic Oscillation and North Atlantic Oscillation data
ao <- read.csv("/Users/laura.ganley001/Documents/R_Projects/SEM/raw_data/monthly_ao_index_30Jan2018.csv")
nao <- read.csv("/Users/laura.ganley001/Documents/R_Projects/SEM/raw_data/NAO_station_monthly_ncar_30Jan2018.csv")

nao_gather <- gather(nao, "Month", "NAO_index", 2:13) 
nao_gather$month <- match(nao_gather$Month, month.name) ## replace January with 1
nao_select <- dplyr::select(nao_gather, -Month) %>%
  dplyr::filter(NAO_index != -999)## the end of 2017 has some NA's and -999's for hte NAO
colnames(nao_select) <- c("Year", "NAO_index", "Month")

NAO_yearly_mean <- dplyr::group_by(nao_select, Year) %>%
 dplyr::summarise(NAO_mean_year = mean(NAO_index))

## join the AO and NAO dataframes and remove prior to 1998 and after May 2013
NAO_AO <- dplyr::full_join(ao, nao_select) %>%
  dplyr::filter(Year >=1998)%>%
  dplyr::filter(Year<=2013)
NAO_AO <- NAO_AO[1:185, ]## Get rid of NAO after may 2013

## have a dataframe of NAO and AO 1998 - 2017
NAO_AO.98.17 <- dplyr::full_join(ao, nao_select) %>%
  dplyr::filter(Year>=1998)

NAO_AO.98.17.ts <- ts(NAO_AO.98.17)
autoplot(NAO_AO.98.17.ts[,4], series = "Arctic Oscillation Index") +
  autolayer(NAO_AO.98.17.ts[,3], series = "North Atlantic Oscillation Index") +
  labs(y = "Index") +theme(legend.title=element_blank(), legend.position = c(.85,.93))

## have a dataframe of NAO and AO that allows for a longer lag
NAO_AO.94 <- dplyr::full_join(ao, nao_select) %>%
  na.omit()

## convert to a ts object
NAO_AO.arrange <- dplyr::arrange(NAO_AO, Year)
NAO_AO.ts <- ts(NAO_AO.arrange)

autoplot(NAO_AO.ts[, 4], series = "NAO Index") +
  autolayer(NAO_AO.ts[ ,3], series = "AO Index") 

## check if differencing is necessary for NAO 
adf.test(NAO_AO.ts[ , 4], alternative = "stationary") ## this p-value is small 
##and suggests differencing is not required the data are stationary
kpss.test(NAO_AO.ts[ , 4]) ## this p-value is large and suggests differencing is 
##not required the data are stationary
Box.test(NAO_AO.ts[ , 4], type="Ljung-Box") ## p-value greater than .05 and 
##suggests differencing is not necessary
stats::acf(NAO_AO.ts[ , 4]) ## shows some autocorr at lag 1 and 16

## check if differencing is necessary for AO
adf.test(NAO_AO.ts[, 3], alternative = "stationary") ## this p-value is small 
##and suggests differencing is not required the data are stationary
kpss.test(NAO_AO.ts[ , 3]) ## thsi p-value is large and suggests differencing
## is not required the data are stationary
Box.test(NAO_AO.ts[ , 3], type="Ljung-Box") ## p-value less than .05
## and suggests data should be differenced
stats::acf(NAO_AO.ts[ , 3]) ## shows autocorr at lag 1

##some people suggest that if one of your variables needs differencing you should
## difference all of them.
## difference AO.  Seasonally differencing the AO and NAO made the autocorrelation
## worse.  ANd it doesn't really seem like they need to be differenced since all 3 
## tests indicated the NAO is stationary and 2/3 tests indicated AO is stationary
AO_diff <- diff(NAO_AO.ts[ ,3])
NAO_diff <- diff(NAO_AO.ts[ , 4])

## check if the differencing worked on the AO data
adf.test(AO_diff, alternative = "stationary") ## this p-value is small and suggests
## more differencing is not required the data are stationary
kpss.test(AO_diff) ## this p-value is large and suggests more differencing is not
## required
Box.test(AO_diff, type = "Ljung-Box") ## p-value is less than .05 and suggests
## data more differencing is necessary

## check if the differencing worked on the NAO data
adf.test(NAO_diff, alternative = "stationary") ## this p-value is small and suggests
## more differencing is not required the data are stationary
kpss.test(NAO_diff) ## this p-value is large and suggests more differencing is not
## necessary
Box.test(NAO_diff, type = "Ljung-Box") ## this p-value is small and suggests
## more differencing is necessary

## bind the two differenced objects together
NAO_AO_diff <- cbind(NAO_diff, AO_diff)

#########################
##########CROSS CORR FOR NAO AND AO
#########################
##to determine the lagged relationship between AO data and NAO data. 
## The AO data is the "input" variable (the input variable is the influencer). 
## The NAO data is the "output" variable (the output variable is the influenced variable).
## To do a cross correlation you must...
##1.) difference the data: both NAO and AO have already been differenced
##2.) fit an arima for the input variable (AO)
NAO_AO.df <- as.data.frame(NAO_AO.ts)
ao.arima <- auto.arima(NAO_AO.df$AO_index, xreg =  time(NAO_AO.df$AO_index), D = 1,
                        stepwise = FALSE, approximation = FALSE)
  ## residuals look good

##3.) use the arima model to prewhiten and do the cross correlation

ao.nao.corr <- TSA::prewhiten(NAO_AO_diff[,2], 
                                     NAO_AO_diff[,1],
                                     x.model = ao.arima, 
                                     main = "NAO Lags AO")

##The cross correlation shows that the most 
## dominant cross correlations occur at +1 and -1.  So, I think this means there is
## no lag
##############################################

## since I think they may not be lagged try doing a linear model of the differenced
## AO vs the differenced NAO
ggplot(as.data.frame(NAO_AO_diff), aes(x = NAO_diff, y = AO_diff)) +
  geom_smooth(method = "lm") +
  geom_line()

## the linear model indicates that the differenced AO is a significant indicator
## of the differenced NAO with a moderate correlation adj R^2 = 0.453
nao_ao_diff_lm <- lm(NAO_diff ~ AO_diff, NAO_AO_diff)
plot(nao_ao_diff_lm)
summary(nao_ao_diff_lm)

autoplot(NAO_AO_diff[, 1], series = "NAO Index") +
  autolayer(NAO_AO_diff[ ,2], series = "AO Index") 

##**************************at what time lag does the NAO influence CPR********

## bind the non-differenced NAO, AO, and CPR 
NAO_AO_cpr <- (NAO_AO.ts)
colnames(NAO_AO_cpr) <- c("Year", "Month", "AO_index", "NAO_index")

## use only first 185 rows of CPR (after 2013 was all interpolated)
NAO_AO_cpr <- NAO_AO_cpr[1:185, ]

## try an arima to determine the lag between NAO and cpr
NAO_AO_cpr <- ts(NAO_AO_cpr)
NAO_lag <- cbind(
  AdLag0 = NAO_AO_cpr[,"NAO_index"],
  AdLag1 = stats::lag(NAO_AO_cpr[,"NAO_index"],-1),
  AdLag2 = stats::lag(NAO_AO_cpr[,"NAO_index"],-2),
  AdLag3 = stats::lag(NAO_AO_cpr[, "NAO_index"], -3),
  AdLag4 = stats::lag(NAO_AO_cpr[, "NAO_index"], -4),
  AdLag5 = stats::lag(NAO_AO_cpr[, "NAO_index"], -5),
  AdLag6 = stats::lag(NAO_AO_cpr[, "NAO_index"], -6),
  AdLag7 = stats::lag(NAO_AO_cpr[, "NAO_index"], -7),
  AdLag8 = stats::lag(NAO_AO_cpr[, "NAO_index"], -8),
  AdLag9 = stats::lag(NAO_AO_cpr[, "NAO_index"], -9),
  AdLag10 = stats::lag(NAO_AO_cpr[, "NAO_index"], -10),
  AdLag11 = stats::lag(NAO_AO_cpr[, "NAO_index"], -11),
  AdLag12 = stats::lag(NAO_AO_cpr[, "NAO_index"], -12),
  AdLag13 = stats::lag(NAO_AO_cpr[, "NAO_index"], -13),
  AdLag14 = stats::lag(NAO_AO_cpr[, "NAO_index"], -14),
  AdLag15 = stats::lag(NAO_AO_cpr[, "NAO_index"], -15),
  AdLag16 = stats::lag(NAO_AO_cpr[, "NAO_index"], -16),
  AdLag17 = stats::lag(NAO_AO_cpr[, "NAO_index"], -17),
  AdLag18 = stats::lag(NAO_AO_cpr[, "NAO_index"], -18),
  AdLag19 = stats::lag(NAO_AO_cpr[, "NAO_index"], -19),
  AdLag20 = stats::lag(NAO_AO_cpr[, "NAO_index"], -20),
  AdLag21 = stats::lag(NAO_AO_cpr[, "NAO_index"], -21),
  AdLag22 = stats::lag(NAO_AO_cpr[, "NAO_index"], -22),
  AdLag23 = stats::lag(NAO_AO_cpr[, "NAO_index"], -23),
  AdLag24 = stats::lag(NAO_AO_cpr[, "NAO_index"], -24),
  AdLag25 = stats::lag(NAO_AO_cpr[, "NAO_index"], -25),
  AdLag26 = stats::lag(NAO_AO_cpr[, "NAO_index"], -26),
  AdLag27 = stats::lag(NAO_AO_cpr[, "NAO_index"], -27),
  AdLag28 = stats::lag(NAO_AO_cpr[, "NAO_index"], -28),
  AdLag36 = stats::lag(NAO_AO_cpr[, "NAO_index"], -37),
  AdLag48 = stats::lag(NAO_AO_cpr[, "NAO_index"], -49),
  AdLag60 = stats::lag(NAO_AO_cpr[, "NAO_index"], -61),
  AdLag72 = stats::lag(NAO_AO_cpr[, "NAO_index"], -73))[1:NROW(NAO_AO_cpr),]


## determine the lag between the AO and the cpr data
AO_lag <- cbind(
  AdLag0 = NAO_AO_cpr[,"AO_index"],
  AdLag1 = stats::lag(NAO_AO_cpr[,"AO_index"],-1),
  AdLag2 = stats::lag(NAO_AO_cpr[,"AO_index"],-2),
  AdLag3 = stats::lag(NAO_AO_cpr[, "AO_index"], -3),
  AdLag4 = stats::lag(NAO_AO_cpr[, "AO_index"], -4),
  AdLag5 = stats::lag(NAO_AO_cpr[, "AO_index"], -5),
  AdLag6 = stats::lag(NAO_AO_cpr[, "AO_index"], -6),
  AdLag7 = stats::lag(NAO_AO_cpr[, "AO_index"], -7),
  AdLag8 = stats::lag(NAO_AO_cpr[, "AO_index"], -8),
  AdLag9 = stats::lag(NAO_AO_cpr[, "AO_index"], -9),
  AdLag10 = stats::lag(NAO_AO_cpr[, "AO_index"], -10),
  AdLag11 = stats::lag(NAO_AO_cpr[, "AO_index"], -11),
  AdLag12 = stats::lag(NAO_AO_cpr[, "AO_index"], -12),
  AdLag13 = stats::lag(NAO_AO_cpr[, "AO_index"], -13),
  AdLag14 = stats::lag(NAO_AO_cpr[, "AO_index"], -14),
  AdLag15 = stats::lag(NAO_AO_cpr[, "AO_index"], -15),
  AdLag16 = stats::lag(NAO_AO_cpr[, "AO_index"], -16),
  AdLag17 = stats::lag(NAO_AO_cpr[, "AO_index"], -17),
  AdLag18 = stats::lag(NAO_AO_cpr[, "AO_index"], -18),
  AdLag19 = stats::lag(NAO_AO_cpr[, "AO_index"], -19),
  AdLag20 = stats::lag(NAO_AO_cpr[, "AO_index"], -20),
  AdLag21 = stats::lag(NAO_AO_cpr[, "AO_index"], -21),
  AdLag22 = stats::lag(NAO_AO_cpr[, "AO_index"], -22),
  AdLag23 = stats::lag(NAO_AO_cpr[, "AO_index"], -23),
  AdLag24 = stats::lag(NAO_AO_cpr[, "AO_index"], -24),
  AdLag25 = stats::lag(NAO_AO_cpr[, "AO_index"], -25),
  AdLag26 = stats::lag(NAO_AO_cpr[, "AO_index"], -26),
  AdLag27 = stats::lag(NAO_AO_cpr[, "AO_index"], -27),
  AdLag28 = stats::lag(NAO_AO_cpr[, "AO_index"], -28),
  AdLag36 = stats::lag(NAO_AO_cpr[, "AO_index"], -37),
  AdLag48 = stats::lag(NAO_AO_cpr[, "AO_index"], -49),
  AdLag60 = stats::lag(NAO_AO_cpr[, "AO_index"], -61),
  AdLag72 = stats::lag(NAO_AO_cpr[, "AO_index"], -73))[1:NROW(NAO_AO_cpr),]


#########################
##########CROSS CORR FOR NAO and CPR
#########################
##to determine the lagged relationship between NAO data and CPR data. 
## The NAO data is the "input" variable (the input variable is the influencer). 
## The CPR data is the "output" variable (the output variable is the influenced variable).
## To do a cross correlation you must...
##1.) difference the data: both NAO and CPR have already been differenced
##2.) fit an arima for the input variable (NAO)
nao.arima <- auto.arima(NAO_AO.df$NAO_index, xreg =  time(NAO_AO.df$NAO_index), D = 1,
                        stepwise = FALSE, approximation = FALSE)
## residuals look good except signif spike at lag 12 still.
##3.) use the arima model to prewhiten and do the cross correlation
nao.cpr.cross.corr <- TSA::prewhiten(NAO_AO_diff[, 1], 
                                     log.ccb.cpr.2013[, 3],
                                     x.model = nao.arima, lag.max = 100, 
                                     main = "NAO Lags CPR Calanus")

## run ARIMAs for NAO to log transformed interpolated CPR
# Restrict data so models use same fitting period
## no lag
fit1 <- auto.arima(log.ccb.cpr.2013[,1], xreg=NAO_lag[,1], D = 1,
                   stepwise = F, approximation = F) ## fits
## 1 month lag
fit2 <- auto.arima(log.ccb.cpr.2013[,1], xreg=NAO_lag[,2],D=1, 
                   stepwise = F, approximation = F) ## fits

## lag of 2 months
fit3 <- auto.arima(NAO_AO_cpr[,1], xreg=NAO_lag[,3], D=1,
                   stepwise = F, approximation = F) ## doesn't fit

## lag of 3 months
fit4 <- auto.arima(NAO_AO_cpr[,1], xreg=NAO_lag[,4], D = 1,
                   stepwise = F, approximation = F) ## doesn't fit

fit10 <- auto.arima(NAO_AO_cpr[,1], xreg =NAO_lag[,11], D = 1,
                    stepwise = F, approximation = F) ## doesn't fit

## lag of 12 months
fit13 <- auto.arima(NAO_AO_cpr[,1], xreg=NAO_lag[, 13], D = 1,
                    stepwise = F, approximation = F) ## fits

## lag of 24 months 
fit24 <- auto.arima(NAO_AO_cpr[,1], xreg=NAO_lag[,25], D = 1,
                    stepwise = F, approximation = F) ##fits
##lag of 28 months
fit28 <- auto.arima(NAO_AO_cpr[,1], xreg = NAO_lag[,29], D = 1,
                    stepwise = F, approximation = F) ## doesn't fit but this is where
## the spike is in the cross correlation

## lag of 36 months
fit36 <- auto.arima(NAO_AO_cpr[,1], xreg=NAO_lag[ ,30], D=1,
                    stepwise = F, approximation = F) ## fits

## lag of 48 months
fit48 <- auto.arima(NAO_AO_cpr[,1], xreg=NAO_lag[ ,31], D=1,
                    stepwise = F, approximation = F) ## fits
## lag of 60 months
fit60 <- auto.arima(NAO_AO_cpr[,1], xreg=NAO_lag[,32], D=1,
                    stepwise = F, approximation = F) ## fits

## lag of 72 months
fit72 <- auto.arima(NAO_AO_cpr[,1], xreg=NAO_lag[,33], D = 1,
                    stepwise = F, approximation = F) ## fits

## make a table of results for models that fit
NAO_CPR_lag_aic <- c(fit1$aicc,fit2$aicc, fit13$aicc, fit24$aicc, fit36$aicc, fit48$aicc, fit60$aicc, fit72$aicc)
Lag_time <- c("0 months", "1 month", "1 year", "2 years", "3 years", "4 years", "5 years", "6 years")
NAO_CPR_lag <- cbind(Lag_time, NAO_CPR_lag_aic)
knitr::kable(NAO_CPR_lag, col.names = c("Lag time", "AICc"), caption = "AICc scores
             for ARIMA models with adaquete goodness-of-fit for NAO lagging the interpolated CPR data. 
             The best model appears to be with a lag of 6 years, however in the literature the
             best models are usually 2-4 years.")
## NAO lag of 72 months is the best model



#########################
##########CROSS CORR FOR AO and CPR
#########################
##to determine the lagged relationship between NAO data and CPR data. 
## The AO data is the "input" variable (the input variable is the influencer). 
## The CPR data is the "output" variable (the output variable is the influenced variable).
## To do a cross correlation you must...
##1.) difference the data: both AO and CPR have already been differenced
##2.) we already fit an arima for the input variable (ao.arima)
##3.) use the arima model to prewhiten and do the cross correlation
ao.cpr.cross.corr <- TSA::prewhiten(NAO_AO_diff[, 2], 
                                     log.ccb.cpr.2013[, 3],
                                     x.model = ao.arima, lag.max = 100, 
                                     main = "AO Lags CPR Calanus")


## run ARIMAs for AO to log transformed interpolated CPR
# Restrict data so models use same fitting period
## no lag
fit1 <- auto.arima(log.ccb.cpr.2013[,1], xreg=AO_lag[,1],D=1, 
                   stepwise = F, approximation = F) ## fits
## 1 month lag
fit2 <- auto.arima(log.ccb.cpr.2013[,1], xreg=AO_lag[,2],D=1, 
                   stepwise = F, approximation = F) ## fits

## lag of 2 months
fit3 <- auto.arima(NAO_AO_cpr[,1], xreg=AO_lag[,3], D=1,
                   stepwise = F, approximation = F) ## doesn't fit

## lag of 3 months
fit4 <- auto.arima(NAO_AO_cpr[,1], xreg=AO_lag[,4], D = 1,
                   stepwise = F, approximation = F) ## doesn't fit

fit10 <- auto.arima(NAO_AO_cpr[,1], xreg =AO_lag[,11], D = 1,
                    stepwise = F, approximation = F) ## doesn't fit

## lag of 12 months
fit13 <- auto.arima(NAO_AO_cpr[,1], xreg=AO_lag[, 13], D = 1,
                    stepwise = F, approximation = F) ## doesnt fit

## lag of 24 months 
fit24 <- auto.arima(NAO_AO_cpr[,1], xreg=AO_lag[,25], D = 1,
                    stepwise = F, approximation = F) ##fits

## lag of 36 months
fit36 <- auto.arima(NAO_AO_cpr[,1], xreg=AO_lag[ ,30], D=1,
                    stepwise = F, approximation = F) ## fits

## lag of 48 months
fit48 <- auto.arima(NAO_AO_cpr[,1], xreg=AO_lag[ ,31], D=1,
                    stepwise = F, approximation = F) ## fits
## lag of 60 months
fit60 <- auto.arima(NAO_AO_cpr[,1], xreg=AO_lag[,32], D=1,
                    stepwise = F, approximation = F) ## fits

## lag of 72 months
fit72 <- auto.arima(NAO_AO_cpr[,1], xreg=AO_lag[,33], D = 1,
                    stepwise = F, approximation = F) ## fits


## AO lag of 72 months is the best model
## make a table of results for models that fit
AO_CPR_lag_aic <- c(fit1$aicc,fit2$aicc, fit24$aicc, fit36$aicc, fit48$aicc, fit60$aicc, fit72$aicc)
Lag_time <- c("0 months", "1 month", "2 years", "3 years", "4 years", "5 years", "6 years")
AO_CPR_lag <- cbind(Lag_time, AO_CPR_lag_aic)
knitr::kable(AO_CPR_lag, col.names = c("Lag time", "AICc"), caption = "AICc scores
             for ARIMA models for AO lagging the interpolated CPR data. 
             The best model appears to be with a lag of 6 years. All models showed
             adaquete goodness-of-fit ")
## NAO lag of 72 months is the best model




##*******************************INTERPOLATE CCB ZPL DATA*********************
## I already interpolated data for calanus in ccb above.  but now I need to 
## interpolate the surface tows for the mean total zooplankton, centropages, 
##pseudocalanus and eventually the caloric values.
oblq_sfc$Year <- as.character(oblq_sfc$Year)

ggplot(oblq_sfc, aes(x=Month, y = MeanTotalZplm3sfc)) +
  geom_point() +
  facet_wrap(~Year) +
  scale_x_discrete(limits = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12"))

## convert the ccb data to abundance per 100 m3 so it is like the cpr data
sfc_stations <- sfc_stations%>%
  dplyr::mutate(MeanTotalZpl100m3sfc = 10*MeanTotalZplm3sfc) %>%
 dplyr::mutate(MeanCalanus100m3sfc = 10*MeanCalanusm3sfc) %>%
 dplyr::mutate(MeanPseudo100m3sfc = 10*MeanPseudom3sfc)%>%
  dplyr::mutate(MeanCentro100m3sfc = 10*MeanCentrom3sfc)

# convert this to a timeseries in order to interpolate
sfc.ts <- ts(sfc_stations)

###################
##LOG TRANSFORM the Mean TOTAL ZPL data
###################


## try transforming the data and interpolating 
# Find best Box-Cox transformation of Mean Total Zpl 100m3 sfc
BoxCox.lambda(sfc.ts[,7])
# Best lambda is almost zero --> logarithmic transform
logmtz = log(sfc.ts[,7])
logmtz[is.infinite(logmtz)] = NA

## Find best ARIMA model for Mean Total Zpl log-transformed ts (w.r.t. AICc)
fitlogmtz <- auto.arima(logmtz,approximation=F,stepwise=F)
summary(fitlogmtz)
# Best model = ARIMA(4,0,0)

## Inspect residuals 
elogmtz = residuals(fitlogmtz)
plot(elogmtz) # no trend
qqnorm(elogmtz) # normality assumption ok
stats::acf(elogmtz,na.action = na.pass) 
# Mildly significant autocorrelation at lags 8 & 16. Not too worrisome
pacf(elogmtz,na.action = na.pass) 
# No Significant partial autocorrelations. 
# Note: ACF and PACF must be taken with a grain of salt b/c 
# of the many missing values

## Model accuracy
accuracy(fitlogmtz) # in-sample
f = function(x,h) forecast(Arima(x,c(4,0,0)),h=h)
e = tsCV(logmtz,f) # 1-step ahead forecasting errors (CV)
mean(e,na.rm=T) # mean error (ME) 
mean(abs(e),na.rm=T) # mean absolute error (MAE)
mean(abs(e/logmtz),na.rm=T) # mean absolute percentage error (MAPE)


##impute missing CCB data with log transformed data 
# With model 1
x.imputemtz = exp(na.kalman(logmtz, fitlogmtz$model))

# Plot CCB mean total zpl  (linear scale)
autoplot(ts(x.imputemtz), series="Interpolated") +
  autolayer(sfc.ts[,7], series= "Original") +
  scale_color_manual(values=c(`Interpolated`="red",`Original`="gray")) +
  ylab("CCB Mean Total Zpl data (log transformed before interpolation)")

## bind the log transformed interpolated back onto the dataframe
log.trans.mtz <- x.imputemtz
sfc.ts.mtz <- cbind(sfc.ts, log.trans.mtz)

ccb.mtz.orig.interp.plot <- ggplot(as.data.frame(sfc.ts.mtz),
                                       aes(x=sfc.ts.Month,
                                           y= log.trans.mtz,
                                           color = "Interpolated"))+
  geom_point() +
  geom_point(aes(x=sfc.ts.Month, y = sfc.ts.MeanTotalZpl100m3sfc, color = "Original")) +
  facet_wrap(~sfc.ts.Year) +
  xlab("Month") + ylab("CCB monthly means of Mean Total Zpl/100m3") +
  scale_x_discrete(limits = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12")) +
  theme(legend.position = c(0.95, 0.14), legend.title = element_blank())



####################
## DON"T LOG TRANFORM BEFORE INTERPOLATION MTZ
####################

## look into first difference the mean total zpl data
sfc.ts[,7] %>%
  diff() %>%
  ggtsdisplay

## fit an arima model with the mean total zpl
##to then be smoothed with a kalman smoother
fit.ccb.mtz <- auto.arima(sfc.ts[,7], approximation=F,stepwise=F) 
summary(fit.ccb.mtz)
## best model is ARIMA(0,0,0) (no differencing)
## check residuals
checkresiduals(fit.ccb.mtz) ## acf looks good, Ljung-Box does not
## Inspect residuals 
emtz = residuals(fit.ccb.mtz)
plot(emtz) # no trend
qqnorm(emtz) # normality assumption not good
stats::acf(emtz,na.action = na.pass) 
# Mildly significant autocorrelation at lags 6, 12, 15. Not too worrisome
pacf(emtz,na.action = na.pass) 
# Significant partial autocorrelations at 3 &4
# Note: ACF and PACF must be taken with a grain of salt b/c 
# of the many missing values

#########
##It seems like the residuals from the model fit with the log transformed mtz 
##data are better so it seems smarter to interpolate using the log transformed data
########


#################
##LOG TRANSFORM PSUEDOCALANUS DATA
#################
## try transforming the data and interpolating 
# Find best Box-Cox transformation of Pseudocalanus 100m3 sfc
BoxCox.lambda(sfc.ts[,9])
# Best lambda is almost zero --> logarithmic transform
logpseudo = log(sfc.ts[,9])
logpseudo[is.infinite(logpseudo)] = NA

## Find best ARIMA model for Pseudocalanus log-transformed ts (w.r.t. AICc)
fitlogpseudo <- auto.arima(logpseudo,approximation=F,stepwise=F)
summary(fitlogpseudo)
# Best model = ARIMA(0,0,1)

## Inspect residuals 
elogpseudo = residuals(fitlogpseudo)
plot(elogpseudo) # no trend
qqnorm(elogpseudo) # normality assumption ok
stats::acf(elogpseudo,na.action = na.pass) 
# Mildly significant autocorrelation at lags 4, 9, 12, 15, 20.
pacf(elogpseudo,na.action = na.pass) 
# Significant partial autocorrelations at lag 4. 
# Note: ACF and PACF must be taken with a grain of salt b/c 
# of the many missing values

## Model accuracy
accuracy(fitlogpseudo) # in-sample
f = function(x,h) forecast(Arima(x,c(0,0,1)),h=h)
e = tsCV(logpseudo,f) # 1-step ahead forecasting errors (CV)
mean(e,na.rm=T) # mean error (ME) 
mean(abs(e),na.rm=T) # mean absolute error (MAE)
mean(abs(e/logpseudo),na.rm=T) # mean absolute percentage error (MAPE)


##impute missing CCB pseudocalanus data with log transformed data 
# With model 1
x.imputepseudo = exp(na.kalman(logpseudo, fitlogpseudo$model))

# Plot CCB mean total zpl  (linear scale)
autoplot(ts(x.imputepseudo), series="Interpolated") +
  autolayer(sfc.ts[,9], series= "Original") +
  scale_color_manual(values=c(`Interpolated`="red",`Original`="gray")) +
  ylab("CCB Pseudocalanus data (log transformed before interpolation)")

## bind the log transformed interpolated back onto the dataframe
log.trans.pseudo <- x.imputepseudo
sfc.ts.pseudo <- cbind(sfc.ts.mtz, log.trans.pseudo)

ccb.pseudo.orig.interp.plot <- ggplot(as.data.frame(sfc.ts.pseudo),
                                   aes(x=sfc.ts.mtz.sfc.ts.Month,
                                       y= log.trans.pseudo,
                                       color = "Interpolated"))+
  geom_point() +
  geom_point(aes(x=sfc.ts.mtz.sfc.ts.Month, y = sfc.ts.mtz.sfc.ts.MeanPseudo100m3sfc, color = "Original")) +
  facet_wrap(~sfc.ts.mtz.sfc.ts.Year) +
  xlab("Month") + ylab("CCB monthly means of Pseudocalanus/100m3") +
  scale_x_discrete(limits = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12")) +
  theme(legend.position = c(0.94, 0.15), legend.title = element_blank())



#######################
##TRY NOT LOG TRANFORMING PSEUDO DATA
#######################

## Interpolate the pseudocalanus data
## first difference the pseudocalanus data
sfc.ts[,9] %>%
  diff() %>%
  ggtsdisplay

## fit an arima model with the non differenced PSEUDO data
##to then be smoothed with a kalman smoother
fit.ccb.pseudo <- auto.arima(sfc.ts[,9], approximation=F,stepwise=F) 
summary(fit.ccb.pseudo) ## ARIMA(0,0,0) best model
## check residuals
checkresiduals(fit.ccb.pseudo) ## acf looks good, Ljung-Box does not
## Inspect residuals 
epseudo = residuals(fit.ccb.pseudo)
plot(epseudo) # no trend
qqnorm(epseudo) # normality assumption not good
stats::acf(epseudo,na.action = na.pass) 
# Mildly significant autocorrelation at lags 22. Not too worrisome
pacf(epseudo,na.action = na.pass) 
# No Significant partial autocorrelations
# Note: ACF and PACF must be taken with a grain of salt b/c 
# of the many missing values

#########
##It seems like the residuals from the model fit with the log transformed Pseudocalanus
##data are better so it seems smarter to interpolate using the log transformed data
########


#################
##LOG TRANSFORM Centropages DATA
#################
## try transforming the data and interpolating 
# Find best Box-Cox transformation of Pseudocalanus 100m3 sfc
BoxCox.lambda(sfc.ts[,10])
# Best lambda is almost zero --> logarithmic transform
logcentro = log(sfc.ts[,10])
logcentro[is.infinite(logcentro)] = NA

## Find best ARIMA model for Centropages log-transformed ts (w.r.t. AICc)
fitlogcentro <- auto.arima(logcentro,approximation=F,stepwise=F)
summary(fitlogcentro)
# Best model = ARIMA(5,0,0)

## Inspect residuals 
elogcentro = residuals(fitlogcentro)
plot(elogcentro) # no trend
qqnorm(elogcentro) # normality assumption ok
stats::acf(elogcentro,na.action = na.pass) 
# No significant autocorrelation.
pacf(elogcentro,na.action = na.pass) 
# No Significant partial autocorrelations. 
# Note: ACF and PACF must be taken with a grain of salt b/c 
# of the many missing values

## Model accuracy
accuracy(fitlogcentro) # in-sample
f = function(x,h) forecast(Arima(x,c(5,0,0)),h=h)
e = tsCV(logcentro,f) # 1-step ahead forecasting errors (CV)
mean(e,na.rm=T) # mean error (ME) 
mean(abs(e),na.rm=T) # mean absolute error (MAE)
mean(abs(e/logcentro),na.rm=T) # mean absolute percentage error (MAPE)


##impute missing CCB Centropages data with log transformed data 
# With model 1
x.imputecentro = exp(na.kalman(logcentro, fitlogcentro$model))

# Plot CCB mean total zpl  (linear scale)
autoplot(ts(x.imputecentro), series="Interpolated") +
  autolayer(sfc.ts[,10], series= "Original") +
  scale_color_manual(values=c(`Interpolated`="red",`Original`="gray")) +
  ylab("CCB Centropages data (log transformed before interpolation)")

## bind the log transformed interpolated back onto the dataframe
log.trans.centro <- x.imputecentro
sfc.ts.centro <- cbind(sfc.ts.pseudo, log.trans.centro)
colnames(sfc.ts.centro) <- c("Year", "Month", "MTZm3sfc", "MeanCalanusm3sfc", "MeanPseudom3sfc", 
                             "MeanCentrom3sfc", "MTZ100m3sfc", "MeanCalanus100m3sfc", "MeanPseudo100m3sfc",
                             "MeanCentro100m3sfc", "mtzloginterp", "pseudologinterp", "centrologinterp")

ccb.centro.orig.interp.plot <- ggplot(as.data.frame(sfc.ts.centro),
                                      aes(x=Month,
                                          y= centrologinterp,
                                          color = "Interpolated"))+
  geom_point() +
  geom_point(aes(x=Month, y = MeanCentro100m3sfc, color = "Original")) +
  facet_wrap(~Year) +
  xlab("Month") + ylab("CCB monthly means of Centropages/100m3") +
  scale_x_discrete(limits = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12")) +
  theme(legend.position = c(0.07, 0.92))


#######################
##TRY NOT LOG TRANFORMING Centro DATA
#######################


## fit an arima model with the differenced data Centropages
##to then be smoothed with a kalman smoother
fit.ccb.centro <- auto.arima(sfc.ts[,10], approximation=F,stepwise=F) 
summary(fit.ccb.centro) ## best model ARIMA(0,0,1)
## check residuals
checkresiduals(fit.ccb.centro) ## acf looks good, Ljung-Box does not


## Inspect residuals 
ecentro = residuals(fit.ccb.centro)
plot(ecentro) # no trend
qqnorm(ecentro) # normality assumption not good
stats::acf(ecentro,na.action = na.pass) 
# significant autocorrelations at 3,4,8,11,12
pacf(ecentro,na.action = na.pass) 
# Significant partial autocorrelations at 3 and 4
# Note: ACF and PACF must be taken with a grain of salt b/c 
# of the many missing values

#########
##It seems like the residuals from the model fit with the log transformed Centropages
##data are better so it seems smarter to interpolate using the log transformed data
########






## is there a trend in the pseudocalanus data?
## Try a moving average
## only use the "in-season" data
sfc.ts.pseudo.ma <- dplyr::filter(as.data.frame(sfc.ts.pseudo), sfc.ts.mtz.sfc.ts.Month >= 1 & sfc.ts.mtz.sfc.ts.Month <=5)
sfc.ts.pseudo.ma <- ts(sfc.ts.pseudo.ma, freq = 12)
ma.pseudo <- autoplot(sfc.ts.pseudo.ma[,12], series="Data") +
  autolayer(ma(sfc.ts.pseudo.ma[,12],12), series="12-MA") +
  xlab("Year") + ylab("CCB Pseudo") +
  ggtitle("CCB Pseudocalanus abundance") +
  scale_colour_manual(values=c("Data"="grey50","12-MA"="red"),
                      breaks=c("Data","12-MA")) +
  theme(legend.position = c(0.9, 0.9), legend.title = element_blank())


## is there a trend in ccb calanus data?
## looka at the moving average smoothed over 12 months.  It looks like 
## there was an increasing trend that dropped off after 2011
## only use the "in-season" data
log.interp.orig.ma <- dplyr::filter(as.data.frame(log.interp.orig), Month >=1 & Month <= 5)
log.interp.orig.ma <- ts(log.interp.orig.ma, freq = 5)
ma.cfin <- autoplot(log.interp.orig.ma[,7], series="Data") +
  autolayer(ma(log.interp.orig.ma[,7],12), series="12-MA") +
  ylab("CCB C. fin") +
  ggtitle("CCB Calanus finmarchicus abundance") +
  scale_colour_manual(values=c("Data"="grey50","12-MA"="red"),
                      breaks=c("Data","12-MA")) +
  theme(legend.position = c(0.9, 0.9), legend.title = element_blank())

## is there a trend in the CPR data? Try a moving average
log.interp.orig.cpr.ma <- ts(log.interp.orig.cpr, freq = 12)
ma.cpr <- autoplot(log.interp.orig.cpr.ma[,8], series="Data") +
  autolayer(ma(log.interp.orig.cpr.ma[,8],12), series="12-MA") +
  ylab("CPR") +
  ggtitle("Regional Calanus finmarchicus abundance") +
  scale_colour_manual(values=c("Data"="grey50","12-MA"="red"),
                      breaks=c("Data","12-MA")) +
  theme(legend.position = c(0.9, 0.9), legend.title = element_blank())

## are the mtz data trending? try a 12 month moving average
## only use the "in-season" data
sfc.ts.mtz.ma <- dplyr::filter(as.data.frame(sfc.ts.mtz), sfc.ts.Month >= 1 & sfc.ts.Month <=5)
sfc.ts.mtz.ma <- ts(sfc.ts.mtz.ma, freq = 5)
ma.mtz <- autoplot(sfc.ts.mtz.ma[,11], series="Data") +
  autolayer(ma(sfc.ts.mtz.ma[,11],12), series="12-MA") +
  xlab("Year") + ylab("CCB MTZ") +
  ggtitle("CCB MTZ abundance") +
  scale_colour_manual(values=c("Data"="grey50","12-MA"="red"),
                      breaks=c("Data","12-MA")) +
  theme(legend.position = c(0.9, 0.9), legend.title = element_blank())

## is there a trend in the centro data?
## Try a moving average
## only use the "in-season" data 
sfc.ts.centro.ma <-dplyr::filter(as.data.frame(sfc.ts.centro), Month >=1 & Month <=5)
sfc.ts.centro.ma <- ts(sfc.ts.centro.ma, freq = 5)
ma.centro <- autoplot(sfc.ts.centro.ma[,13], series="Data") +
  autolayer(ma(sfc.ts.centro.ma[,13],12), series="12-MA") +
  xlab("Year") + ylab("CCB Centro") +
  ggtitle("CCB Centropages abundance") +
  scale_colour_manual(values=c("Data"="grey50","12-MA"="red"),
                      breaks=c("Data","12-MA")) +
  theme(legend.position = c(0.9, 0.9), legend.title = element_blank())




#############################

######ZPL PATCHINESS

############################

##To indicate zooplankton patchiness in CCB we are going to use the Concentration Index
## Baumgartner ref. in the Urban Whale - the ratio of peak zpl concentration per month to 
## average zpl abundance. For now, the peak zpl is just from the regular stations.
patchy <- read.csv("/Users/laura.ganley001/Documents/R_Projects/SEM/raw_data/zpl_patchiness/peak_zpl_abund_reg_stations.csv")

## convert patchy data to per 100 m3
patchy <- (patchy) %>%
        dplyr::mutate(MaxOftotal.zpl.100m3 = MaxOftotal.zpl.m3*10) %>%
        dplyr::mutate(MaxOfCalanus.100m3 = Max.Of.calanus.total.m3*10) %>%
        dplyr::mutate(MaxOfPseudo.100m3 = Max.Of.pseudo.spp.total.m3*10) %>%
        dplyr::mutate(MaxOfCentro.100m3 = Max.Of.centropages.spp.total.m3*10) 
  
 
  
############################
##LOG TRANSFORMED zpl patchiness
############################

## try transforming the data and re-doing the above (interpolating and search
## for lags).
# Find best Box-Cox transformation of MTZ peak
BoxCox.lambda(patchy$MaxOftotal.zpl.100m3)
# Best lambda is almost zero --> logarithmic transform
log.peakmtz = log(patchy$MaxOftotal.zpl.100m3)
log.peakmtz[is.infinite(log.peakmtz)] <- NA
stats::acf(log.peakmtz, na.action = na.pass)


## Find best ARIMA model for peak mtz log-transformed ts (w.r.t. AICc)
fitlog.peakmtz <- auto.arima(log.peakmtz,approximation=F,stepwise=F)
summary(fitlog.peakmtz)
# Best model = ARIMA(0,0,1)

## Inspect residuals 
elog.peakmtz = residuals(fitlog.peakmtz)
plot(elog.peakmtz) # no trend
qqnorm(elog.peakmtz) # normality assumption ok
stats::acf(elog.peakmtz,na.action = na.pass) 
# Mildly significant autocorrelation at lags 6. Not too worrisome
pacf(elog.peakmtz,na.action = na.pass) 
# Significant partial autocorrelations at lags 6 & 7. 
# Note: ACF and PACF must be taken with a grain of salt b/c 
# of the many missing values

## Model accuracy
accuracy(fitlog.peakmtz) # in-sample
f = function(x,h) forecast(Arima(x,c(0,0,1)),h=h)
e = tsCV(log.peakmtz,f) # 1-step ahead forecasting errors (CV)
mean(elog.peakmtz,na.rm=T) # mean error (ME) 
mean(abs(elog.peakmtz),na.rm=T) # mean absolute error (MAE)
mean(abs(e/log.peakmtz),na.rm=T) # mean absolute percentage error (MAPE)
# ?????

##impute missing CCB data with log transformed data 
# With model 1
x.impute.peak.mtz = exp(na.kalman(log.peakmtz, fitlog.peakmtz$model))

# Plot CCB  (linear scale)
autoplot(ts(x.impute.peak.mtz), series="Interpolated") +
  autolayer(ts(patchy$MaxOftotal.zpl.100m3), series= "Original") +
  scale_color_manual(values=c(`Interpolated`="red",`Original`="gray")) +
  ylab("MTZ peak data (log transformed before interpolation)")



## try transforming the data and re-doing the above (interpolating and search
## for lags).
# Find best Box-Cox transformation of calanus peak
BoxCox.lambda(patchy$MaxOfCalanus.100m3)
# Best lambda is almost zero --> logarithmic transform
log.peakcal = log(patchy$MaxOfCalanus.100m3)
log.peakcal[is.infinite(log.peakcal)] <- NA
stats::acf(log.peakcal, na.action = na.pass)


## Find best ARIMA model for peak mtz log-transformed ts (w.r.t. AICc)
fitlog.peakcal <- auto.arima(log.peakcal,approximation=F,stepwise=F)
summary(fitlog.peakcal)
# Best model = ARIMA(5,0,0)

## Inspect residuals 
elog.peakcal = residuals(fitlog.peakcal)
plot(elog.peakcal) # no trend
qqnorm(elog.peakcal) # normality assumption ok
stats::acf(elog.peakcal,na.action = na.pass) 
# Mildly significant autocorrelation at lags 5. Not too worrisome
pacf(elog.peakcal,na.action = na.pass) 
# Significant partial autocorrelations at lag 5. 
# Note: ACF and PACF must be taken with a grain of salt b/c 
# of the many missing values

## Model accuracy
accuracy(fitlog.peakcal) # in-sample
f = function(x,h) forecast(Arima(x,c(5,0,0)),h=h)
e = tsCV(log.peakcal,f) # 1-step ahead forecasting errors (CV)
mean(elog.peakcal,na.rm=T) # mean error (ME) 
mean(abs(elog.peakcal),na.rm=T) # mean absolute error (MAE)
mean(abs(e/log.peakcal),na.rm=T) # mean absolute percentage error (MAPE)
# ?????

##impute missing CCB data with log transformed data 
# With model 1
x.impute.peak.cal = exp(na.kalman(log.peakcal, fitlog.peakcal$model))

# Plot CCB  (linear scale)
autoplot(ts(x.impute.peak.cal), series="Interpolated") +
  autolayer(ts(patchy$MaxOfCalanus.100m3), series= "Original") +
  scale_color_manual(values=c(`Interpolated`="red",`Original`="gray")) +
  ylab("MTZ peak data (log transformed before interpolation)")


## try transforming the data and re-doing the above (interpolating and search
## for lags).
# Find best Box-Cox transformation of Pseudocalanus peak
BoxCox.lambda(patchy$MaxOfPseudo.100m3)
# Best lambda is almost zero --> logarithmic transform
log.peakpseudo = log(patchy$MaxOfPseudo.100m3)
log.peakpseudo[is.infinite(log.peakpseudo)] <- NA
stats::acf(log.peakpseudo, na.action = na.pass)


## Find best ARIMA model for peak mtz log-transformed ts (w.r.t. AICc)
fitlog.peakpseudo <- auto.arima(log.peakpseudo,approximation=F,stepwise=F)
summary(fitlog.peakpseudo)
# Best model = ARIMA(1,0,0)

## Inspect residuals 
elog.peakpseudo = residuals(fitlog.peakpseudo)
plot(elog.peakpseudo) # no trend
qqnorm(elog.peakpseudo) # normality assumption ok
stats::acf(elog.peakpseudo,na.action = na.pass) 
# No signif autocorr.
pacf(elog.peakpseudo,na.action = na.pass) 
# No Significant partial autocorrelations 
# Note: ACF and PACF must be taken with a grain of salt b/c 
# of the many missing values

## Model accuracy
accuracy(fitlog.peakpseudo) # in-sample
f = function(x,h) forecast(Arima(x,c(1,0,0)),h=h)
e = tsCV(log.peakpseudo,f) # 1-step ahead forecasting errors (CV)
mean(elog.peakpseudo,na.rm=T) # mean error (ME) 
mean(abs(elog.peakpseudo),na.rm=T) # mean absolute error (MAE)
mean(abs(e/log.peakpseudo),na.rm=T) # mean absolute percentage error (MAPE)
# ?????

##impute missing CCB data with log transformed data 
# With model 1
x.impute.peak.pseudo = exp(na.kalman(log.peakpseudo, fitlog.peakpseudo$model))

# Plot CCB  (linear scale)
autoplot(ts(x.impute.peak.pseudo), series="Interpolated") +
  autolayer(ts(patchy$MaxOfPseudo.100m3), series= "Original") +
  scale_color_manual(values=c(`Interpolated`="red",`Original`="gray")) +
  ylab("MTZ peak data (log transformed before interpolation)")



## try transforming the data and re-doing the above (interpolating and search
## for lags).
# Find best Box-Cox transformation of centropages peak
BoxCox.lambda(patchy$MaxOfCentro.100m3)
# Best lambda is almost zero --> logarithmic transform
log.peakcentro = log(patchy$MaxOfCentro.100m3)
log.peakcentro[is.infinite(log.peakcentro)] <- NA
stats::acf(log.peakcentro, na.action = na.pass)


## Find best ARIMA model for peak mtz log-transformed ts (w.r.t. AICc)
fitlog.peakcentro <- auto.arima(log.peakcentro,approximation=F,stepwise=F)
summary(fitlog.peakcentro)
# Best model = ARIMA(5,0,0)

## Inspect residuals 
elog.peakcentro = residuals(fitlog.peakcentro)
plot(elog.peakcentro) # no trend
qqnorm(elog.peakcentro) # normality assumption ok
stats::acf(elog.peakcentro,na.action = na.pass) 
# Slightly signif autocorr at 12.
pacf(elog.peakcentro,na.action = na.pass) 
# Slightly signific pacf at 12 and 15
# Note: ACF and PACF must be taken with a grain of salt b/c 
# of the many missing values

## Model accuracy
accuracy(fitlog.peakcentro) # in-sample
f = function(x,h) forecast(Arima(x,c(5,0,0)),h=h)
e = tsCV(log.peakcentro,f) # 1-step ahead forecasting errors (CV)
mean(elog.peakcentro,na.rm=T) # mean error (ME) 
mean(abs(elog.peakcentro),na.rm=T) # mean absolute error (MAE)
mean(abs(e/log.peakcentro),na.rm=T) # mean absolute percentage error (MAPE)
# ?????

##impute missing CCB data with log transformed data 
# With model 1
x.impute.peak.centro = exp(na.kalman(log.peakcentro, fitlog.peakcentro$model))

# Plot CCB  (linear scale)
autoplot(ts(x.impute.peak.centro), series="Interpolated") +
  autolayer(ts(patchy$MaxOfCentro.100m3), series= "Original") +
  scale_color_manual(values=c(`Interpolated`="red",`Original`="gray")) +
  ylab("MTZ peak data (log transformed before interpolation)")


## filter out June - Dec for background data
log.interp.orig.cpr.df <- as.data.frame(log.interp.orig.cpr)
log.interp.ccb.calanus <-log.interp.orig.cpr.df %>%
  dplyr::filter(Month >= 1 & Month <= 5)

sfc.ts.centro.df <- as.data.frame(sfc.ts.centro)
pseudo.centro.mtz <- dplyr::filter(sfc.ts.centro.df, Month >= 1 & Month <= 5)


## bind the months and years and clean up dataframe
patchy.background <- cbind(patchy[, 1:2], x.impute.peak.centro, 
                           x.impute.peak.pseudo, x.impute.peak.cal, 
                           x.impute.peak.mtz)


## bind the peak patchy data with the background levels. 
patchy.background.2 <- cbind(patchy.background, log.interp.ccb.calanus[ , 7], 
                             pseudo.centro.mtz[, 11:13])


colnames(patchy.background.2) <- c("Years", "Months", "Peak.Centro", "Peak.Pseudo",
                                 "Peak.Cal", "Peak.MTZ", "background.Cal", 
                                 "background.MTZ", "background.Pseudo", 
                                 "background.Centro")

## calculate concentration index
conc.index <- (patchy.background.2) %>%
               dplyr::mutate(centro.conc = Peak.Centro/background.Centro) %>%
               dplyr::mutate(pseudo.conc = Peak.Pseudo/background.Pseudo) %>%
               dplyr::mutate(cal.conc = Peak.Cal/background.Cal) %>%
               dplyr::mutate(mtz.conc = Peak.MTZ/background.MTZ)



concentration.index.plot <- ggplot(conc.index, aes(x = Months, y = centro.conc, color = "Centropages spp.")) +
  geom_point() +
  geom_point(aes(x = Months, y = pseudo.conc, color = "Pseudocalanus spp."))+
  geom_point(aes(x = Months, y = cal.conc, color = "Calanus finmarchicus")) +
  geom_point(aes(x = Months, y = mtz.conc, color = "MTZ")) +
  facet_wrap(~Years) +
  labs(x = "Month", y = "Concentration index") +
  theme(legend.title=element_blank()) +
  guides(col = guide_legend(ncol = 2)) +
  theme(legend.position = c(0.80, 0.16)) 
  
  

## relationship between patchiness and strat

strat.patch <- cbind(ccb.strat.interpolate[,1], conc.index)



cal.strat.lm <- lm(log(cal.conc) ~ ccb.strat.interpolate[,1], strat.patch)

## pull out coefs to put on ggplot
a <- signif(coef(cal.strat.lm)[1], digits = 2)
b <- signif(coef(cal.strat.lm)[2], digits = 2)
c <- signif(summary(cal.strat.lm)$adj.r.squared, digits = 4)
textlab <- paste("y = ",b,"x + ",a, sep="")
textlab2 <- paste("adj. r^2 = ",c, sep = "")

calpatch.strat <- ggplot(strat.patch, aes(x = ccb.strat.interpolate[,1], y = log(cal.conc))) +
  geom_point() +
  stat_smooth(method = "lm") +
  annotate("text", x = 2.5, y = 7.5, label = textlab, color="black", 
           size = 5, parse=FALSE) +
  annotate("text", x = 2.5, y = 6, label = textlab2, color = "black",
           size = 5, parse = FALSE) +
  xlab("Stratification") + ylab("Log Calanus concentration index")


mtz.strat.lm <- lm(log(mtz.conc) ~ ccb.strat.interpolate[,1], strat.patch)
## pull out coefs to put on ggplot
a <- signif(coef(mtz.strat.lm)[1], digits = 2)
b <- signif(coef(mtz.strat.lm)[2], digits = 2)
c <- signif(summary(mtz.strat.lm)$adj.r.squared, digits = 4)
textlab <- paste("y = ",b,"x + ",a, sep="")
textlab2 <- paste("adj. r^2 = ",c, sep = "")

mtzpatch.strat <- ggplot(strat.patch, aes(x = ccb.strat.interpolate[,1], y = log(mtz.conc))) +
  geom_point() +
  stat_smooth(method = "lm") +
  annotate("text", x = 2.5, y = 7.5, label = textlab, color="black", 
           size = 5, parse=FALSE) +
  annotate("text", x = 2.5, y = 6, label = textlab2, color = "black",
           size = 5, parse = FALSE) +
  labs(x = "Stratification", y = "Log MTZ concentration index")


pseudo.strat.lm <- lm(log(pseudo.conc) ~ ccb.strat.interpolate[,1], strat.patch)
## pull out coefs to put on ggplot
a <- signif(coef(pseudo.strat.lm)[1], digits = 2)
b <- signif(coef(pseudo.strat.lm)[2], digits = 2)
c <- signif(summary(pseudo.strat.lm)$adj.r.squared, digits = 4)
textlab <- paste("y = ",b,"x + ",a, sep="")
textlab2 <- paste("adj. r^2 = ",c, sep = "")

pseudopatch.strat <- ggplot(strat.patch, aes(x = ccb.strat.interpolate[,1], y = log(pseudo.conc))) +
  geom_point() +
  stat_smooth(method = "lm") +
  annotate("text", x = 2.5, y = 7.5, label = textlab, color="black", 
           size = 5, parse=FALSE) +
  annotate("text", x = 2.5, y = 6, label = textlab2, color = "black",
           size = 5, parse = FALSE) +
  labs(x = "Stratification", y = "Log Pseudocalanus spp. concentration index")


centro.strat.lm <- lm(log(centro.conc) ~ ccb.strat.interpolate[,1], strat.patch)
## pull out coefs to put on ggplot
a <- signif(coef(centro.strat.lm)[1], digits = 2)
b <- signif(coef(centro.strat.lm)[2], digits = 2)
c <- signif(summary(centro.strat.lm)$adj.r.squared, digits = 4)
textlab <- paste("y = ",b,"x + ",a, sep="")
textlab2 <- paste("adj. r^2 = ",c, sep = "")


centropatch.strat <- ggplot(strat.patch, aes(x = ccb.strat.interpolate[,1], y = log(centro.conc))) +
  geom_point() +
  stat_smooth(method = "lm") +
  annotate("text", x = 2.5, y = 7.5, label = textlab, color="black", 
           size = 5, parse=FALSE) +
  annotate("text", x = 2.5, y = 6, label = textlab2, color = "black",
           size = 5, parse = FALSE) +
  labs(x = "Stratification", y = "Log Centropages spp. concentration index")



###########################
##FAKE POPULATION ESTIMATES
##########################
##these are approximate population estimates I pulled from Figure 2 of Pace et al.2017
## at some point I need to request the real population estimates from NARWC
pop.est <- read.csv("/Users/laura.ganley001/Documents/R_Projects/SEM/raw_data/fake_population_data.csv")

## just keep first two columns
pop.est.fake <- pop.est[,1:3]


#############################
###Put everything in 1 dataframe
#############################

## local wind (t)
## keep wind direction from only one buoy for each month.  
cqx_boston_gloucester$local.wind.direc <- cqx_boston_gloucester$wd.average.gloucester
cqx_boston_gloucester$local.wind.direc[!is.na(cqx_boston_gloucester$wd.average.cqx)] <- cqx_boston_gloucester$wd.average.cqx[!is.na(cqx_boston_gloucester$wd.average.cqx)]
cqx_boston_gloucester$local.wind.direc[!is.na(cqx_boston_gloucester$wd.average.boston)] <- cqx_boston_gloucester$wd.average.boston[!is.na(cqx_boston_gloucester$wd.average.boston)]
cqx_boston_gloucester <- dplyr::arrange(cqx_boston_gloucester, YYYY, MM)
## keep wind speed from only one buoy for each month
cqx_boston_gloucester$local.wind.spd <- cqx_boston_gloucester$ws.scalar.average.gloucester
cqx_boston_gloucester$local.wind.spd[!is.na(cqx_boston_gloucester$ws.scalar.average.cqx)] <- cqx_boston_gloucester$ws.scalar.average.cqx[!is.na(cqx_boston_gloucester$ws.scalar.average.cqx)]
cqx_boston_gloucester$local.wind.spd[!is.na(cqx_boston_gloucester$ws.scalar.average.boston)] <- cqx_boston_gloucester$ws.scalar.average.boston[!is.na(cqx_boston_gloucester$ws.scalar.average.boston)]
cqx_boston_gloucester <- dplyr::arrange(cqx_boston_gloucester, YYYY, MM)


## regional wind (t)
## keep wind direction from only one buoy for each month.  Wind direction needs to be 
## corrected so confidence intervals overlap 1
mattinicus_gom_fil$reg.wind.direc <- mattinicus_gom_fil$wd.average.mattinicus
mattinicus_gom_fil$reg.wind.direc[!is.na(mattinicus_gom_fil$wd.average.gom)] <- mattinicus_gom_fil$wd.average.gom[!is.na(mattinicus_gom_fil$wd.average.gom)]
mattinicus_gom_fil <- dplyr::arrange(mattinicus_gom_fil, YYYY, MM)
## keep wind speed from only one buoy for each month
mattinicus_gom_fil$reg.wind.spd <- mattinicus_gom_fil$ws.scalar.average.mattinicus
mattinicus_gom_fil$reg.wind.spd[!is.na(mattinicus_gom_fil$ws.scalar.average.gom)] <- mattinicus_gom_fil$ws.scalar.average.gom[!is.na(mattinicus_gom_fil$ws.scalar.average.gom)]
mattinicus_gom_fil <- dplyr::arrange(mattinicus_gom_fil, YYYY, MM)

## local stratification (t)
ccb.strat.interpolate

##local sst (t)

##habitat suitability (t)

##regional calanus (t-2 months)

##regional sst (t-?)

##regional sst (t)

##mtz (t)
mtz.ccb <- as.numeric(as.character(sfc.ts.centro[,11]))
mtz.ccb <- as.data.frame(mtz.ccb) %>%
  dplyr::mutate(mtz.sq = (mtz.ccb)^2)
colnames(mtz.ccb) <- c("mtz", "mtz.sq")

##calanus (t)
calanus.ccb <-as.numeric(as.character(log.interp.orig[,7]))
calanus.ccb <- as.data.frame(calanus.ccb)
colnames(calanus.ccb) <- c("CalCCB")

##pseudo (t)
pseudo.ccb <- as.numeric(as.character(sfc.ts.centro[, 12]))
pseudo.ccb <- as.data.frame(pseudo.ccb)
colnames(pseudo.ccb) <- c("pseudo")

##centro (t)
centro.ccb <- as.numeric(as.character(sfc.ts.centro[,13]))
centro.ccb <- as.data.frame(centro.ccb)
colnames(centro.ccb) <- c("centro")


##nao (t)
NAO_AO.98.17[,4]

## nao (t-52)
NAO.AO.prep <- as.data.frame(sfc.ts.centro)
NAO.AO.more.prep <- dplyr::full_join(NAO.AO.prep, NAO_AO.94)
NAO.AO.arrange <- dplyr::arrange(NAO.AO.more.prep, Year, Month)
NAO.AO.arrange$NAO.lag.52 <- dplyr::lag(NAO.AO.arrange$NAO_index, n = 52)

##ao (t)
NAO_AO.98.17[,3]

## ao (t-5) this has a five month lag so does it get lagged by 4?
NAO.AO.arrange$AO.lag.5 <- dplyr::lag(NAO.AO.arrange$AO_index, n = 5)
##only keep 1998 - 2017 jan = may
NAO.AO.lag <- NAO.AO.arrange %>%
  dplyr::filter(Year >= 1998 & Month >=1 & Month <= 5)


## right whale abundance written to a .csv in distance_est_jan_feb.R and then loaded into here
eg.abund <- read.csv("/Users/laura.ganley001/Documents/R_Projects/SEM/raw_data/eg.abund.csv")
eg.abund <- eg.abund$abundance
eg.abund <- as.data.frame(eg.abund)

## yearly population estimate (only varies between years)
pop.est.fake[,3]

## bind together data that still has june - dec
june.dec.data <- cbind(NAO_AO.98.17, mtz.ccb, 
                  calanus.ccb, pseudo.ccb, centro.ccb)

## remove unnecssary months
jan.may.data <- june.dec.data %>%
  dplyr::filter(Month >=1 & Month <= 5)

## now bind variables that are already only jan = may
all.data <- cbind(jan.may.data, pop.est.fake[,3], ccb.strat.interpolate$strat.interpolate, 
                  cqx_boston_gloucester$local.wind.direc, 
                  cqx_boston_gloucester$local.wind.spd, mattinicus_gom_fil$reg.wind.direc, 
                  mattinicus_gom_fil$reg.wind.spd, NAO.AO.lag$NAO.lag.52, NAO.AO.lag$AO.lag.5, 
                  eg.abund)
colnames(all.data)<- c("Year", "Month", "AO", "NAO", "MTZ", "MTZ.sq", "CalCCB",
                       "Pseudo", "Centro", "pop.est", "CCBStrat", "LocalWindDirec", "LocalWindSpd", 
                       "RegWindDirec", "RegWindSpd", "NAOlag52", "AOlag5", "eg.abund")




###################################
########TRY MINI BAYESIAN SEM
###################################

eg_mod <- bf(eg.abund ~ log(MTZ.sq) + log(MTZ.sq) * log(CCBStrat))
local_strat_mod <- bf(log(CCBStrat) ~ RegWindDirec * RegWindSpd)
mtz_mod <- bf(log(MTZ.sq) ~ log(CCBStrat) + RegWindDirec * RegWindSpd)
reg_wind_spd_mod <- bf(RegWindSpd ~ NAO + RegWindDirec)
reg_wind_direc_mod <- bf(RegWindDirec ~ NAO)


k_fit_brms <- brm(eg_mod + 
                  local_strat_mod +
                  mtz_mod +
                  reg_wind_spd_mod +
                  reg_wind_direc_mod, 
                  data=all.data, 
                  cores=4, chains = 2)

## how'd the model do?
plot(k_fit_brms)
summary(k_fit_brms)

#################################
#########TRY MINI FREQ PIECEWISE SEM
#################################
library(nlme)
library(lme4)
library(statmod)

abund.glmer <- glmer(eg.abund ~ pop.est + MTZ + MTZ:CCBStrat + (1|Year), 
      family=Gamma(link=log), data = all.data)

abund.lmer <- lme(eg.abund ~ pop.est + MTZ + MTZ:CCBStrat, random = ~1|Year,  
                     data = all.data)

abund.lm <- lm(eg.abund ~ pop.est + MTZ + MTZ:CCBStrat,  
               data = all.data)

abund.tweed <- glmer(eg.abund ~ pop.est + MTZ + MTZ*CCBStrat + (1|Year), 
                   family=tweedie(var.power=1.1, link.power=0), data = all.data)


abund.tweed <- glm(eg.abund ~ pop.est + MTZ + MTZ*CCBStrat, 
                     family=tweedie(var.power=1.1, link.power=0), data = all.data)

##Fake model 1
k_fit_psem <- psem(
  glmer(eg.abund ~ pop.est + MTZ + MTZ * CCBStrat + (1|Year), 
        family=tweedie(var.power=1.1, link.power=0), data = all.data), ## include random effect of Year because the population estimate was measured on a yearly level.
  lme(CCBStrat ~ RegWindDirec * RegWindSpd, random = ~1|Year, data = all.data),
  lme(MTZ.sq ~ CCBStrat + RegWindDirec * RegWindSpd, random = ~1|Year, data = all.data),
  lme(RegWindSpd ~ NAO + RegWindDirec, random = ~1|Year, data = all.data),
  lme(RegWindDirec ~ NAO, random = ~1|Year, data = all.data),
  data = all.data
)

summary(k_fit_psem)
## the marginal R-squared is the variance explained by only the fixed effects
## the conditional R-squared describes the proportion of the variance explained by both the fixed and random factors

##Fake model 2
k_fit_psem2 <- psem(
  lme(eg.abund ~ pop.est + log(CalCCB) + log(CalCCB) * log(CCBStrat), 
      random = ~1|Year, data = all.data), ## include random effect of Year because the population estimate was measured on a yearly level.
  lme(log(CCBStrat) ~ RegWindDirec * RegWindSpd, random = ~1|Year, data = all.data),
  lme(log(CalCCB) ~ log(CCBStrat) + RegWindDirec * RegWindSpd, random = ~1|Year, data = all.data),
  lme(RegWindSpd ~ NAO + RegWindDirec, random = ~1|Year, data = all.data),
  lme(RegWindDirec ~ NAO, random = ~1|Year, data = all.data),
  data = all.data
)

summary(k_fit_psem2)

## Fake model 3
k_fit_psem3 <- psem(
  lme(eg.abund ~ pop.est + log(CalCCB) + log(CalCCB) * log(CCBStrat), 
      random = ~1|Year, data = all.data), ## include random effect of Year because the population estimate was measured on a yearly level.
  lme(log(CCBStrat) ~ LocalWindDirec * LocalWindSpd + RegWindSpd, random = ~1|Year, data = all.data),
  lme(log(CalCCB) ~ log(CCBStrat) + LocalWindSpd + RegWindDirec * RegWindSpd, random = ~1|Year, data = all.data),
  lme(RegWindSpd ~ NAO + RegWindDirec, random = ~1|Year, data = all.data),
  lme(RegWindDirec ~ NAO, random = ~1|Year, data = all.data),
  lme(LocalWindSpd ~ NAO + LocalWindDirec, random = ~1|Year, data = all.data),
  lme(LocalWindDirec ~ NAO, random = ~1|Year, data = all.data),
  data = all.data
)

summary(k_fit_psem3)

## try some quadratics for MTZ and eg abundance (we now MTZ and eg abundance don't
## have a linear relationship)
mtz.lm <- lm(eg.abund ~ log.mtz, data = all.data)
plot(eg.abund ~ log.mtz, pch=16, ylab = "eg.abund ", data = all.data, cex.lab = 1.3, col = "red" )
abline(mtz.lm, col = "blue")

log.mtz <- log(all.data$MTZ)
mtz.sq <- (log.mtz)^2
mtz.cu <- (log.mtz)^3
mtz.qu <- (log.mtz)^4
quadratic.model <-lm(eg.abund ~ log.mtz + mtz.sq, data = all.data)
mtzvals <- seq(0, 15, 1)
predictedabund <- predict(quadratic.model, list(log.mtz = mtzvals, mtz.sq=mtzvals^2))
plot(eg.abund ~ log.mtz, pch=16, ylab = "eg.abund ", data = all.data, cex.lab = 1.3, col = "red" )
lines(mtzvals, predictedabund, col = "darkgreen", lwd = 3)


log.CalCCB <- log(all.data$CalCCB)
CalCCB.sq <- (log.CalCCB)^2
CalCCB.cu <- (log.CalCCB)^3
CalCCB.qu <- (log.CalCCB)^4
quadratic.model <-lm(eg.abund ~ log.CalCCB + CalCCB.sq, data = all.data)
CalCCBvals <- seq(0, 15, 1)
predictedabund <- predict(quadratic.model, list(log.CalCCB = CalCCBvals, CalCCB.sq=CalCCBvals^2))
plot(eg.abund ~ log.CalCCB, pch=16, ylab = "eg.abund ", data = all.data, cex.lab = 1.3, col = "red" )
lines(CalCCBvals, predictedabund, col = "darkgreen", lwd = 3)



