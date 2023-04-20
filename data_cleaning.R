library(tidyverse)
library(dplyr)
library(readr)
library(plyr)
library(sqldf)
##########################################################################
# Read in the data
##########################################################################
dv<-read_csv('./dataquest-mlb-game-logs/data/game_logs.csv')
dv[c('year', 'month','day')] <- str_split_fixed(dv$date, '-',n=3)
dv$year<-as.numeric(dv$year)
dv$month<-as.numeric(dv$month)
dv$day<-as.numeric(dv$day)
dv1<-dv %>% filter(year >= 2010)
dv1<-dv1 %>% filter(year <= 2015)

dv1<-dv1[,c(1,4,7,10,11,13,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,
            34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,
            55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,162,163,164
            )]
dv1<-data.frame(dv1)

data_files <- list.files("./512_Project_Data")  # Identify file names
for(i in 1:length(data_files)) {                              # Head of for-loop
  assign(paste0("dv_", data_files[i]),                                   # Read and store data frames
         read.csv(paste0("./512_Project_Data/",
                          data_files[i])))
}

weather_clean_date = function(x){
  x[c('month', 'day','year')] <- str_split_fixed(x$DATE, '/',n=3) 
}
park_codes<-read_csv('./parkcode.txt')

dv12<- sqldf::sqldf(
  'SELECT dv1.*, park_codes.PARKID, park_codes.CITY
  FROM dv1, park_codes
  WHERE park_codes.PARKID=dv1.park_id
  '
)
# Boston
dv_BOSTON.csv[c('month', 'day','year')] <- str_split_fixed(dv_BOSTON.csv$DATE, '/',n=3) 
dv_BOSTON.csv$year<-paste0("20", dv_BOSTON.csv$year)
dv_BOSTON.csv$year<-as.numeric(dv_BOSTON.csv$year)
dv_BOSTON.csv$month<-as.numeric(dv_BOSTON.csv$month)
dv_BOSTON.csv$day<-as.numeric(dv_BOSTON.csv$day)
dv_BOSTON.csv$city<-'Boston'
dv_BOSTON<-dv_BOSTON.csv

weather_1<-dv_BOSTON
#Atlanta
dv_ATLANTA.csv[c('month', 'day','year')] <- str_split_fixed(dv_ATLANTA.csv$DATE, '/',n=3) 
dv_ATLANTA.csv$year<-paste0("20", dv_ATLANTA.csv$year)
dv_ATLANTA.csv$year<-as.numeric(dv_ATLANTA.csv$year)
dv_ATLANTA.csv$month<-as.numeric(dv_ATLANTA.csv$month)
dv_ATLANTA.csv$day<-as.numeric(dv_ATLANTA.csv$day)
dv_ATLANTA.csv$city<-'Atlanta'
dv_ATLANTA<-dv_ATLANTA.csv

weather_1<-dplyr::bind_rows(weather_1,dv_ATLANTA)

#BALTIMORE
dv_BALTIMORE.csv[c('month', 'day','year')] <- str_split_fixed(dv_BALTIMORE.csv$DATE, '/',n=3) 
dv_BALTIMORE.csv$year<-paste0("20", dv_BALTIMORE.csv$year)
dv_BALTIMORE.csv$year<-as.numeric(dv_BALTIMORE.csv$year)
dv_BALTIMORE.csv$month<-as.numeric(dv_BALTIMORE.csv$month)
dv_BALTIMORE.csv$day<-as.numeric(dv_BALTIMORE.csv$day)
dv_BALTIMORE.csv$city<-'Baltimore'
dv_BALTIMORE<-dv_BALTIMORE.csv

weather_1<-dplyr::bind_rows(weather_1,dv_BALTIMORE)

#Chicago
dv_CHICAGO.csv[c('month', 'day','year')] <- str_split_fixed(dv_CHICAGO.csv$DATE, '/',n=3) 
dv_CHICAGO.csv$year<-paste0("20", dv_CHICAGO.csv$year)
dv_CHICAGO.csv$year<-as.numeric(dv_CHICAGO.csv$year)
dv_CHICAGO.csv$month<-as.numeric(dv_CHICAGO.csv$month)
dv_CHICAGO.csv$day<-as.numeric(dv_CHICAGO.csv$day)
dv_CHICAGO.csv$city<-'Chicago'
dv_CHICAGO<-dv_CHICAGO.csv

weather_1<-dplyr::bind_rows(weather_1,dv_CHICAGO)

#CINCINNATI
dv_CINCINNATI.csv[c('month', 'day','year')] <- str_split_fixed(dv_CINCINNATI.csv$DATE, '/',n=3) 
dv_CINCINNATI.csv$year<-paste0("20", dv_CINCINNATI.csv$year)
dv_CINCINNATI.csv$year<-as.numeric(dv_CINCINNATI.csv$year)
dv_CINCINNATI.csv$month<-as.numeric(dv_CINCINNATI.csv$month)
dv_CINCINNATI.csv$day<-as.numeric(dv_CINCINNATI.csv$day)
dv_CINCINNATI.csv$city<-'Cincinnati'
dv_CINCINNATI<-dv_CINCINNATI.csv

weather_1<-dplyr::bind_rows(weather_1,dv_CINCINNATI)

#CLEVELAND
dv_CLEVELAND.csv[c('month', 'day','year')] <- str_split_fixed(dv_CLEVELAND.csv$DATE, '/',n=3) 
dv_CLEVELAND.csv$year<-paste0("20", dv_CLEVELAND.csv$year)
dv_CLEVELAND.csv$year<-as.numeric(dv_CLEVELAND.csv$year)
dv_CLEVELAND.csv$month<-as.numeric(dv_CLEVELAND.csv$month)
dv_CLEVELAND.csv$day<-as.numeric(dv_CLEVELAND.csv$day)
dv_CLEVELAND.csv$city<-'Cleveland'
dv_CLEVELAND<-dv_CLEVELAND.csv

weather_1<-dplyr::bind_rows(weather_1,dv_CLEVELAND)

#DETROIT
dv_DETROIT.csv[c('month', 'day','year')] <- str_split_fixed(dv_DETROIT.csv$DATE, '/',n=3) 
dv_DETROIT.csv$year<-paste0("20", dv_DETROIT.csv$year)
dv_DETROIT.csv$year<-as.numeric(dv_DETROIT.csv$year)
dv_DETROIT.csv$month<-as.numeric(dv_DETROIT.csv$month)
dv_DETROIT.csv$day<-as.numeric(dv_DETROIT.csv$day)
dv_DETROIT.csv$city<-'Detroit'
dv_DETROIT<-dv_DETROIT.csv

weather_1<-dplyr::bind_rows(weather_1,dv_DETROIT)

#HOUSTON
dv_HOUSTON.csv[c('month', 'day','year')] <- str_split_fixed(dv_HOUSTON.csv$DATE, '/',n=3) 
dv_HOUSTON.csv$year<-paste0("20", dv_HOUSTON.csv$year)
dv_HOUSTON.csv$year<-as.numeric(dv_HOUSTON.csv$year)
dv_HOUSTON.csv$month<-as.numeric(dv_HOUSTON.csv$month)
dv_HOUSTON.csv$day<-as.numeric(dv_HOUSTON.csv$day)
dv_HOUSTON.csv$city<-'Houston'
dv_HOUSTON<-dv_HOUSTON.csv

weather_1<-dplyr::bind_rows(weather_1,dv_HOUSTON)

#HOUSTON
dv_HOUSTON.csv[c('month', 'day','year')] <- str_split_fixed(dv_HOUSTON.csv$DATE, '/',n=3) 
dv_HOUSTON.csv$year<-paste0("20", dv_HOUSTON.csv$year)
dv_HOUSTON.csv$year<-as.numeric(dv_HOUSTON.csv$year)
dv_HOUSTON.csv$month<-as.numeric(dv_HOUSTON.csv$month)
dv_HOUSTON.csv$day<-as.numeric(dv_HOUSTON.csv$day)
dv_HOUSTON.csv$city<-'Houston'
dv_HOUSTON<-dv_HOUSTON.csv

weather_1<-dplyr::bind_rows(weather_1,dv_HOUSTON)


dv121<- sqldf::sqldf(
  'SELECT *
  FROM  dv12
  FULL JOIN weather_1
  ON weather_1.city=dv12.CITY AND
  weather_1.day=dv12.day AND
  weather_1.month=dv12.month AND
  weather_1.year=dv12.year
  ')


