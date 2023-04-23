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
dv_BALTIMORE.csv[c('year', 'month','day')] <- str_split_fixed(dv_BALTIMORE.csv$DATE, '-',n=3)
#dv_BALTIMORE.csv$year<-paste0("20", dv_BALTIMORE.csv$year)
dv_BALTIMORE.csv$year<-as.numeric(dv_BALTIMORE.csv$year)
dv_BALTIMORE.csv$month<-as.numeric(dv_BALTIMORE.csv$month)
dv_BALTIMORE.csv$day<-as.numeric(dv_BALTIMORE.csv$day)
dv_BALTIMORE.csv$city<-'Baltimore'
dv_BALTIMORE<-dv_BALTIMORE.csv

weather_1<-dplyr::bind_rows(weather_1,dv_BALTIMORE)

#Chicago
dv_CHICAGO.csv[c('year', 'month','day')] <- str_split_fixed(dv_CHICAGO.csv$DATE, '-',n=3)
#dv_CHICAGO.csv$year<-paste0("20", dv_CHICAGO.csv$year)
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
dv_CLEVELAND.csv[c('year', 'month','day')] <- str_split_fixed(dv_CLEVELAND.csv$DATE, '-',n=3)
#dv_CLEVELAND.csv$year<-paste0("20", dv_CLEVELAND.csv$year)
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

#KANSAS CITY
`dv_KANSAS CITY.csv`[c('year', 'month','day')] <- str_split_fixed(`dv_KANSAS CITY.csv`$DATE, '-',n=3) 
#`dv_KANSAS CITY.csv`$year<-paste0("20", `dv_KANSAS CITY.csv`$year)
`dv_KANSAS CITY.csv`$year<-as.numeric(`dv_KANSAS CITY.csv`$year)
`dv_KANSAS CITY.csv`$month<-as.numeric(`dv_KANSAS CITY.csv`$month)
`dv_KANSAS CITY.csv`$day<-as.numeric(`dv_KANSAS CITY.csv`$day)
`dv_KANSAS CITY.csv`$city<-'Kansas City'
`dv_KANSAS CITY`<-`dv_KANSAS CITY.csv`

weather_1<-dplyr::bind_rows(weather_1,`dv_KANSAS CITY`)

#MIAMI
dv_MIAMI.csv[c('month', 'day','year')] <- str_split_fixed(dv_MIAMI.csv$DATE, '/',n=3) 
dv_MIAMI.csv$year<-paste0("20", dv_MIAMI.csv$year)
dv_MIAMI.csv$year<-as.numeric(dv_MIAMI.csv$year)
dv_MIAMI.csv$month<-as.numeric(dv_MIAMI.csv$month)
dv_MIAMI.csv$day<-as.numeric(dv_MIAMI.csv$day)
dv_MIAMI.csv$city<-'Miami'
dv_MIAMI<-dv_MIAMI.csv

weather_1<-dplyr::bind_rows(weather_1,dv_MIAMI)

#MILWAUKEE
dv_MILWAUKEE.csv[c('year', 'month','day')] <- str_split_fixed(dv_MILWAUKEE.csv$DATE, '-',n=3) 
#dv_MILWAUKEE.csv$year<-paste0("20", dv_MILWAUKEE.csv$year)
dv_MILWAUKEE.csv$year<-as.numeric(dv_MILWAUKEE.csv$year)
dv_MILWAUKEE.csv$month<-as.numeric(dv_MILWAUKEE.csv$month)
dv_MILWAUKEE.csv$day<-as.numeric(dv_MILWAUKEE.csv$day)
dv_MILWAUKEE.csv$city<-'Milwaukee'
dv_MILWAUKEE<-dv_MILWAUKEE.csv

weather_1<-dplyr::bind_rows(weather_1,dv_MILWAUKEE)

#MINNEAPOLIS
dv_MINNEAPOLIS.csv[c('year', 'month','day')] <- str_split_fixed(dv_MINNEAPOLIS.csv$DATE, '-',n=3) 
#dv_MINNEAPOLIS.csv$year<-paste0("20", dv_MINNEAPOLIS.csv$year)
dv_MINNEAPOLIS.csv$year<-as.numeric(dv_MINNEAPOLIS.csv$year)
dv_MINNEAPOLIS.csv$month<-as.numeric(dv_MINNEAPOLIS.csv$month)
dv_MINNEAPOLIS.csv$day<-as.numeric(dv_MINNEAPOLIS.csv$day)
dv_MINNEAPOLIS.csv$city<-'Minneapolis'
dv_MINNEAPOLIS<-dv_MINNEAPOLIS.csv

weather_1<-dplyr::bind_rows(weather_1,dv_MINNEAPOLIS)

#NEW YORK CITY
`dv_NEW YORK CITY.csv`[c('year', 'month','day')] <- str_split_fixed(`dv_NEW YORK CITY.csv`$DATE, '-',n=3)  
#`dv_NEW YORK CITY.csv`$year<-paste0("20", `dv_NEW YORK CITY.csv`$year)
`dv_NEW YORK CITY.csv`$year<-as.numeric(`dv_NEW YORK CITY.csv`$year)
`dv_NEW YORK CITY.csv`$month<-as.numeric(`dv_NEW YORK CITY.csv`$month)
`dv_NEW YORK CITY.csv`$day<-as.numeric(`dv_NEW YORK CITY.csv`$day)
`dv_NEW YORK CITY.csv`$city<-'New York'
`dv_NEW YORK CITY`<-`dv_NEW YORK CITY.csv`

weather_1<-dplyr::bind_rows(weather_1,`dv_NEW YORK CITY`)

dv_LA<-dv_OAKLAND.csv %>% filter(NAME == 'LOS ANGELES INTERNATIONAL AIRPORT, CA US')
dv_oakland<-dv_OAKLAND.csv %>% filter(NAME == 'OAKLAND INTERNATIONAL AIRPORT, CA US')
dv_seattle<-dv_OAKLAND.csv %>% filter(NAME == 'SEATTLE TACOMA AIRPORT, WA US')

#LA
dv_LA[c('month', 'day','year')] <- str_split_fixed(dv_LA$DATE, '/',n=3) 
dv_LA$year<-paste0("20", dv_LA$year)
dv_LA$year<-as.numeric(dv_LA$year)
dv_LA$month<-as.numeric(dv_LA$month)
dv_LA$day<-as.numeric(dv_LA$day)
dv_LA$city<-'Los Angeles'


weather_1<-dplyr::bind_rows(weather_1,dv_LA)

#Oakland
dv_oakland[c('month', 'day','year')] <- str_split_fixed(dv_oakland$DATE, '/',n=3) 
dv_oakland$year<-paste0("20", dv_oakland$year)
dv_oakland$year<-as.numeric(dv_oakland$year)
dv_oakland$month<-as.numeric(dv_oakland$month)
dv_oakland$day<-as.numeric(dv_oakland$day)
dv_oakland$city<-'Oakland'


weather_1<-dplyr::bind_rows(weather_1,dv_oakland)

#seattle
dv_seattle[c('month', 'day','year')] <- str_split_fixed(dv_seattle$DATE, '/',n=3) 
dv_seattle$year<-paste0("20", dv_seattle$year)
dv_seattle$year<-as.numeric(dv_seattle$year)
dv_seattle$month<-as.numeric(dv_seattle$month)
dv_seattle$day<-as.numeric(dv_seattle$day)
dv_seattle$city<-'Seattle'


weather_1<-dplyr::bind_rows(weather_1,dv_seattle)

#PHILADELPHIA
dv_PHILADELPHIA.csv[c('month', 'day','year')] <- str_split_fixed(dv_PHILADELPHIA.csv$DATE, '/',n=3) 
dv_PHILADELPHIA.csv$year<-paste0("20", dv_PHILADELPHIA.csv$year)
dv_PHILADELPHIA.csv$year<-as.numeric(dv_PHILADELPHIA.csv$year)
dv_PHILADELPHIA.csv$month<-as.numeric(dv_PHILADELPHIA.csv$month)
dv_PHILADELPHIA.csv$day<-as.numeric(dv_PHILADELPHIA.csv$day)
dv_PHILADELPHIA.csv$city<-'Philadelphia'
dv_PHILADELPHIA<-dv_PHILADELPHIA.csv

weather_1<-dplyr::bind_rows(weather_1,dv_PHILADELPHIA)

#PITTSBURGH
dv_PITTSBURGH.csv[c('year', 'month','day')] <- str_split_fixed(dv_PITTSBURGH.csv$DATE, '-',n=3)  
#dv_PITTSBURGH.csv$year<-paste0("20", dv_PITTSBURGH.csv$year)
dv_PITTSBURGH.csv$year<-as.numeric(dv_PITTSBURGH.csv$year)
dv_PITTSBURGH.csv$month<-as.numeric(dv_PITTSBURGH.csv$month)
dv_PITTSBURGH.csv$day<-as.numeric(dv_PITTSBURGH.csv$day)
dv_PITTSBURGH.csv$city<-'Pittsburgh'
dv_PITTSBURGH<-dv_PITTSBURGH.csv

weather_1<-dplyr::bind_rows(weather_1,dv_PITTSBURGH)

dv_denver<- `dv_SAN DIEGO.csv` %>% filter(NAME == 'DENVER INTERNATIONAL AIRPORT, CO US')
dv_phoenix<- `dv_SAN DIEGO.csv` %>% filter(NAME == 'PHOENIX AIRPORT, AZ US')
dv_sandiego<- `dv_SAN DIEGO.csv` %>% filter(NAME == 'SAN DIEGO INTERNATIONAL AIRPORT, CA US')
dv_sanfran<- `dv_SAN DIEGO.csv` %>% filter(NAME == 'SAN FRANCISCO INTERNATIONAL AIRPORT, CA US')

#denver
dv_denver[c('year', 'month','day')] <- str_split_fixed(dv_denver$DATE, '-',n=3)  
#dv_denver$year<-paste0("20", dv_denver$year)
dv_denver$year<-as.numeric(dv_denver$year)
dv_denver$month<-as.numeric(dv_denver$month)
dv_denver$day<-as.numeric(dv_denver$day)
dv_denver$city<-'Denver'


weather_1<-dplyr::bind_rows(weather_1,dv_denver)

#phoenix
dv_phoenix[c('year', 'month','day')] <- str_split_fixed(dv_phoenix$DATE, '-',n=3) 
#dv_phoenix$year<-paste0("20", dv_phoenix$year)
dv_phoenix$year<-as.numeric(dv_phoenix$year)
dv_phoenix$month<-as.numeric(dv_phoenix$month)
dv_phoenix$day<-as.numeric(dv_phoenix$day)
dv_phoenix$city<-'Phoenix'


weather_1<-dplyr::bind_rows(weather_1,dv_phoenix)

#sandiego
dv_sandiego[c('year', 'month','day')] <- str_split_fixed(dv_sandiego$DATE, '-',n=3) 
#dv_sandiego$year<-paste0("20", dv_sandiego$year)
dv_sandiego$year<-as.numeric(dv_sandiego$year)
dv_sandiego$month<-as.numeric(dv_sandiego$month)
dv_sandiego$day<-as.numeric(dv_sandiego$day)
dv_sandiego$city<-'San Diego'


weather_1<-dplyr::bind_rows(weather_1,dv_sandiego)

#sanfran
dv_sanfran[c('year', 'month','day')] <- str_split_fixed(dv_sanfran$DATE, '-',n=3) 
#dv_sanfran$year<-paste0("20", dv_sanfran$year)
dv_sanfran$year<-as.numeric(dv_sanfran$year)
dv_sanfran$month<-as.numeric(dv_sanfran$month)
dv_sanfran$day<-as.numeric(dv_sanfran$day)
dv_sanfran$city<-'San Francisco'


weather_1<-dplyr::bind_rows(weather_1,dv_sanfran)

#ST LOUIS
`dv_ST LOUIS.csv`[c('year', 'month','day')] <- str_split_fixed(`dv_ST LOUIS.csv`$DATE, '-',n=3) 
#`dv_ST LOUIS.csv`$year<-paste0("20", `dv_ST LOUIS.csv`$year)
`dv_ST LOUIS.csv`$year<-as.numeric(`dv_ST LOUIS.csv`$year)
`dv_ST LOUIS.csv`$month<-as.numeric(`dv_ST LOUIS.csv`$month)
`dv_ST LOUIS.csv`$day<-as.numeric(`dv_ST LOUIS.csv`$day)
`dv_ST LOUIS.csv`$city<-'St. Louis'
`dv_ST LOUIS`<-`dv_ST LOUIS.csv`

weather_1<-dplyr::bind_rows(weather_1,`dv_ST LOUIS`)

#TAMPA
dv_TAMPA.csv[c('month', 'day','year')] <- str_split_fixed(dv_TAMPA.csv$DATE, '/',n=3) 
dv_TAMPA.csv$year<-paste0("20", dv_TAMPA.csv$year)
dv_TAMPA.csv$year<-as.numeric(dv_TAMPA.csv$year)
dv_TAMPA.csv$month<-as.numeric(dv_TAMPA.csv$month)
dv_TAMPA.csv$day<-as.numeric(dv_TAMPA.csv$day)
dv_TAMPA.csv$city<-'St. Petersburg'
dv_TAMPA<-dv_TAMPA.csv

weather_1<-dplyr::bind_rows(weather_1,dv_TAMPA)


#WASHINGTON DC
`dv_WASHINGTON DC.csv`[c('month', 'day','year')] <- str_split_fixed(`dv_WASHINGTON DC.csv`$DATE, '/',n=3) 
`dv_WASHINGTON DC.csv`$year<-paste0("20", `dv_WASHINGTON DC.csv`$year)
`dv_WASHINGTON DC.csv`$year<-as.numeric(`dv_WASHINGTON DC.csv`$year)
`dv_WASHINGTON DC.csv`$month<-as.numeric(`dv_WASHINGTON DC.csv`$month)
`dv_WASHINGTON DC.csv`$day<-as.numeric(`dv_WASHINGTON DC.csv`$day)
`dv_WASHINGTON DC.csv`$city<-'Washington'
`dv_WASHINGTON DC`<-`dv_WASHINGTON DC.csv`

weather_1<-dplyr::bind_rows(weather_1,`dv_WASHINGTON DC`)


dv121<- sqldf::sqldf(
  'SELECT *
  FROM  dv12
  FULL JOIN weather_1
  ON weather_1.city=dv12.CITY AND
  weather_1.day=dv12.day AND
  weather_1.month=dv12.month AND
  weather_1.year=dv12.year
  ')

dv121<-dv121 %>% drop_na(STATION)
dv121<-dv121[,-c(10,11,14,15,18,19,20,22,25,26,27,30,31,34,35,37,38,39,42,43,46,47,48,50,66,67,68,69,70,73,74)]
dv121<-dv121[,-c(76,77,78,79,80,81,82)]
dv121<-dv121[,-c(30,31,34,3538,39,41,42,44)]
dv121 <- dv121 %>% 
  mutate_at(c(51:70), ~replace_na(.,0))
dv121 <- dv121 %>% drop_na(date)
#Changed from na to 0 because it is binary either that type of precipitation present or it isnt


dv121<-dv121[,-c(43,39)]
dv121<-dv121[,-39]
library(naniar)


dv121 <- dv121 %>% 
  mutate_at(c(45), ~replace_na(.,0))
#Was changed to zero instead of NA because it measures amount of water in snowfall aka if there is no snowfall it is left blank
#By some cities thus turned to zero

dv121 <- dv121 %>% 
  mutate_at(c(40), ~replace_na(.,0))
#Was changed because if no sonow in some of warmer climate then they dont record it

dv121 <- dv121 %>% 
  mutate_at(c(30:67), ~replace_na(.,0))
#Changes all missing weather to zero due to them not recording that weatehr statistic for the day

vis_miss(dv121, warn_large_data=F)

write.csv(dv121,'./data/cleaned_baseball_weather.csv')


