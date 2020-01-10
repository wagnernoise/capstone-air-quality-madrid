###########################################################################
#                                                                         #
#                                                                         #
#     Capstone project - Air Quality in Madrid - From 2001 to 2018        #
#                                                                         #
#                                                                         #
###########################################################################

#Loading the necessary libraries
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(forecast)) install.packages("forecast", repos = "http://cran.us.r-project.org")

#Setting the right directory path and importing csv files
setwd("C:/Users/DeOliW19830/Documents/RProjects/air-quality-madrid/air-quality-madrid")
stations.code <- read.csv("stations.csv", sep = ",")

#Getting a glimpse of the station data frame
glimpse(stations.code)

#Assigning the list of csv files to a variable
files <- list.files(
  path = "./csvs_per_year",
  pattern = "*.csv", 
  full.names = T)

#Looping through the files and joining all the data sets to a single data frame
madrid.airquality <- sapply(files, read_csv, simplify=FALSE) %>% bind_rows

#Getting the data frame structure
str(madrid.airquality, give.attr=F)

#Names of the columns
colnames(madrid.airquality) 

#Summarizing the data frame
summary(madrid.airquality)

#Ordering by date and parsing into different formats to be used
madrid.airquality <- madrid.airquality[with(madrid.airquality, order(date)),]
madrid.airquality$full.date.time <- as.POSIXct(madrid.airquality$date,format = "%Y-%m-%d %H:%M:%S", tz='CET')
madrid.airquality$date <- format(madrid.airquality$full.date.time, "%Y-%m-%d")
madrid.airquality$time <- format(madrid.airquality$full.date.time, "%T")
madrid.airquality$day <- format(madrid.airquality$full.date.time, "%d")
madrid.airquality$month <- format(madrid.airquality$full.date.time, "%m")
madrid.airquality$year <- format(madrid.airquality$full.date.time, "%Y")

#Total number of NA's
sum(is.na(madrid.airquality))

#Pollutants that we selected to be analyzed in the time series evolution
indicators <- c("NO_2", "O_3", "SO_2", "CO", "PM10", "PM25")

#Filtering only the subset of pollutants we want to analyze
madrid.airquality.indicators <- madrid.airquality[, indicators]

#Percentage of NA's in the subset
na.indicators <- data.frame(percent=round(
  colSums(
    is.na(madrid.airquality.indicators))/nrow(madrid.airquality.indicators)*100)
)

na.indicators

#Setting the column names in the NA data frame
na.indicators$pollutant <- rownames(na.indicators)
na.indicators$pollutant<-factor(na.indicators$pollutant, 
                                as.character(na.indicators$pollutant))

#Bar plotting the percentage of NA's
na.indicators %>% 
  mutate(pollutant = fct_reorder(pollutant, percent)) %>%
  ggplot(aes(pollutant, percent,fill = pollutant)) +
  geom_bar(stat = "identity") + 
  scale_fill_brewer() +
  geom_text(aes(label=paste0(percent,"%"), y=percent+0.7), 
            size = 3,
            hjust = -0.01
  ) +
  labs(x = "Pollutants", 
       y = "Percentage", 
       title = "Percentage of NAs in Four Pollutants") +
  coord_flip()

