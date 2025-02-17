###########################################################################
#                                                                         #
#                                                                         #
#     Capstone project - Air Quality in Madrid - From 2001 to 2018        #
#                                                                         #
#                                                                         #
###########################################################################

#Loading the necessary libraries
#For data frame operations
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
#For parsing date
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
#For modelling and predictions
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

#Daily average variation
madrid.daily <- madrid.airquality %>%
  group_by(date) %>%
  summarise(NO_2.mean = mean(NO_2, na.rm = T),
            O_3.mean = mean(O_3, na.rm = T),
            PM10.mean = mean(PM10, na.rm = T),
            SO_2.mean = mean(SO_2, na.rm = T),
            CO.mean = mean(CO, na.rm = T),
            PM25.mean = mean(PM25, na.rm = T))

#Getting the total pollutant
madrid.daily <- mutate(madrid.daily, 
                       tot_poll = rowSums(madrid.daily[,-1], 
                                          na.rm = T))

#Plottind the daily data frame
madrid.daily %>% 
  ggplot(aes(x = as.Date(date), 
             y = tot_poll)) + 
  geom_line(color = 'blue') +
  theme(legend.position = "none",
        panel.background = element_rect(fill = "gray94", colour = "white",
                                        size = 0.5, linetype = "solid"),
        legend.background = element_rect(fill = "gray94"),
        plot.background = element_rect(fill = "gray94"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour =                                            "white"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid', colour =                                           "white"),  
        plot.title = element_text(hjust = 0, face = 'bold',color = 'black')) +
  labs(x = '', 
       y = 'Total Pollutants (??g/m�)',
       title='Daily Average of Air Quality in Madrid', 
       subtitle='Indicators: NO2, O3, PM10, SO2, CO, PM25') +
  theme(axis.text.y =element_text(angle = 0,
                                  size = 8),
        axis.text.x=element_text(angle = 90, 
                                 size = 8)) +
  scale_x_date(breaks = seq(as.Date("2001-01-01"), 
                            as.Date("2018-07-01"), 
                            by="12 months"), 
               date_labels = "%Y")

#Monthly average variation
madrid.monthly <- madrid.airquality %>%
  group_by(year, month) %>%
  summarise(NO_2.mean = mean(NO_2, na.rm = T),
            O_3.mean = mean(O_3, na.rm = T),
            PM10.mean = mean(PM10, na.rm = T),
            SO_2.mean = mean(SO_2, na.rm = T),
            CO.mean = mean(CO, na.rm = T),
            PM25.mean = mean(PM25, na.rm = T)) %>%
  mutate(time = paste(year, "-", month, "- 01", sep = ""))

#Getting the total pollutant
madrid.monthly$tot_poll_mon <- rowSums(madrid.monthly[,3:8], na.rm = T)

#The line of code above selects only the columns with the averages. In order to check it out, 
#uncomment the next code line:  
#str(madrid.monthly)
madrid.monthly$time_mon = as.Date(madrid.monthly$time, format = "%Y-%m-%d")

#Plottind the monthly data frame
madrid.monthly %>% 
  ggplot(aes(x = time_mon, 
             y = tot_poll_mon)) + 
  geom_line(color = 'black') +
  theme(legend.position = "none",
        panel.background = element_rect(fill = "gray94", colour = "white",
                                        size = 0.5, linetype = "solid"),
        legend.background = element_rect(fill = "gray94"),
        plot.background = element_rect(fill = "gray94"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour =                                            "white"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid', colour =                                           "white"),  
        plot.title = element_text(hjust = 0, face = 'bold',color = 'black')) +
  labs(x = '', 
       y = 'Total Pollutants (??g/m�)',
       title='Monthly Average of Air Quality in Madrid',
       subtitle='Indicators: NO2, O3, PM10, SO2, CO, PM25') +
  theme(axis.text.y =element_text(angle = 0,
                                  size = 8),
        axis.text.x=element_text(angle = 90, 
                                 size = 8)) +
  scale_x_date(breaks = seq(as.Date("2001-01-01"), 
                            as.Date("2018-07-01"), 
                            by="6 months"), 
               date_labels = "%b-%y") +
  scale_y_continuous(breaks = seq(0,250,50), lim = c(0,250))

#Yearly average variation
madrid.yearly <- madrid.airquality %>%
  select(year, station, NO_2, O_3, PM10, SO_2, CO, PM25) %>%
  group_by(year) %>%
  summarise(NO_2.mean = mean(NO_2, na.rm = T),
            O_3.mean = mean(O_3, na.rm = T),
            PM10.mean = mean(PM10, na.rm = T),
            SO_2.mean = mean(SO_2, na.rm = T),
            CO.mean = mean(CO, na.rm = T),
            PM25.mean = mean(PM25, na.rm = T))

#Getting the total pollutant  
madrid.yearly$tot_poll_year <- rowSums(madrid.yearly[,2:7], na.rm = T)
#Same as above, this selects only the columns with the averages. In order to check it out, 
#uncomment the next code line:  
#str(madrid.yearly)

#Plottind the data frame
madrid.yearly %>%
  ggplot(aes(year, tot_poll_year, group = 1)) + 
  geom_point(aes(year, tot_poll_year), size = 2, color = 'darkblue') + 
  geom_line(size = 3, alpha = 0.5, color = 'blue') +
  theme(legend.position = "none",
        panel.background = element_rect(fill = "gray94", 
                                        colour = "white",
                                        size = 0.5, 
                                        linetype = "solid"),
        legend.background = element_rect(fill = "gray94"),
        plot.background = element_rect(fill = "gray94"),
        panel.grid.major = element_line(size = 0.5, 
                                        linetype = 'solid', 
                                        colour = "white"), 
        panel.grid.minor = element_line(size = 0.25, 
                                        linetype = 'solid', 
                                        colour = "white"),  
        plot.title = element_text(hjust = 0, 
                                  face = 'bold',
                                  color = 'black')) +
  labs(x = 'Month', 
       y = 'Total Pollutants (??g/m�)', 
       title='Yearly Average of Air Quality in Madrid', 
       subtitle='Indicators: NO2, O3, PM10, SO2, CO, PM25') +
  theme(axis.text.y =element_text(angle = 0,
                                  size = 8),
        axis.text.x=element_text(angle = 90, 
                                 size = 8))

#In order to avoid any conflit with the function month, 
#we decided to call our set of month names "monhtly"
monthly <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

#Plotting the monthly average variation through the all time series (2001-2018)
madrid.monthly %>%
  group_by(month) %>%
  mutate(poll_mean = mean(tot_poll_mon)) %>%
  ggplot(aes(month, poll_mean, group = 1)) + 
  geom_point(aes(month, tot_poll_mon), size = 2, color = 'darkblue') + 
  geom_line(size = 1.5, alpha = 0.5, color = 'blue') +
  scale_x_discrete(labels = monthly) +
  theme(legend.position = "none",
        panel.background = element_rect(fill = "gray94", 
                                        colour = "white",
                                        size = 0.5, 
                                        linetype = "solid"),
        legend.background = element_rect(fill = "gray94"),
        plot.background = element_rect(fill = "gray94"),
        panel.grid.major = element_line(size = 0.5, 
                                        linetype = 'solid', 
                                        colour = "white"), 
        panel.grid.minor = element_line(size = 0.25, 
                                        linetype = 'solid', 
                                        colour = "white"),  
        plot.title = element_text(hjust = 0, 
                                  face = 'bold',
                                  color = 'black')) +
  labs(x = 'Month', 
       y = 'Total Polution (??g/m�)', 
       title='Monthly Average of Air Quality in Madrid', 
       subtitle='Indicators: NO2, O3, PM10, SO2, CO, PM25') +
  theme(axis.text.y = element_text(size = 8),
        axis.text.x=element_text(
          angle = 90,
          size = 8)) +
  facet_wrap(~year)

#Modelling and making predictions using the forecast library
#Predicting the total pollutant by month
library(forecast)
#The time series to be take into account. We are going to predict the total pollutants by month
monthly.ts <- ts(madrid.monthly[,10],start=c(2001,1), end=c(2018,5), frequency = 12)

#Simple exponential smooth (SES) model
#Making predictions for the next 36 months
model.ses.ts <- ses(monthly.ts, h=36)
summary(model.ses.ts)

#Plotting the predictions
model.ses.ts %>%
  autoplot() + 
  theme(axis.text.x = element_text(angle = 0),
        panel.background = element_rect(fill = "gray94", colour = "white",
                                        size = 0.5, linetype = "solid"),
        legend.background = element_rect(fill = "gray94"),
        plot.background = element_rect(fill = "gray94"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "white"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid', colour = "white"),  
        plot.title = element_text(hjust = 0, face = 'bold',color = 'black')) +
  labs(x = '', y = 'Total Polution (??g/m�)', title='Forecasting of Air Quality using SES', subtitle='Indicators: NO2, O3, PM10, SO2, CO, PM25') +
  scale_y_continuous(breaks = seq(0,250,50), lim = c(0,250))

#Holt's trend model
#Making predictions for the next 36 months
model.holt.ts <- holt(monthly.ts, h=36)
summary(model.holt.ts)

#Plotting the predictions
model.holt.ts %>%
  autoplot() + 
  theme(axis.text.x = element_text(angle = 0),
        panel.background = element_rect(fill = "gray94", colour = "white",
                                        size = 0.5, linetype = "solid"),
        legend.background = element_rect(fill = "gray94"),
        plot.background = element_rect(fill = "gray94"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "white"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid', colour = "white"),  
        plot.title = element_text(hjust = 0, face = 'bold',color = 'black')) +
  labs(x = '', y = 'Total Polution (??g/m�)', title='Forecasting of Air Quality using Holt\'s trend', subtitle='Indicators: NO2, O3, PM10, SO2, CO, PM25') +
  scale_y_continuous(breaks = seq(0,250,50), lim = c(0,250))

#Autoregressive Integrated Moving Average (ARIMA) model
#Making predictions for the next 36 months
model.arima.ts <- auto.arima(monthly.ts)
summary(model.arima.ts)

#Plotting the predictions
model.arima.ts %>%
  forecast(h=36) %>%
  autoplot() + 
  theme(axis.text.x = element_text(angle = 0),
        panel.background = element_rect(fill = "gray94", colour = "white",
                                        size = 0.5, linetype = "solid"),
        legend.background = element_rect(fill = "gray94"),
        plot.background = element_rect(fill = "gray94"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "white"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid', colour = "white"),  
        plot.title = element_text(hjust = 0, face = 'bold',color = 'black')) +
  labs(x = '', y = 'Total Polution (??g/m�)', title='Forecasting of Air Quality using ARIMA', subtitle='Indicators: NO2, O3, PM10, SO2, CO, PM25') +
  scale_y_continuous(breaks = seq(0,250,50), lim = c(0,250))

#TBATS model
# T: Trigonometric terms for seasonality  
# B: Box-Cox transformations for heterogeneity  
# A: Autoregressive Moving Average (ARMA) errors for short-term dynamics  
# T: Trend  
# S: Seasonal (including multiple and non-integer periods)  

#Making predictions for the next 36 months
model.tbats.ts <- tbats(monthly.ts)
summary(model.tbats.ts)

#Plotting the predictions
model.arima.ts %>%
  forecast(h=36) %>%
  autoplot() + 
  theme(axis.text.x = element_text(angle = 0),
        panel.background = element_rect(fill = "gray94", colour = "white",
                                        size = 0.5, linetype = "solid"),
        legend.background = element_rect(fill = "gray94"),
        plot.background = element_rect(fill = "gray94"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "white"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid', colour = "white"),  
        plot.title = element_text(hjust = 0, face = 'bold',color = 'black')) +
  labs(x = '', y = 'Total Polution (??g/m�)', title='Forecasting of Air Quality using TBATS', subtitle='Indicators: NO2, O3, PM10, SO2, CO, PM25') +
  scale_y_continuous(breaks = seq(0,250,50), lim = c(0,250))