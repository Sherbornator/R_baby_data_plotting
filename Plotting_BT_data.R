#################################################
#### Plotting of data from Baby Tracker in R ####
#################################################

###						 ###
###                      ###
###						 ###
#install.packages("data.table")
library(data.table)

#For the plot
#https://stackoverflow.com/questions/33782218/how-to-create-a-time-series-of-hourly-data?utm_medium=organic&utm_source=google_rich_qa&utm_campaign=google_rich_qa
#https://www.r-graph-gallery.com/283-the-hourly-heatmap/
##https://seaborn.pydata.org/generated/seaborn.heatmap.html

#install.packages("lubridate")
library(lubridate)     # for wday(...)
#install.packages("ggplot2")
library(ggplot2)
#install.packages("RColorBrewer")
library(RColorBrewer)

#install.packages("POSIX")


sleep <- read.csv('Eleanor_sleep.csv', header = TRUE, sep = ",", quote = "\"", dec = ".", fill = TRUE, comment.char = "")

#Format needs some massaging i.e. date and time duration changed from human readable to a useful format:
#      Baby             Time    Duration Note
#10 Eleanor 21/02/2018 07:00 1 hr 25 min

setDT(sleep)[, paste0(cbind("date", "time")) := tstrsplit(Time, " ")]

sleep$min <- NA
sleep$Duration <- as.character(sleep$Duration)

for(i in 1:nrow(sleep)) {
    duration <- as.integer(unlist(strsplit(sleep$Duration[i], " ")))
    duration <- duration[!is.na(duration)]
    if (length(duration) == 2) {
        sleep$min[i] <- (duration[1] * 60) + duration[2]
        } else {
        sleep$min[i] <- duration[1]
        }
}

#create plotting df - has to be array of every minute from start to end of time period
start <- min(as.POSIXct(sleep$Time, tz = "GMT", "%d/%m/%Y %H:%M"))
end <- max(as.POSIXct(sleep$Time, tz = "GMT", "%d/%m/%Y %H:%M"))
no_of_min <- as.numeric(difftime(end, start, units="mins"))
range <- seq(start, end, "min")
sleeps <- double()

for(i in 1:nrow(sleep)) {
    sect_start <- as.POSIXct(sleep$Time[i], tz = "GMT", "%d/%m/%Y %H:%M")
    sect_range <- sleep$min[i]
    sect_end <- sect_start + (sect_range * 60)
    sect_sleeps <- seq(sect_start, sect_end, "min")
    sleeps <- append(sleeps, sect_sleeps)
}

#bind the date, time and minutes to the new df of times
#NEED TO WORK OUT HOW TO ADD THIS TO THE MASSIVE LIST OF ALL TIMES CREATED BELOW
sleep$TimeLong <- as.POSIXct(sleep$Time, tz = "GMT", "%d/%m/%Y %H:%M")
#sleeps <- as.data.frame(sleeps)
colnames(sleeps)[1] <- "TimeLong"
test <- merge(sleeps, sleep, by.x="TimeLong", by.y="Time", sort = FALSE)


df <- data.frame()
for(i in as.POSIXct(range, origin = '1970-01-01', tz = "GMT")) {
    test <- as.POSIXct(i, origin = '1970-01-01', tz = "GMT")
    if (test %in% as.POSIXct(sleeps, origin = '1970-01-01', tz = "GMT")) {
        dfs <- data.frame(TimeLong = test, SleepState = 1)
        } else {
        dfs <- data.frame(TimeLong = test, SleepState = 0)
    }
    df <- rbind(df, dfs)
}

######## NEED TO BIND DATE AND TIME AND DURATION ################

######## Plotting starts here #####################
#install.packages("dplyr")
library(dplyr) # easier data wrangling 
#install.packages("viridis")
library(viridis) # colour blind friendly palette, works in B&W also
#install.packages("Interpol.T")
library(Interpol.T) #  will generate a large dataset on initial load
#install.packages("ggExtra")
library(ggExtra) # because remembering ggplot theme options is beyond me
#install.packages("tidyr")
library(tidyr) 

p <-ggplot(df,aes(TimeLong,fill=SleepState))+
  geom_tile(color= "white",size=0.1) + 
  scale_fill_viridis(name="Hrly Temps C",option ="C")
p <-p + facet_grid(year~month)
p <-p + scale_y_continuous(trans = "reverse", breaks = unique(df$TimeLong))
p <-p + scale_x_continuous(breaks =c(1,10,20,31))
p <-p + theme_minimal(base_size = 8)
p <-p + labs(title= paste("Hourly Temps - Station",statno), x="Day", y="Hour Commencing")
p <-p + theme(legend.position = "bottom")+
  theme(plot.title=element_text(size = 14))+
  theme(axis.text.y=element_text(size=6)) +
  theme(strip.background = element_rect(colour="white"))+
  theme(plot.title=element_text(hjust=0))+
  theme(axis.ticks=element_blank())+
  theme(axis.text=element_text(size=7))+
  theme(legend.title=element_text(size=8))+
  theme(legend.text=element_text(size=6))+
  removeGrid()#ggExtra
  
  
  
###### FROM THE WEBSITE WITH THE HEATMAP CODE ###### 
  
  
library(ggplot2)
library(dplyr) # easier data wrangling 
library(viridis) # colour blind friendly palette, works in B&W also
library(Interpol.T) #  will generate a large dataset on initial load
library(lubridate) # for easy date manipulation
library(ggExtra) # because remembering ggplot theme options is beyond me
library(tidyr) 
 
 
data<- data(Trentino_hourly_T,package = "Interpol.T")
 
names(h_d_t)[1:5]<- c("stationid","date","hour","temp","flag")
df<- tbl_df(h_d_t) %>%
  filter(stationid =="T0001")
 
df<- df %>% mutate(year = year(date),
                  month = month(date, label=TRUE),
                  day = day(date))
  
df$date<-ymd(df$date) # not necessary for plot but 
#useful if you want to do further work with the data
 
#cleanup
rm(list=c("h_d_t","mo_bias","Tn","Tx",
          "Th_int_list","calibration_l",
          "calibration_shape","Tm_list"))
 
 
#create plotting df
df <-df %>% select(stationid,day,hour,month,year,temp)%>%
        fill(temp) #optional - see note below
 
# Re: use of fill
# This code is for demonstrating a visualisation technique
# There are 5 missing hourly values in the dataframe.
 
# see the original plot here (from my ggplot demo earlier this year) to see the white spaces where the missing values occcur:
# https://github.com/johnmackintosh/ggplotdemo/blob/master/temp8.png 
 
# I used 'fill' from  tidyr to take the prior value for each missing value and replace the NA
# This is a quick fix for the blog post only - _do not_ do this with your real world data
 
# Should really use either use replace_NA or complete(with fill)in tidyr 
# OR 
# Look into more specialist way of replacing these missing values -e.g. imputation.
 
 
 
statno <-unique(df$stationid)
 
 
 
######## Plotting starts here#####################
p <-ggplot(df,aes(day,hour,fill=temp))+
  geom_tile(color= "white",size=0.1) + 
  scale_fill_viridis(name="Hrly Temps C",option ="C")
p <-p + facet_grid(year~month)
p <-p + scale_y_continuous(trans = "reverse", breaks = unique(df$hour))
p <-p + scale_x_continuous(breaks =c(1,10,20,31))
p <-p + theme_minimal(base_size = 8)
p <-p + labs(title= paste("Hourly Temps - Station",statno), x="Day", y="Hour Commencing")
p <-p + theme(legend.position = "bottom")+
  theme(plot.title=element_text(size = 14))+
  theme(axis.text.y=element_text(size=6)) +
  theme(strip.background = element_rect(colour="white"))+
  theme(plot.title=element_text(hjust=0))+
  theme(axis.ticks=element_blank())+
  theme(axis.text=element_text(size=7))+
  theme(legend.title=element_text(size=8))+
  theme(legend.text=element_text(size=6))+
  removeGrid()#ggExtra
 
# you will want to expand your plot screen before this bit!
p #awesomeness
