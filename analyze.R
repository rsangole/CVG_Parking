setwd("~/Documents/Data Science/CVG_Parking")

library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)

data.cvg <- tbl_df(read.csv('o.txt',sep = ',',header = F))
names(data.cvg) <- c('Date','TerminalGarageUtilization','ValuParkUtilization')

data.cvg$Date <- as.POSIXct(data.cvg$Date)

data.cvg <- data.cvg %>%
    mutate(TerminalGarageUtilization=100-TerminalGarageUtilization) %>%
    mutate(ValuParkUtilization=100-ValuParkUtilization) %>%
    mutate(Day=format(Date,'%a'))

data.cvg$Day <- factor(data.cvg$Day,levels = c('Sat','Sun','Mon','Tue','Wed','Thu','Fri'),ordered = T)

data.cvg <- data.cvg %>% mutate(Time=as.POSIXct(format(Date, format = "%H:%M:%S %z"), format = "%H:%M:%S %z"))

data.tidy <- data.cvg %>% gather('Parameter','Value',2:3)

ggplot(data.tidy)+
    geom_line(aes(x=Time,y=Value,color=Day))+
    facet_grid(.~Parameter)+
    labs(title='Parking Utilization at CVG',x='Time',y='% Utilization')+
    theme_light()+
    scale_x_datetime(date_breaks = '2 hours',
                     labels = date_format("%H:%M",tz = "EST"))+
    scale_y_continuous(limits=c(30,71.5),breaks = seq.int(from = 0,to = 100,by = 10))+
    theme(axis.text.x = element_text(angle = 45, hjust = 1,size = 9),strip.text.x = element_text(size = 12))+
    annotate('text',x=as.POSIXct(paste(Sys.Date(),' 18:25'),tz = 'EST'),y=71.5,label=paste('Updated:',format(data.tidy$Time[length(data.tidy$Time)],"%a, %H:%M %Z")),size=3,color='lightskyblue4')

ggplot(data.cvg)+
    geom_line(aes(x=Date,y=TerminalGarageUtilization,color=Day))+
    geom_line(aes(x=Date,y=ValuParkUtilization,color=Day),lty=2)+
    scale_color_manual(values=c('coral3','chartreuse3','dodgerblue3','deeppink3','darkorchid3','goldenrod3','limegreen'))+
    labs(title='Solid=Terminal Garage, Dashed=ValuPark',y='% Utilization')+
    theme_light()+
    scale_x_datetime(date_breaks = '1 day', date_minor_breaks = '6 hours',
                     labels = date_format("%a, 03/%d",tz = "EST"))+
    theme(axis.text.x = element_text(hjust = 0,size = 9))

ggplot(data.tidy)+geom_violin(aes(x = Parameter,y = Value,fill=Day),scale = 'area')+
    labs(title='Variation in parking utilization',x='',y='% Utilization')+
    theme(axis.text.x = element_text(size = 10))

data.tidy %>% group_by(Parameter) %>% summarise(Avg=mean(Value),StdDev=sd(Value),COV=StdDev/Avg,N=n())
