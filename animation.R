library(animation)
data.red <- data.tidy
data.red <- data.red[seq(1,3816,2),]
timemax <- dim(data.tidy)[1]
makeplot <- function(){
    for(i in 1:timemax){
        # g <- ggplot(data.cvg[1:i*10,])+
        #     geom_line(aes(x=Date,y=TerminalGarageUtilization,color=Day))+
        #     geom_line(aes(x=Date,y=ValuParkUtilization,color=Day),lty=2)+
        #     scale_color_manual(values=c('coral3','chartreuse3','dodgerblue3','deeppink3','darkorchid3','goldenrod3','limegreen'))+
        #     labs(title='Solid: Terminal Garage, Dashed: ValuPark',y='% Utilization')+
        #     theme_light()+
        #     scale_x_datetime(date_breaks = '1 day', date_minor_breaks = '6 hours',
        #                      labels = date_format("%a, 03/%d",tz = "EST"),
        #                      limits=c(as.POSIXct(data.cvg$Date[1]),
        #                               as.POSIXct(data.cvg$Date[length(data.cvg$Date)])))+
        #     scale_y_continuous(limits=c(30,71.5),breaks = seq.int(from = 0,to = 100,by = 10))

        h <- ggplot(data.tidy[i:i*10,])+
            geom_line(aes(x=Time,y=Value,color=Day))+
            facet_grid(.~Parameter)+
            labs(title='Parking Utilization at CVG',x='Time',y='% Utilization')+
            scale_color_manual(values=c('coral3','chartreuse3','dodgerblue3','deeppink3','darkorchid3','goldenrod3','limegreen'))+
            theme_light()+
            scale_x_datetime(date_breaks = '2 hours',
                             labels = date_format("%H:%M",tz = "EST"),
                             limits=c(as.POSIXct("1960-01-01 00:00"),
                                      as.POSIXct("1960-01-02 00:00")))+
            scale_y_continuous(limits=c(30,71.5),breaks = seq.int(from = 0,to = 100,by = 10))+
            theme(axis.text.x = element_text(angle = 45, hjust = 1,size = 9),strip.text.x = element_text(size = 12))+
            annotate('text',x=as.POSIXct(paste(Sys.Date(),' 18:25'),tz = 'EST'),y=71.5,label=paste('Updated:',format(data.tidy$Time[length(data.tidy$Time)],"%a, %H:%M %Z")),size=3,color='lightskyblue4')
        print(h)
    }
}
saveGIF(expr = makeplot(), interval = .02, ani.width = 800, ani.height = 430,movie.name = 'red.gif')

saveHTML(makeplot(),interval = .001, ani.width = 1028, ani.height = 480)
