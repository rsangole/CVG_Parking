library(animation)

ani.options("convert")

timemax <- dim(data.cvg)[1]
makeplot <- function(){
    for(i in 1:timemax){
        g <- ggplot(data.cvg[1:i,])+
            geom_line(aes(x=Date,y=TerminalGarageUtilization,color=Day))+
            geom_line(aes(x=Date,y=ValuParkUtilization,color=Day),lty=2)+
            labs(title='Solid: Terminal Garage, Dashed: ValuPark',y='% Utilization')+
            theme_light()+
            scale_x_datetime(date_breaks = '1 day', date_minor_breaks = '6 hours',
                             labels = date_format("%a, 03/%d",tz = "EST"),
                             limits=c(as.POSIXct(data.cvg$Date[1]),
                                      as.POSIXct(data.cvg$Date[length(data.cvg$Date)])))+
            scale_y_continuous(limits=c(30,71.5),breaks = seq.int(from = 0,to = 100,by = 10))
        print(g)
    }
}

saveGIF(makeplot(),interval = .01, ani.width = 1028, ani.height = 480,)