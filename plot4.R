## plot4.R - create multiple energy plots and save as plot4.png
plot4 <- function() {
    
    ## should have plent of memory but do a quick check
    n_rows=2075259
    n_cols=9
    avg_siz=8
    byt_need <- n_rows*n_cols*avg_siz  ## rows * cols * avg size in bytes per col
    mb_need <- byt_need / (2^20)       ## bytes/MB
    gb_need <- mb_need / 1000          ## gb needed
    gb_need <- gb_need *2              ## double it just to be safe
    avail_gb <- 4
    
    if( gb_need > avail_gb ) {
        stop("not enough memory")
    }
    
    if( !dir.exists("./data")) { 
        dir.create("./data") 
    }
    
    ## Only download if the file does not already exist
    if ( !file.exists("./data/household_power_consumption.txt")) {
        fileUrl <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"     
        download.file(fileUrl,destfile = "./data/exdata%2Fdata%2Fhousehold_power_consumption.zip", mode='wb')
        dateDownloaded <- date()
        dateDownloaded
        
        z=unzip("./data/exdata%2Fdata%2Fhousehold_power_consumption.zip",exdir = "./data")
    }
    
    ## Initialize some variables:
    ## - Less memory will be used if colClasses is specified as one of the six atomic vector classes. Also,
    ##   using nrows in the read.table call will help memory usage on large files. Per the instructions, 
    ## - The dataset contains 2,075,259 rows
    ## - Missing values are described as ? in this dataset
    
    readcolclasses <- c("character","character","numeric","numeric","numeric","numeric","numeric","numeric","numeric")
    readrows <- 2075259   
    readNAstring <- "?"
    
    dat <- read.table("./data/household_power_consumption.txt", header=TRUE, sep = ";",
                      nrows=readrows, colClasses = readcolclasses, na.strings = readNAstring)
    
    
    ## To make things easier just look at the dates that are needed for this project
    subdat= dat[dat$Date== "1/2/2007" | dat$Date== "2/2/2007", ]
    
    
    dateTime <- paste(subdat$Date,subdat$Time)
    day      <- as.integer(format(as.Date(dateTime,"%d/%m/%Y"),"%u"))
    hr       <- as.integer((format(strptime(dateTime,"%m/%d/%Y %H:%M:%S"),"%H")))
    min      <- as.integer((format(strptime(dateTime,"%m/%d/%Y %H:%M:%S"),"%M")))
    interval <- day*24*60+hr*60+min
    s1       <- as.data.frame(cbind(interval, subdat$Sub_metering_1))
    s2       <- as.data.frame(cbind(interval, subdat$Sub_metering_2))
    s3       <- as.data.frame(cbind(interval, subdat$Sub_metering_3))
    s        <- rbind(s1,s2)
    s        <- rbind(s,s3)
    xtick    <- c(min(interval),median(interval),max(interval))
    xlabel   <- c("Thr","Fri","Sat") 
    cex_adj  <- par("cex")*.8
    leg_adj  <- par("cex")*.7  ## legend needs to be a little smaller

 
    par(mfrow = c(2,2),oma=c(0,1,2,0),mar=c(4,4,2,1),bg="white")
    
    ## plot #1
    plot(interval, subdat$Global_active_power, type = "l", xaxt="n", xlab = "", 
         ylab = "Global Active Power", cex.lab = cex_adj, cex.axis = cex_adj)
    axis(1, xtick, xlabel, cex.axis = cex_adj )

    ## plot #2
    plot(interval,subdat$Voltage,type="l",xaxt="n", xlab="datetime", 
         ylab = "Voltage", cex.lab = cex_adj, cex.axis = cex_adj)
    axis(1, xtick, xlabel, cex.axis = cex_adj)
    
    ## plot #3
    plot(s[,1], s[,2], type="n", xaxt="n", xlab="", ylab="Energy sub metering",
         cex.lab=cex_adj, cex.axis = cex_adj)
    lines(data.frame(i=interval,SubMeeting = subdat$Sub_metering_1))
    lines(data.frame(i=interval,SubMeeting = subdat$Sub_metering_2), col="red")
    lines(data.frame(i=interval,SubMeeting = subdat$Sub_metering_3), col="blue")
 
    axis(1, xtick, xlabel, cex.axis = cex_adj)
    legend("topright", 
           legend = c("Sub_metering_1","Sub_metering_2","Sub_metering_3"),
           lwd = c(1,1,1), lty = c(1,1,1), col = c("black","red","blue"),
           cex = leg_adj, bty="n")

    ## plot #4
    plot(interval,subdat$Global_reactive_power,type="l",xaxt="n", xlab = "datetime", 
         ylab="Global_reactive_power", cex.lab = cex_adj, cex.axis = cex_adj)
    axis(1, xtick, xlabel, cex.axis = cex_adj) 
    
    mtext("Plot 4", outer=TRUE, adj = 0, line = 1)

    ## save as png    
    dev.copy(png, file="plot4.png",width=480,height=480) ## save plot as a .png
    
    dev.off()
    
}