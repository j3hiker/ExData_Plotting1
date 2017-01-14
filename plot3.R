## plot3.R - plot Energy Sub-metering and save as plot3.png
plot3 <- function() {
    
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
    
    par(oma=c(1,1,2,1),mar=c(4,4,4,4),bg="white")
    
    plot(s[,1], s[,2], type="n", xaxt="n", xlab="", ylab="Energy sub metering")
    lines(data.frame(i=interval,SubMeeting = subdat$Sub_metering_1))
    lines(data.frame(i=interval,SubMeeting = subdat$Sub_metering_2), col="red")
    lines(data.frame(i=interval,SubMeeting = subdat$Sub_metering_3), col="blue")
    
    axis(1, c(min(interval),median(interval),max(interval)), c("Thr","Fri","Sat"))
    legend("topright", 
           legend = c("Sub_metering_1","Sub_metering_2","Sub_metering_3"),
           lwd = c(1,1,1), lty = c(1,1,1), col = c("black","red","blue"))
    
    mtext("Plot 3", outer=TRUE, adj = 0, line = 1)
    
    dev.copy(png, file="plot3.png",width=480,height=480) ## save plot as a .png
    
    dev.off()
    
}