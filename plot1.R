## plot1.R - plot a histogram of Global Active Power and store it as plot1.png
plot1 <- function() {
    
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

    par(oma=c(1,1,2,1),mar=c(4,4,4,4),bg="white")
    with(subdat, hist(subdat$Global_active_power, col="red", main="Global Active Power", 
                      xlab = "Global Active Power (kilowatts)"))

    mtext("Plot 1", outer=TRUE, adj = 0, line = 1)


    dev.copy(png, file="plot1.png",width=480,height=480) ## save plot as a .png

    dev.off()
}
