# Input: df
# Output: cleaned data: bus
# applies data filters that are helpful for evaluating the impact of fare policy, but might
# not be appropriate for overall analysis of dwell times (e.g. removing dwell times 
# greater than ~2 mins based on 5 SDs from the mean)
library(pastecs) # used for summary stats for filtering outliers
# get our working copy of the data:

# get rid of extra variables
dropcols <- c("TRIPA", "BLOCKA", "VEHNOA", "daymoyr","ANAME", "MIN", "SEC",
              "DHR" , "DMIN"  , "DSEC", "DLMILES", "DLMIN", "DLPMLS", "DELTA",  "SCHTIM",
              "SRTIME","ARTIME","FID","OBJECTID", "Latitude","Longitude","CleverID", 
              "Routes_Sor", "Mode", "SEATS", "MAXSTOP", "Direction", "HMOD")
bus <- df[ , names(df)[!(names(df) %in% dropcols)]]

# ******************
# ** Clean Data  ***
start.rows <- rows <- nrow(bus)
cat("Started data cleaning with", start.rows, "rows.\n")

bus <- bus[bus$DWTIME > 0, ] # eliminate records where dwell time is zero (1)
cat("Removed", rows - nrow(bus), "not-stop rows where dwell time = 0\n")
rows <- nrow(bus)

bus <- bus[bus$DWTIME < 300, ]
cat("Removed", rows - nrow(bus), "rows where dwell time greater than 5 minutes\n")
rows <- nrow(bus)

bus <- bus[bus$STOPA < 999 , ] # filter out rows where STOP # = 999
cat("Removed", rows - nrow(bus), "rows where stop number = 999\n")
rows <- nrow(bus)

bus <- bus[bus$PAX > 0, ] # eliminate records where zero passenger movements occured
cat("Removed", rows - nrow(bus), "rows where no passengers boarded or alighted\n")
rows <- nrow(bus)

bus <- bus[bus$FIRSTSTOP == 0, ] # get rid of stops where first stop is true (dwell time at the first stop is uncorrelated to passenger activity)
cat("Removed", rows - nrow(bus), "rows for the first stop on a line\n")
rows <- nrow(bus)

bus <- bus[bus$LASTSTOP == 0, ] # get rid of stops where last stop is true (dwell time at the first stop is uncorrelated to passenger activity)
cat("Removed", rows - nrow(bus), "rows for the last stop on a line\n")
rows <- nrow(bus)

# keep only weekdays
bus <- bus[bus$DOW %in% c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday'), ]
cat("Removed", rows - nrow(bus), " by filtering to just weekends\n")
rows <- nrow(bus)

# drop holidays
holidays <- as.Date(c('11/11/2014', '11/27/2014', '12/25/2014', '01/01/2015', '01/19/2015', '02/16/2015', '05/25/2015', '07/03/2015', '09/07/2015', '11/11/2015', '11/26/2015', '12/25/2015'))
bus <- bus[!(bus$DATE %in% holidays), ]
cat("Removed", rows - nrow(bus), "rows for holidays\n")
rows <- nrow(bus)

# plot histograms for every stop. Visually inspect for outliers
#ggplot(bus, aes(DWTIME)) +
#  geom_histogram(bins = 30) +
#  facet_wrap(~QSTOPA)

dt <- as.data.table(subset(bus, select=-c(ATIME, DTIME)))
stop.stats <- dt[, list(dwt.mean=mean(DWTIME), 
                        dwt.sd=sd(DWTIME), 
                        dwt.median=median(DWTIME), 
                        dwt.total=sum(DWTIME),
                        count=.N, 
                        mean.pax=mean(PAX),
                        total.pax=sum(PAX),
                        mean.dwt.per.pax=mean(DWTIME / PAX)
                        ), 
                 by=QSTOPA]

# remove stops where the stop's average dwell time per passenger action is > 15 seconds
long.stops <- stop.stats$QSTOPA[stop.stats$mean.dwt.per.pax > 15]
bus <- bus[!bus$QSTOPA %in% long.stops, ]
cat("Removed", rows - nrow(bus), "where the stop's average dwell time per passenger action is > 15 seconds\n")
rows <- nrow(bus)

# remove stops with fewer than 10 observations
infrequent.stops <- stop.stats$QSTOPA[stop.stats$count < 10]
bus <- bus[!bus$QSTOPA %in% infrequent.stops, ] 
cat("Removed", rows - nrow(bus), "where the stop has fewer than 10 observations\n")
rows <- nrow(bus)

# remove observations with dwell time or PAX more than 5 SD from mean
var.stats <- stat.desc(bus[, c("DWTIME", "PAX")])
bus <- bus[bus$DWTIME < (var.stats["mean", "DWTIME"] + 5*var.stats["std.dev","DWTIME"]), ]
cat("Removed", rows - nrow(bus), "rows where dwell time more than 5 SD from mean\n")
rows <- nrow(bus)
bus <- bus[bus$PAX < (var.stats["mean", "PAX"] + 10*var.stats["std.dev","PAX"]), ] 
cat("Removed", rows - nrow(bus), "rows where PAX more than 10 SD from mean\n")
rows <- nrow(bus)


# remove odd anomaly where dwell time = 10 seconds, uncorrelated to PAX
cat("Discarding ", nrow(bus[bus$DWTIME==10 & bus$STATION==1, ]), " station stop rows where dwell time = 10 seconds.\n")
bus <- bus[!(bus$DWTIME==10 & bus$STATION==1), ]

# define bands on what's a reasonable time
# min time of .05 seconds per passenger, plus 0.1 seconds per entry fare paid
bus$min.time <- with(bus, (ON * ENTRYFARE * .5) + (OFF * !as.logical(ENTRYFARE) * 0.25) + (PAX * .05))
bus$min.time <- with(bus, (ON * ENTRYFARE * .5) + (OFF * !as.logical(ENTRYFARE) * 0.25) + (PAX * .05))

cat("Discarding ", nrow(bus[bus$DWTIME < bus$min.time, ]), " values where dwell time improbably short with respect to passenger actions.\n")
bus <- bus[bus$DWTIME > bus$min.time, ]

# max time of 15 seconds per passenger action
bus$max.time <- bus$PAX * 30
cat("Discarding", nrow(bus[bus$DWTIME > bus$max.time, ]), "values where the dwell time is greater than 30 seconds per passenger movement.\n")
bus <- bus[bus$DWTIME < bus$max.time, ]

rows <- nrow(bus)
# troublesome stops:
s <- c('EAST BUSWAY AT PENN STATION A', 'EAST BUSWAY AT PENN STATION C', 'GRANT ST AT LIBERTY AVE FS (FED BLDG)', 
       'GRANT ST AT POST OFFICE', 'GRANT ST AT US STEEL STOP A')
# chuck out Grant St stops with 10 second dwell
bus <- bus[!(bus$NAME %in% s & bus$DWTIME==10), ]
cat("Removed", rows - nrow(bus), "rows at other troublesome stops where dwell time = 10\n")
rows <- nrow(bus)

# some validation:
cat("Entry / Exit / None", table(sum(bus$ENTRYFARE, bus$EXITFARE)))
cat ("CBD rows with entryfare: ", nrow(bus[bus$ZFREE==1 & bus$ENTRYFARE==1 & bus$RouteName=='P1' & bus$HR < 19, ]), "\n")
cat ("CBD rows with entryfare: ", nrow(bus[bus$ZFREE==1 & bus$EXITFARE==1 & bus$RouteName=='P1' & bus$HR < 19, ]), "\n")
cat("Finishing up. Removed", start.rows - nrow(bus),"rows, or",  (start.rows - nrow(bus)) / start.rows, "% of total.")
cat("Resulting dataset describes", length(unique(bus$ROUTE)), "routes,", length(unique(bus$TRIPUNIQUEID)), "trips, and", nrow(bus),"stops.")

validation.plots <- function() {
  ggplot(bus, aes(x=PAX, y=DWTIME)) +
    geom_point(aes(color=RouteName)) + 
    geom_smooth()
  

  j <- ggplot(bus, aes(PAX, DWTIME, fill=RouteName)) + 
    geom_point(aes(color=RouteName)) + 
    labs(title='Dwell Time vs Passenger Actions', y = 'Dwell time (seconds)', x = 'Boarding / Alighting Passengers') 
  print(j)
  
  hist(bus$DWTIME, breaks=max(bus$DWTIME))
  hist(log(bus$DWTIME), breaks=max(log(bus$DWTIME)))
}