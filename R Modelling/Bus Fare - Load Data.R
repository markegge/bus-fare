# G2 is front door board with stops in close proximity to the P1, which is all door board when outbound
# investigation of the effect of load policy on dwell time
# data source: PAAC? October 2014 Weekdays

library(plyr)
library(lattice)
library(ggplot2)
library(data.table)
options(scipen=20) # display numbers up to 20 decimal points w/o scientific notation

import.csv <- function(filename){
  return(read.csv(filename, header=TRUE, sep=",", strip.white=TRUE))
}

this.dir <- dirname(parent.frame(2)$ofile); setwd(this.dir) # set working directory to script directory

# load data
if(!exists("rawdata")) { rawdata <- import.csv("sept-oct-14-p1-g2-61x-71x.csv") } # import data if not already in working env
if(!exists("stops")) { stops <- import.csv("stops.csv") }
if(!exists("route.names")) { route.names <- import.csv("route-name-lookup.csv") }

df <- rawdata # create a working copy...

#rtnums = c(444, 900, 611, 612, 613, 614, 711, 712, 713, 714)
#rtnames = c('P1', 'G2', '61A', '61B', '61C', '61D', '71A', '71B', '71C', '71D')
#df$ROUTE <- factor(df$ROUTE, levels=rtnums, labels=rtnames) # label route numbers with route names based on vectors above

df <- merge(df, route.names, by="ROUTE")

df$dir <- factor(df$dir, levels=c(0,1), labels=c('Outbound', 'Inbound')) # label direction binary variable

# add stop details to dataframe:
names(stops)[names(stops)=='StopID'] <- "QSTOPA"
df <- merge(df, stops, by="QSTOPA")
names(df)[names(df)=='Zone_'] <- "ZONE"
names(df)[names(df)=='Name'] <- "NAME"
names(df)[names(df)=='dir'] <- "DIRECTION"

# derive additional variables of interest
df$PAX <- df$ON + df$OFF # create new variable for passenger movements as sum of ONs and OFFs
#df$ON2 <- df$ON ^ 2 # create non-linear term for passenger boards
#df$OFF2 <- df$OFF ^ 2 # create non-linear eterm for passenger alightments
#df$LOG.ON <- ifelse(df$ON > 0, log(df$ON), 0) # passenger loads have log normal distribution?
#df$LOG.OFF <- ifelse(df$OFF > 0, log(df$OFF), 0) # passenger exists have log normal distribution?


#df$DATE <- as.Date(df$daymoyr, format='%d-%b-%Y') # create R format date column
df$DATE <- as.Date(df$daymoyr, origin = "1899-12-30") # convert Excel date format to R format date column
df$DOW <- weekdays(df$DATE) # add day of week
df$TRIPUNIQUEID <- paste(as.character(df$DATE), df$ROUTE, df$TRIPA, sep='-') # create unique TRIP id

df$MOD = df$HR*60 + df$MIN # derive minute of day
df$HMOD = floor(df$MOD/2) # divide day into two-minute intervals for bunching calculation below

# convert to data table for fast tabulation
# create PROGRESS variable which is the number of the current stop divided by total stops
# calculat load on arrival
dt <- as.data.table(df)
setkeyv(dt, c("TRIPUNIQUEID", "STOPA"))
m <- nrow(dt)
dt <- unique(dt)
cat("Removed", m - nrow(dt), "duplicate values.\n")
dt <- dt[, 'MAXSTOP' := max(STOPA), by=TRIPUNIQUEID ] # calculate the load on arrival
dt <- dt[, 'TRIPSTARTTIME' := paste(DATE[1], HR[1], MIN[1], SEC[1], sep='-'), by=TRIPUNIQUEID]
dt <- dt[, 'ALOAD' := shift(LOAD, n=1, fill=0, type="lag"), by=TRIPUNIQUEID ] # calculate the load on arrival

# code for instances where doors opened multiple times a single stop:
#dt <- dt[, 'STOPRECORDS' := .N, by=key(dt)]
cat("Number of duplicate entries for any one stop:", anyDuplicated(dt, by=key(dt)))

 #calculate number of buses to pass this stop in the previous 2 minutes
setkeyv(dt, c("QSTOPA", "daymoyr", "HMOD"))
dt <- dt[, 'BUNCHES' := .N, by=list(QSTOPA, daymoyr, HMOD)]

df <- as.data.frame(dt)


# code the first stop and last stop on each route
cat('Add dummy vars for first and last stops of each route')
df$FIRSTSTOP <- ifelse(df$STOPA == 0, 1, 0)
df$LASTSTOP <- ifelse(df$STOPA == df$MAXSTOP, 1, 0)

# The 3,000 and 4,000 series vehicles are articulated buses, and generally have 60 seats
# The 1,000, 2,000, 5,000, 6,000 series buses generally have 40 seats
df$SEATS <- ifelse(substr(as.character(df$VEHNOA), 0, 1) %in% c('3', '4'), 55, 35 ) # create variable for number of seats
df$ARTICULATING <- ifelse(df$SEATS == 55, 1, 0) # if 60 seats, then we have an articulating bus
df$STANDEES <- pmax(0, (df$ALOAD - df$SEATS), (df$LOAD - df$SEATS)) # create variable for standees - the great of zero or the number of passengers minus seats
df$FRICTION <- ifelse(df$STANDEES > 0, log(df$PAX * (df$STANDEES^2)), 0) # create standee-pax interaction term for Standees^2 * PAX

df$PROGRESS <- round(df$STOPA / df$MAXSTOP, 2)

# convert arrival ATIME and departure DTIME values to R's POSIX format
df$ATIME <- strptime(with(df, paste(DATE, HR, MIN, SEC, sep='-')), format='%Y-%m-%d-%H-%M-%S')
df$TRIPSTARTTIME <- strptime(df$TRIPSTARTTIME, format='%Y-%m-%d-%H-%M-%S')
df$DTIME <- strptime(with(df, paste(DATE, DHR, DMIN, DSEC, sep='-')), format='%Y-%m-%d-%H-%M-%S')


# Code Zone Dummy Vars
df$ZFREE <- ifelse(df$ZONE == 'Free', 1, 0)
df$Z1 <- ifelse(df$ZONE %in% c('1', '1A'), 1, 0)
df$Z2 <- ifelse(df$ZONE == '2', 1, 0)

# create dummy variable for front door load
# all buses are FD load after 7:00 pm
# if buses are entry fare only, or if it's after 7:00 pm, or if direction is inbound
# dir = 1 if inbound

entry.fare.routes = c('G2', '28X')
df$ENTRYFARE <- with(df, (DIRECTION == 'Inbound')*(Z1 + Z2) + (RouteName %in% entry.fare.routes) + (format(df$TRIPSTARTTIME, '%H') > 18))
df$ENTRYFARE <- ifelse(df$ENTRYFARE >= 1, 1, 0)
#df$ENTRYFARE <- ifelse(df$ENTRYFARE == 0 & df$ROUTE %in% entry.fare.routes, 1, 0)
table(df$ENTRYFARE)
df$EXITFARE <- ifelse(df$ENTRYFARE == 1 | df$ZFREE == 1, 0, 1)

df$DWTIME <- round(df$DWTIME * 60, 0) # convert dwell time from minutes to seconds, rounded to the nearest whole second
df$LOG.DWTIME <- log(df$DWTIME) # add log dwell

# add dummy variable for PM rush hour (hour of day = 16 or 17)
rushhr <- c(16,17)
df$PMRUSHHOUR <- ifelse(df$HR %in% rushhr, 1, 0) #set to 1 if rush hour


# Create dummy vars for "station" stops
df$STATION <- ifelse(df$Shelter_Pu == 'Port Authority Station', 1, 0)

cat("** Dataset of n=", nrow(df), "observations, describing", nlevels(df$ROUTE), "routes on", nlevels(df$DATE),
    "days spanning from", min(df$DATE), "to", max(df$DATE), ".\n")

#cat("=============================\n")
#cat("Correlation Matrix\n")
#cat("=============================\n")
#print(  cor(df[ , c('DWTIME', 'ON', 'OFF', 'LOAD', 'HR', 'STOPA', 'SCHDEV') ], method = "pearson") )

# clean up
remove(dt, m, stops, route.names)

print("Data loaded.\n")
