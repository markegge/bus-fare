# load in all data for study period, and compute stats for whole lines
library(data.table)

this.dir <- dirname(parent.frame(2)$ofile); setwd(this.dir) # set working directory to script directory
if(!exists("rawdata")) { rawdata <- read.csv("combined-all-routes.csv", header=TRUE, sep=",", strip.white=TRUE) } # import data if not already in working env
if(!exists("stops")) { stops <- read.csv("stops-lookup.csv", header=TRUE, sep=",", strip.white=TRUE) }
if(!exists("route.names")) { route.names <- read.csv("route-name-lookup.csv", header=TRUE, sep=",", strip.white=TRUE) }
if(!exists("route.details")) { route.details <- read.csv("route-details.csv", header=TRUE, sep=",", strip.white=TRUE) }

DT <- as.data.table(rawdata)

# filters:
# weekdays only
# extra header columns
DT <- DT[HR != "HR"] # remove extra header rows
DT <- DT[HR >= 5] # get rid of observations outside of 5:00 am to midnight window
DT <- DT[DHR < 24]
DT <- DT[DT$SCHTIM != 9999] # get rid of 36 unplanned trips

DT$DATE <- as.IDate(DT$daymoyr, origin = "1899-12-30") # convert Excel date format to R format date column
DT$ATIME <- as.ITime(as.character(paste(DT$HR, DT$MIN, DT$SEC, sep=':')))
DT$DTIME <- as.ITime(as.character(paste(DT$DHR, DT$DMIN, DT$DSEC, sep=':')))
#DT$ATIME <- as.ITime(DT$SEC + DT$MIN * 60 + DT$HR * 60 * 60, origin='2014-09-01')
#DT$DTIME <- as.ITime(DT$DSEC + DT$DMIN * 60 + DT$DHR * 60 * 60, origin='2014-09-01')

DT$TRIPUNIQUEID <- paste(as.character(DT$DATE), DT$ROUTE, DT$TRIPA, as.character(DT$dir), sep='-') # create unique TRIP id
setkeyv(DT, c("TRIPUNIQUEID", "STOPA"))
m <- nrow(DT)
DT <- unique(DT)
cat("Removed", m - nrow(DT), "duplicate values.\n")

# convert from factors
char.cols <- quote(list(QSTOPA, ANAME))
DT <- modifyList(DT, lapply(DT[, eval(char.cols)],   function(x) as.character(x) ))
DT$QSTOPA <- sub("^\\s+", "", DT$QSTOPA)
#num.cols <- quote(list(daymoyr, STOPA, HR, MIN, SEC, DHR, DMIN, DSEC, ON, OFF, LOAD, DWTIME))
#DT <- modifyList(DT, lapply(DT[, eval(num.cols)],   function(x) as.numeric(as.character(x)) ))
#asNumeric <- function(x) as.numeric(as.character(x))
#factorsNumeric <- function(d) modifyList(d, lapply(d[, eval(num.cols)],   asNumeric))
#DT <- factorsNumeric(DT)
#DT <- droplevels(DT) # remove header levels from factors

DT <- merge(DT, route.names, by="ROUTE") # merge route names
names(stops)[names(stops)=='StopID'] <- "QSTOPA" # merge stop details to dataframe:
DT <- merge(DT, stops, by="QSTOPA")

remove(stops, route.names)

DT$dir <- factor(DT$dir, levels=c(0,1), labels=c('Outbound', 'Inbound')) # label direction binary variable

names(DT)[names(DT)=='Zone_'] <- "ZONE"
names(DT)[names(DT)=='Name'] <- "NAME"
names(DT)[names(DT)=='dir'] <- "DIRECTION"

DT$PAX <- DT$ON + DT$OFF # create new variable for passenger movements as sum of ONs and OFFs
DT$DWTIME <- round(DT$DWTIME * 60, 0) # convert dwell time from minutes to seconds, rounded to the nearest whole second

#DT$DOW <- wday(DT$DATE) # add day of week
#DT$DOW <- factor(DT$DOW, levels=c(2, 3, 4, 5, 6), labels=c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday'))
#DT$MOD = DT$HR*60 + DT$MIN # derive minute of day

#DT <- DT[, 'NUMSTOPS' := max(STOPA), by=TRIPUNIQUEID ]
#DT <- DT[, 'CUMSUM.ON' := cumsum(ON), by=TRIPUNIQUEID ]
#DT <- DT[, 'ONS' := sum(ON), by=TRIPUNIQUEID ]
#DT <- DT[, 'OFFS' := sum(OFF), by=TRIPUNIQUEID ]

#DT$PROGRESS <- round(DT$STOPA / DT$MAXSTOP, 2)
# remove dupes -- presumably instances where the doors opened twice at a single stop
#setkeyv(DT, c("TRIPUNIQUEID", "STOPA"))
#before <- nrow(DT)
#DT <- unique(DT)
#cat("Removed", before - nrow(DT), "duplicate values.\n")

inner.trip.sum <- function(dwtimes) {
  # sum dwell times where total dwell time less than 5 minutes and not first or last stop
  dwt <- dwtimes[2:(length(dwtimes)-1)]
  dwt <- sum(dwt[dwt < 300]) 
  return (dwt)
}
# Trips Data Table

setkeyv(DT, c("TRIPUNIQUEID", "STOPA"))
trips <- DT[, .(
  BEGIN = min(ATIME), 
  END = max(ATIME),
  NumStops = max(STOPA),
  NumObs = .N,
  DWTIME = inner.trip.sum(DWTIME),
  FZONE = max(ZONE == 'Free'), # does this trip have at least one stop in the free zone?
  PAS = sum(ON),
  AVG.LOAD = mean(LOAD)
), by=c("TRIPUNIQUEID", "RouteName", "DIRECTION")]
trips$BEGIN <- as.ITime(trips$BEGIN, origin='2014-09-01')
trips$END <- as.ITime(trips$END, origin='2014-09-01')
trips <- trips[, DURATION := END - BEGIN]

# ******************
# ** Clean Data  ***
start.rows <- rows <- nrow(trips)
cat("Started data cleaning with", start.rows, "rows.\n")

trips <- trips[DURATION > 0,] # drop obs where duration is zero
trips <- trips[PAS > 0,] # drop obs where no passengers rode

# Code Exit Fare observations
# if FZONE = 0, entry fare at all times
trips$EXITFARE <- ( trips$FZONE + (trips$DIRECTION == 'Outbound') + (hour(trips$BEGIN) < 19) )
table(trips$EXITFARE)
trips$EXITFARE <- ifelse(trips$EXITFARE == 3, TRUE, FALSE)

entry.fare.routes = c('11', 'G2', '28X', '54')
exit.fare.routes = c('75', 'P3')

trips$RUSHHOURAM <- ifelse(hour(trips$BEGIN) %in% c(7, 8), 1, 0)
trips$RUSHHOURPM <- ifelse(hour(trips$BEGIN) %in% c(16, 17), 1, 0)
trips$HR <- as.factor(hour(trips$BEGIN))
trips$EXITFARE[which(trips$RouteName %in% exit.fare.routes)] <- 1
table(trips$EXITFARE)
trips$EXITFARE[which(trips$RouteName %in% entry.fare.routes)] <- 0
table(trips$EXITFARE)
trips <- trips[RouteName != '44']
setkey(trips, RouteName)
table(trips["61C"]$NumStops)
hist(trips["61C"]$NumStops)
trips$ROUTEID <- as.factor(paste(trips$RouteName, trips$DIRECTION, sep="-"))

trips <- trips[,avg := mean(NumStops), by=RouteName] # FILTER OUT THE INCOMPLETE TRIPS
trips <- trips[,stop.count := .N, by=c("RouteName", "DIRECTION", "NumStops")]
trips <- trips[,avg := mean(stop.count), by=c("RouteName", "DIRECTION")]
trips <- trips[abs(stop.count - avg) < avg / 2 ]

trips$DWTPPAS = trips$DWTIME / trips$PAS # dwell time per passenger (each passenger is one on and one off)

names(route.details)[names(route.details)=='ROUTE'] <- 'RouteName'
trips <- merge(trips, route.details, by='RouteName')

save(trips, file="bus-trips.RData")


