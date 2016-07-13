# do the actual analyais
#options(stringsAsFactors = FALSE)
load("bus-trips.RData")

hist(trips$DWTPPAS)

trips$TypeServ <- relevel(trips$TypeServ, ref = 'Local')
trips <- trips[DWTPPAS < 50]
trips$TYPE <- cut(trips$Wkdy_PPH, c(1, 33, 66, 200), labels=c("low", "medium", "high"))
table(trips$TYPE)


cat("\n===============================================\n")
cat("Model 0 - Exit Fare by Service Type on Duration (PPH Type) \n")
cat("Controlling for route, direction, and time of day \n")
cat("===============================================\n")
model <- lm(DURATION ~ EXITFARE*TYPE + ROUTEID + HR*DIRECTION, data=trips)
print( summary(model) )

# what do I want to measure? Effect on total trip? Effect on dwell time per passenger?
cat("\n===============================================\n")
cat("Model 1- Exit Fare by Service Type on Duration (PPH Type) \n")
cat("===============================================\n")
model <- lm(DURATION ~ PAS + EXITFARE*TYPE + ROUTEID + HR*DIRECTION, data=trips)
print( summary(model) )

cat("\n===============================================\n")
cat("Model 1 Alt - Exit Fare by Service Type on Duration (PPAC Type) \n")
cat("===============================================\n")
model <- lm(DURATION ~ PAS + EXITFARE*TypeServ + ROUTEID + HR*DIRECTION, data=trips)
print( summary(model) )

cat("\n===============================================\n")
cat("Model 1.3 - Stops, Passengers, and Exit Fare by Service Type on Duration (PPAC Type) \n")
cat("===============================================\n")
model <- lm(DURATION ~ NumStops + PAS + EXITFARE*TypeServ + ROUTEID + HR*DIRECTION, data=trips)
print( summary(model) )

# what do I want to measure? Effect on total trip? Effect on dwell time per passenger?
cat("\n===============================================\n")
cat("Model 2.0 - Dwell Time per Passenger \n")
cat("===============================================\n")
model <- lm(DWTPPAS ~ EXITFARE + TYPE + AVG.LOAD, data=trips)
print( summary(model) )

cat("\n===============================================\n")
cat("Model 2 - Fare Policy by Service Type on Dwell Time per Passenger \n")
cat("===============================================\n")
model <- lm(DWTPPAS ~ EXITFARE*TypeServ + AVG.LOAD, data=trips)
print( summary(model) )

cat("\n===============================================\n")
cat("Model 2.3 - Dwell Time per Passenger \n")
cat("Controlling for route")
cat("===============================================\n")
model <- lm(DWTPPAS ~ EXITFARE*TypeServ + RouteName + AVG.LOAD, data=trips)
print( summary(model) )

cat("\n===============================================\n")
cat("Model 3 - Dwell Time \n")
cat("===============================================\n")
model <- lm(DWTIME ~ NumStops + EXITFARE*PAS + AVG.LOAD, data=trips)
print( summary(model) )
