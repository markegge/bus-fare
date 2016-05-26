library(car) # used for residual plots

# route output to exernal files
#sink('analysis-output.txt')
#pdf('analysis-figures.pdf')

#tapply(bus$PAX, bus$ROUTE, stat.desc)

sdf <- bus[bus$ROUTE == 'P1', ] # P1 only
sdf <- sdf[sdf$ZFREE == 1, ] # CBD only
sdf <- sdf[sdf$HR < 19, ] # before 7 pm only
cat("\n===============================================\n")
cat("Model 0 - No Fares: P1 in Free Zone (CBD) Before 7 PM (no fares collected) \n")
cat('Mean Dwell Time (both stops, all routes):', mean(sdf$DWTIME), 'seconds.\n')
cat("===============================================\n")
#elm <- lm(DWTIME ~ ON + OFF + FRICTION + PROGRESS + ZFREE, data=sdf) # r2 = .65
elm <- lm(DWTIME ~ ON + OFF + FRICTION, data=sdf) # r2 = .62
print( summary(elm) )



sdf <- bus[bus$ROUTE %in% c('P1') & bus$ZFREE==FALSE & bus$HR < 19 & bus$DIRECTION=='Outbound', ] # P1 exit fare
cat("\n===============================================\n")
cat("Model 1: P1 with exit fare \n")
cat('Mean Dwell Time (both stops, all routes):', mean(sdf$DWTIME), 'seconds.\n')
cat("===============================================\n")
elm <- lm(DWTIME ~ ON + OFF + FRICTION, data=sdf)
print( summary(elm) )


sdf <- bus[bus$ROUTE == 'G2', ] # G2 only
#sdf <- sdf[sdf$PMRUSHHOUR == 1, ] # PM rush hour only
cat("\n===============================================\n")
cat("Model 2: G2 (Entry Fare) only \n")
cat('Mean Dwell Time (both stops, all routes):', mean(sdf$DWTIME), 'seconds.\n')
cat("===============================================\n")
m2 <- lm(DWTIME ~ ON + OFF + FRICTION, data=sdf)
print( summary(m2) )


sdf <- bus[bus$ROUTE %in% c('P1', 'G2'), ] # P1 and G2, all time
cat("\n===============================================\n")
cat("Model 3: P1 and G2 Route Regression \n")
cat('Mean Dwell Time (both stops, all routes):', mean(sdf$DWTIME), 'seconds.\n')
cat("===============================================\n")
elm <- lm(DWTIME ~ (ON*ENTRYFARE) + (OFF*EXITFARE) + FRICTION, data=sdf)
print( summary(elm) )


sdf <- bus[bus$ROUTE == 'P1', ] # P1 only
cat("\n===============================================\n")
cat("Model 4: Within P1 Route Regression \n")
cat('Mean Dwell Time (both stops, all routes):', mean(sdf$DWTIME), 'seconds.\n')
cat("===============================================\n")
elm <- lm(DWTIME ~ (ON*ENTRYFARE) + (OFF*EXITFARE) + FRICTION, data=sdf)
print( summary(elm) )


sdf <- bus[bus$ROUTE %in% c('61A', '61B', '61C', '61D'), ] # 61x only
cat("\n===============================================\n")
cat("Model 5: Within 61 Route Regression \n")
cat('Mean Dwell Time (both stops, all routes):', mean(sdf$DWTIME), 'seconds.\n')
cat("===============================================\n")
elm <- lm(DWTIME ~ (ON*ENTRYFARE) + (OFF*EXITFARE) + FRICTION, data=sdf)
print( summary(elm) )


sdf <- bus[bus$ROUTE %in% c('71A', '71B', '71C', '71D'), ] # 71x only
# sdf <- sdf[sdf$PMRUSHHOUR == 1, ] # PM rush hour only. During PM rush hour, this effect disappears
cat("\n===============================================\n")
cat("Model 6: Within 71 Route Regression \n")
cat('Mean Dwell Time (both stops, all routes):', mean(sdf$DWTIME), 'seconds.\n')
cat("===============================================\n")
elm <- lm(DWTIME ~ (ON*ENTRYFARE) + (OFF*EXITFARE) + FRICTION, data=sdf)
print( summary(elm) )


sdf <- bus
cat("\n===============================================\n")
cat("Model 7: All buses, all the time \n")
cat('Mean Dwell Time (both stops, all routes):', mean(sdf$DWTIME), 'seconds.\n')
cat("===============================================\n")
elm <- lm(DWTIME ~ (ON*ENTRYFARE) + (OFF*EXITFARE) + FRICTION, data=sdf)
print( summary(elm) )

sdf <- bus
cat("\n===============================================\n")
cat("Model 8: All buses, controlling for route \n")
cat('Mean Dwell Time (both stops, all routes):', mean(sdf$DWTIME), 'seconds.\n')
cat("===============================================\n")
elm <- lm(DWTIME ~ (ON*ENTRYFARE) + (OFF*EXITFARE) + FRICTION + ROUTE, data=sdf)
print( summary(elm) )


sdf <- bus
cat("\n===============================================\n")
cat("Model 8: All buses, controlling for route*stop \n")
cat('Mean Dwell Time (both stops, all routes and stops):', mean(sdf$DWTIME), 'seconds.\n')
cat("===============================================\n")
elm <- lm(DWTIME ~ (ON*ENTRYFARE) + (OFF*EXITFARE) + FRICTION + QSTOPA, data=sdf)
print( summary(elm) )


# return output to local
#dev.off()
sink()
closeAllConnections()


# # PM rush hour only, CBD only
# sdf <- bus[bus$ZFREE == 1, ] # CBD Free zone only. No exit fare in the CBD
# sdf <- sdf[sdf$PMRUSHHOUR == 1, ] # PM rush hour only
# cat("\n===============================================\n")
# cat("Model 1: OLS Regression of Boarding (with and without entry fare), alighting, and friction on Dwell Time (PM Rush Hour Only\n")
# cat('Mean Dwell Time (both stops, all routes):', mean(sdf$DWTIME), 'seconds.\n')
# cat("===============================================\n")
# # there is no exit fare in the free zone. G2 is entry fare, all other are no entry fare
# fit <- lm(DWTIME ~ (ON*ENTRYFARE) + OFF + FRICTION, data=sdf)
# print( summary(fit) )
# #print( anova(dwlm) )

# I don't love the way my residual plot looks....
#plot(bdf$DWTIME, resid(fit), ylab="Residuals", xlab="Dwell time")
#abline(0,0)
#qqPlot(fit, id.method = "identify" )
#avPlots(fit, id.method="identify")

#z <- (   (coef(summary(elm))["ON:ENTRYFARE", "Estimate"] - coef(summary(m4))["OFF:EXITFARE", "Estimate"]) / 
#         sqrt( coef(summary(elm))["ON:ENTRYFARE", "Std. Error"]^2 + coef(summary(m4))["OFF:EXITFARE", "Std. Error"]^2 )       )
#cat("z SCORE for difference between entryfare time and exit fare time:", z, " || Significance test Pr(>|z|): ", 2*pnorm(-abs(z)), "\n")


