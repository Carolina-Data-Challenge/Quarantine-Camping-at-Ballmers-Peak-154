
setwd("~/Downloads/us_cities/")

pdf("./plots.pdf", width = 8, height = 4)

cases <- read.csv('covid_confirmed_usafacts.csv', header = TRUE, sep = ",")

durham <- read.csv('Durham.csv', header = TRUE, sep = ",")
durham <- durham[durham$parameter == "pm25",]

locations <- unique(durham$location)
durham.hravg <- c()
for ( t in unique(durham$utc) ) {
  hr_avg <- mean(durham$value[durham$utc == t])
  durham.hravg <- c(durham.hravg, hr_avg)
}
#durham <- cbind(unique(durham$utc), durham.avg)

durham.avg <- c()
days <- sort(unique(as.Date( unique(durham$utc) )))
for (d in days) {
  avg <- mean( durham.hravg[ as.Date(unique(durham$utc)) == d ] )
  durham.avg <- c(durham.avg, avg)
}

delta <- c()
for (i in 1:length(durham.avg)-1) {
  delta <- c( delta, durham.avg[i+1] - durham.avg[i] )
}

cases.durham <- cases[cases$County.Name == "Durham County",][5:length(cases)]
cases.durham_delta <- vector( "integer", length(delta) - (length(cases)-5) )
for (i in 1:length(cases.durham)-1) {
  cases.durham_delta <- c( cases.durham_delta, as.integer(cases.durham[i+1]) - as.integer(cases.durham[i]) )
}

plot( days[2:length(days)], delta, type = 'l', main = "Durham, NC", xlab = "Sep. 10 to Oct. 3", ylab = "Daily change", ylim = c(min(delta,0.1*(cases.durham_delta)),max(delta,0.1*(cases.durham_delta))) )
lines(days[2:length(days)], 0.1*(cases.durham_delta), col = "blue")
legend('topleft', legend = c("PM2.5 (micrograms per cubic meter)", "Covid 19 cases"), col = c("black", "blue"), lwd = 1, box.lty = 0)

# AUSTIN ----------------------------------------------------
austin <- read.csv('Austin.csv', header = TRUE, sep = ",")
austin.hravg <- c()
for ( t in unique(austin$utc) ) {
  hr_avg <- mean(austin$value[austin$utc == t])
  austin.hravg <- c(austin.hravg, hr_avg)
}

austin.avg <- c()
days <- sort( unique(as.Date( unique(austin$utc) )) )
for (d in days) {
  avg <- mean( austin.hravg[ as.Date(unique(austin$utc)) == d ] )
  austin.avg <- c(austin.avg, avg)
}

delta <- c()
for (i in 1:length(austin.avg)-1) {
  delta <- c( delta, austin.avg[i+1] - austin.avg[i] )
}

cases.austin <- cases[cases$County.Name == "Austin County",][5:length(cases)]
cases.austin_delta <- c()
for (i in which(days == as.Date("2019-09-01")):which(days == as.Date("2020-02-18"))-1) {
  cases.austin_delta <- c( cases.austin_delta, as.integer(cases.austin[i+1]) - as.integer(cases.austin[i]) )
}

plot( days[2:length(days)], delta, type = 'l', main = "Austin, TX", xlab = "Feb. 18 to Oct. 3", ylab = "Daily change", ylim = c(min(delta,(cases.austin_delta)),max(delta,(cases.austin_delta))) )
lines(days[2:length(days)], (cases.austin_delta), col = "blue")
legend('topleft', legend = c("PM2.5 (micrograms per cubic meter)", "Covid 19 cases"), col = c("black", "blue"), lwd = 1, box.lty = 0, cex = 0.7, y.intersp = .8)

# HOUSTON ----------------------------------------------------
houston <- read.csv('Houston.csv', header = TRUE, sep = ",")
houston.hravg <- c()
for ( t in unique(houston$utc) ) {
  hr_avg <- mean(houston$value[houston$utc == t])
  houston.hravg <- c(houston.hravg, hr_avg)
}

houston.avg <- c()
days <- sort( unique(as.Date( unique(houston$utc) )) )
for (d in days) {
  avg <- mean( houston.hravg[ as.Date(unique(houston$utc)) == d ] )
  houston.avg <- c(houston.avg, avg)
}

delta <- c()
for (i in 1:length(houston.avg)-1) {
  delta <- c( delta, houston.avg[i+1] - houston.avg[i] )
}

cases.houston <- cases[cases$County.Name == "Houston County" & cases$State == "TX",][5:length(cases)]
cases.houston_delta <- c()
cases.houston_delta <- vector( "integer", length(delta) - (length(cases)-5) )
for (i in 1:length(cases.houston)-1) {
  cases.houston_delta <- c( cases.houston_delta, as.integer(cases.houston[i+1]) - as.integer(cases.houston[i]) )
}

plot( days[2:length(days)], delta, type = 'l', main = "Houston, TX", xlab = "Sep. 1 to Oct. 3", ylab = "Daily change", ylim = c(min(delta,(cases.houston_delta)),max(delta,(cases.houston_delta))) )
lines(days[2:length(days)], (cases.houston_delta), col = "blue")
legend('topleft', legend = c("PM2.5 (micrograms per cubic meter)", "Covid 19 cases"), col = c("black", "blue"), lwd = 1, box.lty = 0, cex = 0.7, y.intersp = .8)


# LA ---------------------------------------------
la <- read.csv('LA.csv', header = TRUE, sep = ",")
la.hravg <- c()
for ( t in unique(la$utc) ) {
  hr_avg <- mean(la$value[la$utc == t])
  la.hravg <- c(la.hravg, hr_avg)
}

la.avg <- c()
days <- sort( unique(as.Date( unique(la$utc) )) )
for (d in days) {
  avg <- mean( la.hravg[ as.Date(unique(la$utc)) == d ] )
  la.avg <- c(la.avg, avg)
}

delta <- c()
for (i in 1:length(la.avg)-1) {
  delta <- c( delta, la.avg[i+1] - la.avg[i] )
}

cases.la <- cases[cases$County.Name == "Los Angeles County",][5:length(cases)]
cases.la_delta <- c()
cases.la_delta <- vector( "integer", length(delta) - (length(cases)-5) )
for (i in 1:length(cases.la)-1) {
  cases.la_delta <- c( cases.la_delta, as.integer(cases.la[i+1]) - as.integer(cases.la[i]) )
}

plot( days[2:length(days)], delta, type = 'l', main = "Los Angeles, CA", xlab = "Oct. 19 to Oct. 3", ylab = "Daily change", ylim = c(min(delta,0.01*(cases.la_delta)),max(delta,0.01*(cases.la_delta))) )
lines(days[2:length(days)], 0.01*(cases.la_delta), col = "blue")
legend('topleft', legend = c("PM2.5 (micrograms per cubic meter)", "Covid 19 cases (in hundreds)"), col = c("black", "blue"), lwd = 1, box.lty = 0, cex = 0.7, y.intersp = .8)


# MIAMI ------------------------------------------------------------------
miami <- read.csv('Miami.csv', header = TRUE, sep = ",")
miami.hravg <- c()
for ( t in unique(miami$utc) ) {
  hr_avg <- mean(miami$value[miami$utc == t])
  miami.hravg <- c(miami.hravg, hr_avg)
}

miami.avg <- c()
days <- sort( unique(as.Date( unique(miami$utc) )) )
for (d in days) {
  avg <- mean( miami.hravg[ as.Date(unique(miami$utc)) == d ] )
  miami.avg <- c(miami.avg, avg)
}

delta <- c()
for (i in 1:length(miami.avg)-1) {
  delta <- c( delta, miami.avg[i+1] - miami.avg[i] )
}

cases.miami <- cases[cases$County.Name == "Miami-Dade County",][5:length(cases)]
cases.miami_delta <- c()
cases.miami_delta <- vector( "integer", length(delta) - (length(cases)-5) )
for (i in 1:length(cases.miami)-1) {
  cases.miami_delta <- c( cases.miami_delta, as.integer(cases.miami[i+1]) - as.integer(cases.miami[i]) )
}

plot( days[2:length(days)], delta, type = 'l', main = "Miami, FL", xlab = "Sep. 4 to Oct. 3", ylab = "Daily change", ylim = c(min(delta,0.01*(cases.miami_delta)),max(delta,0.01*(cases.miami_delta))) )
lines(days[2:length(days)], 0.01*(cases.miami_delta), col = "blue")
legend('topleft', legend = c("PM2.5 (micrograms per cubic meter)", "Covid 19 cases (in hundreds)"), col = c("black", "blue"), lwd = 1, box.lty = 0, cex = 0.7, y.intersp = .8)

# SAN FRAN  -------------------------------------------------
sf <- read.csv('SanFran.csv', header = TRUE, sep = ",")
sf.hravg <- c()
for ( t in unique(sf$utc) ) {
  hr_avg <- mean(sf$value[sf$utc == t])
  sf.hravg <- c(sf.hravg, hr_avg)
}

sf.avg <- c()
days <- sort( unique(as.Date( unique(sf$utc) )) )
for (d in days) {
  avg <- mean( sf.hravg[ as.Date(unique(sf$utc)) == d ] )
  sf.avg <- c(sf.avg, avg)
}

delta <- c()
for (i in 1:length(sf.avg)-1) {
  delta <- c( delta, sf.avg[i+1] - sf.avg[i] )
}

cases.sf <- cases[cases$County.Name == "San Francisco County",][5:length(cases)]
cases.sf_delta <- c()
for (i in which(cases.sf == cases.sf$X7.3.20)[1] : (which(cases.sf == cases.sf$X9.15.20)-3) ) {
  cases.sf_delta <- c( cases.sf_delta, as.integer(cases.sf[i+1]) - as.integer(cases.sf[i]) )
}

plot( days[2:length(days)], delta, type = 'l', main = "San Francisco, CA", xlab = "Jul. 3 to Sep. 15", ylab = "Daily change", ylim = c(min(delta,0.01*(cases.sf_delta)),max(delta,0.01*(cases.sf_delta))) )
lines(days[2:length(days)], 0.01*(cases.sf_delta), col = "blue")
legend('topleft', legend = c("PM2.5 (micrograms per cubic meter)", "Covid 19 cases (in hundreds)"), col = c("black", "blue"), lwd = 1, box.lty = 0, cex = 0.7, y.intersp = .8)


dev.off()