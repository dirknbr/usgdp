
# data from fred https://fred.stlouisfed.org/series/GDPC1

library(dplyr)
library(lubridate)

gdp <- read.csv('GDPC1.csv', stringsAsFactors = F)
unemp <- read.csv('ICSA.csv', stringsAsFactors = F)
pop <- read.csv('LFWA64TTUSM647S.csv', stringsAsFactors = F)

# make dates

gdp$DATE <- as.Date(gdp$DATE)
unemp$DATE <- as.Date(unemp$DATE)
pop$DATE <- as.Date(pop$DATE)
gdp$quarter <- paste(strftime(gdp$DATE, "%Y"), quarter(gdp$DATE))
unemp$quarter <- paste(strftime(unemp$DATE, "%Y"), quarter(unemp$DATE))
pop$quarter <- paste(strftime(pop$DATE, "%Y"), quarter(pop$DATE))

lag <- function(x, l=1) {
  return(c(rep(NA, l), x[1:(length(x) - l)]))
}

# test 
lag(1:10, 4)

unempq <- dplyr::summarise(group_by(unemp, quarter), ICSA = mean(ICSA))
popq <- dplyr::summarise(group_by(pop, quarter), pop = mean(LFWA64TTUSM647S))

# merge

gdp <- merge(gdp, unempq, by = 'quarter', all.x = T, all.y = T)
gdp <- merge(gdp, popq, by = 'quarter', all.x = T, all.y = T)

# make q on q growth

gdp$gdpgrowth <- gdp$GDPC1 / lag(gdp$GDPC1, 4) - 1
gdp$unemp_pop <- gdp$ICSA / gdp$pop

summary(gdp)
cor(gdp[, 3:7], use = 'compl')
# png('icsa.png')
plot(gdp$ICSA, type = 'l')
# dev.off()

model <- lm(gdpgrowth ~ ICSA, data = gdp)
summary(model)
plot(model)

tail(gdp)

coef(model)[1] + coef(model)[2] * tail(unempq$ICSA, 2)