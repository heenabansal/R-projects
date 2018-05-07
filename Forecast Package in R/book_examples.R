library(forecast)
library(fpp)
library(fpp2)
library(ggplot2)
data("melsyd")
data("a10")
data("elecdemand")
data("ausbeer")

autoplot(melsyd[,"Economy.Class"]) +
  ggtitle("Economy class passengers: Melbourne-Sydney") +
  xlab("Year") + ylab("Thousands")

autoplot(a10) +
  ggtitle("Antidiabetic drug sales") +
  ylab("$ million") + xlab("Year")

ggseasonplot(a10, year.labels=TRUE, year.labels.left=TRUE) +
  ylab("$ million") + ggtitle("Seasonal plot: antidiabetic drug sales")

ggseasonplot(melsyd[,"Economy.Class"], year.labels=TRUE, year.labels.left=TRUE) +
  ylab("Thousands") + ggtitle("Economy class passengers: Melbourne-Sydney")

ggseasonplot(a10, polar=TRUE) +
  ylab("$ million") + ggtitle("Polar seasonal plot: antidiabetic drug sales")

ggseasonplot(melsyd[,"Economy.Class"], polar=TRUE) +
  ylab("Thousands") + ggtitle("Economy class passengers: Melbourne-Sydney")

ggsubseriesplot(a10) + ylab("$ million") +
  ggtitle("Seasonal subseries plot: antidiabetic drug sales")

ggsubseriesplot(melsyd[,"Economy.Class"]) +  ylab("Thousands") + ggtitle("Economy class passengers: Melbourne-Sydney")

month.breaks <- cumsum(c(0,31,28,31,30,31,30,31,31,30,31,30,31)*48)
autoplot(elecdemand[,c(1,3)], facet=TRUE) +
  xlab("Year: 2014") + ylab("") +
  ggtitle("Half-hourly electricity demand: Victoria, Australia") +
  scale_x_continuous(breaks=2014+month.breaks/max(month.breaks), 
                     minor_breaks=NULL, labels=c(month.abb,month.abb[1]))


autoplot(melsyd[,c(1,3)], facet=TRUE) +
  xlab("") + ylab("") +
  ggtitle("Economy class passengers: Melbourne-Sydney") 

qplot(Temperature, Demand, data=as.data.frame(elecdemand)) +
  ylab("Demand (GW)") + xlab("Temperature (Celsius)")


beer2 <- window(ausbeer, start=1992)
gglagplot(beer2)

ggAcf(beer2)

aelec <- window(elec, start=1980)
autoplot(aelec) + xlab("Year") + ylab("GWh")
ggAcf(aelec, lag=48)

set.seed(30)
y <- ts(rnorm(50))
autoplot(y) + ggtitle("White noise")

ggAcf(y)

frequency(gold)

autoplot(gold)

meanf(gold, 10)
naive(gold, 10)
snaive(gold,10)
rwf(gold,10, drift=TRUE)


beer2 <- window(ausbeer,start=1992,end=c(2007,4))
# Plot some forecasts
autoplot(beer2) +
  forecast::autolayer(meanf(beer2, h=11), PI=FALSE, series="Mean") +
  forecast::autolayer(naive(beer2, h=11), PI=FALSE, series="Naïve") +
  forecast::autolayer(snaive(beer2, h=11), PI=FALSE, series="Seasonal naïve") +
  ggtitle("Forecasts for quarterly beer production") +
  xlab("Year") + ylab("Megalitres") +
  guides(colour=guide_legend(title="Forecast"))

dj2 <- window(dj,end=250)
# Plot some forecasts
autoplot(dj2) +
  forecast::autolayer(meanf(dj2, h=42), PI=FALSE, series="Mean") +
  forecast::autolayer(rwf(dj2, h=42), PI=FALSE, series="Naïve") +
  forecast::autolayer(rwf(dj2, drift=TRUE, h=42), PI=FALSE, series="Drift") +
  ggtitle("Dow Jones Index (daily ending 15 Jul 94)") +
  xlab("Day") + ylab("") +
  guides(colour=guide_legend(title="Forecast"))

dframe <- cbind(Monthly = milk, DailyAverage=milk/monthdays(milk)) 
autoplot(dframe, facet=TRUE) + xlab("Years") + ylab("Pounds") +
  ggtitle("Milk production per cow")

autoplot(BoxCox(elec,1))
(lambda <- BoxCox.lambda(elec))
#> [1] 0.265
autoplot(BoxCox(elec,lambda))

fc <- rwf(eggs, drift=TRUE, lambda=0, h=50, level=80)
fc2 <- rwf(eggs, drift=TRUE, lambda=0, h=50, level=80, biasadj=TRUE)
autoplot(eggs) +
  forecast::autolayer(fc, series="Simple back transformation") +
  forecast::autolayer(fc2, PI=FALSE, series="Bias adjusted") +
  guides(colour=guide_legend(title="Forecast"))

dj2 <- window(dj, end=250)
autoplot(dj2) + xlab("Day") + ylab("") +
  ggtitle("Dow Jones Index (daily ending 15 Jul 94)")

res <- residuals(naive(dj2))
autoplot(res) + xlab("Day") + ylab("") +
  ggtitle("Residuals from naïve method")

gghistogram(res) + ggtitle("Histogram of residuals")

ggAcf(res) + ggtitle("ACF of residuals")








































