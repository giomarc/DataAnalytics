# Get Dairy Data

file <- "CADairyProduction.csv"
dairy <- read.csv(file, header = TRUE, stringsAsFactors = FALSE)
str(dairy)

IceCream <- dairy[,c("Year", "Month", "Icecream.Prod")]

head(IceCream)

IceCream.ts <- ts(IceCream[,3], start = IceCream[1,1], frequency = 12)

head(IceCream.ts)

par(mfrow = c(1,2))
plot(IceCream.ts)
plot(log(IceCream.ts))
par(mfrow = c(1,1))


require(tseries)
adf.test(IceCream.ts, alternative = "stationary")

dist.ts = function(df, col = 'residual', bins = 40){
  par(mfrow = c(1,2))
  temp = as.vector(df)
  breaks = seq(min(temp), max(temp), length.out = (bins + 1))
  hist(temp, breaks = breaks, main = paste('Distribution of ', col), xlab = col)
  qqnorm(temp, main = paste('Normal Q-Q plot of ', col))
  par(mfrow = c(1,1))
}

dist.ts(IceCream.ts, col = "Icecream.Prod")



options(repr.pmales.extlot.width=8, repr.plot.height=8)
plot.acf <- function(df, col = 'remainder', is.df =TRUE){
  if(is.df) temp <- df[, col]
  else temp <- df
  par(mfrow = c(2,1))
  acf(temp, main = paste('ACF of', col))
  pacf(temp, main = paste('PACF of', col))
  par(mfrow = c(1,1))
}

plot.acf(IceCream.ts, col ="Icecream.Prod", is.df = FALSE)

ts.decomp <- function(df, col = 'elec.ts', span = 0.5, Mult = TRUE, is.df = TRUE){
  # if(Mult) temp = log(df[, col])  else temp = ts(df[, col]
  if(is.df) temp = log(df[, col])  
  else temp = df
  spans = span * length(temp)  
  fit <- stl(temp, s.window = "periodic", t.window = spans)
  plot(fit, main = paste('Decompositon of',col,'with lowess span = ', as.character(span)))
  fit$time.series
}

IceCream.decomp <- ts.decomp(IceCream.ts, is.df = FALSE, Mult = FALSE)

plot.acf(IceCream.decomp[,3], col = "remainder", is.df = FALSE)

require(forecast)
IceCream.ARIMA <- auto.arima(IceCream.ts, max.p=4, max.q=4,
                             max.P=2, max.Q=2, max.order=5, max.d=2, max.D=1,
                             start.p=0, start.q=0, start.P=0, start.Q=0)
summary(IceCream.ARIMA)

plot.acf(IceCream.ARIMA$residuals, col = "residuals", is.df = FALSE)

IceCream.forecast <- forecast(IceCream.ARIMA, h = 12)
summary(IceCream.forecast)

plot(IceCream.forecast)

IceCream.forecast <- forecast(IceCream.ARIMA, h = 36)
range.95 <- IceCream.forecast$upper[,2]-IceCream.forecast$lower[,2]
plot(range.95)
rep(dairy$Month[1:12], 3)
