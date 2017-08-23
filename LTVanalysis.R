# load data

file <- "playersample.txt"
players <- read.table(file, header = TRUE, stringsAsFactors = FALSE)


# understand data structure
str(players)
head(players)
summary(players)

#remove ID column as it has no affect on analysis
require(dplyr)

players <- players %>% select(-ID)

#change numeric columns with any NAs to 0 

players[is.na(players)] <- 0

#change character column to date
players[, "INSTALLDATE"] <- as.Date(players[, "INSTALLDATE"])

#factor charater columns
players <- data.frame(lapply(players, function(x) if(is.character(x)) as.factor(x) else x))

#create a purchaser column, fails, success.rate column
players <- players %>% mutate(purchaser_30 = PURCHASES_30 > 0)
players <- players %>% mutate(purchaser_3 = PURCHASES_3 > 0)
players <- players %>% mutate(fails = (ATTEMPTS + RETRYS) - COMPLETES)
players <- players %>% mutate(success.rate = ifelse(ATTEMPTS > 0, COMPLETES/(ATTEMPTS), 0))

#statistics from fbasics
require(fBasics)

basicStats(players[, sapply(players, is.numeric)])[c("nobs", 
                                                     "Minimum", 
                                                     "1. Quartile", 
                                                     "Median", 
                                                     "Mean", 
                                                     "Median", 
                                                     "3. Quartile",
                                                     "Maximum",
                                                     "Stdev",
                                                     "Skewness"),]

#understand payer summary vs non-payers

summary(dplyr::filter(players, purchaser_30 == TRUE))
summary(dplyr::filter(players, purchaser_30 == FALSE))
summary(dplyr::filter(players, SESSIONS == 1))


players %>% dplyr::filter(SESSIONS == 1)  %>% count(PURCHASES_30)

# bar plots of purchasers/purchases

require(ggplot2)
require(gridExtra)

# How many purchasers are there as a proportion of players monetize after 3 days and after 30 days 

p1 <- ggplot(players, aes(x = as.factor(purchaser_3))) +
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25) +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Monetizers after 3 days", y = "Percent", x = "Monetizer")

p2 <- ggplot(players, aes(x = as.factor(purchaser_30))) +
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25) +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Monetizers after 30 days", y = "Percent", x = "Monetizer")

grid.arrange(p1, p2, ncol = 2)

# Examine 

ggplot(players) + stat_count(mapping = aes(x=purchaser_30, y=..prop.., group=1))

# Monetizers sessiontime
bw1 = (max(players$SESSIONS) - min(players$SESSIONS))/200
ggplot(players, aes(SESSIONS)) + 
  geom_histogram(binwidth = bw1) + 
  xlab('Sessions (sec.)') +
  ylab('Counts') +
  xlim(NA, 100) +
  ggtitle('Session Time first 3 days')

# plot session time for 30 day purchasers

a <- players %>% dplyr::filter(purchaser_30 == TRUE) %>% select(SESHTIME)  
b <- players %>% dplyr::filter(purchaser_30 == FALSE) %>% select(SESHTIME) 
plot.t(a$SESHTIME, b$SESHTIME, cols = c('First 3 days Session Time (sec.) Monetizer', 'First 3 days Session Time (sec.) Non-Monetizer'))

# plot session time for 30 day purchasers

a <- players %>% dplyr::filter(purchaser_30 == TRUE & SESHTIME > 0) %>% select(SESHTIME)  
b <- players %>% dplyr::filter(purchaser_30 == FALSE & SESHTIME > 0) %>% select(SESHTIME) 
plot.t(a$SESHTIME, b$SESHTIME, cols = c('First 3 days Session Time (sec.) Monetizer', 'First 3 days Session Time (sec.) Non-Monetizer'))

# plot sinks time for 30 day purchasers

a <- players %>% dplyr::filter(purchaser_30 == TRUE & SESHTIME > 0) %>% select(TOTALSINKS)  
b <- players %>% dplyr::filter(purchaser_30 == FALSE & SESHTIME > 0) %>% select(TOTALSINKS) 
plot.t(a$TOTALSINKS, b$TOTALSINKS, cols = c('First 3 days Sinks Monetizer', 'First 3 days Sinks Non-Monetizer'))

plot.dists.cut(a$TOTALSINKS, b$TOTALSINKS, 
               cols = c('First 3 days Sinks Monetizer', 'First 3 days Sinks Non-Monetizer'), 
               nbins = 160)

# plot last course time for 30 day purchasers

a <- players %>% dplyr::filter(purchaser_30 == TRUE & SESHTIME > 0) %>% select(LASTCOURSE)  
b <- players %>% dplyr::filter(purchaser_30 == FALSE & SESHTIME > 0) %>% select(LASTCOURSE) 
plot.dists(a$LASTCOURSE, b$LASTCOURSE, cols = c('Last Course Reached Day 3 Monetizer', 'Last Course Reached Day 3 Non-Monetizer'), nbins = 500)


plot.dists.cut(a$LASTCOURSE, b$LASTCOURSE, cols = c('Last Course Reached Day 3 Monetizer', 'Last Course Reached Day 3 Non-Monetizer'), nbins = 838)


players.sub <- players %>% dplyr::filter((SESSIONS > 1 & SESHTIME >0) | purchaser_30 == TRUE)
summary(players.sub)

ggplot(sample_frac(players.sub, 0.10), aes(SESHTIME, TOTALSINKS)) + 
  geom_point(aes(color = factor(purchaser_30), size = BOOKINGS_30), alpha = 0.3) + 
  xlab('Session Time (sec.)') + ylab('Total Sinks') + 
  ggtitle('Engagement vs. Sinking')


ggplot(sample_frac(players.sub, 0.05), aes(x = SESHTIME, y= TOTALSINKS, fill = PURCHASES_30)) + 
  stat_binhex(aes(alpha = ..count..)) + 
  scale_alpha(name = 'Frequency', range = c(0,1)) + 
  xlab('Session Time (sec.)') + ylab('Total Sinks') + 
  ggtitle('Engagement vs. Sinking')

ggplot(sample_frac(players.sub, 0.05), aes(x = SESHTIME, y = TOTALSINKS)) +
  geom_raster(aes(fill = PURCHASES_30), interpolate = TRUE)

# density plots
colfunc <- colorRampPalette(c("white", "lightblue", "green", "yellow", "red"))
p1 <- ggplot(dplyr::filter(players.sub, purchaser_30 == TRUE), aes(x = SESHTIME, y = TOTALSINKS)) +
  ylim(0, 500) + xlim(0,30000) +
  stat_density2d(geom="tile", aes(fill = ..density..), contour = FALSE) +
  scale_fill_gradientn(colours=colfunc(400)) + 
  geom_density2d(colour="black", bins=5)

p2 <- ggplot(dplyr::filter(players.sub, purchaser_30 == FALSE), aes(x = SESHTIME, y = TOTALSINKS)) +
  ylim(0, 500) + xlim(0,30000) +
  stat_density2d(geom="tile", aes(fill = ..density..), contour = FALSE) +
  scale_fill_gradientn(colours=colfunc(400)) + 
  geom_density2d(colour="black", bins=5)
# density plots with 5x zoom
p3 <- ggplot(dplyr::filter(players.sub, purchaser_30 == TRUE), aes(x = SESHTIME, y = TOTALSINKS)) +
  ylim(0, 500/5) + xlim(0,30000/5) +
  stat_density2d(geom="tile", aes(fill = ..density..), contour = FALSE) +
  scale_fill_gradientn(colours=colfunc(400)) + 
  geom_density2d(colour="black", bins=5)

p4 <- ggplot(dplyr::filter(players.sub, purchaser_30 == FALSE), aes(x = SESHTIME, y = TOTALSINKS)) +
  ylim(0, 500/5) + xlim(0,30000/5) +
  stat_density2d(geom="tile", aes(fill = ..density..), contour = FALSE) +
  scale_fill_gradientn(colours=colfunc(400)) + 
  geom_density2d(colour="black", bins=5)

grid.arrange(p1, p2, p3, p4, ncol = 2, nrow = 2)

ggplot(dplyr::filter(players.sub, purchaser_30 == TRUE), aes(x = SESHTIME, y = TOTALSINKS)) +
  geom_point(aes(color = PURCHASES_30), alpha = 0.3) +
  geom_density2d(colour="white", bins=5)

ggplot(dplyr::filter(players.sub, purchaser_30 == FALSE), aes(x = SESHTIME, y = TOTALSINKS)) +
  geom_point(alpha = 0.3) +
  geom_density2d(colour="white", bins=5)

# correlation plot

require(corrplot)
cors = cor(players.sub[, sapply(players.sub, is.numeric)], method = 'pearson')
corrplot.mixed(cors, upper = "ellipse", tl.cex = 0.8)

cex.before <- par("cex")
par(cex = 0.7)
corrplot(cors, method = "color",
         addCoef.col="grey", 
         order = "AOE", tl.cex = 0.8,
         cl.cex = 1/par("cex"), addCoefasPercent = FALSE)
par(cex = cex.before)
