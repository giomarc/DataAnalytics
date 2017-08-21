# load data

file <- "playersample.txt"
players <- read.table(file, header = TRUE, stringsAsFactors = FALSE)
str(players)

head(players)

summary(players)

max(players$BOOKINGS_3)
