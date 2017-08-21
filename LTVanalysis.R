# load 

file <- "ltvsampl.txt"
players <- read.table(file, header = TRUE, stringsAsFactors = FALSE)
str(players)

max(players$BOOKINGS_3)
