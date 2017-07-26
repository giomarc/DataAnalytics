dice <- function(no_of_rolls=1){
  x <- sample(1:6, size=no_of_rolls, replace=TRUE)
  y <- sample(1:6, size=no_of_rolls, replace=TRUE)
  return(cbind(x,y))
}

set.seed(20485)
rolls <- as.data.frame(dice(100))

library(plyr)
freq_table <- ddply(rolls, ~x, summarize,
                    y1=sum(y==1), y2=sum(y==2), y3= sum(y==3),
                    y4 = sum(y==4), y5=sum(y==5), y6=sum(y==6))
row.names(freq_table) <- paste0('x',1:6)
prob_table <- freq_table[,-1]/100
prob_table
