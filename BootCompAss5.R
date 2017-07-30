boot.comb <- function(a = auto.price, f = auto.price$body.style, n = 100){
  split.a <- split(a, f)
  names.col <- names(split.a)
  i <- length(split.a)
  y <- combn(i, 2)
  
  df <- data.frame()
  
  
  for(j in 1:length(split.a)){
    split.a.boot <- cbind(split.a.boot, one.boot(split.a[[j]]$log.price, mean, R = n)$t)
  }
  
  for(x in 1:ncol(y)){
    x.1 <- y[1,x]
    x.2 <- y[2,x]
    t <- t.test(split.a.boot[,x.1], split.a.boot[,x.2])
    r <-  c(names.col[x.1],
            names.col[x.2],
            as.numeric(t$estimate[1]),
            as.numeric(t$estimate[2]),
            as.numeric(t$estimate[1]) - as.numeric(t$estimate[2]),
            as.numeric(t$conf.int)[1],
            as.numeric(t$conf.int)[2],
            as.numeric(t$p.value))
    r <- as.data.frame(t(r))
    df <- rbind(df, r)
    #two.boot.mean <- two.boot(split.a auto.diesel$log.price, mean, R = 100000)
  }
  colnames(df) <- c('body.1', 'body.2', 
                    'mean.1', 'mean.2 ',
                    'mean.diff', 'lower', 'upper', 'p.value')
  df[, 3:ncol(df)] <- lapply(df[,3:ncol(df)], as.character)
  df[, 3:ncol(df)] <- lapply(df[,3:ncol(df)], as.numeric)
  df[, 3:ncol(df)] <- lapply(df[,3:ncol(df)], round, digits = 5)
  return(df)
}