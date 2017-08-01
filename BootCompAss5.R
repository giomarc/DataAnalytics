boot.comb <- function(a = auto.price, f = auto.price$body.style, n = 100){
  split.a <- split(a, f)
  names.col <- names(split.a)
  i <- length(split.a)
  y <- combn(i, 2)
  
  df <- data.frame()
  
  
  for(j in 1:length(split.a)){
    if(j==1){
      split.a.boot <- one.boot(split.a[[j]]$log.price, mean, R = n)$t
    } else {
      split.a.boot <- cbind(split.a.boot, one.boot(split.a[[j]]$log.price, mean, R = n)$t)
    }
  }
  
  # set layout
  ml <- matrix(0, ncol = i-1, nrow = i-1)
  mcol <- 1
  mrow <- 1
  for(l in 1:ncol(y)){
    ml[mrow, mcol] <- l
    if (l != ncol(y)){
      if(y[1, l] != y[1,l+1]){
        mcol <- 1
        mrow <- mrow + 1 }
      else {
        mcol <- mcol + 1
      }
    }
  }
  
  layout(matrix(as.numeric(matrix(ml, nrow = i-1, byrow = T)), nrow = i-1, byrow=T))
  
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
    two.boot.mean <- two.boot(split.a[[x.1]]$log.price, split.a[[x.2]]$log.price, mean, R = n)
    plot.diff(two.boot.mean$t, cols = paste(names.col[x.1], names.col[x.2], sep = "-"))
  }
  
  colnames(df) <- c('body.1', 'body.2', 
                    'mean.1', 'mean.2 ',
                    'mean.diff', 'lower', 'upper', 'p.value')
  df[, 3:ncol(df)] <- lapply(df[,3:ncol(df)], as.character)
  df[, 3:ncol(df)] <- lapply(df[,3:ncol(df)], as.numeric)
  df[, 3:ncol(df)] <- lapply(df[,3:ncol(df)], round, digits = 5)
  
  par(mfrow =c(1,1))
  
  return(df)
}


boot.comb.one <- function(a = auto.price, f = auto.price$body.style, n = 100){
  split.a <- split(a, f)
  names.col <- names(split.a)
  i <- length(split.a)

  for(j in 1:length(split.a)){
    if(j==1){
      split.a.boot <- one.boot(split.a[[j]]$log.price, mean, R = n)$t
    } else {
      split.a.boot <- cbind(split.a.boot, one.boot(split.a[[j]]$log.price, mean, R = n)$t)
    }
  }
  
  plot.t.n(a = split.a.boot, n = i, cols = names.col)
}

plot.t.n <- function(a, n, cols = c('pop_A', 'pop_B'), nbins = 80, p = 0.05){
  maxs <- max(as.numeric(sapply(a, max)))
  mins <- min(as.numeric(sapply(a, min)))
  par(mfrow = c(n, 1))
  for(i in 1:n){plot.hist(a[,i], maxs, mins, cols = cols[i], nbins = nbins)}
  par(mfrow = c(1, 1))
}

boot.comb.two <- function(a = auto.price, f = auto.price$body.style, n = 100){
  split.a <- split(a, f)
  names.col <- names(split.a)
  i <- length(split.a)
  y <- combn(i, 2)
  
  # set layout
  ml <- matrix(0, ncol = i-1, nrow = i-1)
  mcol <- 1
  mrow <- 1
  for(l in 1:ncol(y)){
    ml[mrow, mcol] <- l
    if (l != ncol(y)){
      if(y[1, l] != y[1,l+1]){
        mcol <- 1
        mrow <- mrow + 1 }
      else {
        mcol <- mcol + 1
      }
    }
  }
  
  layout(matrix(as.numeric(matrix(ml, nrow = i-1, byrow = T)), nrow = i-1, byrow=T))
  
  for(x in 1:ncol(y)){
    x.1 <- y[1,x]
    x.2 <- y[2,x]
    two.boot.mean <- two.boot(split.a[[x.1]]$log.price, split.a[[x.2]]$log.price, mean, R = n)
    plot.diff(two.boot.mean$t, cols = paste(names.col[x.1], names.col[x.2], sep = "-"))
  }
  
  par(mfrow =c(1,1))
}