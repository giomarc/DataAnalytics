lm.evals <- function(p, a){
  p <- ifelse(p < 0, 0, p)
  mse <- mean((p - a)^2)
  nsme <- mse/mean((mean(a) - a)^2)
  c(mse = mse, nsme = nsme)
}

glm.lin.evals <- function(model, a, mod.m){
  #require(abind)
  r <- data.frame(actual = a)
  e <- data_frame()
  for (i in 1:length(model$lambda)){
    r$score <- predict(model, newx = mod.m)[,i]
    c <- as.data.frame(t(lm.evals(p = r$score, a = r$actual)))
    e <- rbind(e, c)
  }
  
  return(e)
}