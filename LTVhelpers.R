lm.evals <- function(p, a){
  p <- ifelse(p < 0, 0, p)
  mse <- mean((p - a)^2)
  nsme <- mse/mean((mean(a) - a)^2)
  c(mse = mse, nsme = nsme)
}