lm.evals <- function(p, a){
  p <- ifelse(p < 0, 0, p)
  mse <- mean((p - a)^2)
  nsme <- mse/mean((mean(a) - a)^2)
  c(mse = mse, nsme = nsme)
}

#provide glm model, actuals, and test model matrix
glm.lin.evals <- function(model, a, mod.m){
  #require(abind)
  r <- data.frame(actual = a)
  e <- data_frame()
  #iterate through all lamdas and get eval metrics
  for (i in 1:length(model$lambda)){
    r$score <- predict(model, newx = mod.m)[,i]
    c <- as.data.frame(t(lm.evals(p = r$score, a = r$actual)))
    e <- rbind(e, c)
  }
  
  return(e)
}

glm.logit.eval <- function(model, a, mod.m, thrsh = 0.5) {
  
  r <- data.frame(actual = a)
  e <- data_frame() 
  #iterate through all lamdas and get eval metrics
  for (i in 1:length(model$lambda)){
    r$score <- predict(model, newx = mod.m)[,i]
    r$score <- exp(r$score)/(1 + exp(r$score))
    r$class <- ifelse(r$score > thrsh, 1, 0)
    c <- as.data.frame(t(logistic.eval(r)))
    e <- rbind(e, c)
  }
  
  return(e)
}

logistic.eval <- function(df){ 
  # First step is to find the TP, FP, TN, FN cases
  df$conf <- ifelse(df$actual == 1 & df$class == 1, 'TP',
                    ifelse(df$actual == 0 & df$class == 1, 'FP',
                           ifelse(df$actual == 0 & df$class == 0, 'TN', 'FN')))
  
  # Elements of the confusion matrix
  TP <- length(df[df$conf == 'TP', 'conf'])
  FP <- length(df[df$conf == 'FP', 'conf'])
  TN <- length(df[df$conf == 'TN', 'conf'])
  FN <- length(df[df$conf == 'FN', 'conf'])
  
  # Compute metrics

  accuracy <- (TP + TN)/(TP + TN + FP + FN)     
  precision <- TP/(TP + FP)
  recall <- TP/(TP + FN)

  c(TP = TP, FP = FP, TN = TN, FN = FN, accuracy = accuracy, precision = precision, recall = recall)
}

print.confusion <- function(TP, FP, FN, TN){
  
  out <- data.frame(Positive = c(TP, FP), Negative = c(FN, TN))
  row.names(out) <- c('TruePos', 'TrueNeg')
  print(out)  

}

## Compute the performance metrics of the model
performance <-  function(df, score, label){ 
  # Elements of the confusion matrix
  temp = table(df[, score], df[, label])
  TP = temp[2,2]
  FP = temp[2,1]
  TN = temp[1,1]
  FN = temp[1,2]  
  
  # Compute and print the metrics
  cat('\n')
  cat(paste('accuracy =', as.character((TP + TN)/(TP + TN + FP + FN)), '\n'))      
  cat(paste('precision =', as.character(signif(TP/(TP + FP)), digits = 2)), '\n')     
  cat(paste('recall =', as.character(TP/(TP + FN))))
  
  ## Return confusion matrix
  temp  
}