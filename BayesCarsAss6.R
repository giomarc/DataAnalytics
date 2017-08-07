# get data
read.auto = function(file = 'Automobile price data _Raw_.csv'){
  ## Read the csv file
  auto.price <- read.csv(file, header = TRUE, 
                         stringsAsFactors = FALSE)
  
  ## Coerce some character columns to numeric
  numcols <- c('price', 'bore', 'stroke', 'horsepower', 'peak.rpm')
  auto.price[, numcols] <- lapply(auto.price[, numcols], as.numeric)
  
  ## Remove cases or rows with missing values. In this case we keep the 
  ## rows which do not have nas. 
  auto.price[complete.cases(auto.price), ]
}
auto.price = read.auto()

auto.price$log.price <- log(auto.price$price) # add log price

#Bayes Helper functions

posterior = function(prior, like){
  post = prior * like  # Compute the product of the probabilities
  post / sum(post) # Normalize and return
}

plot.post = function(prior, like, post, x, name = 'test'){
  maxy = max(c(prior, like, post))
  plot(x, like, , lty = 1, ylim = c(0.0, maxy), 
       ylab = 'Density', xlab = 'Parameter value',
       main = paste(name, '- Density of prior, likelihood, posterior'),
       lwd = 2, col = 'green')
  lines(x, prior, lty = 2, lwd = 2, col = 'blue')    
  lines(x, post, lty = 1, lwd = 2, col = 'red')
  legend('topright', c('likelihood', 'prior', 'posterior'), 
         lty=1, col=c('green', 'blue', 'red'), bty='n', cex=1.0)
  
  cat(name, ':\n',
      'Maximum of prior density =', round(x[which.max(prior)], 3), '\n',
      'Maximum likelihood =', round(x[which.max(like)], 3), '\n',
      'MAP =', round(x[which.max(post)], 3))
}

comp.like = function(p, x, name = 'a'){
  l = rep(0, length = length(p))
  sigmaSqr = sd(x)^2
  xBar = mean(x)
  cat(name, ' Mean =', xBar, 'Standard deviation =', sqrt(sigmaSqr), '\n')
  n = length(x)
  #    l = sapply(p, function(u) dnorm(u, mean = xBar, sd = sigmaSqr))
  l = sapply(p, function(u) exp(- n* (xBar - u)^2 / (2 * sigmaSqr)))
  l / sum(l) # Normalize and return
}

plot.ci = function(p, post, nSamps, qs, name = "test"){
  ## This function computes a credible interval using an assumption
  ## of symetry in the bulk of the distribution to keep the 
  ## calculation simple. 
  ## Compute a large sample by resampling with replacement
  samps = sample(p, size = nSamps, replace = TRUE, prob = post)
  ci = quantile(samps, probs = qs) # compute the quantiles
  
  ## Plot the density with the credible interval
  interval = qs[2] - qs[1]
  title = paste(name, ' - Posterior density with', interval, 'credible interval')
  plot(p, post, , typ = 'l', ylab = 'Density', xlab = 'Parameter value',
       main = title, lwd = 2, col = 'blue')
  abline(v = ci[1], col = 'red', lty = 2, lwd = 2)
  abline(v = ci[2], col = 'red', lty = 2, lwd = 2)
  cat('The', interval, 'Credible interval is', 
      round(ci[1], 2), 'to', round(ci[2], 2), 'for', name)
}

bayes.pairs <- function(a, b, p, pp,   nSamps = 100000, qs = c(0.025, 0.975), name = c('a', 'b')){
  
  # sort log prices
  a <- sort(a, decreasing = FALSE)
  b <- sort(b, decreasing = FALSE)
  
  # calculate liklihood and posterior
  like.a = comp.like(p, a, name[1])
  post.a = posterior(pp, like.a)
  
  like.b = comp.like(p, b, name[2])
  post.b = posterior(pp, like.b)
  
  # plot ci 
  nSamps = 100000
  qs = c(0.025, 0.975)
  plot.ci(p, post.a, nSamps, qs, name[1])
  plot.ci(p, post.b, nSamps, qs, name[2])
  
  plot.post(pp, like.a, post.a, p, name[1])
  plot.post(pp, like.b, post.b, p, name[2])
  
}

#Setup prior

N = 20000 
p = seq(min(auto.price$log.price), max(auto.price$log.price), length = N) 
pp = dnorm(p, mean = mean(auto.price$log.price), sd = sd(auto.price$log.price)) ## start with a fairly broad prior
pp = pp / sum(pp)

#### Aspiration Std vs Turbo
require(dplyr)
auto.turbo <- filter(auto.price, aspiration == "turbo")
auto.standard <- filter(auto.price, aspiration == "std")

bayes.pairs(auto.turbo$log.price, auto.standard$log.price, p = p, pp = pp, name = c('Turbo', 'Standard'))

# sort log prices
auto.turbo$log.price <- sort(auto.turbo$log.price, decreasing = FALSE)
auto.standard$log.price <- sort(auto.standard$log.price, decreasing = FALSE)

# calculate liklihood and posterior
like.auto.turbo = comp.like(p, auto.turbo$log.price)
post.auto.turbo = posterior(pp, like.auto.turbo)

like.auto.standard = comp.like(p, auto.standard$log.price)
post.auto.standard = posterior(pp, like.auto.standard)

# plot ci 
nSamps = 100000
qs = c(0.025, 0.975)
plot.ci(p, post.auto.turbo, nSamps, qs)
plot.ci(p, post.auto.standard, nSamps, qs)

plot.post(pp, like.auto.turbo, post.auto.turbo, p)
plot.post(pp, like.auto.standard, post.auto.standard, p)

#### Aspiration gas vs diesel
auto.gas <- filter(auto.price, fuel.type == "gas")
auto.diesel <- filter(auto.price, fuel.type == "diesel")

bayes.pairs(auto.gas$log.price, auto.diesel$log.price, p =p, pp = pp, name = c('Gas', "Diesel"))


#### Body Style 

auto.conv <- filter(auto.price, body.style == "convertible")
auto.hardtop <- filter(auto.price, body.style == "hardtop")
auto.hatchback <- filter(auto.price, body.style == "hatchback")
auto.sedan <- filter(auto.price, body.style == "sedan")
auto.wagon <- filter(auto.price, body.style == "wagon")

# conv - hardtop
bayes.pairs(auto.conv$log.price, auto.hardtop$log.price, p =p, pp = pp)

# conv - hatchback
bayes.pairs(auto.conv$log.price, auto.hatchback$log.price, p =p, pp = pp)

# conv - sedan
bayes.pairs(auto.conv$log.price, auto.sedan$log.price, p =p, pp = pp)

# conv - wagon
bayes.pairs(auto.conv$log.price, auto.wagon$log.price, p =p, pp = pp)

# hardtop - hatchback
bayes.pairs(auto.hardtop$log.price, auto.hatchback$log.price, p =p, pp = pp)

# hardtop - sedan
bayes.pairs(auto.hardtop$log.price, auto.sedan$log.price, p =p, pp = pp)

# hardtop - wagon
bayes.pairs(auto.hardtop$log.price, auto.wagon$log.price, p =p, pp = pp)

# hatchback - sedan
bayes.pairs(auto.hatchback$log.price, auto.sedan$log.price, p =p, pp = pp)

# hatchback - wagon
bayes.pairs(auto.hatchback$log.price, auto.wagon$log.price, p =p, pp = pp)

# hatchback - sedan
bayes.pairs(auto.hatchback$log.price, auto.sedan$log.price, p =p, pp = pp)
