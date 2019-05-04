# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Cmd + Shift + B'
#   Check Package:             'Cmd + Shift + E'
#   Test Package:              'Cmd + Shift + T'

check_prob <- function(prob) {
  if (prob <= 1 & prob >= 0){
    return(TRUE)
  }
  stop("invalid prob value")
}

check_trials <- function(trials) {
  if (trials >= 0){
    return(TRUE)
  }
  stop("invalid trials value")
}

check_success <- function(success, trials) {
  if (success >= 0 && success <= trials){
    return(TRUE)
  }
  stop("'invalid success value")
}

aux_mean = function(trials, prob) {
  return(prob*trials)
}

aux_variance = function(trials, prob) {
  return(trials*prob*(1-prob))
}

aux_mode = function(trials, prob) {
  return(as.integer(trials*prob + prob))
}

aux_skewness = function(trials, prob) {
  return((1-2*prob)/sqrt(trials*prob*(1-prob)))
}

aux_kurtosis = function(trials, prob) {
  return((1-6*prob*(1-prob))/(trials*prob*(1-prob)))
}

bin_choose = function(n, k) {
  if(check_trials(n)){
    if(!check_success(k, n)){
      stop("k cannot be greater than n")
    }
    return(factorial(n)/(factorial(k)*factorial(n-k)))
  }
}

bin_probability = function(success, trials, prob) {
  check_trials(trials)
  check_prob(prob)
  check_success(success, trials)
  return(bin_choose(trials, success)*(prob^success)*(1-prob)^(trials-success))
}

bin_distribution = function(trials, prob) {
  bin_frame = data.frame("success" = 0:trials, "probability" = bin_probability(0:trials, trials, prob))
  class(bin_frame) = c("binomial distribution", "data.frame")
  return(bin_frame)
}

plot.bindis = function(bindist) {
  plot(bindist)
}

bin_cumulative = function(trials, prob) {
  bin_frame = bin_distribution(trials, prob)
  cum = c()
  for (i in 0:trials) {
    cum = append(cum, sum(subset(bin_frame, success <= i)["probability"]))
  }
  bin_frame$cumulative = cum
  class(bin_frame) = c("bincum", "data.frame")
  return(bin_frame)
}

#halp
plot.bincum = function(bincum) {
  plot(bincum)
}

bin_variable = function(trials, prob) {
  check_trials(trials)
  check_prob(prob)
  binvar = list("trials" = trials, "prob" = prob)
  class(binvar) = "bin_var"
  '@export'
  print.binvar = function(binvar) {
    print("Binomial variable")
    print('- number of trials: ', binvar[trials])
    print('- prob of success : 0.3 ', prob)
  }
  summary.binvar = function(binvar) {
    attributes = list("trials" = binvar[trials],
                      "prob" = binvar[prob],
                      "mean" = aux_mean(binvar[trials], binvar[prob]),
                      "variance" = aux_variance(binvar[trials], binvar[prob]),
                      "mode" = aux_mode(binvar[trials], binvar[prob]),
                      "skewness" = aux_skewness(binvar[trials], binvar[prob]),
                      "kurtosis" = aux_kurtosis(binvar[trials], binvar[prob]))
    return(attributes)
  }
  print.summary.binvar = function(binvar) {
    print("Summary Binomial")
    print()
    print('Paramaters')
    print('- number of trials: ', binvar[trials])
    print('- prob of success : ', binvar[prob])
    print('Measures')
    print('- mean : ', binvar[mean])
    print('- variance: ', binvar[variance])
    print('- mode : ', binvar[mode])
    print('- skewness: ', binvar[skewness])
    print('- kurtosis: ', binvar[kurtosis])
  }
  return(binvar)
}

bin_mean = function(trials, prob) {
  return(prob*trials)
}

bin_variance = function(trials, prob) {
  return(trials*prob*(1-prob))
}

bin_mode = function(trials, prob) {
  return(as.integer(trials*prob + prob))
}

bin_skewness = function(trials, prob) {
  return((1-2*prob)/sqrt(trials*prob*(1-prob)))
}

bin_kurtosis = function(trials, prob) {
  return((1-6*prob*(1-prob))/(trials*prob*(1-prob)))
}

#Use cat for representations









