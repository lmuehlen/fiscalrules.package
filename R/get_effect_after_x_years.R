get_effect_after_x_years <- function(shortrun, dyn_effect, years, vcov,report="both") {

  # shortrun is the effect of the coefficient in question
  # dyn_effect is a vector including the coefficients of the lagged dependent variables
  # years defines the year (effect after t+years)
  # vcov is the covariance matrix for the shortrun effect and the dyn_effects
  #report is either both for se and effect, or effect, or se

  n_lags <- length(dyn_effect)
  limit<-years+1
  # Compute effect
  effect <- numeric(limit)
  effect[1] <- shortrun
  for(i in 2:limit) {
    if(i <= n_lags) {
      effect[i] <- effect[1] + sum(sapply(1:(i-1), function(j) effect[j]*dyn_effect[i-j]))
    } else {
      effect[i] <- effect[1] + sum(sapply(1:n_lags, function(j) effect[i-j]*dyn_effect[j]))
    }
  }

  #Compute Standard error
  vcov_m<-list()
  vcov_m[[1]]<-vcov
  for(i in 2:(n_lags+2)) {
    vcov_m[[i]] <- deltamethod(make_formula_deltamethod(i,n_lags),
                               c(dyn_effect, effect[1:(i-1)]),
                               vcov_m[[i-1]], ses = FALSE)
  }
  for(i in (n_lags+3):limit) {
    vcov_m[[i]] <- deltamethod(make_formula_deltamethod(n_lags+2,n_lags),
                               c(dyn_effect, effect[c(1,(i-n_lags):(i-1))]),
                               vcov_m[[i-1]][-(n_lags+2),-(n_lags+2)], ses=FALSE)
  }


  # Compute effect and se
  effect_longrun <- effect[limit]
  se_longrun <- sqrt(diag(vcov_m[[limit]]))
  se_longrun<-se_longrun[length(se_longrun)]

  effect_and_se <- c(effect_longrun, se_longrun)
  names(effect_and_se) <- c("effect", "standard error")
  effect_and_se

  if(report=="effect"){
    return(effect_longrun)
  }else if(report=="se"){
    return(se_longrun)
  }else{
    return(effect_and_se)
  }

}
