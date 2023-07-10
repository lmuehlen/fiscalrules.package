make_formula_deltamethod<-function(n,n_lags){

  make_pattern<- function(n,n_lags) {
    # Construct the pattern
    n<-n-1
    x <- paste0("x", n+3)
    pattern <- paste0("~", x, "*x1")
    if (n > 1) {
      for (j in 2:n) {
        if(j<n_lags+1){
          pattern <- paste0(pattern, "+x", (n+n_lags-j+1), "*x", j)
        }
      }
    }
    pattern <- paste0(pattern, "+x",n_lags+1)

    # Convert the pattern to a formula and store it
    pattern <- as.formula(pattern)

    # Return the pattern
    return(pattern)
  }

  a<-lapply(1:(3+n-1), function(t){paste0("~x",t)%>%as.formula()})
  b<-make_pattern(n,n_lags)

  return(c(a,b))

}
