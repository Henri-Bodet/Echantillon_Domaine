### fonctions distributions
### proba de m=0
prb0 <- function(N=,M=,n=){
  retour <- NA_real_
  if (!check_NMn(N,M,n)){
    stop(paste("données incohérentes","N=",N,"M=",M,"n=",n))
  }
  else{
  v <- (N-M-0:(n-1))/(N-0:(n-1))
  retour <- prod(v)
  return(retour)
  }
  
}

### vérifie que les valeurs de N,M et n sont crédibles
check_NMn <-function(N,M,n){
  plausibles <- (M <= N) & (n <= N) & (n > 0) 
  plausibles <- plausibles & (is.integer(M) & is.integer(N) & is.integer(n))
  }