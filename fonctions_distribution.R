### fonctions distributions
### proba de m=0
prb0 <- function(N=NULL,M=NULL,n=NULL){
  retour <- NA_real_
  if (!check_NMn(N,M,n)){
    stop(paste("données incohérentes","N=",N,"M=",M,"n=",n))
  }
  else{1
    if(support(N=N,M=M,n=n)[1] > 0){
      retour <- 0
    }else{
      v <- (N-M-0:(n-1))/(N-0:(n-1))
    retour <- prod(v)
    }
  return(retour)
  }
}
### proba de m=n
prbn <- function(N=NULL,M=NULL,n=NULL){
  retour <- NA_real_
  if (!check_NMn(N,M,n)){
    stop(paste("données incohérentes","N=",N,"M=",M,"n=",n))
  }
  else{1
    if(support(N=N,M=M,n=n)[1] > 0){
      retour <- 0
    }else{
      v <- (N-0:(n-1))/(N-0:(n-1))
      retour <- prod(v)
    }
    return(retour)
  }
}
#support de la loi de m
#renvoie le support
support <-  function(N=NULL,M=NULL,n=NULL){

  if (!check_NMn(N,M,n)){
    stop(paste("données incohérentes","N=",N,"M=",M,"n=",n))
  }
  else{
    binf <- max(0, n - N + M)
    bsup <- min(n,M)
    return(binf:bsup)
  }
}
## proba de m=x si 0<x<n

prbx <- function(x,N=NULL,M=NULL,n=NULL){retour <- NA_real_
if (!check_NMn(N,M,n)){
  stop(paste("données incohérentes","N=",N,"M=",M,"n=",n))
}
else{
  if(support(N=N,M=M,n=n)[1] > 0){
    retour <- 0
  }else{
    v1 <- prod((N-M-0:(n-x-1))/(N-0:(n-x-1)))
    k<- 0:(x-1)
    v2 <- prod((M-k)*(n-k)/k)
     j <- (n-x):(n-1)
     v3 <- prod(N-j)
    retour <- v2 * v1 / (x*v3)
  }
  return(retour)
 
}}

#fonction qui renvoie TRUE si l'argument est un entier ou un réel à valeur entière
# si l'argument est un vecteur : renvoie vrai si cela l'est pour tous les arguements
is.integer.value <- function(x){
  if (length(x)==1){
  result <- is.integer(x)
  if (!result){result <- ( x == trunc(x)) }
  return(result)}
  else{
    result <- mapply(x,FUN=is.integer.value)
    return(all(result))
  }
}

### vérifie que les valeurs de N,M et n sont crédibles
check_NMn <-function(N,M,n){
  plausibles <- (M <= N) & (n <= N) & (n > 0) 
 
  plausibles <- plausibles & is.integer.value(c(N,M,n))
  
  if (any(is.na(c(N,M,n)))){plausibles <- FALSE}
 
  return(plausibles)
}
