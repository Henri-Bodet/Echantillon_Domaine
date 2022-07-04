### fonctions distributions
### proba de m=0
prb0 <- function(N=NULL,M=NULL,n=NULL){
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
 
  return(plausibles)
}
