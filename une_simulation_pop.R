#### fonctions qui fait des simulations de population, de tirages et de la variable m

une_simulation_pop <- function(N,M,n,seed=NULL,nb_simul = 1000){
  require(data.table)
  
  if (!is.null(seed)){set.seed(seed)}
  
  #construction de la pop
  pop <- 1:N
  D <- rep(0,N)
  D[1:M] <- 1
  p <- M/N
  
 U <- data.table(data.frame(id=pop,D=D))
 
 #fonction qui tire un échantillon et compte le nombre d'unités tombant dans le domaine
  
  m_h <- function(U,n,D = "D"){
    echantillon <- U[sample(.N,n)]
    return(sum(echantillon[,D]))
    
  }
 
  simulations <- do.call(c,lapply(as.list(1:nb_simul),FUN=function(x){m_h(U,n)}))
  
  return(simulations)
  
}
