#### effectue les simulations pour une strate
set.seed(42)
N <- c(1:100)*10
M <- round(N*runif(100))
n <- round(N*runif(100,min=.20,max=.9))

une_simul <- function(N,M,n){
  simulation <- une_simulation_pop(N,M,n,seed = 42)
  saveRDS(simulation,paste("simulations/simulation",N,M,n,".RDS",sep="_"))
}



pour_appel <- function(x){
    simulations <-  une_simulation_pop(x[1],x[2],x[3],seed=42)
  list(Nh=x[1],Mh=x[2],nh=x[3],simulations = simulations)
}

donnees <- data.frame(N,M,n)
lignes <- as.list(as.data.frame(t(donnees)))

liste_simulations <- lapply(lignes, pour_appel)

saveRDS(liste_simulations,file="liste_simulations.RDS")


