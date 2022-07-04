N <- 1000
n <- 80
M <- 6

pop <- 1:N
D <- rep(0,N)
D[1:M] <- 1
p <- M/N

table(D)
library(data.table)
U <- data.table(data.frame(id=pop,D=D))



m_h <- function(U,n,D = "D"){
  echantillon <- U[sample(.N,n)]
 return(sum(echantillon[,D]))

}

nb_simul <- 10000
set.seed(42)

simulations <- do.call(c,lapply(as.list(1:nb_simul),FUN=function(x){m_h(U,n)}))

estims_simul <- c(E=mean(simulations),V = var(simulations))

formules <- c(E = M/N*n,V=(N-n)/(N-1)*n*p*(1-p))

rbind(simulations = estims_simul,formules=formules)

fivenum(simulations)
hist(simulations)
IC <-( (estims_simul[1] - 1.6*sqrt(estims_simul[2])) < simulations) &
  ( (estims_simul[1]  + 1.6*sqrt(estims_simul[2]) > simulations)  ) 
table(IC)
mean(IC)
estims_simul[1] + 2*sqrt(estims_simul[2])

saveRDS(simulations,file=paste0("simulations_",N,"_",M,"_",n,".RDS"))
