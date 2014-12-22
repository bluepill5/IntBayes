#########################################################################################
#   
#  modelRJAGS.R 
#   
#  Autor: Alexandro Mayoral <https://github.com/bluepill5> 
#  Creado: Diciembre 04, 2014 
#  Última modificación: Diciembre 04, 2014
# 
######################################################################################### 
setwd("D:/__ESP__/4.IntBayesiana/__project__/__code__")
library(rjags)
library(R2jags)
library(R2WinBUGS)

r_P <- as.numeric(equity.retornos.diarios)
r_M <- as.numeric(market.retornos.diarios)


RegDLM = "
model{
#### Data Model
for(i in 1:n){
r_P[i] ~ dnorm(alpha[i] + beta[i] * r_M[i], 1/v)
}

#### Process Model
for(i in 2:n){
alpha[i] ~ dnorm(alpha[i-1], 1/w_1)
beta[i] ~ dnorm(beta[i-1], 1/w_2)
}

#### Priors
alpha[1] ~ dnorm(m_alpha, v_alpha)
beta[1] ~ dnorm(m_beta, v_beta)
v ~ dgamma(a_v, r_v)
w_1 ~ dgamma(a_w1, r_w1)
w_2 ~ dgamma(a_w2, r_w2)
}
"

data <- list(r_P = r_P,
             r_M = r_M,
             n = length(r_P),
             m_alpha = 0.001,
             v_alpha = 0.01,
             m_beta = 1,
             v_beta = 0.05,
             a_v = 1,
             r_v = 1,
             a_w1 = 1,
             r_w1 = 1,
             a_w2 = 1,
             r_w2 = 1)

nchain = 4
init <- list()
for(i in 1:nchain){
    e <- runif(1, 0.00001, 0.00005)
    init[[i]] <- list(v = 0.00393,
                      w_1 = 0.00001,
                      w_2 = 0.005)
}

fit <- jags(data = data, inits = init, 
            parameters.to.save = c("v","w_1", "w_2"), 
            n.chains = 4, n.iter = 15000, n.burnin = 5000, 
            model.file = textConnection(RegDLM))
# Diagnóstico
# Convirtiendo el modelo en un objeto MCMC
fit.mcmc <- as.mcmc(fit)
summary(fit.mcmc)
# Utilizando los comandos de coda
xyplot(fit.mcmc)
xyplot(fit.mcmc,layout = c(2,6), aspect = "fill")
# Graficas de  las Densidades
densityplot(fit.mcmc)
densityplot(fit.mcmc, layout = c(2,6), aspect = "fill")


mcmc.chain1.dV <- as.vector(as.list(fit.mcmc)[[1]][,2])
mcmc.chain1.dW <- data.frame(W.1 = as.vector(as.list(fit.mcmc)[[1]][,3]),
                             W.2 = as.vector(as.list(fit.mcmc)[[1]][,4]))

mcmc.chain2.dV <- as.vector(as.list(fit.mcmc)[[2]][,2])
mcmc.chain2.dW <- data.frame(W.1 = as.vector(as.list(fit.mcmc)[[2]][,3]),
                             W.2 = as.vector(as.list(fit.mcmc)[[2]][,4]))


mcmc.chain3.dV <- as.vector(as.list(fit.mcmc)[[3]][,2])
mcmc.chain3.dW <- data.frame(W.1 = as.vector(as.list(fit.mcmc)[[3]][,3]),
                             W.2 = as.vector(as.list(fit.mcmc)[[3]][,4]))


mcmc.chain4.dV <- as.vector(as.list(fit.mcmc)[[4]][,2])
mcmc.chain4.dW <- data.frame(W.1 = as.vector(as.list(fit.mcmc)[[4]][,3]),
                             W.2 = as.vector(as.list(fit.mcmc)[[4]][,4]))

# Salvamos las cadenas
saveRDS(mcmc.chain1.dV, "mcmc.chain1.dV.rds")
saveRDS(mcmc.chain1.dW, "mcmc.chain1.dW.rds")
saveRDS(mcmc.chain2.dV, "mcmc.chain2.dV.rds")
saveRDS(mcmc.chain2.dW, "mcmc.chain2.dW.rds")
saveRDS(mcmc.chain3.dV, "mcmc.chain3.dV.rds")
saveRDS(mcmc.chain3.dW, "mcmc.chain3.dW.rds")
saveRDS(mcmc.chain4.dV, "mcmc.chain4.dV.rds")
saveRDS(mcmc.chain4.dW, "mcmc.chain4.dW.rds")





