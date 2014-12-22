########################################################
## "Assessment" output in the MCMC
########################################################
chain1.dV = paste(directoryForCodes, "/mcmc.chain1.dV.rds", sep = "")
chain1.dW = paste(directoryForCodes, "/mcmc.chain1.dW.rds", sep = "")
chain2.dV = paste(directoryForCodes, "/mcmc.chain2.dV.rds", sep = "")
chain2.dW = paste(directoryForCodes, "/mcmc.chain2.dW.rds", sep = "")
chain3.dV = paste(directoryForCodes, "/mcmc.chain3.dV.rds", sep = "")
chain3.dW = paste(directoryForCodes, "/mcmc.chain3.dW.rds", sep = "")
chain4.dV = paste(directoryForCodes, "/mcmc.chain4.dV.rds", sep = "")
chain4.dW = paste(directoryForCodes, "/mcmc.chain4.dW.rds", sep = "")

mcmc.chain1.dV <- readRDS(chain1.dV)
mcmc.chain1.dW <- readRDS(chain1.dW)
mcmc.chain2.dV <- readRDS(chain2.dV)
mcmc.chain2.dW <- readRDS(chain2.dW)
mcmc.chain3.dV <- readRDS(chain3.dV)
mcmc.chain3.dW <- readRDS(chain3.dW)
mcmc.chain4.dV <- readRDS(chain4.dV)
mcmc.chain4.dW <- readRDS(chain4.dW)

# Analysis using ggmcmc package:
# Parameter dV
ggmcmc.chain1.dV <- data.frame(Iteration = 1:iterations, Parameter = rep("V", iterations), value = mcmc.chain1.dV, Chain = rep(as.integer(1), iterations))
ggmcmc.chain2.dV <- data.frame(Iteration = 1:iterations, Parameter = rep("V", iterations), value = mcmc.chain2.dV, Chain = rep(as.integer(2), iterations))
ggmcmc.chain3.dV <- data.frame(Iteration = 1:iterations, Parameter = rep("V", iterations), value = mcmc.chain3.dV, Chain = rep(as.integer(3), iterations))
ggmcmc.chain4.dV <- data.frame(Iteration = 1:iterations, Parameter = rep("V", iterations), value = mcmc.chain4.dV, Chain = rep(as.integer(4), iterations))

ggmcmc.chain.dV <- rbind(ggmcmc.chain1.dV, ggmcmc.chain2.dV, ggmcmc.chain3.dV, ggmcmc.chain4.dV)
colnames(ggmcmc.chain.dV) <- c("Iteration", "Parameter", "value", "Chain")

# attributes(ggmcmc.chain.dV)
attr(ggmcmc.chain.dV,"nChains") = 4
attr(ggmcmc.chain.dV,"nParameters") = 1
attr(ggmcmc.chain.dV,"nIterations") = iterations
attr(ggmcmc.chain.dV,"nBurnin") = 1000
attr(ggmcmc.chain.dV,"nThin") = 1
attr(ggmcmc.chain.dV,"description") = "Simulación"
attr(ggmcmc.chain.dV,"parallel") = FALSE

graf.mcmc.histogram.dV <- ggs_histogram(ggmcmc.chain.dV) + 
    my.theme +
    ggtitle("Histograma") +
    labs(x = "Valores") + 
    labs(y = "Cantidad")

graf.mcmc.running.means.dV <- ggs_running(ggmcmc.chain.dV) +
   my.theme2 +
   ggtitle("Medias Moviles del parametro V") +
   labs(x = "Iteracion") + 
   labs(y = "Media Movil")

graf.mcmc.density.dV <- ggs_density(ggmcmc.chain.dV) + 
   my.theme +
   ggtitle("Densidad Posterior") +
   labs(x = "Valores") + 
   labs(y = "Densidad")

graf.mcmc.traceplot.dV <- ggs_traceplot(ggmcmc.chain.dV) + 
   my.theme +
   scale_linetype_manual(values = c(rep("solid", 11), rep("dashed", 1))) +
   ggtitle("Valores de la Simulacion") +
   labs(x = "Iteracion") + 
   labs(y = "Valores")

graf.mcmc.autocorrelation.dV <- ggs_autocorrelation(ggmcmc.chain.dV) + 
   my.theme +
   ggtitle("Autocorrelaciones") +
   labs(x = "Retraso") + 
   labs(y = "Autocorrelacion")
   
# ggs_compare_partial(chain)

# Saving the plot
ggsave(graf.mcmc.density.dV, file="dV_Densities.pdf", path = directoryForImages) 
ggsave(graf.mcmc.running.means.dV, file="dV_RunningMeans.pdf", path = directoryForImages) 
ggsave(graf.mcmc.traceplot.dV, file="dV_Traceplot.pdf", path = directoryForImages) 
ggsave(graf.mcmc.autocorrelation.dV, file="dV_Autocorrelation.pdf", path = directoryForImages) 



# Parameter dW
ggmcmc.chain1.dW1 <- data.frame(Iteration = 1:iterations, Parameter = rep("W1", iterations), value = mcmc.chain1.dW[, 1], Chain = rep(as.integer(1), iterations))
ggmcmc.chain1.dW2 <- data.frame(Iteration = 1:iterations, Parameter = rep("W2", iterations), value = mcmc.chain1.dW[, 2], Chain = rep(as.integer(1), iterations))
ggmcmc.chain2.dW1 <- data.frame(Iteration = 1:iterations, Parameter = rep("W1", iterations), value = mcmc.chain2.dW[, 1], Chain = rep(as.integer(2), iterations))
ggmcmc.chain2.dW2 <- data.frame(Iteration = 1:iterations, Parameter = rep("W2", iterations), value = mcmc.chain2.dW[, 2], Chain = rep(as.integer(2), iterations))
ggmcmc.chain3.dW1 <- data.frame(Iteration = 1:iterations, Parameter = rep("W1", iterations), value = mcmc.chain3.dW[, 1], Chain = rep(as.integer(3), iterations))
ggmcmc.chain3.dW2 <- data.frame(Iteration = 1:iterations, Parameter = rep("W2", iterations), value = mcmc.chain3.dW[, 2], Chain = rep(as.integer(3), iterations))
ggmcmc.chain4.dW1 <- data.frame(Iteration = 1:iterations, Parameter = rep("W1", iterations), value = mcmc.chain4.dW[, 1], Chain = rep(as.integer(4), iterations))
ggmcmc.chain4.dW2 <- data.frame(Iteration = 1:iterations, Parameter = rep("W2", iterations), value = mcmc.chain4.dW[, 2], Chain = rep(as.integer(4), iterations))

ggmcmc.chain.dW <- rbind(ggmcmc.chain1.dW1, ggmcmc.chain1.dW2, ggmcmc.chain2.dW1, ggmcmc.chain2.dW2, ggmcmc.chain3.dW1, ggmcmc.chain3.dW2, ggmcmc.chain4.dW1, ggmcmc.chain4.dW2)
colnames(ggmcmc.chain.dW) <- c("Iteration", "Parameter", "value", "Chain")

# attributes(ggmcmc.chain.dW)
attr(ggmcmc.chain.dW,"nChains") = 4
attr(ggmcmc.chain.dW,"nParameters") = 2
attr(ggmcmc.chain.dW,"nIterations") = iterations
attr(ggmcmc.chain.dW,"nBurnin") = 1000
attr(ggmcmc.chain.dW,"nThin") = 1
attr(ggmcmc.chain.dW,"description") = "Simulación"
attr(ggmcmc.chain.dW,"parallel") = FALSE

graf.mcmc.histogram.dW <- ggs_histogram(ggmcmc.chain.dW) + 
   my.theme +
   ggtitle("Histogramas") +
   labs(x = "Valores") + 
   labs(y = "Cantidad")

graf.mcmc.running.means.dW <- ggs_running(ggmcmc.chain.dW) +
   my.theme2 +
   ggtitle("Medias Moviles del parametro W") +
   labs(x = "Iteracion") + 
   labs(y = "Media Movil")

graf.mcmc.density.dW <- ggs_density(ggmcmc.chain.dW) + 
   my.theme +
   ggtitle("Densidades Posteriores") +
   labs(x = "Valores") + 
   labs(y = "Densidad")

graf.mcmc.traceplot.dW <- ggs_traceplot(ggmcmc.chain.dW) + 
   my.theme +
   ggtitle("Valores de la Simulacion") +
   labs(x = "Iteracion") + 
   labs(y = "Valor")

graf.mcmc.autocorrelation.dW <- ggs_autocorrelation(ggmcmc.chain.dW) + 
   my.theme +
   ggtitle("Autocorrelaciones") +
   labs(x = "Autocorrelacion") + 
   labs(y = "Retraso")

# ggs_compare_partial(chain)
# scale_colour_brewer(palette="Set1")

# Saving the plot
ggsave(graf.mcmc.density.dW, file="dW_Densities.pdf", path = directoryForImages) 
ggsave(graf.mcmc.running.means.dW, file="dW_RunningMeans.pdf", path = directoryForImages) 
ggsave(graf.mcmc.traceplot.dW, file="dW_Traceplot.pdf", path = directoryForImages) 
ggsave(graf.mcmc.autocorrelation.dW, file="dW_Autocorrelation.pdf", path = directoryForImages) 

# closing device
dev.off()



