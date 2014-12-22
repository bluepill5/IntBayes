# Tarea 4:
# Alexandro Mayoral: alexandro.mayoral@gmail.com
## ----echo=FALSE----------------------------------------------------------
# NOTA: Por como se implmentaron las funciones se tiene que tener cuidado con los decimales
#       ya que al tener mucha precisión en 'q1' y 'q2' puede afectar el resultado de la 
#       gráfica
# Función para generar las gráficas de la beta posteriori
shadeBetaPost <- function(q1, q2, alpha, beta, add = FALSE, title) {
    cord.x1 <- c(0, seq(0, q1, 0.01), q1) 
    cord.x2 <- c(q2, seq(q2, 1.0, 0.01), 1.0) 
    cord.y1 <- c(0, dbeta(seq(0, q1, 0.01), alpha, beta), 0) 
    cord.y2 <- c(0, dbeta(seq(q2, 1.0, 0.01), alpha, beta), 0)
    curve(dbeta(x, alpha, beta), from = 0, 
          to = 0.8,  col = "#104E8B", lwd = 2, 
          main = paste(title), 
          xlab = "Valores", ylab = "f(x)",
          font.lab = "3", family = "mono", add = add)
    polygon(cord.x1, cord.y1, col = "#1E90FF", border = "#104E8B", lwd = 2)
    polygon(cord.x2, cord.y2, col = "#1E90FF", border = "#104E8B", lwd = 2)
}
# Función para generar las gráficas de la distribución a posteriori considerando una
# distribución a posteriori especificada
shadePostDen <- function(fn, q1, q2) {
    cord.x1 <- c(0, seq(0, q1, 0.01), q1) 
    cord.x2 <- c(q2, seq(q2, 0.5, 0.01), 0.5) 
    cord.y1 <- c(0, fn(seq(0, q1, 0.01)), 0) 
    cord.y2 <- c(0, fn(seq(q2, 0.5, 0.01)), 0)
    curve(fn(x), from = 0, 
          to = 0.5,  col = "#104E8B", lwd = 2, 
          main = paste("Distribución Posterior"), 
          xlab = "Valores", ylab = "f(x)",
          font.lab = "3", family = "mono")
    polygon(cord.x1, cord.y1, col = "#1E90FF", border = "#104E8B", lwd = 2)
    polygon(cord.x2, cord.y2, col = "#1E90FF", border = "#104E8B", lwd = 2)
}


## ------------------------------------------------------------------------
# Límite inferior
qbeta(0.025, 7, 10)
# Límite superior
qbeta(0.975, 7, 10)


## ------------------------------------------------------------------------
# Límite inferior
qbeta(0.025, 8, 13)
# Límite superior
qbeta(0.975, 8, 13)


## ----echo=FALSE, fig.align='center', fig.height=5, fig.width=5-----------
q1 <- signif(qbeta(0.025, 8, 13), digits = 2)
q2 <- signif(qbeta(0.975, 8, 13), digits = 2)
shadeBetaPost(q1, q2, 8, 13, title = "Distribución Posterior")
text(0.1, 3.3, paste("Beta(7, 10)"), 
     cex = 0.8, family = "mono")
q1 <- signif(qbeta(0.025, 7, 10), digits = 2)
q2 <- signif(qbeta(0.975, 7, 10), digits = 2)
shadeBetaPost(q1, q2, 7, 10, add = TRUE, title = "Distribución Posterior")
text(0.7, 2.0, paste("Beta(8, 13)"), 
     cex = 0.8, family = "mono")


## ------------------------------------------------------------------------
# Beta(7, 10)
abs(0.1975 -  0.6457)
# Beta(8, 13)
abs(0.1912 - 0.5922)


## ----fig.align='center', fig.height=5, fig.width=5-----------------------
# Definimos la función a priori
priorFunction <- function(phi) {
    for (e in 1:length(phi)) {
        if (phi[e] <= 0.2) {
            phi[e] <- phi[e]
        } else if (phi[e] < 0.3) {
            phi[e] <- 0.2
        } else if (phi[e] <= 0.5) {
            phi[e] <- 0.5 - phi[e]
        } else {
            phi[e] <- 0
        }
    }
    phi
}
# Gráficamos las distribución
curve(priorFunction, from = 0, to = 0.5, col = "#B22222", main = "Distribución A priori", 
      xlab = "Valores", ylab = "f(x)", font.lab = "3", family = "mono")


## ----fig.align='center', fig.height=5, fig.width=5-----------------------
# Como se realizan n = 20 pruebas independiente, y son observados y = 7 éxitos
n <- 20
y <- 7

# Separamos la parte de la constante de integración
funToIntegrate <- function (phi) {
    if (phi > 1 || phi < 0) { 
        0 
    } else { 
        priorFunction(phi) * dbinom(y, n, prob = phi) 
    }
}
# Obtenemos la distribucón a posteriori de pi|Y
posteriorFunction <- function(phi) {
    if (phi > 1 || phi < 0) {
        0
    } else {
        priorFunction(phi) * dbinom(y, n, prob = phi) / 
            integrate(funToIntegrate, lower = 0, upper = 1)$value
    }
}
# Entonces la función de densidad de la distribución a posteriori es:
curve(posteriorFunction, from = 0, to = 0.5, col = "#B22222", 
      main = "Distribución A posteriori", xlab = "Valores", ylab = "f(x)", 
      font.lab = "3", family = "mono")


## ------------------------------------------------------------------------
meanPost <- integrate(function (phi) phi * posteriorFunction(phi), 
                      lower = 0, upper = 1)$value
varPost <- integrate(function (phi) ((phi - meanPost)**2) * posteriorFunction(phi), 
                      lower = 0, upper = 1)$value
sdPost <- sqrt(varPost)
# Media a posteriori:
meanPost
# Desviación estándar a posteriori:
sdPost


## ----fig.align='center', fig.height=5, fig.width=5-----------------------
# Calculamos la función de distribución a posteriori
F <- function(x) {integrate(posteriorFunction, 0, x)$value}
F <- Vectorize(F)
# Calculamos la inversa de la función de distribución a posteriori
F.inv <- function(y){ uniroot(function(x){F(x) - y}, interval = c(0,0.5))$root }
F.inv <- Vectorize(F.inv)
# Aplicamos el métodos de la transformada inversa para obetener muestras de nuestra
# distribución a posteriori
X <- runif(1000)
Z <- F.inv(X)
qs <- quantile(Z, probs = c(0.025, 0.975))
shadePostDen(posteriorFunction, signif(qs[1], digits = 2), signif(qs[2], digits = 2))


## ------------------------------------------------------------------------
# Cuantiles
qs
# Longitud del intervalo
as.numeric(abs(qs[1] - qs[2]))


