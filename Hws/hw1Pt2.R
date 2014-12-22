# Tarea 1 Pt. 2:
# Alexandro Mayoral: alexandro.mayoral@gmail.com
dirWD <- readline("Dame la ruta de tu directorio de trabajo: ")
setwd(dirWD)

# Escriba una función en R que implemente el método de cuadrados medios
# (Midle-square method)
#
genCM <- function(n, seed) {
    # Genera un vector 'aleatorio' de una distribución Uniforme(0, 1) usando
    # el Método de Cuadrados Medios.
    #
    # Args:
    #   n: El tamaño esperado del vector de valores, ya que en caso de que 
    #      durante el algoritmo se generé un número que ya se había generado
    #      antes se regresará el vector que se tenga en ese momento.
    #   seed: La semilla con la que iniciará el algoritmo.
    #
    # Returns:
    #   Un vector de tamaño n o menor de valores aletorios Uniforme(0, 1).
    #
    d <- 4 # Se extraerán números de d-dígitos
    randNums <- c()
    repeat {
        preSeed <- toString(seed**2)
        lo <- ((nchar(preSeed) - d) %/% 2) + 1
        hi <- lo + d - 1
        seed <- as.integer(substr(preSeed, lo, hi))
        # Verifica si ya se tiene el vector o ya se repitió algún valor
        if (((seed / 10**d) %in% randNums) || n == 0)
            break
        randNums <- c(randNums, seed / 10**d)
        n <- n - 1 
    }
    randNums
}

rn <- genCM(5, 2310)


# Escriba una función en R que implemente un generador congruencial de 
# números aleatorios con módulo 2**31 − 1 y a = 62089911. Use esta función 
# para generar 10000 números con distribución Uniforme(0, 1). Haga una 
# gráfica del resultado contra el índice, un histograma y una gráfica de 
# U_2n−1 vs. U_2n. Comente sus resultados.
#
genCong <- function(n, seed, a = 62089911, c, m = 2**31 − 1) {
    # Genera un vector 'aleatorio' de una distribución Uniforme(0, 1)
    # usando el Método Congruencial:
    #    U_i = (a * U_(i - 1)) mod m
    #
    # Args:
    #   seed: La semilla con la que iniciará el algoritmo.
    #   n: El tamaño del vector de valores aleatorios.
    #   a, c, m: Las constantes que definen al generador.
    #
    # Returns:
    #   Un vector de tamaño n de valores aletorios Uniforme(0, 1).
    #
    u_old <- seed
    randNums <- c()
    while (n > 0) {
        u_new <- ((a * u_old) + c) %% m
        randNums <- c(randNums, u_new / m)
        u_old <- u_new
        n <- n - 1
    }
    randNums
}

rn <- genCong(1000,  seed = 7, c = 0)
U_odd <- rn[seq(2, length(rn), 2)]
U_even <- rn[seq(1, length(rn), 2)]

# Gráfica
plot(rn, main = "Simulación usando el Método Congruencial",
     xlab = "Índice", ylab = "Valor", col="#1E90FF", 
     font.lab = "3", family = "mono")
# Histograma
hist(rn, main = "Hist. de la simulación usando\nel Método Congruencial", 
     xlab = "Valores", ylab = "Frecuencia", col = "#1E90FF",
     border = "#FFFFFF", font.lab = "3", family = "mono")
# Plot U_2n−1 vs. U_2n
plot(U_odd, U_even, main = "Gráfica de Valores Impares vs. Pares",
         xlab = "Impar", ylab = "Par", col="#1E90FF", 
     font.lab = "3", family = "mono")


# Haga un programa que permita generar n variables aleatorias con la
# siguiente distribución: cada variable puede tomar valores 10, 20, 30, 40, 
# 50 con probabilidades respectivas 0.05, 0.1, 0.15, 0.3, 0.4.
#
genDiscInv <- function(n, probs, support) {
    # Genera un vector 'aleatorio' de una distribución discreta objetivo 
    # usando el Método de Transformada Inversa:
    #    X = F**-1(U)
    # 
    # Args:
    #   n: El tamaño del vector de valores aleatorios.
    #   probs: El vector de la probabilidades de la variable aleatoria,
    #          las probabilidades tienen que estar ordenadas.
    #   support: El vector de los valores que puede tomar la variable
    #            aleatoria, los valores tienen que estar ordenados.
    #
    # Returns:
    #   Un vector de tamaño n de valores de una distribución aleatoria
    #   discreta objetivo.
    #
    psum <- cumsum(probs)
    randNums <- c()
    u <- runif(n)
    for (i in 1:n)
        randNums <- c(randNums, min(support[u[i] < psum]))
    randNums
}

probs <- c(0.05, 0.1, 0.15, 0.3, 0.4)
support <- c(0, 20, 30, 40, 50)
rn <- genDiscInv(10000, probs, support)
# Histograma
hist(rn, main = "Hist. de la simulación usando\nla Transformada Inversa", 
     xlab = "Valores", ylab = "Frecuencia", col = "#1E90FF",
     border = "#FFFFFF", font.lab = "3", family = "mono")


# Realice una programa en R, para generar n de una distribución Gamma con 
# parámetros alpha y beta.
#
genGamma <- function(n, alpha, beta) {
    # Genera un vector 'aleatorio' de una distribución Gamma con parámetros 
    # alpha y beta, usando el Método de Transformada Inversa:
    #    X = F**-1(U)
    # 
    # Args:
    #   n: El tamaño del vector de valores aleatorios.
    #   alpha: El valor del parámetro alpha de la función gamma.
    #   beta: El valor del parámetro beta de la función gamma.
    #
    # Returns:
    #   Un vector de tamaño n de valores de una distribución gamma con
    #   parámetros alpha y beta.
    #
    
    # La idea es generar un y ~ exp(beta) alpha veces, ya que se sabe que
    # sum_to_alpha(y) ~ gamma(alpha, beta)
    randNums <- c()
    for (i in 1:n) {
        u <- runif(alpha)
        randNums <- c(randNums, sum(-(1/beta) * log(1 - u)))
    }
    randNums    
}
alpha <- 2
beta <- 2
rn <- genGamma(1000, alpha, beta)
# Histograma
hist(rn, main = "Hist. de la simulación  de una Gamma(2, 2)
     usando la Transformada Inversa", ylim = c(0, 1.0), prob = TRUE,
     xlab = "Valores", ylab = "Frecuencia", col = "#1E90FF",
     border = "#FFFFFF", font.lab = "3", family = "mono")
curve(dgamma(x, shape = alpha, rate = beta), from = 0, 
      to = 10 * (alpha / beta**2), col = "#B22222", lwd = 2, add = TRUE)


# Use el método de rechazo comparando con una distribución uniforme para 
# generar variables que tengan una densidad triangular en [−1, 1]:
#
#             x + 1 para −1 < x <= 0,
#    f (x) =
#            1 − x para 0 < x <= 1
#
genTriang <- function(n) {
    # Genera un vector 'aleatorio' de una distribución Triangular usando el
    # el Método de Aceptación y Rechazo, tomando a g = unif(-1, 1):
    #
    #    U <= f(Y) / M*g(Y)
    # 
    # Args:
    #   n: El tamaño del vector de valores aleatorios.
    #
    # Returns:
    #   Un vector de tamaño n de valores de una distribución triangular
    #
    
    # Observe que el sup(f(y) / g(y)) = 2
    M <- 2
    g <- function(x) { dunif(x, min = -1, max = 1) }
    f <- function(x) {
        # Implementation of triangular function in [-1, 1]
        for (i in 1:length(x)) {
            if (x[i] > -1 && x[i] <= 0) {
                x[i] <- x[i] + 1
            } else if (x[i] > 0 && x[i] <= 1) {
                x[i] <- 1 - x[i]
            } else {
                x[i] <- 0
            }
        }
        x            
    }
    randNums <- c()
    while(n > 0) {
        u <- M * runif(1)
        y <- runif(1, min = -1, max = 1)
        if (u <= (f(y) / g(y))) {
            randNums <- c(randNums, y)
            n <- n - 1
        }   
    }
    randNums    
}

rn <- genTriang(1000)
# Histograma
hist(rn, main = "Hist. de la simulación  de una distribución
     Traingular usando el algoritmo de 
     Aceptación-Rechazo", ylim = c(0, 1.0), 
     prob = TRUE, xlab = "Valores", ylab = "Frecuencia", col = "#1E90FF",
     border = "#FFFFFF", font.lab = "3", family = "mono")
curve(f(x), from = -1, to = 1, col = "#B22222", lwd = 2, add = TRUE)


# Calcule P(X <= 3), donde X ~ N(0, 2), es decir:
#
#     f_x = (1 / sqrt(4 * pi)) * e**(-x**2 / 4)
#
cdfNorm <- function(n, x, mu, sd) {
    # Genera una aproximación de la función de distribución acumulada
    # usando el Método de integración de Monte Carlo:
    #
    #    E_f[h(X)] = integral(h(x)*f(x))_soporte, entonces:
    #
    #    est(E_f[h(X)]) = 1/n * sum_to_n(h(x_i))
    # 
    # Args:
    #   n: El tamaño del vector a usar para la estimación
    #   x: El valor en el que deseamos valuar la P(X <= x)
    #   mu: La media de la distribución normal
    #   sd: La desviación estándar de la distribución normal
    #
    # Returns:
    #   Una estimación de la función de disitribuciónvaluada en x
    # 
    h <- rnorm(n, mean = mu, sd = sd)
    est <- mean(h < x)
    est
}

cdfNorm(100000, 3, mu = 0, sd = sqrt(2))
pnorm(3, mean = 0, sd = sqrt(2))


# Calcule:
#     integral(g(x) from = 0, to = inf)
# para cualquier g usando variables de la distribución uniforme en (0,1)
#
# Nota: La función que sigue usa la idea del método de resampling, la 
# función que continua es la usa el método normal de monte carlo
#
estIntGRee <- function(n, g) {
    # Genera una aproximación de:
    #
    #    integral(g(x) from = 0, to = inf)
    #
    # usando el Método de integración de Monte Carlo:
    #
    #    E_f[h(X)] = integral(h(x)*f(x))_soporte, entonces:
    #
    #    est(E_f[h(X)]) = 1/n * sum_to_n(h(x_i))
    #
    # En este caso la idea es incluir un uno en la integral de la forma
    # f(x) / f(x) pero tal que f(x) se una función de densidad con el
    # soporte adecuado, y se haga la estimación como:
    #
    #    est(E_f[h(X)]) = 1/n * sum_to_n(h(x_i) / f(x_i))
    # 
    # Args:
    #   n: El tamaño del vector a usar para la estimación
    #   g: Función objetivo, la cual se quiere integrar
    #
    # Returns:
    #   Una estimación de:
    #   integral(g(x) from = 0, to = inf)
    # 
    xs <- rexp(n, rate = 1)
    f <- dexp(xs)
    h <- g(xs) / f
    est <- mean(h)
    esterr <- sqrt(sum((h - est)**2) / (n - 1)) / sqrt(n)
    list(estimador = est, error = esterr)
}
# A continuación comparamos con la función integrate de R
g <- function(n) { 1 / (1 + n**2)} # pi/2
estIntGRee(100000, g)
integrate(g, lower = 0, upper = Inf)

g <- function(n) { 1 / sqrt(exp(1)**(n))} # 2
estIntGRee(100000, g)
integrate(g, lower = 0, upper = Inf)

g <- function(n) { dnorm(n, mean = 0, sd = 1)} # 0.5
estIntGRee(100000, g)
integrate(g, lower = 0, upper = Inf)

g <- function(n) { exp(1)**(-n) / sqrt(n)} # sqrt(pi)
estIntGRee(100000, g)
integrate(g, lower = 0, upper = Inf)


estIntGMC <- function(n, g) {
    # Genera una aproximación de:
    #
    #    integral(g(x) from = 0, to = inf)
    #
    # usando el Método de integración de Monte Carlo:
    #
    #    E_f[h(X)] = integral(h(x)*f(x))_soporte, entonces:
    #
    #    est(E_f[h(X)]) = 1/n * sum_to_n(h(x_i))
    #
    # En este caso la idea es incluir un uno en la integral de la forma
    # f(x) / f(x) pero tal que f(x) se una función de densidad con el
    # soporte adecuado, y se haga la estimación como:
    #
    #    est(E_f[h(X)]) = 1/n * sum_to_n(h(x_i) / f(x_i))
    # 
    # Args:
    #   n: El tamaño del vector a usar para la estimación
    #   g: Función objetivo, la cual se quiere integrar
    #
    # Returns:
    #   Una estimación de:
    #   integral(g(x) from = 0, to = inf)
    # 
    u <- runif(n)
    f <- 1 / (1 - u)
    h <- g(-log(1 - u)) * f
    est <- mean(h)
    esterr <- sqrt(sum((h - est)**2) / (n - 1)) / sqrt(n)
    list(estimador = est, error = esterr)
}
# A continuación comparamos con la función integrate de R
g <- function(n) { 1 / (1 + n**2)} # pi/2
estIntGMC (1000000, g)
integrate(g, lower = 0, upper = Inf)

g <- function(n) { 1 / sqrt(exp(1)**(n))} # 2
estIntGMC (1000000, g)
integrate(g, lower = 0, upper = Inf)

g <- function(n) { dnorm(n, mean = 0, sd = 1)} # 0.5
estIntGMC (1000000, g)
integrate(g, lower = 0, upper = Inf)

g <- function(n) { exp(1)**(-n) / sqrt(n)} # sqrt(pi)
estIntGMC (1000000, g)
integrate(g, lower = 0, upper = Inf)


# Encuentre un valor aproximado para la integral:
#     integral(sqrt(x + sqrt(x)), from = 0, to = 1)
# Calcule también el valor estimado de la varianza. El valor
# exacto de la integral es 1.04530138
#
estSqrts <- function(n, from, to) {
    # Genera una aproximación de la función de distribución acumulada
    # usando el Método de integración de Monte Carlo:
    #
    #    E_f[h(X)] = integral(h(x)*f(x))_soporte, entonces:
    #
    #    est(E_f[h(X)]) = 1/n * sum_to_n(h(x_i))
    # 
    # Args:
    #   n: El tamaño del vector a usar para la estimación
    #   from: Límite inferior de la integral
    #   to: Límite superior de la integral
    #
    # Returns:
    #   Una estimación de:
    #   integral(sqrt(x + sqrt(x)), from = from, to = to)
    # 
    u <- runif(n, min = from, max = to)
    h <- sqrt(u + sqrt(u))
    est <- (to - from) * mean(h)
    esterr <- sqrt(sum((h - est)**2) / (n - 1)) / sqrt(n)
    list(estimador = est, error = esterr)
}
# Valor exacto de la integral: 1.045301308
estSqrts(1000000, from = 0, to = 1)

