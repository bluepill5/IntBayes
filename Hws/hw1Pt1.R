# Tarea 1 Pt. 1:
# Alexandro Mayoral: alexandro.mayoral@gmail.com
dirWD <- readline("Dame la ruta de tu directorio de trabajo: ")
setwd(dirWD)
# 1. Use el sistema de ayuda para encontrar información sobre las funciones 
# mean y median
?mean
?median


# 2. Haga una lista de todas las funciones en R que contienen la palabra 
# test
apropos("test")


# 3. Use R para calcular las siguientes expresiones:
# a) |5**2 − 2**3|
abs(5**2 - 2**3)
# b) e**2, e**10 y e**pi
e <- exp(1)
e**2; e**10; e**pi
# c) 3**6 − ln 8 + cos(5pi/2)
3**6 - log(8) + cos((5 * pi) / 2)
# d) Sean A y B:
A = matrix(c(2, 3, 5, 1, -1, 2, 1, 2, 3, -4, 1, 1), nrow = 3, byrow = T)
B = matrix(c(-1, 2, 3, 1, -2, 3, 1, -1, 1, 2, -1, 2), nrow = 4, byrow = T)
# Hallar AB, BA, A'B', B'A'
A %*% B
B %*% A
t(A) %*% t(B)
t(B) %*% t(A)
# e) Calcule esl producto escalar de (2, 1, 1, 3) y (-1, 2, 3, 2)
x <- c(2, 1, 1, 3)
y <- c(-1, 2, 3, 2)
sum(x * y)


# 4. Genere las siguientes sucesiones en R:
# a) 1 2 3 1 2 3 1 2 3
rep(1:3, times = 3)
# b) 1 2 2 3 3 3 4 4 4 4
rep(1:4, times = 1:4)
# c) 10.00000 10.04545 10.09091 10.13636 10.18182 10.22727 10.27273 
# 10.31818 10.36364 10.40909 10.45455 10.50000
seq(10, 10.5, length.out = 12)
# d) "1" "2" "3" "verde" "1" "2" "3" "verde"
rep(c("1", "2", "3", "verde"), times = 2)


# Usando la función scan() cree un vector llamado num con 10 números 
# escogidos del 1 al 100.
# Suponemos que el usario no es "malicioso"
askForNumbers <- function(n = 10) {
    result <- numeric(n)
    i <- 1
    while (n > 0) {
        cat("Dame un número del 1 al 100: ")
        x <- scan(n = 1)
        if (x >= 1 && x <= 100) {
            result[i] <- x
            i = i + 1;
            n = n - 1;
        }
        else
            cat("Invalid Input \n")
   }
   print(result)
   result
}

v <- askForNumbers(10)


# 6. A partir del conjunto de datos iris de R, guarde las variables 
# Petal.Width y Petal.Length en un archivo llamado petaliris.
data(iris)
subsetIris <- cbind("Petal.Width" = iris$Petal.Width, 
           "Petal.Length" = iris$Petal.Length)
write.table(subsetIris, file="petaliris.txt", row.names=FALSE)


# 7. Una persona mide el tiempo que tarda en llegar de su casa al trabajo 
# durante 10 días, obteniendo los siguientes resultados (en minutos):
# - 17 21 18 24 22 27 19 24 22 20
# Cree un vector en R con estos valores, y halle el máximo, mínimo, 
# promedio, mediana, varianza y desviación típica.
casaTrabajo <- c(17, 21, 18, 24, 22, 27, 19, 24, 22, 20)
# Max:
max(casaTrabajo)
# Min:
min(casaTrabajo)
# Media:
mean(casaTrabajo)
# Mediana:
median(casaTrabajo)
# Varianza:
var(casaTrabajo)
# Desvación Estándar:
sd(casaTrabajo)

# El valor 17 es incorrecto. El verdadero valor es 27. Corrija este valor 
# y calcule los valores correctos de los parámetros que se pidieron antes.
casaTrabajo[which(casaTrabajo == 17)] = 27
# Max:
max(casaTrabajo)
# Min:
min(casaTrabajo)
# Media:
mean(casaTrabajo)
# Mediana:
median(casaTrabajo)
# Varianza:
var(casaTrabajo)
# Desvación Estándar:
sd(casaTrabajo)

# ¿Puede hallar, usando R, cuántas veces la persona tardó 20 minutos o más?
length(which(casaTrabajo >= 20))

# ¿Qué porcentaje de las veces tardó más de 23 minutos?
length(which(casaTrabajo >= 23)) / length(casaTrabajo)


# 8. Genere muestras de tamaño 25 de las siguientes distribuciones. En 
# cada caso calcule media y varianza y compare con los valores de la 
# distribución original.
compDist <- function(n = 25) {
    # a) Distribución normal con media 2 y varianza 4
    rNorms <- rnorm(n, mean = 2, sd = 2)
    # b) Distribución exponencial de parámetro 2
    rExps <- rexp(n, rate = 2)
    # c) Distribución de Poisson con intensidad 5
    rPois <- rpois(n, lambda = 5)
    # d) Distribución uniforme en [−1, 1]
    rUnifs <- runif(n, min = -1, max = 1)
    # e) Distribución t con 3 grados de libertad
    rTs <- rt(n, df = 3)
    # Vector de las simulaciones
    namesDist <- c("Distribución Normal con media 2 y varianza 4", 
                   "Distribución exponencial de parámetro 2",
                   "Distribución de Poisson con intensidad 5",
                   "Distribución uniforme en [−1, 1]",
                   "Distribución t con 3 grados de libertad")
    sims <- list(rNorms, rExps, rPois, rUnifs, rTs)
    realMeans <- c(2, 1/2, 5, 0, 0)
    realVars <- c(4, 1/2**2, 5, 2**2/12, 3/(3 - 2))
    # Comparaciones de medias y varianzas:
    for (i in 1:length(sims)) {
        cat(namesDist[i], "\n")
        cat("Media:\n")
        cat("Muestra: ", mean(sims[[i]]), ", Real: ", realMeans[i], "\n")
        cat("Varianza:\n")
        cat("Muestra: ", var(sims[[i]]), ", Real: ", realVars[i], "\n\n")
        
    }
}
compDist()


# 9. Para las distribuciones de la pregunta anterior, haga una gráfica de 
# la densidad (o la función de probabilidad, si es discreta) y de la 
# función de distribución (En cada caso es necesario seleccionar un rango 
# finito de valores que contenga las características más relevantes de la 
# distribución)

# a) Distribución normal con media 2 y varianza 4
# Función de densidad
curve(dnorm(x, mean = 2, sd = 2), from = (2 - 3*2), to = (2 + 3*2), 
      ylab = "")
title(main = substitute(paste("Densidad Normal (" , mu, " = 2, ", sigma,
                              " = 2)")), ylab = "p")
# Función de Distribución
curve(pnorm(x, mean = 2, sd = 2), from = (2 - 3*2), to = (2 + 3*2), 
      ylab = "")
title(main = substitute(paste("Distribución Normal (" , mu, " = 2, ", sigma,
                              " = 2)")), ylab = "p")

# b) Distribución exponencial de parámetro 2
# Función de densidad
curve(dexp(x, rate = 2), from = 0, to = 2.5, ylab = "")
title(main = substitute(paste("Densidad Exponencial (" , lambda, " = 2)")),
      ylab = "p")
# Función de Distribución
curve(pexp(x, rate = 2), from = 0, to = 2.5, ylab = "")
title(main = substitute(paste("Distribución Exponencial (" , lambda, 
                              " = 2)")), ylab = "p")

# c) Distribución de Poisson con intensidad 5
# Función de densidad
x <- -0:15 
y<-dpois(x, lambda = 5)
plot(x, y, type = "h", lwd = 2, col = "blue", ylab = "")
points(x, y, col = 2, pch = 16)
title(main = substitute(paste("Densidad Poisson (" , lambda, " = 5)")),
      ylab = "p")
# Función de Distribución
y <- ppois(x, lambda = 5)
plot(x, y, type = "s", lwd = 2, col = "blue", ylab = "")
points(x, y, col = 2, pch = 16)
title(main = substitute(paste("Distribución Poisson (" , lambda, " = 5)")),
      ylab = "p")

# d) Distribución uniforme en [−1, 1]
# Función de densidad
curve(dunif(x, min = -1 , max = 1), from = -1.5, to = 1.5, ylab = "")
title(main = "Densidad Uniforme [-1, 1]", ylab = "p")
# Función de Distribución
curve(punif(x, min = -1 , max = 1), from = -1.5, to = 1.5, ylab = "")
title(main = "Distribución Uniforme [-1, 1]", ylab = "p")

# e) Distribución t con 3 grados de libertad
# Función de densidad
curve(dt(x, df = 3), from = -3*2, to = 3*2, ylab = "")
title(main = "Densidad t (df = 3)", ylab = "p")
# Función de Distribución
curve(pt(x, df = 3), from = -3*2, to = 3*2, ylab = "")
title(main = "Distribución t (df = 3)" , ylab = "p")


# 10. Sean x = c(1, 3, 5, 7, 9), y = c(2, 3, 5, 7, 11, 13). Antes de 
# ejecutar los siguientes comandos trata de deducir cuál es el resultado.
x <- c(1, 3, 5, 7, 9)
y <- c(2, 3, 5, 7, 11, 13)
# x + 3
x + 3
# 2 * y
2 * y
# length(x) + length(y)
length(x) + length(y)
# x + y
x + y
# sum(x > 5) y sum(x[x > 5])
sum(x > 5); sum(x[x > 5])
# x[3] y x[-3]
x[3]; x[-3]
# y[x](¿Cómo se interpreta NA?)
y[x]
# y[y > =7]
y[y >= 7]


# 11. Sea x = c(1, 6, 5, 4, 2, 4, 7, 5, 2, 3) y llamemos xi a la i-ésima 
# componente de este vector. Use R para calcular las siguientes funciones.
# Trate de hacerlo de la manera más económica posible, usando las 
# facilidades vectoriales de R.
x <- c(1, 6, 5, 4, 2, 4, 7, 5, 2, 3)
# a) Halle el vector y cuya i-ésima componente sea la suma de las x's
#    hasta i
y <- cumsum(x)
y
# b) ¿En qué lugares se encuentran las componentes menores que 5?
which(x<5)
# c) Halle log10(xi) para todas las componentes.
log(x, base = 10)
# d) Halle la media μ y desviación típica  de x. Obtenga (xi − μ)/sd para 
#    todos los valores de i.
mu <- mean(x)
sd <- sd(x)
(x - mu) / sd
# e) Halle la diferencia entre la mayor y la menor componentes de x.
max(x) - min(x)
# f) Ordene las componentes del vector y calcule la diferencia entre 
#    términos consecutivos.
diff(sort(x))
# g) Escriba una expresión que cuente cuántas veces se repite el número 5.
sum(x == 5)


# 12. El área de una circunferencia de radio r está dada por pi*r**2 y su 
# perímetro por 2*pi*r. Para circunferencias de radios 1, 2, 3, . . . , 15 
# halle el área de la circunferencia y la longitud del círculo 
# correspondiente. Construya un cuadro de datos con columnas radio, área y 
# perímetro.
rs <- 1:15
areas <- pi * rs**2
perímetros <- 2 * pi * rs
dt.circulo <- data.frame(radio = rs, área = areas, perímetro = perímetros)
dt.circulo

