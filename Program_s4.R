#Programacion S4 en R

#S4

#Es más formal en la definición de objetos.
#Los métodos pueden tener múltiples argumentos.
#El operador para extraer atributos es @. Los atributos se denominan slots
rm(list = ls())

library(stats4)
y1 <- rbinom(100, size = 1, p = 0.3)

y2 <- rnorm(100, mean = 0.3, sd = 1)

y <- c(26, 17, 13, 12, 20, 5, 9, 8, 5, 4, 8)
nLL <- function(lambda) - sum(dpois(y, lambda, log = TRUE))
fit <- mle(nLL, start = list(lambda = 5), nobs = length(y))

require(pryr)

isS4(fit)

otype(fit)

otype(y1)

#otype(juan)

#Definicion de objetos

setClass("Persona",
         slots = list(nombre = "character", edad = "numeric"))
setClass("Empleado",
         slots = list(jefe = "Persona"),
         contains = "Persona") #Contain una clase que herada

#Hablar sobre la funcion validacion

pedro <- new("Persona", nombre = "Pedro", edad = 40)
lina <- new("Empleado", nombre = "Lina", edad = 20, jefe = pedro) #Pedro es un objeto

pedro@edad

slot(lina, "jefe") #Igual al @

setClass("RangedNumeric",
         contains = "numeric",
         slots = list(min = "numeric", max = "numeric"))

rn <- new("RangedNumeric", 1:10, min = 1, max = 10)

rn@min

rn@.Data

df1 <- data.frame(nam=c("a","b","c","d"),val=c(1,2,3,4))

df2 <- data.frame(nam=c("c","d","f","g"),val=c(3,4,5,6))

union(df1,df2)

setGeneric("union") #Crear un metodo generico

setMethod("union",
          c(x = "data.frame", y = "data.frame"),
          function(x, y) {
              unique(rbind(x, y))
          }
)

union(df1,df2)

union(c(1,2,3,4),c(3,4,5,6))


#De tarea revisar

#Definición de métodos nuevos
lados <- function(object) 0

setGeneric("lados")

setGeneric("lados", function(object) {
    standardGeneric("lados")
})


#Definicion de jerarquias

setClass("Forma")
setClass("Poligono", representation(lados = "integer"), contains = "Forma")
setClass("Triangulo", contains = "Poligono")
setClass("Cuadrado", contains = "Poligono")
setClass("Circulo", contains = "Forma")

#definicion de metodos

setMethod("lados", signature(object = "Poligono"), function(object) {
    object@lados
})

setMethod("lados", signature("Triangulo"), function(object) 3)

setMethod("lados", signature("Cuadrado"),   function(object) 4)

setMethod("lados", signature("Circulo"),   function(object) Inf)

#Uso de los metodos
showMethods("lados")


forma1 <- new("Circulo")

lados(forma1)

forma2 <- new("Cuadrado")

lados(forma2)

