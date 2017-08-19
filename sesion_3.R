
#Curso R sesion 3 Programacion y graficos en ggplot2
#Hugo Andres Dorado
#18-08-2017

#Operadores logicos

set.seed(123)

v1 <- runif(20,1,4)
v2 <- runif(20,1,4)

#Igualdad

v1 == v2

a <- "9"

a <- factor(c("9","8"))

as.numeric(as.matrix(a))

a==9

#Desigualdad

v1 <= 2.1 ; v1 > 2.1

#And y or

v1 >= 2 & v1 <=3

v2[v1<2 | v2 >3]

#Extraer un subconjunto

int_v1 <- as.integer(v1)

int_v1[int_v1 %in% c(1,3)]

g <- sample(1:5,size = 20,replace = T)
f <- sample(c("a","b","c","d","e"),20,replace = T)

paste(g,f,sep= ":")

#Contro de estructutras

#if

c <- 5

if(c==3){
  "rojo"
}else if(c==5){
    "negro"
}else{"Sin Color"}

?switch

#loops

b <- 0
for(i in 1:6){
  print(i)
  b[i] <- i*runif(1)
  
}
b

x <- c("a","b","c","d")

for(i in x){
  print(i)
}

for(i in seq_along(x)){
  print(x[i])
}

listUnif <- list(a = runif(5),b=runif(20),c=runif(10))

ptm <- proc.time()
lapply(listUnif,mean,na.rm=T)
proc.time()-ptm

sapply(listUnif,mean)

install.packages("snowfall")
library("snowfall")

sfInit(parallel = T,cpus = 4)
sfLibrary(ggplot2)
sfExport("listUnif")

ptm <- proc.time()
meanResults <- sfLapply(listUnif,mean,na.rm=T)
proc.time()-ptm

sfStop()

#foreach,paralell

#While

count <- 0
  
while(count < 10 ){
  print(count)
  count <- count + 1
}  

#Funciones en R

exponente <- function(a,b=2){
    a/b
    a*3
    a^b
    
}

#<<- asignacion desde el ambiente local al ambiente global

exponente(a=2,b=3)

#...

dobleMedia <- function(x,...){
  c(mean(x,...)*2,
  sd(x,...))
}

dobleMedia(c(3,2,3,4,5,NA,NA),na.rm=T,digits = 1)

#Programacion orientada a objetos con base en S3

?summary

x <- factor(rep(c("a","b"),c(7,13)))

class(x)

summary(x)
table(x)

y <- rnorm(20)
class(y)

summary(y)

model1 <- lm(y~x)
class(model1)

summary(model1)

methods(summary)

summary.factor

summary.lm

#Explorar 

estadisticos <- function(x){
  n = length(x)
  p = mean(x)
  mu = n*p
  sigma = sqrt(n*p*(1-p))
  list(mu=mu,sigma=sigma,n=n)
}

y1 <- rbinom(100,size = 1, p = 0.3)

estadisticos(y1)


y2 <- rnorm(100,mean=0.3,sd=0.1)

estadisticos(y2)

#Definir una clase

class(y1);class(y2)

as.binomial <- function(x){
  class(x) <- "binomial"
  x
}

as.normal <- function(x){
  class(x) <- "normal"
  x
}

#Crear instancias

y1 <- as.binomial(y1)
class(y1)

y2 <- as.normal(y2)
class(y2)

#Definir metodos

estadisticos <- function(x) UseMethod("estadisticos")
  
estadisticos.binomial <- function(x){
    n = length(x)
    p = mean(x)
    mu = n*p
    sigma = sqrt(n*p*(1-p))
    return(list(mu=mu,sigma=sigma,n=n))
  }
  
estadisticos.normal <- function(x){
    n <- length(x)
    mu = mean(x)
    sigma = sd(x)
  return(list(mu=mu,sigma=sigma,n=n))
  }
  
estadisticos.default <- function(x) stop("No se conoce la distribución")
  
estadisticos(y1)
estadisticos(y2)

y3


save(estadisticos,as.binomial,as.normal,"mis_objs.RDATA")
load("mis_objs")


#Herencia

as.standarNormal <- function(x){
  x <- as.normal(x)
  class(x) <- c("standarNormal",class(x))
  return(x)
}

estadisticos.standarNormal <- function(x){
  object <- estadisticos.normal(x)
  object$sigma <- 1
  return(object)
}


y3 <- rnorm(100)

y3 <- as.standarNormal(y3)

class(y3)

estadisticos(y3)

#Encapsulamiento


juan <- list(height= 173, wight = 75 , name = "Juan")

class(juan) <- "persona"

class(juan)

print(juan)


print.persona <- function(x,...){
  cat("Nombre:",x$name,"\n")
  cat("Estatura:", x$height,"metros","\n")
}

print.persona(juan)


#install.packages("data.table")

# Graficos en ggplot2

library(ggplot2)

summary(airquality)

ggplot(airquality,aes(x=Day,y=Wind))+
  geom_point(aes(colour =factor(Month) ))+xlab("Dia")+
  ylab("Velocidad del viento (km/ha)")+theme_bw()


ggplot(airquality,aes(x=Day,y=Temp))+geom_point()+
  geom_smooth()+facet_grid(.~Month)



ggplot(airquality,aes(x=Temp))+geom_histogram()+
  theme_bw()+ggtitle("Histograma de temperatura")+
  facet_grid(~Month)

abc <- 5

get("abc")

ggplot(mpg, aes(displ, hwy)) +
  geom_point() +
  facet_wrap(~class, scales = "free")



