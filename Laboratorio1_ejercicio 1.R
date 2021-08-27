#Carga de Librerias
#install.packages('fitdistrplus')
library('MASS')
library('survival')
library('fitdistrplus')
#Carga de Datos
setwd("~/Documentos/Cursos 2021/Redes/Laboratorios/Laboratorio1")
n<-length(data$V1)

pi <- function(i, m, p) {
  num=1-p
  pot=m+1
  den=(1-p^pot)
  resultado<-(num/den)*p^i
  return(resultado)
}
p<-0.7254931
pi(50,50,p)
#Ejercicio 1
lamda=mean(paquete)*8/1000
mu=4
p=lamda/mu
m=50
f<-rep(0)
x<-rep(0)
buf<-rep(0)
for(i in 50:20){
  buf[i]<-i*50
}

for(i in 1:200){
 x[i]<-i
f[i]<-pi(i,i,p)
tru<-mu*(1-pi(0,i,p))
}
plot(x,f)
pi(2,1,p)
throughput<-function(m, lamda,p) {
  resultado<-lamda*((1-p^m)/(1-p^m+1))
  return(resultado)
}
throughput(50,lamda,p)


#Ejercicio 2
lamda=mean(paquete)*8
ro=6000
p=lamda/ro
m=50
f<-rep(0)
x<-rep(0)
for(i in 1:1000){
  num=1-p
  pot=i+1
  den=(1-p^pot)
  x[i]<-i
  f[i]<-(num/den)*p^i
  
}
plot(x,f)




paquete<-data$V2
mean(paquete)
length(paquete)
sd(paquete)
descdist(paquete)
laplot(fitdist(paquete,'norm'))
hist(paquete[500000:1000000])
tiempos<-data$V2
descdist(tiempos)
lambda=mean(tiempos)
t_inter_clientes <-rep(0)
for (i in 1:100) {
  t_inter_clientes[i]<-rexp(1,lambda)
  print( t_inter_clientes[i])
}
xmin<- 10
xmax<- 15
t_servicio<- runif(n,xmin,xmax)                  # TS
t_inter_clientes<- t_inter_clientes[1:n]               # TIC
h_llega_clientes<-cumsum(t_inter_clientes)            # HL
h_inicio<- rep(0)
h_inicio[1]<- h_llega_clientes[1]                 # HI del cliente 1
h_salida<- rep(0)
h_salida[1]<- h_llega_clientes[1]+t_servicio[1]   # HS=HL+TS del cliente 1, o equivalentemente, HS=HI+TS solo para el cliente 1
t_espera<- rep(0)
t_espera[1] <- h_inicio[1]-h_llega_clientes[1]     # TE=HI-HL del cliente 1
t_total_atencion<- rep(0)
t_total_atencion[1]<-t_espera[1]+t_servicio[1]           # TTA=TE+TS del cliente 1
for (i in 2:n) {
  h_inicio[i]<- max(h_llega_clientes[i],h_salida[i-1])
  # En efecto, si el cliente i llega antes de la hora de salida 
  # del cliente i-1, entonces la hora de inicio del cliente i sera
  # hora de salida del cliente i-1. Ahora bien, si el cliente i llega
  # despues de la hora de salida del cliente i-1, entonces la hora
  # de inicio del cliente i sera la hora de llegada del cliente i
  h_salida[i]<-h_inicio[i]+t_servicio[i]
  t_espera[i]<-h_inicio[i]-h_llega_clientes[i]
  t_total_atencion[i]<-t_espera[i]+t_servicio[i] 
}
# Redondeamos los tiempos a dos decimales para hacer mas 
# agradable la presentacion de los datos



t_inter_clientes <- round(t_inter_clientes,2)
h_llega_clientes <- round(h_llega_clientes,2)
t_espera         <- round(t_espera,2)
h_inicio         <- round(h_inicio,2)
t_servicio       <- round(t_servicio,2)
h_salida         <- round(h_salida,2)
t_total_atencion <- round(t_total_atencion,2)
clientesespera   <- rep(0)

for(l in 2:n){
  if(h_llega_clientes[l]>=h_salida[l-1]) {
    clientesespera[l]<-0  
  }else{
    clientesespera[l] <-l-1-sum(h_salida[1:l]<h_llega_clientes[l])
  }
}

# Esta es la forma de hacer un ifelse cuando dentro del mismo, definimos
# mas variables. Esta forma del if else es mas general que la forma
# ifelse(si_condicion,entonces, si_no)


hist(tiempos)

plotdist(tiempos)
descdist(tiempos)
f1g<-fitdist(tiempos, "exponential")
descdist(tiempos, boot = 1000)
plot(f1g)
summary(f1g)

paquete=data$V2



mean(paquete)
var(paquete)
hist(tiempos)
hist(paquete)
descdist(x1)fw=fitdist(x,"weibull")
plot(fw)
#fg=fitdist(x,"gamma")
fln=fitdist(x,"lnorm")
summary(fw)
summary(fln)
#shape=1.0000000 #parametros yo creo
#scale=0.3151225 #parametros yo creo
#pweibull(1000,shape,scale)
#Graficas
plot.legend=c("Weinbull","LogNormal")
denscomp(list(fw,fln), legendtext=plot.legend)
qqcomp(list(fw,fln),legendtext=plot.legend)
cdfcomp(list(fw,fln), legendtext=plot.legend)
ppcomp(list(fw,fln), legendtext=plot.legend)
x1=data$V2
descdist(x1)
library(BSDA)
set.seed(2000)
ro<-1.2765796
1_P<-1-ro
num=1-ro
den=1-ro^51
(num/den)*ro^50
