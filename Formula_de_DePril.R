#x= valor de g_x=P(S=x) que queramos calcular
#I= monto maximo de reclamacion
#J= numero de probabilidades de reclamacion distintas
#n= matriz que contiene el numero de polizas en cada grupo
#qj= probabilidades de reclamo
########################################
# Funcion para calcular valores de h
h <- function(i,k,n,qj){
  i*(-1)^(k-1)*sum(n[i,]*(qj/(1-qj))^k)
}
########################################
DePril <- function(x,I,J,n,qj){
  ###################
  P <- matrix(0,I,J)
  for(i in 1:I){
    for(j in 1:J){
      P[i,j] <- (1-qj[j])^(n[i,j])
    }
  }
  ##################
  g <- numeric((x+1))
  g[1] <- prod(P)
  for(r in 1:x){
    L1 <- min(r,I); G <- matrix(0,L1,r)
    for(i in 1:L1){
      L2 <- floor(r/i)  
        for(k in 1:L2){
          G[i,k] <- g[(r-i*k+1)]*h(i,k,n,qj)
        }
    }
    g[r+1] <- 1/r*sum(G)
  }
  #########################
  plot(0:x,g,type='h',col='blue',lwd=3,las=2,
       main='P(S=x)')
  abline(h=0)
  
  resultados <- data.frame(
    x = 0:x,
    P_S_igual_a_x = g,
    P_S_menor_o_igual_x_Acumulada = cumsum(g)
  )
  return(resultados)
}
###############

#Ejemplo
x=1600
I=18
J=4
n=matrix(c(rep(0,9),500,rep(0,8),rep(0,14),1000,rep(0,3),rep(0,17),700,rep(0,12),100,rep(0,5)),I,J)
qj=c(0.01,0.008,0.05,0.1)

tabla_resultados <- DePril(x,I,J,n,qj)




###EJEMPLO 1 libro pag 13
#UNA forma
n=matrix(c(1,3,5,2,2,3,5,3,2,3,1,4,4,6,4),nrow = 5,ncol = 3)
#OTRA forma
n1=c(1,3,5,2,2)
n2=c(3,5,3,2,3)
n3=c(1,4,4,6,4)
n=cbind(n1,n2,n3)
#ULTIMA forma
n1=c(1,3,1)
n2=c(3,5,4)
n3=c(5,3,4)
n4=c(2,2,6)
n5=c(2,3,4)
n=rbind(n1,n2,n3,n4,n5)
#################
qj=c(0.03,0.04,0.05)
#x=20, I=5, J=3, n=matrizdellibropag13, qj=probas 0.03, etc
DePril(100,5,3,n,qj)
