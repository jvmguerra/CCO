f <- function(x)
{
  return (0.2 + 25*x - 200*(x^2) + 675* (x^3) - 900 * (x^4) + 400* (x^5)  )
}


f2 <- function(derivada_segunda,x)
{
  return (eval(derivada_segunda))
}



a <- 0
b <- 0.8
e <- expression(0.2 + 25*x - 200*(x^2) + 675* (x^3) - 900 * (x^4) + 400* (x^5))
n <- 91 ## SERVE PARA O CALCULO DO ERRO, ESSE N VAI SER O NUMERO DE ITERAÇÕES QUE EU REALIZAREI BUSCANDO UM MÁXIMO PRA F(ALFA) EM MÓDULO.
        ## NOS MÉTODOS MÚLTIPLOS O N TAMBÉM É UTILIZADO PARA PARA MONTAR OS INTERVALOS NO CALCULO DA INTEGRAL

#ERRO RELATIVO  E CALCULO DA INTEGRAL --------------------------------------
g <- integrate(f,a,b)$value
I <- h/3 * (f(a) + 4*f((a+b)/2) + f(b))
Rreal <- g
erroR <- abs((Rreal - I)/Rreal) * 100
# ----------------------------------------------------

#ERRO DA INTEGRAÇÃO ----------------------------------
derivada_quarta <-D( D (D ( D (e,"x"), "x"),"x"),"x")

h <- ((b-a)/2)
intervalo <-c()
intervalo[1] <- a
for(i in 2:(n-1))
{
  intervalo[i] <- intervalo[i-1] + h
}

x <- abs(f2(derivada_quarta,intervalo[1]))
for(i in 2:(length(intervalo)))
{
  
  if(x < abs(f2(derivada_quarta,intervalo[i])))
    x <- abs(f2(derivada_quarta,intervalo[i]))
  
}

Et <- - (((b-a)^5) / 90 ) * x
##---------------------------------------------------



