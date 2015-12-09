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
n <- 91 ## SERVE PARA O CALCULO DO ERRO, ESSE N VAI SER O NUMERO DE ITERAÇÕES QUE EU REALIZAREI BUSCANDO UM MÁXIMO PRA F(ALFA) EM MÓDULO.
        ## NOS MÉTODOS MÚLTIPLOS O N TAMBÉM É UTILIZADO PARA PARA MONTAR OS INTERVALOS NO CALCULO DA INTEGRAL
e <- expression(0.2 + 25*x - 200*(x^2) + 675* (x^3) - 900 * (x^4) + 400* (x^5))


##DETERMINANDO O INTERVALO, O SOMATÓRIO E A INTEGRAL ------

h <- ((b-a)/n)
intervalo <-c()
intervalo[1] <- a
somatorio <- intervalo[1]
for(i in 2:(n-1))
{
  intervalo[i] <- intervalo[i-1] + h
  somatorio <- somatorio + f(intervalo[i])
}

I <- (h/2) * (f(a) + 2* somatorio + f(b))

#CALCULO DO ERRO DA INTEGRAÇÃO------------------------------ 
fdd <- D((D(e,"x")),"x")

F_alfamax <- abs(f2(fdd,intervalo[1]))

for(i in 2:(n-1))
{
  if(F_alfamax < abs(f2(fdd,intervalo[i])) )
    F_alfamax <- abs(f2(fdd,intervalo[i]))
}


Et <- -( ( (b-a)^3 )/ (12*( n ^ 2) ) ) * F_alfamax
# ----------------------------------------------------------
