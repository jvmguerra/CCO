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

#ERRO RELATIVO --------------------------------------
g <- integrate(f,a,b)$value
Raprox <- (b-a) * (( f(a) + f(b) ) /(2))
Rreal <- g
erroR <- abs((Rreal - Raprox)/Rreal) * 100
# ----------------------------------------------------

#ERRO DA INTEGRAÇÃO ----------------------------------
derivada_segunda <- D( D(e,"x"), "x" )
intervalo <- seq(a,b,length.out=1000)

x <- abs(f2(derivada_segunda,intervalo[1]))
for(i in 2:(length(intervalo)))
{

  if(x < abs(f2(derivada_segunda,intervalo[i])))
		x <- abs(f2(derivada_segunda,intervalo[i]))

}

Et <- - ( (  ( (b-a)^3) ) / 12) * x
##---------------------------------------------------

cat("Valor aproximado",Raprox,"\nValor Real:",Rreal)
cat("\nO erro eh de ",erroR,"%")
cat("\nErro da integração: ",Et)
