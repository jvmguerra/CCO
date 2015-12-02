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
n <- 1000
e <- expression(0.2 + 25*x - 200*(x^2) + 675* (x^3) - 900 * (x^4) + 400* (x^5))

h <- ((b-a)/n)
vetor <- seq(a,b,length.out = n)

somatorio <-0
for(i in 1:(n))
{
  somatorio <- somatorio + f(vetor[i])
}


I <- (h/2) * (f(a) + 2* somatorio + f(b))



#CALCULO DO ERRO 
fdd <- D((D(e,"x")),"x")


F_alfamax <- abs(f2(fdd,vetor[1]))

for(i in 2:n)
{
  if(F_alfamax < abs(f2(fdd,vetor[i])) )
    F_alfamax <- abs(f2(fdd,vetor[i]))
}

Et <- -( ( (b-a)^3 )/ (12*( n ^ 2) ) ) * F_alfamax

