Trapeziom <-function(a,b,n)
{
	a <- 0
	b <- 0.8
	n <- 100

	pontos <- seq(a,b,length.out=(n+1))

	f(x) %as% {0.2 + 25*x - 200*(x^2) + 675* (x^3) - 900 * (x^4) + 400* (x^5)}
	g(x) %as% {integrate(f,a,b)$value}
  h(x) %as% {D(f,"x")}
	
	somatorio <-0
	for(i in 2:n)
	{
		somatorio <- somatorio + f(pontos[i])
		cat("\nf na posição", i," = ",f(pontos[i]))
	}

	Raprox <- ((f(a) + (2*somatorio) + f(b))/  (2 * n) ) * (b-a)


	Rreal <- g(b)


	erroR <- abs((Rreal - Raprox)/Rreal) * 100



	cat("Valor aproximado",Raprox,"\nValor Real:",Rreal)
	cat("O erro eh de ",erroR,"%")

	## plotando -------

	plot(f,col="blue")

	gx <- c()

	for(i in 1: (length(pontos) ))
	{
		gx[i] <- g(pontos[i])
	}

	lines(pontos,gx,type="l",col="green")

}
