trapezio <-function(a,b)
{
	a <- 0
	b <- 0.8


	f(x) %as% {0.2 + 25*x - 200*(x^2) + 675* (x^3) - 900 * (x^4) + 400* (x^5)}
	g(x) %as% {integrate(f,a,b)}

	Raprox <- (b-a) * (( f(a) + f(b) ) /(2))
	Rreal <- g(b)$value


	erroR <- abs((Rreal - Raprox)/Rreal) * 100


	cat("Valor aproximado",Raprox,"\nValor Real:",Rreal)
	cat("\nO erro eh de ",erroR,"%")




}
