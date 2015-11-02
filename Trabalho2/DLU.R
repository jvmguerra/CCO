LU<- function(A,b)
{
	n <- sqrt(length(A))
	m <- n

	if (m != n) return ("Erro") 
	
	#TEM QUE MUDAR PRA VER SE O RESULT DA RAIZ EH INTEIRO
	#TA FALATANDO O DETERMINANTE		

	A <- c(1,-2,4,-3,8,-6,2,-1,5)
	b <- c(11,-15,29)
  d <- c()

	A <- matrix(A,m,n)

	L <- matrix(nrow= n,ncol= n)
	for (i in 1:n)
	{
		for (j in i:n)
		{
			L[i,j] <- 0
		}
		L[i,i] <- 1
	}

  U <- A
	
	nc <- (n+1)
	X <- c()
	

	for (k in 1:(n-1))
	{
		for (i in (k+1):n)
		{
			fator <- U[i,k]/U[k,k]
			#cat("Fator",fator,"\n")
			L[i,k] <- fator
			for(j in 1:n)
			{
				U[i,j] <- U[i,j] - (fator * U[k,j])
				#cat("U[i,j]",U[i,j],"\n")
			}
		}
	}
	
	d[1] <- b[1] ##pq a primeira linha de L sempre só tem um valor != de 0

	for(i in 2:n)
	{
		soma <- 0
		for(j in 1:(i-1))
		{
			soma <- soma + L[i,j] * d[j]
		}
		d[i] <- b[i] - soma
	}

	X[n] <- d[n]/U[n,n]
	
	for(i in (n-1):1)
	{
		somatorio <- 0
		for(j in (i+1):n)
		{
			somatorio <- somatorio + U[i,j]*X[j]
		}
		X[i] <- (d[i] - somatorio)/U[i,i]
	}
	
	
}


LU(c(1,-2,4,-3,8,-6,2,-1,5) , c(11,-15,-29) )