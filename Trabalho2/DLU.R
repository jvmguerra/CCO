LU<- function(A,b)
{

	#TEM QUE MUDAR PRA VER SE O RESULT DA RAIZ EH INTEIRO
	#TA FALATANDO O DETERMINANTE	
  ## OS VALORES SÃO INFORMADOS POR COLUNA

	A <- c(4,1,0,5,-1,-2,4,0,0,1,-4,5,-1,0,1,-1)
	b <- c(1,-2,-3,4)
  
  n <- sqrt(length(A))
  m <- n
  
  if (m - floor(m) != 0) return ("Erro")
  
  nc <- (n+1)
  X <- c()
  d <- c()
  
	A <- matrix(A,m,n) ## Tornando A uma matriz

	L <- matrix(0,n,n) ## Montando L
	for (i in 1:n)
	{
		L[i,i] <- 1
	}

  U <- A
	
	for (k in 1:(n-1))  ## Decomposição de Gauss ingênua
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

	for(i in 2:n) # Utilizando L e b obtem-se d
	{
		soma <- 0
		for(j in 1:(i-1))
		{
			soma <- soma + L[i,j] * d[j]
		}
		d[i] <- b[i] - soma
	}
  
	X[n] <- d[n]/U[n,n] ## Ux =d (com u e d eu determino x)
	
	for(i in (n-1):1)
	{
		soma <- 0
		for(j in (i+1):n)
		{
			soma <- soma + (U[i,j]*X[j])
		}
		X[i] <- (d[i] - soma)/U[i,i]
	}
	
	
}


LU(c(1,-2,4,-3,8,-6,2,-1,5) , c(11,-15,-29) )