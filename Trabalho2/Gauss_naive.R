Gauss_naive <- function(A,b,m,n)
{
	if (m != n) return ("Erro")
	#cat("Coloque os valores por coluna no A\n")
	#A <- c()
	#A <- scan()
	#cat("Coloque os valores por coluna B\n")
	#b <- c()
	#b <- scan()
	
	A <- c(0.003, 1 , 3 , 1)
	b <- c(2.0001, 1)
	m <- 2
	n <-2
	#A <- c(1, 0.0003, 1,3)
	#b <- c(1, 2.0001)

	B <- c(A,b)
	nc <- n+1
	X <- c()
	dim(B) <- c(m,nc)

	
	for(k in 1 :(n-1))
	{
		for(i in (k+1) : n)
		{
			fator <- B[i,k]/B[k,k]
			for(j in (k+1): nc)
			{
				B[i,j] <- B[i,j] - (fator * B[k,j])
			}
		}
	}

	X[n] <- B[n,nc] / B[n,n]
	for(i in seq( (n-1) , 1, -1) )
	{
		soma <- 0
		for(j in (i+1) : n)
		{
			soma <- soma  + B[i,j] * X[j]
		}
		X[i] <- (B[i,nc]- soma) / (B[i,i])
	}
	

	cat("X = ", X, "\n")
	#return X

}

Gauss_naive(c(0.003, 1 , 3 , 1) , c(2.0001, 1)  ,2  , 2)
