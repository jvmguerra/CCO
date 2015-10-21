Gauss<- function(A,b,m,n)
{
	if (m != n) return ("Erro")
	#cat("Coloque os valores por coluna no A\n")
	#A <- c()
	#A <- scan()
	#cat("Coloque os valores por coluna B\n")
	#b <- c()
	#b <- scan()
	#TA FALATANDO O DETERMINANTE		

	#A <- c(1,0.0003,1,3)
	#b <- c(1,2.0001)
	#m <- 2
	#n <-2
	#A <- c(1, 0.0003, 1,3)
	#b <- c(1, 2.0001)

	B <- c(A,b)
	nc <- n+1
	X <- c()
	dim(B) <- c(m,nc)

	
	for(k in 1 :(n-1))
	{	
		MAX <- abs(B[k,k])
		cat("\nPara k = ",k,", B[k,k] = ",B[k,k],"\n")
		IND <- k
		for(i in (k+1) : n)
		{
			if(abs(B[i,k]) > MAX)
			{
			  
			  cat("OPA, Hora de trocar a linha!\n")
			  cat("|B[i,k]|    --    MAX\n")
			  cat(IND,"   >   ",k,"\n")
				
			  MAX <- abs(B[i,k])
				IND <- i 
			}
		}
		if(IND != k)
		{
		  cat("OPA! IND eh != de k\n")
		  cat("IND    --    k\n")
		  cat(IND,"   !=   ",k,"\n")
		  
			for(j in k : nc)
			{
				aux <- B[k,j]
				B[k,j] <- B[IND,j]
				B[IND,j] <- aux
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

Gauss(c(0.0003, 1, 3, 1) , c(2.0001, 1)  ,2  , 2)
