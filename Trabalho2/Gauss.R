Gauss<- function(A,b)
{
  A <- c(4,1,0,5,-1,-2,4,0,0,1,-4,5,-1,0,1,-1)
  b <- c(1,-2,-3,4)
  n <- sqrt(length(A))
  m <- n
  
  
  if (m - floor(m) != 0) return ("Erro")
	
	B <- c(A,b)
	nc <- n+1
	X <- c()
	dim(B) <- c(m,nc) ##formando a matriz A com os b´s juntos

	
	for(k in 1 :(n-1)) ## Decomposição: Adquirindo L e U com pivoteamento
	{	
		MAX <- abs(B[k,k])
		#cat("\nPara k = ",k,", B[k,k] = ",B[k,k],"\n")
		IND <- k
		for(i in (k+1) : n)
		{
			if(abs(B[i,k]) > MAX)
			{
			  
			  #cat("OPA, Hora de trocar a linha!\n")
			  #cat("|B[i,k]|    --    MAX\n")
			  #cat(abs(B[i,k]),"   >   ",MAX,"\n")
				
			  MAX <- abs(B[i,k])
				IND <- i 
			}
		}
		
		if(IND != k)
		{
		  #cat("OPA! IND eh != de k\n")
		  #cat("IND    --    k\n")
		  #cat(IND,"   !=   ",k,"\n")
		  
			for(j in k : nc)
			{
				aux <- B[k,j]
				B[k,j] <- B[IND,j]
				B[IND,j] <- aux
			}
		}
		
		for(i in (k+1):n) 
		{
		  fator <- B[i,k]/B[k,k]
		  for(i_aux in (k+1):nc)
		  {
		    B[i,i_aux] <- B[i,i_aux] - (fator * B[k,i_aux])
		  }
		}
		
	}
	## Calculo do determinante
	det_result <-1
	for (det_index in 1:n)
	{
	  det_result <- det_result * U[det_index,det_index]
	}
	if(det_result == 0) return ("Erro")
	

	X[n] <- B[n,nc] / B[n,n]
	for(i in (n-1):1)
	{
		soma <- 0
		for(j in (i+1) : n)
		{
			soma <- soma  + (B[i,j] * X[j])
		}
		X[i] <- (B[i,nc]- soma) / (B[i,i])
	}
	

	#cat("X = ", X, "\n")
	#return X

}

Gauss(c(0.0003, 1, 3, 1) , c(2.0001, 1))
