LUP<- function(A,b)
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
  
  L <- matrix(0,n,m)
  
  
  for(i in 1:n)
  {
    L[i,i] <- 1
  }
  
  P <- L
  U <- A
  
  for(k in 1 :(n-1))
  {	
    MAX <- abs(U[k,k])
    
    cat("\nPara k = ",k,", U[k,k] = ",U[k,k],"\n")
    
    IND <- k
    for(i in (k+1) : n)
    {
      if(abs(U[i,k]) > MAX)
      {
        
        cat("OPA, Hora de trocar a linha!\n")
        cat("|U[i,k]|    --    MAX\n")
        cat(abs(U[i,k]),"   >   ",MAX,"\n")
        
        MAX <- abs(U[i,k])
        IND <- i 
      }
    }
    if(IND != k)
    {
      
      cat("OPA! IND eh != de k\n")
      cat("IND    --    k\n")
      cat(IND,"   !=   ",k,"\n")
      
      for(j in k : n)
      {
        aux <- U[k,j]
        U[k,j] <- U[IND,j]
        U[IND,j] <- aux
      }
      
      for(j in 1:n)
      {
        aux <- P[k,j]
        P[k,j] <- P[IND,j]
        P[IND,j] <- aux
      }
      
      aux <- b[IND]
      b[IND] <- b[k]
      b[k] <- aux
      
    }
  }
  
  d[1] <- b[1]
  for(i in 2:n)
  {
    soma <- 0
    for(j in 1:(i-1))
    {
      soma <- soma + (L[i,j] * d[j])
    }
    d[i] <- b[i] - soma
  }
  
  X[n] <- d[n]/ U[n,n]
  
  for(i in (n-1):1)
  {
    soma <-0
    for(j in 1:(i-1))
    {
      soma <- soma + (U[i,j] * X[j])
    }
    X[i] <- (d[i] - soma)/ U[i,i]
  }
}

LUP(c(1,-2,4,-3,8,-6,2,-1,5) , c(11,-15,-29) )