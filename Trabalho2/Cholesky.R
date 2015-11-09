Cholesky <- function(A,b)
{
  A <- c(5,-1,2,-1,8,4,2,4,10) ## DIGITAR OS VALORES POR COLUNA
  b <- c(21,10,50) ## O b pode ser qualquer um 
  
  n <- sqrt(length(A))
  m <- n
  d <-c()
  X <- c()
  
  if (m - floor(m) != 0) return ("Erro")
  
  
  
  A <- matrix(A,m,n)
  
  if (identical(A,t(A)) == TRUE && all(eigen(A)$values >= 0) == TRUE)
  {
    
    L <- matrix(0,n,n)
    Lt <- t(L)
    
    for(i in 1:n)
    {
      for(j in 1:n)
      {		
        if(j == i && i <= j)
        {
          somatorio <- 0
          if((i-1) != 0)
          {
            for(k in 1:(i-1))
            {
              somatorio <- somatorio + (Lt[k,i]^2)
            }
          }
          
          Lt[i,j] <- sqrt(A[i,j] - somatorio)
        }
        
        else if(i <= j)
        {
          somatorio <- 0
          
          if((j-1) != 0)
          {
            for(k in 1:(j-1))
            {
              somatorio <- somatorio + Lt[k,i]*Lt[k,j]
            }
          }
          
          Lt[i,j] <- (A[i,j] - somatorio)/Lt[i,i]
        }
      }#fecha for de j
    }#fecha for de i
    
    L <- t(Lt)
    
    for(i in 1:n)
    {
      somatorio <- 0
      if((i-1) != 0)
      {
        for(j in 1:(i-1))
        {
          somatorio <- somatorio + L[i,j]*d[j]
        }
      }
      
      d[i] <- (b[i] - somatorio)/L[i,i]
      
    }#fecha for de i
    
    for(i in n:1)
    {
      somatorio <- 0
      
      if (i != n)
      {
        for(j in (i+1):n)
        {
          somatorio <- somatorio + Lt[i,j]
        }
      }
      
      X[i] <- (d[i] - somatorio)/Lt[i,i]
    }#fecha for de i
  }else
  {
    print("Matriz A nao simetrica ou nao definida positiva")
  }
  
}

