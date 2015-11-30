Jacobi <- function(A,b)
{
  ## DIGITAR OS VALORES POR COLUNA
  A <- c(10,2,1,3,8,1,-2,-1,5)
  b <- c(57,20,-4)
  it_max <- 50
  it <- 0
  erro_limite <- (10 ^ (-5))
  erro <-1


  n <- sqrt(length(A))
  m <- n
  d <- c()
  x <- c()
  x_prox <- c()

  A <- matrix(A,m,n)
  D <- matrix(0:0,m,n)
  Di <- matrix(0:0,m,n)
  E <- matrix(0:0,m,n)
  EFE <- matrix(0:0,m,n)
  J <- matrix(0:0,m,n)

  #montando as matrizes e o x0
  for(i in 1:n)
  {
    for(j in 1:n)
    {
      if(i == j)
      {
        D[i,j] <- A[i,j]
        Di[i,j] <- (1/D[i,j])
        x[i] <- (b[i] / A[i,j])
      }
      else if(i>j) E[i,j] <- A[i,j]
      else if(i<j) EFE[i,j] <- A[i,j]
    }
  }
  J <- -(Di) %*% (E+EFE)


  ## CALCULANDO O VETOR Xk
  while(it < it_max && erro_limite < erro)
  {
    maximo <- c()
    for(i in 1:n)
    {
      somatorio <- 0

      for(j in 1:n)
      {
        if(i != j)
        {
          somatorio <- somatorio + (A[i,j] * x[j])
        }
      }
      x_prox[i] <- (1/A[i,i]) * (b[i]  - somatorio  )

      maximo[i] <- abs(x_prox[i] - x[i])
    }

    erro <- max(maximo)/max(x_prox)

    it <- it + 1
    x <- x_prox


  }

}
