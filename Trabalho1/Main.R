Lagrange <- function(X,Y,n,x)
{

	#x  = Valor a ser interpolado
	#X  = pontos dos x usados para fazer o polinomio XY
	#Y  = pontos dos y para fazer o polinomio XY
	#n  = numero de pontos do vetor X e Y (eles tem o mesmo tamanho)

	
	R <- 0
	for(i in 1:n)
	{
		c <- 1 #numerador
		d <- 1 #denominador
		for(j in 1:n)
		{
			if(j !=i )
			{
				c <- c * (x - X[j])
				d <- d * (X[i] - X[j])
			}
		}
		R <- R + Y[i] * c/d
	}	
	
	
	
	return(R)
}



Newton <- function(X,Y,n,x)
{		
	#x  = Valor a ser interpolado
	#X  = pontos dos x usados para fazer o polinomio XY
	#Y  = pontos dos y para fazer o polinomio XY
	#n  = numero de pontos do vetor X e Y (eles tem o mesmo tamanho)

	xy <- Y
	
	for(k in 1:(n-1))
		for(i in n:(k+1))
			xy[i] <- (xy[i] - xy[i-1])/(X[i] - X[i-k])
			
	R<- xy[n]

	for(i in (n-1):1)
		R <- R * (x - X[i]) + xy[i]
	
		
	#print(R)
	
	#plot(x,R,type = "l",col = "blue")

	return (R)
	
}


Runge <-function(intervalo_i,intervalo_f,m)
{
	# m eh o tanto de pontos gerados (o tamanho dos vetores X e Y)
	# os intervalos estipulam o começo e o fim dos pontos em X
	
	Y <- c()
	X <- seq(intervalo_i,intervalo_f,length.out = m)

	for(i in 1:m)
	{
		Y[i] <-  1/(1+25*X[i]*X[i]) 
	}


	return (Y)
}

Exercicio1 <-function()
{

}

Exercicio2 <-function(intervalo_i,intervalo_f)
{

	x <- seq(intervalo_i,intervalo_f,length.out = 50) ## todos usam

	Ri <-  Runge(intervalo_i,intervalo_f,50) ## os ys do Runge

	length(Ri)
	length(x)

	for (i in 4:14)
	{
		
		X   <- seq(intervalo_i,intervalo_f,length.out = i)
		Y_n <- Newton(X,Runge(intervalo_i,intervalo_f,i),i,x)
		Y_l <- Lagrange(X,Runge(intervalo_i,intervalo_f,i),i,x)
		
		length(X)
		length(Y_n)
		length(Y_l)

		plot(x,Ri,type = "l",col = "green")
		lines(x,Y_n,type = "l",col = "red")
		lines(x,Y_l,type = "l",col = "blue")
			
		scan()
		graphics.off()
	}

}


Spline_linear <- function(X,Y,n,x)
{
	#x  = Valor a ser interpolado
	#X  = pontos dos x usados para fazer o vetor A e B(Coeficientes)
	#Y  = pontos dos y para fazer o vetor A e B(Coeficientes)
	#n  = numero de pontos do vetor

	A <-Y # Ai = F(Xi)
	B <- c()
	
	count <- 1

	for (i in 1:(n-1))
		B[i] <- (Y[i+1] - Y[i]) / ( X[i+1] - X[i] )
	

	for(k in 1:(n-1))
		if(X[k] < x && X[k+1]>x)
			break
		count = k

	S <- A[count] + B[count] * (x - X[count])

	S

}

Spline_quadratica <- function(X,Y,n,x)
{	

#X <- c(3.0,4.5,7.0,9.0)
#Y <- c(2.5,1.0,2.5,0.5)
#n <-length(X)	
#x<- c(5,6)

	#n eh o numero de pontos
	tam <- 2*n-3
	a <- c()	

	for(i in 1:(n-1))
	{
		a[i] <- Y[i]		
	}
	

	H<-c()
	D <- seq(0,0, length.out = tam)
	A <- matrix(data = 0:0,nrow = tam, ncol = tam)	

	for( k in 1:(n-1))
	{
		H[k] <- X[k+1] - X[k]
		D[k] <- Y[k+1] - Y[k] 
	}

	A[1,1] <- H[1] # setando a primeira linha da matriz
	
	j<-2
	for(i in 2:(n-1)) # preenchendo com o primeiro bloco de condições
	{
		A[i,j] <- H[i]
		A[i,(j+1)] <- H[i] * H[i]
		j <- j + 2
	}

	A[(i+1),1] <- (-1)
	A[(i+1),2] <- (1)
	
	j <- 2	
	h_iterator <- 2
	i <- i +1	

	for(k in (i+1):tam)
	{
		A[k,j] <- (-1)
		A[k,(j+1)] <-  (-1)*(2 * H[h_iterator])
		A[k,(j+2)] <- (1)
		h_iterator <- h_iterator +1 
		j <- j + 2
	}

	m <- solve(A,D)
		
	b <- c()
	c <- c()

	b[1] <- m[1]
	c[1] <- 0	
	
	##COSNTRUINDO OS Bis E OS Cis
	fim <- length(m)
	for(i in 2:fim)
	{
		if(i %% 2 ==0)
		{
			b <- append(b,m[i])
		}
		else
		{
			c <- append(c,m[i])
		}

	}

	S <- c()

	for(j in 1:length(x))
	{
		xj <- x[j]
		for(i in 1:n)
		{
			if(xj > X[i] && xj < X[i+1])
			{
				R <- a[i] + b[i] * (xj - X[i]) + c[i] * ((xj - X[i])^2)
				print(R)
				S <- append(S, R)
			}
		}
	}
		
	
	plot(x,S,type = "l",col = "green")
	#title(main="Spline Quadratica", sub="sub-title", 
  	#xlab="X", ylab="F(x)")
}


#Exercicio2(-1,1)