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
	x_p <- c(1.9,2.1,2.5)
	y_p <- c(1.759,1.6562,3.425)
	
	x_s <- c(1.9,2.1,2.5,3.0)
	y_s <- c(1.759,1.6562,3.425,3.0855)
	
	x_t <- c(1.9,2.1,2.5,3.0,3.2)
	y_t <- c(1.759,1.6562,3.425,3.0855,11.0925)
	
	interpolar <- seq(x_p[1],x_p[length(x_p)],length.out = 50) ## vetor a ser interpolado

	## tentar plotar a original para comparações depois

	# plotando newton de 2,3,4 grau

	plot(interpolar,(Newton(x_p,y_p,length(x_p),interpolar)),type = "l",col = "red")
	lines(interpolar,(Newton(x_s,y_s,length(x_s),interpolar)),type = "l",col = "blue")
	lines(interpolar,(Newton(x_t,y_t,length(x_t),interpolar)),type = "l",col = "green")

	graphics.off()
	scan()

	plot(interpolar,(Lagrange(x_p,y_p,length(x_p),interpolar)),type = "l",col = "red")
	lines(interpolar,(Lagrange(x_s,y_s,length(x_s),interpolar)),type = "l",col = "blue")
	lines(interpolar,(Lagrange(x_t,y_t,length(x_t),interpolar)),type = "l",col = "green")

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

Exercicio1()