Spline_quadratica <- function(X,Y,n,x)
{
	A <- Y #Ai = F(Xi)
	H<-c()

	for( i in 1:(n-1))
		H[i] <- X[i+1] - X[i]

	

	
}