############################################################################
#################### Empircal Illustration of Random Matrix Theory Results #######################
par(mfrow=c(1,2))#graphical parameters

############################################################################
#################### Empircal Illustration of Random Matrix Theory Results #######################
par(mfrow=c(1,2))#graphical parameters

#generate a symmetric random matrix whose entries are normally distributted and the off-diagonal elements have variance 1/N
n <- 5000;
m <- array(rnorm(n^2),c(n,n)); #rnorm creates n^2 random variates and allocates them to an array
m2 <- (m+t(m))/sqrt(2*n);# Make m symmetric and normalize for variance 1
lambda <- eigen(m2, symmetric=T, only.values = T);
e <- lambda$values;
hist(e,breaks=seq(-2.01,2.01,.02), main=NA, xlab="Eigenvalues",freq=F)
sigma <- 1;
par(col="red");
curve ( 1/(2*pi * sigma^2)*sqrt(4*sigma^2-x^2), -2*sigma, 2*sigma, add=TRUE);

#output in: random_matrix_eigenvalue_dist.png
############################################################################

#generate a uniform random matrix whose entries are uniformally distributted and the off-diagonal elements have variance 1/N
#demonstrate 'universality' - distributions other than normal lead to the same asymptotic result
n <- 5000;
mu <- array(runif(n^2),c(n,n)); #runif generates 5000^2 random deviates from the uniform distribution and adds fills them into an n x n array
mu2 <-sqrt(12)*(mu+t(mu)-1)/sqrt(2*n); # Make m symmetric and normalize for variance 1
lambdau <- eigen(mu2, symmetric=T, only.values = T);
eu <- lambdau$values;
histeu<-hist(eu,breaks=seq(-2.01,2.01,0.02), main=NA, xlab="Eigenvalues",freq=F)
#The density of eigenvalues is a semicircle, as predicted by Wigner's semicircle law.

sigma <- 1;
par(col="pink");
curve ( 1/(2*pi * sigma^2)*sqrt(4*sigma^2-x^2), -2*sigma, 2*sigma, add=TRUE);

#output in: random_matrix_normal_and_uniform_dist_w_semicircular_law.png
############################################################################



#generate a random correlation matrix
par(mfrow=c(1,1))#graphical parameters
t <- 5000;
m <- 1000;
Q <- t/m
h <- array(rnorm(m*t),c(m,t)); # Time series in rows
e <- h %*% t(h)/t; # Form the correlation matrix
lambdae <- eigen(e, symmetric=T, only.values = T);
ee <- lambdae$values;
hist(ee,breaks=seq(0.01,3.01,.02), main="Random Sample Correlation" ,xlab="Eigenvalues",freq=F)

#superimpose Marcenko-Pastur
lambdaMax = sigma * (1 + sqrt(1/Q))^2
lambdaMax
lambdaMin = sigma * (1 - sqrt(1/Q))^2
lambdaMin
par(col="green");
curve (  Q /(2*pi*sigma^2) * sqrt( (lambdaMax - x)* (x-lambdaMin))/x, from = lambdaMin, to = lambdaMax, add=TRUE);
segments(0, 0, lambdaMin, 0, col="green")

#output in: random_matrix_correlation_w_MP_law.png
############################################################################
#####################################################################


#################### Comparison of Scalar Asymptotic Results to Matrix Results #######################
par(mfrow=c(1,2))#graphical parameters

###################Normal Distribution################################
x <- seq(-5, 5, length=100)
y <- dnorm(x, mean=0, sd=1)
plot(x, y , type="l", lty=1, xlab="x value",  ylab="Density",  lwd=10, col="black")
###################Wigner's Semi-Circular Law################################
sigma <- 1;
par(col="pink");
curve ( 1/(2*pi * sigma^2)*sqrt(4*sigma^2-x^2), -2*sigma, 2*sigma, add=FALSE, xlab="x value",  ylab="Density",  lwd=10, col="red");

par(mfrow=c(1,2))#graphical parameters
###################X^2 Distribution ################################
x <- seq(0, 20, length=100)
numdf=1
y <- dchisq(x, df=numdf)
plot(x, y , type="l", lty=1, xlab="x value",  ylab="Density",  lwd=10, col=1)
numdf=2
y <- dchisq(x, df=numdf)
lines(x, y , type="l", lty=1, xlab="x value",  ylab="Density",  lwd=10, col=2)
numdf=5
y <- dchisq(x,df=numdf)
lines(x, y , type="l", lty=1, xlab="x value",  ylab="Density",  lwd=10, col=3)
numdf=10
y <- dchisq(x, df=numdf)
lines(x, y , type="l", lty=1, xlab="x value",  ylab="Density",  lwd=10, col=4)

colors <- seq(1:4)
labels <- c("df=1", "df=2", "df=3", "df=4")

legend("topright", inset=.05,
  labels, lwd=10, lty=c(1, 1, 1, 1), col=colors)

#################### Marcenko Pastur Density #######################
x <- seq(0, 20, length=100)
Q=1
y <- dmp(x, var=1, svr=Q)
plot(x, y , type="l", lty=1, xlab="x value",  ylab="Density",  lwd=10, col=1, ylim=c(0,2))
Q=2
y <-dmp(x, var=1, svr=Q)
lines(x, y , type="l", lty=1, xlab="x value",  ylab="Density",  lwd=10, col=2)
Q=5
y <-dmp(x, var=1, svr=Q)
lines(x, y , type="l", lty=1, xlab="x value",  ylab="Density",  lwd=10, col=3)
Q=10
y <- dmp(x, var=1, svr=Q)
lines(x, y , type="l", lty=1, xlab="x value",  ylab="Density",  lwd=10, col=4)

colors <- seq(1:4)
labels <- c("Q=1", "Q=2", "Q=3", "Q=4")

legend("topright", inset=.05,
  labels, lwd=10, lty=c(1, 1, 1, 1), col=colors)
