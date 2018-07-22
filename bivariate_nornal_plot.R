# Some variable definitions
mu1 <- 0  # expected value of x
mu2 <- 0.5	# expected value of y
sig1 <- 1	# variance of x
sig2 <- 1	# variance of y
rho <- 0.5	# corr(x, y)

# Some additional variables for x-axis and y-axis 
xm <- -3
xp <- 3
ym <- -3
yp <- 3

x <- seq(xm, xp, length= as.integer((xp + abs(xm)) * 10))  # vector series x
y <- seq(ym, yp, length= as.integer((yp + abs(ym)) * 10))  # vector series y

# Core function
bivariate <- function(x,y){
  term1 <- 1 / (2 * pi * sig1 * sig2 * sqrt(1 - rho^2))
  term2 <- (x - mu1)^2 / sig1^2
  term3 <- -(2 * rho * (x - mu1)*(y - mu2))/(sig1 * sig2)
  term4 <- (y - mu2)^2 / sig2^2
  z <- term2 + term3 + term4
  term5 <- term1 * exp((-z / (2 *(1 - rho^2))))
  return (term5)
}

# Computes the density values
z <- outer(x,y,bivariate)

# Plot
persp(x, y, z, main = "Bivariate Normal Distribution",
      sub = bquote(bold(mu[1])==.(mu1)~", "~sigma[1]==.(sig1)~", "~mu[2]==.(mu2)~
                     ", "~sigma[2]==.(sig2)~", "~rho==.(rho)),
      col="orchid2", theta = 55, phi = 30, r = 40, d = 0.1, expand = 0.5,
      ltheta = 90, lphi = 180, shade = 0.4)