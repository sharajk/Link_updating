library(deSolve)

#Generating code for incoming model 
#Voter like model where receivers of opinions update links

##############################

# #INPUT
# #for example:
# 
# #set constant global features
# N <- 100 #total population size
# T <- 10000 #number of time steps
# n0 <- 0.8 #majority proportion
# 
# ###set parameter values
# 
# #rewiring probabilities
# p1p <- 0 #phi1+
# p2p <- 0 #phi2+
# p1m <- 0 #phi1-
# p2m <- 0 #phi2-
# 
# #stubbornness 
# bp <- 0.1 # + stubbornness
# bm <- 0.1 # - stubbornness
# 
# #inertia
# l <- 0.3 

##############################

#Simulating the function

IM <- function(p1p,p2p,p1m,p2m,bp,bm,l,n0)
{
  #Initialization
  T <- 10000 #total number of time steps
  t <- 1:T
  N <- 100 #population size
  
  #initialize variables for population
  n <- n0 #plus population density
  m <- 1-n #minus population density
  M <- n-m #magnetization
  x <- 5*n #l++ link density
  y <- 2.5 #l+- link density
  z <- 5*m #l-- link density
  
  L <- x+y+z #total number of links
  
  Bp <- bp*n #net + preference
  Bm <- bm*m #net - preference
  
  #defining a matrix sum function
  Msum <- function(a,b)
  {
    s = 0
    for (i in 1:22)
    {
      s <- s + a[i]*b[i]
    }
    return(s)
  }
  
  #define parameters and state variables
  vars <- c(M,x,y,z)
  params <- c(p1p,p2p,p1m,p2m,bp,bm,l,n0)
  
  
  IM_ode <- function(t,vars,params)
  {
    #parameters 
    p1p <- params[1]
    p2p <- params[2]
    p1m <- params[3]
    p2m <- params[4]
    bp <- params[5]
    bm <- params[6]
    l <- params[7]
    n0 <- params[8]
    
    #variables
    M <- vars[1]
    x <- vars[2]
    y <- vars[3]
    z <- vars[4]
    n <- (M+1)/2
    m <- 1-n
    
    U <- array (dim = c(22, 4)) #N * displacement vector array with 22 possible interactions, 4 components
    P <- numeric(22) #probability vector for each interaction
    
    eps <- 0.01 #epsilon value to avoid convergence issues
    if ((M < 1-eps) & (M > -1+eps) & (x > eps) & (y > eps) & (z > eps) & (x < L-eps) & (y < L-eps) & (z < L-eps))
    {
      
      #Displacement vector for each reaction
      U[1,] <- c(0,1,-1,0)
      U[2,] <- c(0,0,0,0)
      U[3,] <- c(0,0,0,0)
      U[4,] <- c(0,0,0,0)
      U[5,] <- c(-2*n, x-y, -x, y)/n
      U[6,] <- c(0,-1,1,0)
      U[7,] <- c(0,0,0,0)
      U[8,] <- c(-2*n, x-y, -x, y)/n 
      U[9,] <- c(0,0,0,0)
      U[10,] <- c(0,0,0,0)
      U[11,] <- c(-2*n, x-y, -x, y)/n
      U[12,] <- c(0,-1,0,1)
      U[13,] <- c(0,0,0,0)
      U[14,] <- c(2*m, z-y, y, -z)/m 
      U[15,] <- c(0,0,0,0)
      U[16,] <- c(0,0,0,0)
      U[17,] <- c(2*m, z-y, y, -z)/m
      U[18,] <- c(0,1,0,-1)
      U[19,] <- c(0,0,0,0)
      U[20,] <- c(0,0,0,0)
      U[21,] <- c(0,0,0,0)
      U[22,] <- c(2*m, z-y, y, -z)/m
      
      #assign probabilities to each reaction
      P[1] <- (1 - Bp - Bm)*p1p*n*M*x/(x+y)
      P[2] <- (1 - Bp - Bm)*p1p*n**2*x/(x+y)
      P[3] <- (1 - Bp - Bm)*(1-p1p)*n*x/(x+y)
      P[4] <- Bp*n*x/(x+y)
      P[5] <- Bm*n*x/(x+y)
      P[6] <- (1 - Bp - Bm)*p2p*n**2*y/(x+y)
      P[7] <- (1 - Bp - Bm)*p2p*n*M*y/(x+y)
      P[8] <- (1 - Bp - Bm)*(1-p2p)*(1-l)*n*y/(x+y)
      P[9] <- (1 - Bp - Bm)*(1-p2p)*l*n*y/(x+y)
      P[10] <- n*Bp*y/(x+y)
      P[11] <- n*Bm*y/(x+y)
      P[12] <- (1 - Bp - Bm)*p2m*m**2*y/(z+y)
      P[13] <- (1 - Bp - Bm)*p2m*n*m*y/(z+y)
      P[14] <- (1 - Bp - Bm)*(1-p2m)*(1-l)*m*y/(z+y)
      P[15] <- (1 - Bp - Bm)*(1-p2m)*l*m*y/(z+y)
      P[16] <-  m*Bm*y/(z+y)
      P[17] <-  m*Bp*y/(z+y)
      P[18] <- (1 - Bp - Bm)*p1m*n*m*z/(z+y)
      P[19] <- (1 - Bp - Bm)*p1m*m**2*z/(z+y)
      P[20] <- (1 - Bp - Bm)*(1-p1m)*m*z/(z+y)
      P[21] <- Bm*m*z/(z+y)
      P[22] <- Bp*m*z/(z+y)
      
      return(list(c(Msum(U[,1],P)/N,
                    Msum(U[,3],P)/N,
                    Msum(U[,2],P)/N,
                    Msum(U[,4],P)/N)))
      
    }  
    
    else 
    {
      return(list(c(0,0,0,0)))
    }
  }
  
  out <- ode(t,y = vars, parms = params, func = IM_ode)
  out <- as.data.frame(out[,c(2,3,4,5,1)])
  colnames(out) <- c("M","x","y","z","time")
  return(out)
}

###############################



