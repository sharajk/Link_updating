#Generating code for outgoing model 
#Voter like model where senders of opinions update links

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

OM <- function(p1p,p2p,p1m,p2m,bp,bm,l,n0)
{
  #Initialization
  
  #initialize variables for population
  n <- numeric(T) #plus population density
  m <- numeric(T) #minus population density
  M <- numeric(T) #magnetization
  x <- numeric(T) #l++ link density
  y <- numeric(T) #l+- link density
  z <- numeric(T) #l-- link density
  
  U <- array (dim = c(T, 22, 4)) #N * displacement vector array with T time steps, 22 possible interactions, 4 components
  P <- matrix(nrow = T, ncol = 22) #probability matrix
  
  n[1] <- n0 #plus population fraction
  m[1] <- 1 - n[1] #minus population fraction
  M[1] <- n[1] - m[1] #magnetisation
  x[1] <- 5*n[1]  #l++link density
  z[1] <- 5*m[1] #l-- link density
  y[1] <- 2.5  #l+- link density
  
  L <- x[1]+y[1]+z[1] #total number of links
  
  
  Bp <- bp*n[1] #net + preference
  Bm <- bm*m[1] #net - preference
  
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
  
  ##############################
  
  for (t in 1:T)
  {
    
    if ((M[t] < 1) & (M[t] > -1) & (x[t] > 0) & (y[t] > 0) & (z[t] > 0) & (x[t] < L) & (y[t] < L) & (z[t] < L))
    {
      
      #Displacement vector for each reaction
      U[t, 1,] <- c(0,1,-1,0)
      U[t, 2,] <- c(0,0,0,0)
      U[t, 3,] <- c(0,0,0,0)
      U[t, 4,] <- c(0,0,0,0)
      U[t, 5,] <- c(-2*n[t], x[t]-y[t], -x[t], y[t])/n[t]
      U[t, 6,] <- c(0,-1,1,0)
      U[t, 7,] <- c(0,0,0,0)
      U[t, 14,] <- c(-2*n[t], x[t]-y[t], -x[t], y[t])/n[t] 
      U[t, 9,] <- c(0,0,0,0)
      U[t, 10,] <- c(0,0,0,0)
      U[t, 11,] <- c(-2*n[t], x[t]-y[t], -x[t], y[t])/n[t]
      U[t, 12,] <- c(0,-1,0,1)
      U[t, 13,] <- c(0,0,0,0)
      U[t, 8,] <- c(2*m[t], z[t]-y[t], y[t], -z[t])/m[t] 
      U[t, 15,] <- c(0,0,0,0)
      U[t, 16,] <- c(0,0,0,0)
      U[t, 17,] <- c(2*m[t], z[t]-y[t], y[t], -z[t])/m[t]
      U[t, 18,] <- c(0,1,0,-1)
      U[t, 19,] <- c(0,0,0,0)
      U[t, 20,] <- c(0,0,0,0)
      U[t, 21,] <- c(0,0,0,0)
      U[t, 22,] <- c(2*m[t], z[t]-y[t], y[t], -z[t])/m[t]
      
      #assign probabilities to each reaction
      P[t,1] <- (1 - Bp - Bm)*p1p*n[t]*m[t]*x[t]/(x[t]+y[t])
      P[t,2] <- (1 - Bp - Bm)*p1p*n[t]**2*x[t]/(x[t]+y[t])
      P[t,3] <- (1 - Bp - Bm)*(1-p1p)*n[t]*x[t]/(x[t]+y[t])
      P[t,4] <- Bp*n[t]*x[t]/(x[t]+y[t])
      P[t,5] <- Bm*n[t]*x[t]/(x[t]+y[t])
      P[t,6] <- (1 - Bp - Bm)*p2p*n[t]**2*y[t]/(x[t]+y[t])
      P[t,7] <- (1 - Bp - Bm)*p2p*n[t]*m[t]*y[t]/(x[t]+y[t])
      P[t,8] <- (1 - Bp - Bm)*(1-p2p)*(1-l)*n[t]*y[t]/(x[t]+y[t])
      P[t,9] <- (1 - Bp - Bm)*(1-p2p)*l*n[t]*y[t]/(x[t]+y[t])
      P[t,10] <- n[t]*Bp*y[t]/(x[t]+y[t])
      P[t,11] <- n[t]*Bm*y[t]/(x[t]+y[t])
      P[t,12] <- (1 - Bp - Bm)*p2m*m[t]**2*y[t]/(z[t]+y[t])
      P[t,13] <- (1 - Bp - Bm)*p2m*n[t]*m[t]*y[t]/(z[t]+y[t])
      P[t,14] <- (1 - Bp - Bm)*(1-p2m)*(1-l)*m[t]*y[t]/(z[t]+y[t])
      P[t,15] <- (1 - Bp - Bm)*(1-p2m)*l*m[t]*y[t]/(z[t]+y[t])
      P[t,16] <-  m[t]*Bm*y[t]/(z[t]+y[t])
      P[t,17] <-  m[t]*Bp*y[t]/(z[t]+y[t])
      P[t,18] <- (1 - Bp - Bm)*p1m*n[t]*m[t]*z[t]/(z[t]+y[t])
      P[t,19] <- (1 - Bp - Bm)*p1m*m[t]**2*z[t]/(z[t]+y[t])
      P[t,20] <- (1 - Bp - Bm)*(1-p1m)*m[t]*z[t]/(z[t]+y[t])
      P[t,21] <- Bm*m[t]*z[t]/(z[t]+y[t])
      P[t,22] <- Bp*m[t]*z[t]/(z[t]+y[t])
      
      if (t < T)
      {
        
        M[t+1] <- M[t] + Msum(U[t,,1],P[t,])/N
        x[t+1] <- x[t] + Msum(U[t,,3],P[t,])/N
        y[t+1] <- y[t] + Msum(U[t,,2],P[t,])/N
        z[t+1] <- z[t] + Msum(U[t,,4],P[t,])/N
        n[t+1] <- (M[t+1]+1)/2
        m[t+1] <- 1 - n[t+1]
      }
      
      else 
      {
        if (t < T)
        {
          M[t+1] <- M[t]
          n[t+1] <- n[t]
          m[t+1] <- m[t]
          x[t+1] <- x[t]
          y[t+1] <- y[t]
          z[t+1] <- z[t] 
        }
      }
    }
  }
  
  #OUTPUT
  time <- seq(1,T,by=1)
  data <- list(M,x,y,z,time)
  names(data) <- c("M","x","y","z","time")
  return(data)
}

###############################



