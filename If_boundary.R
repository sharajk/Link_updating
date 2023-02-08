#function to check if the variables are within the boundary condtions
#Magnetisation is difference in proportion of the individuals of the two opinions
#hence M belongs to [-1,1]
#Link densities cannot be negative and cannot exceed the total number of links possible
#due to conservation of the number of links

boundary <- function(M,x,y,z,eps=0.01)
{
  #Change total number of links here
  L <- 7.5
  
  if ((M < 1 - eps) & 
      (M > -1 + eps) & 
      (x > eps) & 
      (y > eps) & 
      (z > eps) & 
      (x < L - eps) &
      (y < L - eps) &
      (z < L - eps))
  {
    return(TRUE)
  }
  else
  {
    return(FALSE)
  }
}

