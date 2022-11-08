#function to extract consensus speed given input of time evolving magnetization

#default: consensus defined as when |M(t)|>0.8 i.e., when either subpopulation is 90% or more

CSpeed <- function(vec,thresh=0.8)
{
  if(abs(vec[length(vec)])>0.8)
  {
    Tc <- 1
    for(i in 1:length(vec))
    {
      if(abs(vec[i])<=thresh)
      {
        Tc <- i
      }
      else
      {
        return(1/Tc)
        break
      }
    }
  }
  
  else
  {
    return(NA)
  }
}