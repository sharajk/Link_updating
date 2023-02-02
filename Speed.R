#function to extract consensus speed given input of time evolving magnetization

#default: consensus defined as when |M(t)|>0.8 i.e., when either subpopulation is 90% or more

CSpeed <- function(vec, thresh=0.8)
{
  if(abs(vec[length(vec)]) > thresh) #TODO: (Pranav): I changed the if condition from ...>0.8 to ...>thresh. Is that what you meant?
  {
    Tc <- 1
    for(i in 1:length(vec))
    {
      if(abs(vec[i]) <= thresh)
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

# TODO: (Pranav) Sorry, I'm struggling to understand this function. Why do you return 1/Tc as your speed?
# Tc is just the last index of the vector where the function was within thresholds right?
# This makes sense in monotonic functions, but I'm not convinced it makes sense generally, and especially here.
