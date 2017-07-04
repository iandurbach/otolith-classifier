#produces a vector of the ith (i=1,...,4) coefficient of the jth (j=1,...,10) harmonic for each observation
give_vector<-function(harm,coeff,nef,n)
{
  vec<-numeric(0)
  for (i in 1:n)
  {
    if (harm==1)
    {
      vec[i]<-nef[[i]]$A[coeff]
    }
    else if (harm==2)
    {
      vec[i]<-nef[[i]]$B[coeff]
    }
    else if (harm==3)
    {
      vec[i]<-nef[[i]]$C[coeff]
    }
    else
    {
      vec[i]<-nef[[i]]$D[coeff]
    } 
  }
  return(vec) 
}
