#Compute the normalised EFD's for a set of coordinates
nef_set<-function(coords)
{
  num_coords<-length(coords)
  nef<-list()
  for (i in 1:num_coords)
  {
    nef[[i]]<-NEF(coords[[i]])
  }
  
  return(nef)
}
