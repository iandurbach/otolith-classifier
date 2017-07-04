#normalises the fourier series for comparison

NEF <- function(M, n=dim(M)[1]/2,start=F){
  ef<-efourier(M,n)
  A1<-ef$an[1]; B1<-ef$bn[1]
  C1<-ef$cn[1]; D1<-ef$dn[1]
  theta<-0.5*atan(2*(A1*B1+C1*D1)/(A1^2+C1^2-B1^2-D1^2))
  #phi is constrained between 0 and 2 pi
  if (theta<0){theta<-theta*-1}
  Aa<-A1*cos(theta)+B1*sin(theta)
  Cc<-C1*cos(theta)+D1*sin(theta)
  scale<-sqrt(Aa^2+Cc^2)
  psi<-atan(Cc/Aa)%%pi
  # Small psi seems to lead to a reflection
  if (psi < pi/2)
  {
    psi<-pi-psi
  }
  size<-(1/scale)
  
  rotation<-matrix(c(cos(psi),-sin(psi),sin(psi),cos(psi)),2,2)
  A<-B<-C<-D<-numeric(n)
  if (start){theta<-0}
  for (i in 1:n){
    mat<-size*rotation%*%matrix(c(ef$an[i],ef$cn[i],ef$bn[i],ef$dn[i]),2,2)%*%matrix(c(cos(i*theta),sin(i*theta),-sin(i*theta),cos(i*theta)),2,2)
    A[i]<-mat[1,1]
    B[i]<-mat[1,2]
    C[i]<-mat[2,1]
    D[i]<-mat[2,2]}
  list(A=A,B=B,C=C,D=D,size=scale,theta=theta,
       psi=psi,ao=ef$ao,co=ef$co)
}
