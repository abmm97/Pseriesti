function(datos,r){
  S=matrix(0,nrow = nrow(datos),ncol = length(r))
  for (i in 1:length(r)) {
    S[,i]=round(MedMovSimple(datos,r[i])[,3],3)
  }
  m=max(r)
  S=S[-c(1:m),]
  SC=rep(0,length(r))
  for (i in 1:length(r)) {
    SC[i]=sum(S[,i]^2)
  }
  list(rbind(r,SC))
}
