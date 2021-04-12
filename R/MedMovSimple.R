function (datos, r){
  Mt=rep(0,nrow(datos))
  Zt_1=rep(0,nrow(datos))
  S=rep(0,nrow(datos))
  for (i in (r+1):nrow(datos)) {
    Mt[i-1]=mean(datos[(i-1):(i-r),2])
    Zt_1[i]=Mt[i-1]
    S[i]=datos[i,2]-Zt_1[i]
  }
  Mt[nrow(datos)]=mean(datos[nrow(datos):(nrow(datos)-r+1),2])
  Media_Movil_Simple=data.frame(cbind(Mt,Zt_1,S))
  Media_Movil_Simple
}
