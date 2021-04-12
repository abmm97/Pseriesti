function(datos,r,posicion,alfa){
  Mt=MedMovSimple(datos,r)[,1]
  at=datos[-(1:(r-1)),2]-Mt[-(1:(r-1))]
  var=var(at)
  if (posicion>nrow(datos)){
    a=Mt[nrow(datos)]
    LI=a-(sqrt(var/r))*qnorm(1-alfa/2)
    LS=a+(sqrt(var/r))*qnorm(1-alfa/2)
  } else {
    a=Mt[posicion]
    LI=a-(sqrt(var/r))*qnorm(1-alfa/2)
    LS=a+(sqrt(var/r))*qnorm(1-alfa/2)
  }
  val=c(LI,a,LS)
  round(val,3)
}
