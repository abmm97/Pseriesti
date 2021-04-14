#' @name suavi_expon
#' @title Suavización Exponencial Simple
#'
#' @description  Método.
#'
#' @param datos Arreglo de datos con las columnas t, Zt
#' @param alfa valor del parametro <1, puede ser un vector de valores
#'
#' @return tabla de datos Ft, Zt , Tt, Zt suavizado y el at
#' @export suavi_expon
suavi_expon<-function(datos,alfa){
  Zt_sua=matrix(datos[,2],nrow(datos),length(alfa))
  Zt_1=matrix(0,nrow(datos),length(alfa))
  S=matrix(0,nrow(datos),length(alfa))
  for (i in 2:nrow(datos)) {
    for (j in 1:length(alfa)) {
      Zt_sua[i,j]=datos[i,2]*alfa[j] + Zt_sua[(i-1),j]*(1-alfa[j])
      Zt_1[i,j]=Zt_sua[(i-1),j]
      S[i,j]=datos[i,2]-Zt_1[i,j]
    }
  }
  SC=rep(0,length(alfa))
  for (k in 1:length(alfa)) {
    SC[k]=sum(S[,k]^2)
  }
  list(ValoresSuavizados_Zt=Zt_sua,Zt_1,Valores_S_cada_alfa=S,Suma_Cuadrados=SC)
}
