#' @name SuavizaHOLTW
#' @title Suavización Holt-Winter
#'
#' @description  Método de suavización aditivo o multiplicativo, que brinda la tabla con los valores
#'de Ft, Zt , Tt, Zt suavizado y el at (error). Además de obtener la desviación general
#'de la serie suavizada y valores predecidos si fuera necesario.
#'
#' @param datos Arreglo de datos con las columnas t, Zt, mes y año
#' @param col_zt valor numérico de la columna de los Zt
#' @param anio valor numérico de la columna de los años
#' @param mes valor numérico de la columna de los meses
#' @param A valor del parametro <1, A+C+D =1
#' @param C valor del parametro <1, A+C+D =1
#' @param D valor del parametro <1, A+C+D =1
#' @param metodo "multiplicativo" o "aditivo"
#' @param prediccion valor numérico del tiempo (t) o periodo del que se quiere predecir el Zt suavizado
#'
#' @return tabla de datos Ft, Zt , Tt, Zt suavizado y el at
#' @importFrom 'utils' 'tail'
#' @export SuavizaHOLTW
SuavizaHOLTW<-function(datos,col_zt,anio,mes,A,C,D,metodo=c("aditivo","multiplicativo"),prediccion=NULL){
  Ft_as=rep(0,nrow(datos))
  Zt_as=rep(0,nrow(datos))
  Tt_as=rep(0,nrow(datos))
  Zt_suav=rep(0,nrow(datos))
  at=rep(0,nrow(datos))
  media_anio1=mean(datos[1:12,col_zt])
  Zt_as[12]=media_anio1

  if (metodo=="multiplicativo"){
    for (i in 1:12) {
      Ft_as[i]=datos[i,col_zt]/media_anio1
    }
    for (j in 13:nrow(datos)) {
      Zt_as[j]=A*(datos[j,col_zt]/Ft_as[(j-12)])+(1-A)*(Zt_as[(j-1)]+Tt_as[(j-1)])
      Tt_as[j]=C*(Zt_as[j]-Zt_as[(j-1)])+(1-C)*Tt_as[(j-1)]
      Ft_as[j]=D*(datos[j,col_zt]/Zt_as[j])+(1-D)*Ft_as[(j-12)]
      Zt_suav[j]=(Zt_as[(j-1)]+1*Tt_as[(j-1)])*Ft_as[(j-12)]
      at[j]=datos[j,col_zt]-Zt_suav[j]
    }
  } else {
    for (i in 1:12) {
      Ft_as[i]=datos[i,col_zt]-media_anio1
    }
    for (j in 13:nrow(datos)) {
      Zt_as[j]=A*(datos[j,col_zt]-Ft_as[(j-12)])+(1-A)*(Zt_as[(j-1)]+Tt_as[(j-1)])
      Tt_as[j]=C*(Zt_as[j]-Zt_as[(j-1)])+(1-C)*Tt_as[(j-1)]
      Ft_as[j]=D*(datos[j,col_zt]-Zt_as[j])+(1-D)*Ft_as[(j-12)]
      Zt_suav[j]=(Zt_as[(j-1)]+1*Tt_as[(j-1)])+Ft_as[(j-12)]
      at[j]=datos[j,col_zt]-Zt_suav[j]
    }
  }

  Zt_as_p=Zt_as[nrow(datos)]
  Tt_as_p=Tt_as[nrow(datos)]
  Ft_as_p=tail(Ft_as,12)

  ###para prediccion
  if(!is.null(prediccion)){
    h=prediccion-nrow(datos)
    mes=prediccion %% 12
    for (i in 1:12) {
      for (j in 1:length(prediccion)) {
        if (mes[j]==i){
          Ft_as_pr=Ft_as_p[i]
        }
      }
    }
    if(metodo=="multiplicativo"){
      Estimado=(Zt_as_p + h*Tt_as_p)*Ft_as_pr
    } else {
      Estimado=(Zt_as_p + h*Tt_as_p)+Ft_as_pr
    }
    SC=sum(at^2)
    resul=data.frame(cbind(Ft_as,Zt_as,Tt_as,Zt_suav,at))
    list(HoltWinters=resul,
         Suma_Cuadrados_at=SC,
         Prediccion=Estimado)
  } else {
    SC=sum(at^2)
    resul=data.frame(cbind(Ft_as,Zt_as,Tt_as,Zt_suav,at))
    list(HoltWinters=resul,
         Suma_Cuadrados_at=SC)
  }
}
