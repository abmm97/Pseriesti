% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/SuavizaHOLTW.R
\name{SuavizaHOLTW}
\alias{SuavizaHOLTW}
\title{Suavización Holt-Winter}
\usage{
SuavizaHOLTW(
  datos,
  col_zt,
  anio,
  mes,
  A,
  C,
  D,
  metodo = c("aditivo", "multiplicativo"),
  prediccion = NULL
)
}
\arguments{
\item{datos}{Arreglo de datos con las columnas t, Zt, mes y año}

\item{col_zt}{valor numérico de la columna de los Zt}

\item{anio}{valor numérico de la columna de los años}

\item{mes}{valor numérico de la columna de los meses}

\item{A}{valor del parametro <1, A+C+D =1}

\item{C}{valor del parametro <1, A+C+D =1}

\item{D}{valor del parametro <1, A+C+D =1}

\item{metodo}{"multiplicativo" o "aditivo"}

\item{prediccion}{valor numérico del tiempo (t) o periodo del que se quiere predecir el Zt suavizado}
}
\value{
tabla de datos Ft, Zt , Tt, Zt suavizado y el at
}
\description{
Método de suavización aditivo o multiplicativo, que brinda la tabla con los valores
de Ft, Zt , Tt, Zt suavizado y el at (error). Además de obtener la desviación general
de la serie suavizada y valores predecidos si fuera necesario.
}
