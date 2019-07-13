
#' Funcion para extraer solo variables numericas
#'
#' @param x parametro de entrada corresponde al dataframe inicial
#'
#' @return
#' @export
#'
#' @examples
solnum <- function(x) {
  sn <- dplyr::select_if(x,is.numeric)
  return(sn)
}

#datos <- solnum(datos)

#' Funcion de Reemplazo
#'
#' @param x primer parametro dataframe de solo variables numericas
#' @param value segundo parametro parametro de reemplazo
#'
#' @return
#' @export
#'
#' @examples
`replace.na<-` <- function(x,value) {
    x[is.na(x)] <- value
    return(x)
  }


#' Funcion para generar graficos histograma y boxplot
#'
#' @param x parametro de entrada dataframe
#'
#' @return
#' @export
#'
#' @examples
Graficos <- function(x) {

    #Realiza graficos de Histograma y Boxplot
    hist(x,
              main=paste("Histograma para Variable"),
              xlab= "Variable",
              ylab= "Frecuencia",
              xlim=c(1,500),
              col="lightblue",
              freq=T)
    boxplot(x,
                    main = paste("Boxplot para Variable"),
                    xlab = "Variable",
                    ylab = "Dato",
                    col = "salmon",
                    border = "brown",
                    horizontal = T,
                    notch = T)
    }

