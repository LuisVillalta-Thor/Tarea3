library(dplyr)
library(purrr)

str(datos)

datos <- datos %>% select(-IdCliente,-Apellido,-IdCuenta)
obsdatos1 <- dim(datos)[1] #Cuenta el total de observaciones del df importado.
any(!complete.cases(datos)) #Consulta si hay NAs
datosAusentes <- sum(is.na(datos)) #Cuenta el total de NAs
datos <- na.omit(datos) #Como son pocos NAs puede excluirlos del df.
obsdatos2 <- dim(datos)[1]
obsPerdidasTrain <- obsdatos1 - obsdatos2 #Al final se eliminaron 58 observaciones estadisticas.
#Hay que recodificar algunas variables que se pasaron con un tipo que no les corresponde
datos <- within(datos,{
ScoringCrediticio <- as.numeric(ScoringCrediticio)
Edad <- as.numeric(Edad)
CantidadProductos <- as.numeric(CantidadProductos)
TarjetaCredito <- as.factor(TarjetaCredito)
Activo <- as.factor(Activo)
DejaBanco <- as.factor(DejaBanco)
})
#Verifica con un glimpse de dplyr como quedan los tipos de datos por variable
glimpse(datos)
#Verifica que no existan valores NAs o campos nulos ("")
datos %>% map_lgl(.f = function(x){any(!is.na(x) & x == "")})
