library(testthat)
a <- c("Luis","Ana","Maria","Pedro","Juan","Arturo","Oscar")
b <- c(1,2,3,4,5,6,7)
c <- c(18,25,45,16,17,20,29)

df <- data.frame(a,b,c)

d <- solnum(df)

Graficos(d$b)
