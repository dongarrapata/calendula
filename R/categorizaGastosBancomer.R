library(stringr)
directorio <- dirname(parent.frame(2)$ofile)
setwd(directorio)
gastos <- read.csv("../csv/edoCuentaBancomer.csv")
gastos$FECHA <- as.Date(gastos$FECHA, "%m/%d/%Y")
gastos$DESCRIPCIÓN <- as.character(gastos$DESCRIPCIÓN)
gastos$DESCRIPCIÓN <- word(as.character(gastos$DESCRIPCIÓN), sep = "/")
gastos$CARGO <- as.double(gastos$CARGO)
gastos$ABONO <- as.double(gastos$ABONO)
gastos$SALDO <- as.double(as.character(gastos$SALDO))
gastos$CATEGORIA <- ""
print(getwd())
categorias <- readRDS("datos/categorias.rds")
m <- nrow(gastos)
i <- 1
seguir <- TRUE
while (seguir & i <= m) {
    numCategorias <- length(names(categorias))
    yaTieneCategoria <- FALSE
    j <- 1
    while (j <= numCategorias & yaTieneCategoria == FALSE) {
      print(j)
      print(names(categorias)[j])
      if (gastos$DESCRIPCIÓN[i] %in% categorias[[j]]) {
        gastos$CATEGORIA[i] <- names(categorias)[j]
        categorias[[j]] <- c(categorias[[j]], gastos$DESCRIPCIÓN[i])
        yaTieneCategoria <- TRUE
      }
      j <- j + 1
    }
    if (yaTieneCategoria == FALSE) {
      texto <- as.character(sprintf("El gasto por $%6.2f en %s el %s no tiene categoria, \n ¿que nueva categoría le asigno? \n", 
                                    gastos$CARGO[i], substr(gastos$DESCRIPCIÓN[i], 1, 20), gastos$FECHA[i]))
      nuevaCategoria <- readline(prompt = texto)
      gastos$CATEGORIA[i] <- nuevaCategoria
      if (!(nuevaCategoria %in% names(categorias))) {
        categorias[nuevaCategoria] <- gastos$DESCRIPCIÓN[i]
      } else {
        categorias[[nuevaCategoria]] <- c(categorias[[nuevaCategoria]], gastos$DESCRIPCIÓN[i])
      }
      textoSeguir <- "¿Deseas continuar con las categorias? (y o n)"
      deseoSeguir <- "~"
      while (!(deseoSeguir %in% c("y", "Y", "n", "N", ""))) {
        deseoSeguir <- readline(textoSeguir)
      }
      if (deseoSeguir %in% c("n", "N")) {
        seguir <- FALSE
      }
    }
  i <- i + 1
}
categorias <- lapply(categorias, unique)
# saveRDS(categorias, "datos/categorias.rds")
# write.csv(gastos, "datos/edoCuentaBancomerCategoria.csv")
