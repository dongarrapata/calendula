setwd("~/workspace/calendula")
dietas <- read.csv("csv/dietas.csv")

trim <- function (x) gsub("^\\s+|\\s+$", "", x)
trim.leading <- function (x)  sub("^ +", "", x)
comidas <- c("desayuno", "colacion", "media mañana", "comida", "cena", "media tarde", "refrigerio")
for (i in 1:ncol(dietas)) {
  dietas[,i] <- as.character(dietas[,i])
  dietas[,i] <- trim(dietas[,i])
  dietas[,i] <- tolower(dietas[,i])
  dietas[,i] <- gsub("\n", "", dietas[,i])
  dietas[,i] <- trim.leading(dietas[,i])
  for (j in 1:length(comidas)){
    dietas[,i] <- gsub(comidas[j], "", dietas[,i])
  }
}

for (i in 1:nrow(dietas)) {
  if (grepl("igual al", dietas[i, 6])) {
    dietas[i, 6] <- dietas[i, 2]
  }
  if (grepl("igual que", dietas[i, 5])) {
    dietas[i, 5] <- dietas[i, 3]
  }
  for (j in 1:ncol(dietas)) {
    if (grepl("igual al", dietas[i, j])) {
      diaSemana <- gsub("igual al ", "", dietas[i, j])
      k <- i
      while (!grepl(dietas[k, 1], diaSemana)) {
        k <- k - 1
      }
      dietas[i, j] <- dietas[k, j]
      print(c(i, diaSemana, k, dietas[k, 1]))
    }
  }
}
write.csv(dietas, "~/Dropbox/Cuentas/dietasArregladas.csv")
# arreglar "igual al lunes", "igual a media mañana", etc LISTO!!
catalogoComidas <- c()
for (i in 2:ncol(dietas)) {
  catalogoComidas <- c(catalogoComidas, dietas[,i])
}
catalogoComidas <- unique(catalogoComidas)

recetas <- list(comidas = catalogoComidas, ingredientes = list(), preparacion = list())

