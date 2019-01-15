x <- 1:10
for (i in x) {
    if (i %% 2 == 1) {
        print(paste0(i, ". hola"))
    } else {
        print(paste0(i, ". adios"))
    }
}
