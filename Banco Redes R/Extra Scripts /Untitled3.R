par(mfrow=(c(1,2)))
g <- makeDepGraph(list_pkgs, enhances = FALSE, suggests = FALSE)
set.seed(123)
plot(g, pkgsToHighlight = "ggplot2",vertex.size=20, cex=1, main = "Argumento suggests = FALSE", legendPosition = NULL)

g <- makeDepGraph(list_pkgs, enhances = FALSE, suggests = TRUE)
set.seed(123)
plot(g, pkgsToHighlight = "ggplot2", vertex.size=20, cex=1, main = "Argumento suggests = TRUE", legendPosition = c(-1, -1))