suppressMessages(library(igraph))
suppressMessages(library(miniCRAN))
suppressMessages(library(magrittr))
suppressMessages(library(keyplayer))
suppressMessages(library(dplyr))
library(feather)
suppressMessages(library(visNetwork))
library(knitr)
suppressMessages(library(DT))

devtools::install_github("wch/webshot")
webshot::install_phantomjs()

df_pkgs <- read_feather("/home/sillas/R/Projetos/PaixaoPorDados/sillasgonzaga.github.io/data/df_pkgs.feather")
df_pkgs %<>% top_n(50, wt = downloads)
(list_pkgs <- df_pkgs$package)

g <- makeDepGraph(list_pkgs, suggests = FALSE)

plot(g, main = "")

plot.igraph(g, vertex.size=10, layout = layout_with_fr(g)) 


# baixar lista de pacotes disponíveis no CRAN
#download.file("http://cran.r-project.org/web/packages/packages.rds", "packages.rds") cerca de 3 MB
rds <- readRDS(file="packages.rds")
data <- as.data.frame(rds, stringsAsFactors = FALSE)

# Limpar os dados
data <- data[,!duplicated(names(data))]
names(data) <- gsub(" ","_", names(data))
names(data) <- gsub("/","_", names(data))
names(data) <- gsub("@","_", names(data))

# Filtrar pacotes do Hadley
hadley <- data %>%
  filter(grepl("Hadley Wickham|Hadley\nWickham", Author)) %>%
  select(Package, Author, Depends, Imports, Suggests, LinkingTo, Enhances)

#Adicionar atributo de grupo aos vértices do grafo
V(g)$group <- ifelse(V(g)$name %in% hadley$Package, "hadleyverse", "no_hadley")

visIgraph(g, physics = TRUE, smooth = TRUE, type = "full", randomSeed = 123) %>%
  visOptions(width = "100%", height = "90%",
             highlightNearest = list(enabled = TRUE, degree = 1, hover = TRUE),
             nodesIdSelection = list(enabled = TRUE)) %>%
  visInteraction(hover = TRUE, navigationButtons = TRUE) %>%
  visGroups()

indegree <- degree(g, mode = "in")
outdegree <- degree(g, mode = "out")
incloseness <- closeness(g, mode = "in") %>% round(6)
outcloseness <- closeness(g, mode = "out") %>% round(6)
betweenness <- betweenness(g) %>% round(1)
eigenvector <- eigen_centrality(g, directed = FALSE) %$% vector %>% round(3)

df_sna <- data.frame(indegree, outdegree, incloseness, outcloseness, betweenness, eigenvector)
# criar tabela interativa com pacote DT
datatable(df_sna)


