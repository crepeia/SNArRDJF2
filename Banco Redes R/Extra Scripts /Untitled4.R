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

visIgraph(full_no_zero, physics = TRUE, smooth = TRUE, type = "full", randomSeed = 123) %>%
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



```{r}
# Re-generate dataframes for both nodes and edges, now containing
require(ggplot2)
# calculated network attributes

node_list <- get.data.frame(full_no_zero, what = "vertices")

#Writing Node CSV 
write.csv2(node_list, "~/SNArRDJF/Banco Redes R/Networks/node_full_no_zero.csv", fileEncoding = "macintosh")

# Edge List
edge_list <- get.data.frame(full_no_zero, what = "edges") %>%
  inner_join(node_list %>% select(name, walktrap.community), by = c("from" = "name")) %>%
  inner_join(node_list %>% select(name, walktrap.community), by = c("to" = "name")) %>%
  mutate(group = ifelse(walktrap.community.x == walktrap.community.y, walktrap.community.x, NA) %>% factor())

#Writing Edge List CSV
write.csv2(edge_list, "~/SNArRDJF/Banco Redes R/Networks/edge_full_no_zero.csv", fileEncoding = "macintosh")


# Create a character vector containing every node name
all_nodes <- sort(node_list$name)

# Adjust the 'to' and 'from' factor levels so they are equal
# to this complete list of node names
plot_data <- edge_list %>% mutate(
  to = factor(to, levels = all_nodes),
  from = factor(from, levels = all_nodes))

# Create the adjacency matrix plot
ggplot(plot_data, aes(x = from, y = to, fill = group)) +
  geom_raster() +
  theme_bw() +
  # Because we need the x and y axis to display every node,
  # not just the nodes that have connections to each other,
  # make sure that ggplot does not drop unused factor levels
  scale_x_discrete(drop = FALSE) +
  scale_y_discrete(drop = FALSE) +
  theme(
    # Rotate the x-axis lables so they are legible
    axis.text.x = element_text(angle = 270, hjust = 0),
    # Force the plot into a square aspect ratio
    aspect.ratio = 1,
    # Hide the legend (optional)
    legend.position = "none")


#Create a character vector of node names sorted by their
# community membership. Here, I rearrange the node_list
# table by the "comm" variable, then extract the
# "name" vector
name_order <- (node_list %>% arrange(walktrap.community))$name

# Reorder edge_list "from" and "to" factor levels based on
# this new name_order
plot_data <- edge_list %>% mutate(
  to = factor(to, levels = name_order),
  from = factor(from, levels = name_order))


# Create the adjacency matrix plot
ggplot(plot_data, aes(x = from, y = to, fill = group)) +
  geom_raster() +
  theme_bw() +
  # Because we need the x and y axis to display every node,
  # not just the nodes that have connections to each other,
  # make sure that ggplot does not drop unused factor levels
  scale_x_discrete(drop = FALSE) +
  scale_y_discrete(drop = FALSE) +
  theme(
    # Rotate the x-axis lables so they are legible
    axis.text.x = element_text(angle = 270, hjust = 0),
    # Force the plot into a square aspect ratio
    aspect.ratio = 1,
    # Hide the legend (optional)
    legend.position = "none")

# Now run the ggplot code again
```



