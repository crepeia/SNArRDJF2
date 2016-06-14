full <- graph.data.frame(full_nonzero_edges) 
summary(full)

full2 <- graph.data.frame(full_data_frame) 
summary(full)

get.edge.attribute(full, 'var0')

full_symmetrized <- as.undirected(full, mode='collapse')

V(full)


for (i in V(full)) {
  for (j in names(attributes2)) {
    full <- set.vertex.attribute(full, 
                                 j, 
                                 index = i, 
                                 attributes2[i + 1, j])
  }
}


attributes3<-attributes2
full3<-full
attributes3 = cbind(1:length(attributes3[,1]), attributes3)
full4 <- graph.data.frame(d = full_nonzero_edges, vertices = attributes3)


krack_advice_only <- delete.edges(full, 
                                  E(full)[get.edge.attribute(full,
                                                             name = "var1") == 0])
summary(krack_advice_only)
plot(krack_advice_only)

krack_reports_to_only <- delete.edges(full, 
                                      E(full)[get.edge.attribute(full, 
                                                                 name = "total") == 0])
summary(krack_reports_to_only)



```



```{r}
g<-graph_from_adjacency_matrix(confianca, mode="directed",weighted=T,diag=F)
V(g)$label <- V(g)$name
V(g)$degree <- degree(g)

# set seed to make the layout reproducible
set.seed(3952)
layout1 <- layout.fruchterman.reingold(g)
plot(g, layout=layout1)


plot(g, layout=layout.kamada.kawai)
V(g)$label <- V(g)$name
V(g)$degree <- degree(g)


# construct the nodes and edges data for gexf conversion
nodes <- data.frame(cbind(V(g), as.character(V(g))))
edges <- t(Vectorize(get.edge, vectorize.args='id')(g, 1:ecount(g)))



objectlinks<-igraph_to_networkD3(gMatriz_23.csv, what="links", charge=-500)
objectnodes<-igraph_to_networkD3(gMatriz_23.csv, what="nodes")

```

