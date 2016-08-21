require(animation)
require(network)

full_no_zero<-as.network(full_no_zero, matrix.type == "edgelist")

jitter.ani <-function(x, g){
  
  l <- layout.kamada.kawai(g, niter=1000)
  ebc <- edge.betweenness.community(g)
  
  colbar <- rainbow(6)
  colbar2 <- c(rainbow(5), rep("black",15))
  
  for (i in 1:x) {
    g2 <- delete.edges(g, ebc$removed.edges[seq(length=i-1)])
    eb <- edge.betweenness(g2)
    cl <- clusters(g2)$membership
    q <- modularity(g, cl)
    E(g2)$color <- "grey"
    E(g2)[ order(eb, decreasing=TRUE)[1:5]-1 ]$color <- colbar2[1:5]
    
    E(g2)$width <- 1
    E(g2)[ color != "grey" ]$width <- 2
    
    plot(g2, layout=l, vertex.size=12,
         edge.label.color="red", vertex.color=colbar[cl+2],
         edge.label.font=2)
    title(main=paste("Q=", round(q,3)), font=2)
    ty <- seq(1,by=-strheight("1")*1.5, length=20)
    text(-1.3, ty, adj=c(0,0.5), round(sort(eb, dec=TRUE)[1:20],2),
         col=colbar2, font=2)
  }
}

saveGIF(jitter.ani(20, full_no_zero), interval = 0.5, outdir = getwd())
        