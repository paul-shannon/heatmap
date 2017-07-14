library(gplots)
cluster.info <- heatmap.2(as.matrix(USArrests), trace="none", col=rev(heat.colors(10)), margins=c(10,8))
print(names(cluster.info))


