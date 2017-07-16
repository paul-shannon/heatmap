library(gplots)   # needed for heatmap.2 function
print(dim(USArrests))
# let's keep the matrix small for study and demo purposes.  choose 3 states at random
# set.seed(somePrimeNumber) ensures that we get the same 6 states every time
set.seed(37)
rows.of.interest <- sample(1:nrow(USArrests), size=3)
mtx <- as.matrix(USArrests[rows.of.interest,])
heatmap.2(mtx,  trace="none", col=rev(heat.colors(10)), margins=c(20,20))

hc1 <- hclust(dist(mtx))      # cluster on state.  read up on dist and hclust (i.e., ?dist and ?hclust)
hc2 <- hclust(dist(t(mtx)))   # cluster on statistical categories
# explore each of the fields in the named list redtured by hclust.
# can you extract the information needed to create suitable input for clustergrammer?
print(names(hc1))
print(names(hc2))
print(hc1$labels)
print(hc2$labels)
