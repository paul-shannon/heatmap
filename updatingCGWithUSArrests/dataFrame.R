library(gplots)
library(jsonlite)
#--------------------------------------------------------------------------------
dataFrame <<- function() {

   #creating matrix from USArrests Data
   set.seed(37)
   rows.of.interest <<- sample(1:nrow(USArrests), size=3)
   mtx <<- as.matrix(USArrests[rows.of.interest,])
   #heatmap.2(mtx, trace="none", col=rev(heat.colors(10)), margins=c(20, 20))
   hc1 <<- hclust(dist(mtx))
   hc2 <<- hclust(dist(t(mtx)))

   treeMtx <<- cutree(hc1, k=1:3)
   treeMtx2 <<- cutree(hc2, k=1:3)

   rowname <<- hc1$labels
   rowclust <<- hc1$order
   rowrank <<- hc1$order
   rowgroup <<- I(list(treeMtx[1,], treeMtx[2,], treeMtx[3,])) #turn into for-loop that fills in a list, use test.R

   row_nodes <<- data.frame(name=rowname,
                            clust=rowclust,
			    rank=rowrank,
			    group=rowgroup,
			    stringsAsFactors=FALSE)

   colname <<- hc2$labels
   colclust <<- hc2$order
   colrank <<- hc2$order
   colgroup <<- I(list(treeMtx2[1,], treeMtx2[2,], treeMtx2[3,], treeMtx2[4,])) #same as above

   col_nodes <<- data.frame(name=colname,
                            clust=colclust,
			    rank=colrank,
			    group=colgroup,
			    stringsAsFactors=FALSE)

   mat <<- unname(mtx, force=FALSE)

   dataFrameRaw <<- list(row_nodes=row_nodes, col_nodes=col_nodes, mat=mat)
   dataFramejson <<- toJSON(dataFrameRaw)

   return(dataFrameRaw) #to check if the RUnit test find true that matrix is returned

   }#dataFrame
#--------------------------------------------------------------------------------
