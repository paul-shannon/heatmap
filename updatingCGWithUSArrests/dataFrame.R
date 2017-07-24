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
   treeMtx2 <<- cutree(hc2, k=1:4)

   rowname <<- hc1$labels
   rowclust <<- hc1$order
   rowrank <<- hc1$order
   rowgroup <<- list()
   rowFill <<- function() {
                  for(i in 1:nrow(treeMtx)) {
                     rowgroup[[i]] <<- treeMtx[i,]
                     }
                  return(I(rowgroup))
                  }

   row_nodes <<- data.frame(name=rowname,
                            clust=rowclust,
			    rank=rowrank,
			    group=rowFill(), #function that fills list called rowgroup with rows from treeMtx
			    stringsAsFactors=FALSE)

   colname <<- hc2$labels
   colclust <<- hc2$order
   colrank <<- hc2$order
   colgroup <<- list()
   colFill <<- function() {
                  for(i in 1:nrow(treeMtx2)) {
                     colgroup[[i]] <<- treeMtx2[i,]
                     }
                  return(I(colgroup))
                  }

   col_nodes <<- data.frame(name=colname,
                            clust=colclust,
			    rank=colrank,
			    group=colFill(), #function that fills list called colgroup with rows from treeMtx2
			    stringsAsFactors=FALSE)

   mat <<- unname(mtx, force=FALSE)

   dataFrameRaw <<- list(row_nodes=row_nodes, col_nodes=col_nodes, mat=mat)
   dataFramejson <<- toJSON(dataFrameRaw)
   
   }#dataFrame
#--------------------------------------------------------------------------------
