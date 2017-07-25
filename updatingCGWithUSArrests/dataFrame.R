library(gplots)
library(jsonlite)
library(RUnit)

mtx <- NULL #as.matrix(USArrests) as example
#--------------------------------------------------------------------------------
USArrestsSmall <- function() {

   #creating sample matrix from USArrests Data
   set.seed(37)
   rows.of.interest <- sample(1:nrow(USArrests), size=3)
   mtx <- as.matrix(USArrests[rows.of.interest,])
   #heatmap.2(mtx, trace="none", col=rev(heat.colors(10)), margins=c(20, 20))
   hc1 <- hclust(dist(mtx))
   hc2 <- hclust(dist(t(mtx)))

   treeMtx <- cutree(hc1, k=1:nrow(mtx))
   treeMtx2 <- cutree(hc2, k=1:ncol(mtx))

   rowname <- hc1$labels
   rowclust <- hc1$order
   rowrank <- hc1$order
   rev_hc1 <- rev(hc1$order) 
   rowgroup <- list()
   rowFill <- function() {
                  for(i in 1:nrow(treeMtx)) {
                     rowgroup[[i]] <- rev(treeMtx[rev_hc1[i],])
                     }
                  return(I(rowgroup))
                  }

   row_nodes <- data.frame(name=rowname,
                            clust=rowclust,
			    rank=rowrank,
			    group=rowFill(), #function that fills list called rowgroup with rows from treeMtx
			    stringsAsFactors=FALSE)

   colname <- hc2$labels
   colclust <- hc2$order
   colrank <- hc2$order
   rev_hc2 <- rev(hc2$order) 
   colgroup <- list()
   colFill <- function() {
                  for(i in 1:nrow(treeMtx2)) {
                     colgroup[[i]] <- rev(treeMtx2[rev_hc2[i],])
                     }
                  return(I(colgroup))
                  }

   col_nodes <- data.frame(name=colname,
                            clust=colclust,
			    rank=colrank,
			    group=colFill(), #function that fills list called colgroup with rows from treeMtx2
			    stringsAsFactors=FALSE)

   mat <- unname(mtx, force=FALSE)

   rawListSmall <- list(row_nodes=row_nodes, col_nodes=col_nodes, mat=mat)
   dataFramejson <- toJSON(rawListSmall)

   return(rawListSmall)
   return(dataFramejson) 
   
   }#USArrestsSmall
#--------------------------------------------------------------------------------
matrixToClusterGrammer <- function(mtx) {
    
   #mtx <<- as.matrix(USArrests) #changed from USArrests[rows.of.interest]
   #heatmap.2(mtx, trace="none", col=rev(heat.colors(10)), margins=c(20, 20))

   hc1 <- hclust(dist(mtx))
   hc2 <- hclust(dist(t(mtx)))

   treeMtx <- cutree(hc1, k=1:nrow(mtx))
   treeMtx2 <- cutree(hc2, k=1:ncol(mtx))

   rowname <- hc1$labels
   rowclust <- hc1$order
   rowrank <- hc1$order
   hc1_rev <- rev(hc1$order) 
   rowgroup <- list()
   rowFill <- function() {
                  for(i in 1:nrow(treeMtx)) {
                     rowgroup[[i]] <- rev(treeMtx[hc1_rev[i],])
                     }
                  return(I(rowgroup))
                  }

   row_nodes <<- data.frame(name=rowname,
                            clust=rowclust,
			    rank=rowrank,
			    group=rowFill(), #function that fills list called rowgroup with rows from treeMtx
			    stringsAsFactors=FALSE)

   colname <- hc2$labels
   colclust <- hc2$order
   colrank <- hc2$order
   hc2_rev <- rev(hc2$order) 
   colgroup <- list()
   colFill <- function() {
                  for(i in 1:nrow(treeMtx2)) {
                     colgroup[[i]] <- rev(treeMtx2[hc2_rev[i],])
                     }
                  return(I(colgroup))
                  }

   col_nodes <<- data.frame(name=colname,
                            clust=colclust,
			    rank=colrank,
			    group=colFill(), #function that fills list called colgroup with rows from treeMtx2
			    stringsAsFactors=FALSE)

   mat <<- unname(mtx, force=FALSE)

   rawList <<- list(row_nodes=row_nodes, col_nodes=col_nodes, mat=mat)
   listToJson <- toJSON(rawList)

   return(rawList)
   return(listToJson)
   return(mat)
   return(col_nodes)
   return(row_nodes) 
    
   }#matrixToClusterGrammer
#--------------------------------------------------------------------------------
test_matrixToClusterGrammer <- function() {
    checkTrue(is.list(rawList))
    checkTrue(is.matrix(mat))
    checkTrue(is.data.frame(col_nodes))
    checkTrue(is.data.frame(row_nodes))
    }#test_matrixToClusterGrammer
#--------------------------------------------------------------------------------
