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
   heatmap.2(mtx, trace="none", col=rev(heat.colors(10)), margins=c(20, 20))
   hc1 <<- hclust(dist(mtx))
   hc2 <<- hclust(dist(t(mtx)))

   treeMtx <<- cutree(hc1, k=1:nrow(mtx))
   treeMtx2 <<- cutree(hc2, k=1:ncol(mtx))

   rowname <- hc1$labels[hc1$order]
   rowclust <- 1:nrow(treeMtx)
   #rowrank <- hc1$order
   rev_hc1 <- rev(hc1$order)
   rowgroup <- list()
   rowFill <- function() {
                  for(i in 1:nrow(treeMtx)) {
                     rowgroup[[i]] <- rev(treeMtx[hc1$order[i],])
                     }
                  return(I(rowgroup))
                  }

   row_nodes <- data.frame(name=rowname,
                            clust=rowclust,
			    #rank=rowrank,
			    group=rowFill(), #function that fills list called rowgroup with rows from treeMtx
			    stringsAsFactors=FALSE)

   colname <- hc2$labels[hc2$order]
   colclust <- 1:nrow(treeMtx2)
   #colrank <- hc2$order
   rev_hc2 <- rev(hc2$order)
   colgroup <- list()
   colFill <- function() {
                  for(i in 1:nrow(treeMtx2)) {
                     colgroup[[i]] <- rev(treeMtx2[hc2$order[i],])
                     }
                  return(I(colgroup))
                  }

   col_nodes <- data.frame(name=colname,
                            clust=colclust,
			    #rank=colrank,
			    group=colFill(), #function that fills list called colgroup with rows from treeMtx2
			    stringsAsFactors=FALSE)

   mat <- unname(mtx, force=FALSE)
   mat <- mat[hc1$order, hc2$order]

   rawListSmall <- list(row_nodes=row_nodes, col_nodes=col_nodes, mat=mat)
   dataFramejson <<- toJSON(rawListSmall)

   return(rawListSmall)
   #return(dataFramejson)

   }#USArrestsSmall
#--------------------------------------------------------------------------------
matrixToClusterGrammer <- function(mtx) {

   hc1 <- hclust(dist(mtx))
   hc2 <- hclust(dist(t(mtx)))

   treeMtx <- cutree(hc1, k=1:11)
   treeMtx2 <- cutree(hc2, k=1:ncol(mtx))

   rowname <- hc1$labels[hc1$order]
   rowclust <- 1:nrow(treeMtx)
   #rowrank <- hc1$order
   rowgroup <- list()
   rowFill <- function() {
                  for(i in 1:nrow(treeMtx)) {
                     rowgroup[[i]] <- rev(treeMtx[hc1$order[i],])
                     }
                  return(I(rowgroup))
                  }

   row_nodes <<- data.frame(name=rowname,
                            clust=rowclust,
			    #rank=rowrank,
			    group=rowFill(), #function that fills list called rowgroup with rows from treeMtx
			    stringsAsFactors=FALSE)

   #browser()

   colname <- hc2$labels[hc2$order]
   colclust <- 1:nrow(treeMtx2)
   #colrank <- hc2$order
   colgroup <- list()
   colFill <- function() {
                  for(i in 1:nrow(treeMtx2)) {
                     colgroup[[i]] <- rev(treeMtx2[hc2$order[i],])
                     }
                  return(I(colgroup))
                  }

   col_nodes <- data.frame(name=colname,
                            clust=colclust,
			    #rank=colrank,
			    group=colFill(), #function that fills list called colgroup with rows from treeMtx2
			    stringsAsFactors=FALSE)

   mat <- unname(mtx, force=FALSE)
   mat <- mat[hc1$order, hc2$order]

   #browser()

   rawList <- list(row_nodes=row_nodes, col_nodes=col_nodes, mat=mat)
   listToJson <- toJSON(rawList)

   return(rawList)

   }#matrixToClusterGrammer
#--------------------------------------------------------------------------------
test_matrixToClusterGrammer <- function() {

    mtx <- as.matrix(USArrests)

    x <- matrixToClusterGrammer(mtx)

    print(x)
    print(toJSON(x))
    #print(mtx)

    #TESTS
    checkTrue(is.list(x))

    checkTrue(is.data.frame(col_nodes))
    checkTrue(is.data.frame(col_nodes))
    checkTrue(is.list(col_nodes[1,4]))
    checkTrue(is.character(col_nodes[1,1]))

    checkTrue(is.matrix(mat))
    checkEquals(dim(rawList$mat), c(3, 4))
    checkEquals(mat[1,1], 12.2)
    checkEquals(mat[3,4], 26.1)

    }#test_matrixToClusterGrammer
#--------------------------------------------------------------------------------
test_USArrestsSmall <- function() {

    set.seed(37)
    rows.of.interest <- sample(1:nrow(USArrests), size=3)
    mtx <- as.matrix(USArrests[rows.of.interest,])

    x <- matrixToClusterGrammer(mtx)

    #print(x)
    print(toJSON(x))
    print(mtx)

    #TESTS
    checkTrue(is.list(x))

    checkTrue(is.data.frame(col_nodes))
    checkTrue(is.data.frame(col_nodes))
    checkTrue(is.list(col_nodes[1,4]))
    checkTrue(is.character(col_nodes[1,1]))

    checkTrue(is.matrix(mat))
    checkEquals(dim(rawList$mat), c(3, 4))
    checkEquals(mat[1,1], 12.2)
    checkEquals(mat[3,4], 26.1)

    }#test_USArrestsSmall
#--------------------------------------------------------------------------------
test_BioMatrix <- function() {
    printf("\ test_clusteringToJSON_mediumSizedMatrix")
    print(load("clustergrammer/matrixToClusteredJSON/mtx.248.8.RData"))

    mtx <- mtx.248.8

    x <- matrixToClusterGrammer(mtx)

    #print(x)
    print(toJSON(x))

    }#test_BioMatrix
#--------------------------------------------------------------------------------
