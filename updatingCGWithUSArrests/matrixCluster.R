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
			    group=rowFill(), #function that fills list called rowgroXup with rows from treeMtx
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

   mat <- unname(data.frame(mtx[hc1$order, hc2$order]), force=FALSE)
   #newCol <- c(1:nrow(mtx))
   mat$newCol <- c(1:nrow(mtx))
   mat <- unname(mat) 
    
   #mat <- data.frame(mat[hc1$order, hc2$order])
   printf(class(mat)) 

   rawListSmall <- list(row_nodes=row_nodes, col_nodes=col_nodes, mat=mat)
   print(toJSON(rawListSmall))

   return(rawListSmall)
   #return(dataFramejson)

   }#USArrestsSmall
#--------------------------------------------------------------------------------
matrixToClustergrammer <- function(mtx, colGroups=diagnosis.groups) {

  #heatmap.2(mtx, trace="none", col=rev(heat.colors(10)), margins=c(300, 100))

   hc1 <- hclust(dist(mtx))
   hc2 <- hclust(dist(t(mtx)))

   treeMtx <- cutree(hc1, k=1:11) #change based on size of matrix
   treeMtx2 <- cutree(hc2, k=1:8) # " "      

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

   row_nodes <- data.frame(name=rowname,
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

   colGroups <- diagnosis.groups
   
   #forloop for colGroups here
   if(nrow(rowGroups) > 0) {
      printf("success")# add the cat-0, cat-1, … for each kind of metadata (group data) for each gene
      
             
      }
   if(nrow(colGroups) > 0) {
      # add the cat-0, cat-1, … for each kind of metadata (group data) for each sample
      printf("double Success")
      }

    
   mat <- unname(mtx, force=FALSE)
   mat <- mat[hc1$order, hc2$order]

   #browser()

   rawList <- list(row_nodes=row_nodes, col_nodes=col_nodes, mat=mat)
   listToJson <- toJSON(rawList)

   return(rawList)

   }#matrixToClustergrammer
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
test_bioMatrix <- function() {
    
    printf("— test_bioMatrix")
    print(load("clustergrammer/matrixToClusteredJSON/mtx.248.8.RData"))

    mtx <- mtx.248.8
    x <- matrixToClusterGrammer(mtx)
    printf("Success")

    #print(x)
    #print(toJSON(x))

    }#test_BioMatrix
#--------------------------------------------------------------------------------
test_microGlial <- function() {
    
    printf("--- test_microGlial")
    print(load("clustergrammer/matrixToClusteredJSON/mtx.microglial.RData"))

    mtx <- mtx.microglial
    print(dim(mtx))

    tbl.meta <- read.table("MayoRNAseq_RNAseq_TCX_covariates.csv", sep=",", as.is=TRUE, header=TRUE) #Meta-Data
    diagnosis.groups <- tbl.meta$Diagnosis
    names(diagnosis.groups) <- tbl.meta$ID
    print(head(diagnosis.groups))
    
    x <- matrixToClusterGrammer(mtx, colGroups=diagnosis.groups)
    print(nchar(toJSON(x)))

    text <- sprintf("cgStructure = %s", toJSON(x))

    writeLines(text, "clustergrammer/requirejs/microglial.js")
    
    }#test_microGlial
#--------------------------------------------------------------------------------
