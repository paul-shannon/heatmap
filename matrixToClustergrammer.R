library(gplots)
library(jsonlite)
library(RUnit)
#--------------------------------------------------------------------------------
runTests <- function() {

    mtx <- matrix(c(1:9), nrow=3, dimnames=list(c("R1","R2","R3"), c("C1","C2","C3")))

    test_rowFill(mtx)
    test_colFill(mtx)
    test_simpleMatrix(mtx)
    test_addRowMetaData(mtx)
    test_addColumnMetaData(mtx)

    }#runTests
#--------------------------------------------------------------------------------
matrixToClustergrammerList <- function(mtx, tbl.rowmd=NULL, tbl.colmd=NULL) {

    hc1 <- hclust(dist(mtx))
    hc2 <- hclust(dist(t(mtx)))

    treeMtx <- cutree(hc1, k=1:3) #change based on size of matrix, max k-value = 11
    treeMtx2 <- cutree(hc2, k=1:ncol(mtx)) # " "

    rowname <- hc1$labels[hc1$order]
    rowclust <- 1:nrow(treeMtx)
    #rowrank <- hc1$order

    #browser()
    row_nodes <- data.frame(name=rowname,
                            clust=rowclust,
  		 	    #rank=rowrank,
 			    group=rowFill(treeMtx, hc1), #function that fills list called rowgroup with rows from treeMtx
                            stringsAsFactors=FALSE,
                            check.names=FALSE)

    colname <- hc2$labels[hc2$order]
    colclust <- 1:nrow(treeMtx2)
    #colrank <- hc2$order

    col_nodes <- data.frame(name=colname,
                            clust=colclust,
 			    #rank=colrank,
                            group=colFill(treeMtx2, hc2), #function that fills list called colgroup with rows from treeMtx2
                            stringsAsFactors=FALSE,
                            check.names=FALSE)        
    
    mat <- unname(mtx, force=FALSE)
    mat <- mat[hc1$order, hc2$order]

    list.cg <- list(row_nodes=row_nodes, col_nodes=col_nodes, mat=mat)

    if(!is.null(tbl.rowmd)) {
        list.cg <- addRowMetaData(list.cg, tbl.rowmd, hc1)
        }
    if(!is.null(tbl.colmd)) {
        list.cg <- addColumnMetaData(list.cg, tbl.colmd, hc2)
        }
#browser()
    list.cg
    
    }#matrixToClustergrammer
#--------------------------------------------------------------------------------
rowFill <- function(treeMtx, hc1) {
    
              rowgroup <- list()
              for(i in 1:nrow(treeMtx)) {
                 rowgroup[[i]] <- rev(treeMtx[hc1$order[i],])
                 }
              return(I(rowgroup))
              
              }#row names
#--------------------------------------------------------------------------------
colFill <- function(treeMtx2, hc2) {
    
              colgroup <- list()
              for(i in 1:nrow(treeMtx2)) {
                 colgroup[[i]] <- rev(treeMtx2[hc2$order[i],])
                 }
              return(I(colgroup))

              }#column names
#--------------------------------------------------------------------------------
addColumnMetaData <- function(list.cg, tbl.colmd, hc2) {
    
    list.cg$col_nodes$"cat-0" <- tbl.colmd[,1]
    list.cg$col_nodes$"cat-0" <- list.cg$col_nodes$"cat-0"[hc2$order]
    list.cg$col_nodes$"cat-0" <- paste(sep='', colnames(tbl.colmd[1]), ': ', list.cg$col_nodes$"cat-0")
    list.cg$col_nodes$"cat-1" <- tbl.colmd[,2]
    list.cg$col_nodes$"cat-1" <- list.cg$col_nodes$"cat-1"[hc2$order]
    list.cg$col_nodes$"cat-1" <- paste(sep='', colnames(tbl.colmd[2]), ': ', list.cg$col_nodes$"cat-1")

    return(list.cg)
    
    }#addColumnMetaData
#--------------------------------------------------------------------------------
addRowMetaData <- function(list.cg, tbl.rowmd, hc1) {

    list.cg$row_nodes$"cat-0" <- tbl.rowmd[,1]
    list.cg$row_nodes$"cat-0" <- list.cg$row_nodes$"cat-0"[hc1$order]
    list.cg$row_nodes$"cat-0" <- paste(sep='', colnames(tbl.rowmd), ': ', list.cg$row_nodes$"cat-0")

    return(list.cg)
    
    }#addRowMetaData
#--------------------------------------------------------------------------------
simpleMatrix <- function() {
    mtx <- matrix(c(1:9), nrow=3, dimnames=list(c("R1","R2","R3"), c("C1","C2","C3")))
    tbl.rowmd <- data.frame(row.names=c("R1","R2","R3"),
                            Placement=c("First", "Second", "Third"),
                            stringsAsFactors=FALSE)
    tbl.colmd <- data.frame(row.names=c("C1","C2","C3"),
                            Placement=c("One", "Two", "Three"),
                            Color=c("Blue", "Yellow", "Red"),
                            stringsAsFactors=FALSE)
    
    list.cg <- matrixToClustergrammerList(mtx, tbl.rowmd, tbl.colmd)

    return(list.cg)
    }#simpleMatrix
#--------------------------------------------------------------------------------
USArrestsExample <- function() {
    
    set.seed(37)
    rows.of.interest <- sample(1:nrow(USArrests), size=3)
    mtx <- as.matrix(USArrests[rows.of.interest,])
    
    tbl.rowmd <- data.frame(row.names=c("Nevada", "Arkansas", "New York"),
                            "Region"=c("South West", "South", "North East"),
                            stringsAsFactors=FALSE)
    tbl.colmd <- data.frame(row.names=c("Murder", "Assault", "UrbanPop", "Rape"),
                            Severity=c("Terrible", "Pretty Bad", "N/A", "Terrible"),
                            Jail_Time=c("20 Years", "1-5 Years", "N/A", "5-10 Years"),
                            stringsAsFactors=FALSE)

    list.cg <- matrixToClustergrammerList(mtx, tbl.rowmd, tbl.colmd)

    return(list.cg)
    
    }#USArrestsExample
#--------------------------------------------------------------------------------
test_simpleMatrix <- function(mtx) {

    printf("=== test_simpleMatrix")
    
    list.cg <- matrixToClustergrammerList(mtx)
    hc1 <- hclust(dist(mtx))
    hc2 <- hclust(dist(t(mtx)))

    checkTrue(is.list(list.cg))
    checkEquals((list.cg$row_nodes$name), rownames(mtx[hc1$order, hc2$order]))
    checkEquals((list.cg$col_nodes$name), colnames(mtx[hc1$order, hc2$order]))
    
    }# test_simpleMatrix
#--------------------------------------------------------------------------------
test_addRowMetaData <- function(mtx) {

    printf("=== test_addRowMetaData")
    
    list.cg <- matrixToClustergrammerList(mtx)
    hc1 <- hclust(dist(mtx))
    hc2 <- hclust(dist(t(mtx)))


    tbl.rowmd <- data.frame(row.names=c("R1","R2","R3"), Placement=c("First", "Second", "Third"), stringsAsFactors=FALSE)
    list.cg <- addRowMetaData(list.cg, tbl.rowmd)

    checkTrue(is.list(list.cg))
    checkEquals((list.cg$row_nodes$name), rownames(mtx[hc1$order, hc2$order]))
    checkEquals((list.cg$col_nodes$name), colnames(mtx[hc1$order, hc2$order]))
    
    }#test_addRowMetaData
#--------------------------------------------------------------------------------
test_addColumnMetaData <- function(mtx) {

    printf("=== test_addColumnMetaData")

    list.cg <- matrixToClustergrammerList(mtx)
    hc1 <- hclust(dist(mtx))
    hc2 <- hclust(dist(t(mtx)))
    
    tbl.colmd <- data.frame(row.names=c("C1","C2","C3"), Placement=c("One", "Two", "Three"), stringsAsFactors=FALSE)
    list.cg <- addColumnMetaData(list.cg, tbl.colmd)

    checkTrue(is.list(list.cg))
    checkEquals((list.cg$row_nodes$name), rownames(mtx[hc1$order, hc2$order]))
    checkEquals((list.cg$col_nodes$name), colnames(mtx[hc1$order, hc2$order]))
    
    }#test_addColumnMetaData
#--------------------------------------------------------------------------------
test_rowFill <- function(mtx) {

    printf("=== test_rowFill")
    
    hc1 <- hclust(dist(mtx))
    treeMtx <- cutree(hc1, k=1:3) #change based on size of matrix, max k-value = 11

    checkTrue(is.list(rowFill(treeMtx, hc1)))
    
    }#test_rowFill
#--------------------------------------------------------------------------------
test_colFill <- function(mtx) {

    printf("=== test_colFill")
    
    hc2 <- hclust(dist(t(mtx)))
    treeMtx2 <- cutree(hc2, k=1:3)
    
    checkTrue(is.list(colFill(treeMtx2, hc2)))

    }#test_colFill
#--------------------------------------------------------------------------------
