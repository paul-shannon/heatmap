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
matrixToClustergrammerList <- function(mtx, tbl.rowmd=NULL, tbl.colmd=NULL, r=ncol(tbl.rowmd), c=ncol(tbl.colmd)) {
    
    hc.rows <- hclust(dist(mtx))
    hc.cols <- hclust(dist(t(mtx)))

    treeMtx.rows <- cutree(hc.rows, k=1:3) #change based on size of matrix, max k-value = 11
    treeMtx.cols <- cutree(hc.cols, k=1:ncol(mtx)) # " "

    rowname <- hc.rows$labels[hc.rows$order]
    rowclust <- 1:nrow(treeMtx.rows)
    #rowrank <- hc.rows$order

    row_nodes <- data.frame(name=rowname,
                            clust=rowclust,
  		 	    #rank=rowrank,
 			    group=rowFill(treeMtx.rows, hc.rows), #function that fills list called rowgroup with rows from treeMtx.rows
                            stringsAsFactors=FALSE,
                            check.names=FALSE)

    colname <- hc.cols$labels[hc.cols$order]
    colclust <- 1:nrow(treeMtx.cols)
    #colrank <- hc.cols$order

    col_nodes <- data.frame(name=colname,
                            clust=colclust,
 			    #rank=colrank,
                            group=colFill(treeMtx.cols, hc.cols), #function that fills list called colgroup with rows from treeMtx.cols
                            stringsAsFactors=FALSE,
                            check.names=FALSE)        

    mat <- unname(mtx, force=FALSE)
    mat <- mat[hc.rows$order, hc.cols$order]

    list.cg <- list(row_nodes=row_nodes, col_nodes=col_nodes, mat=mat)

    if(!is.null(tbl.rowmd)) {
        list.cg <- addRowMetaData(list.cg, tbl.rowmd, hc.rows, r)
        }
    if(!is.null(tbl.colmd)) {
        list.cg <- addColumnMetaData(list.cg, tbl.colmd, hc.cols, c)
        }
#browser()
    list.cg
    
    }#matrixToClustergrammer
#--------------------------------------------------------------------------------
rowFill <- function(treeMtx.rows, hc.rows) {
    
              rowgroup <- list()
              for(i in 1:nrow(treeMtx.rows)) {
                 rowgroup[[i]] <- rev(treeMtx.rows[hc.rows$order[i],])
                 }
              return(I(rowgroup))
              
              }#row names
#--------------------------------------------------------------------------------
colFill <- function(treeMtx.cols, hc.cols) {
    
              colgroup <- list()
              for(i in 1:nrow(treeMtx.cols)) {
                 colgroup[[i]] <- rev(treeMtx.cols[hc.cols$order[i],])
                 }
              return(I(colgroup))

              }#column names
#--------------------------------------------------------------------------------
old.addColumnMetaData <- function(list.cg, tbl.colmd, hc.cols, c) {
    
    list.cg$col_nodes$"cat-0" <- paste(sep='', colnames(tbl.colmd[1]), ': ', tbl.colmd[,1][hc.cols$order])
        
    #list.cg$col_nodes$"cat-1" <- paste(sep='', colnames(tbl.colmd[2]), ': ', tbl.colmd[,2][hc.cols$order])

    return(list.cg)
    
}#old.addColumnMetaData
#--------------------------------------------------------------------------------
addColumnMetaData <- function(list.cg, tbl.colmd, c) {

    desired.order <- match(list.cg$col_nodes$name, rownames(tbl.colmd))
    tbl.colmd <- tbl.colmd[desired.order,,drop=FALSE]

    category.name <- sprintf("cat-%d", c-1)    
    list.cg$col_nodes[category.name] <- paste(sep='', colnames(tbl.colmd)[c], ': ', tbl.colmd[,c])
    
    return(list.cg)
    
    }#addColumnMetaData
#--------------------------------------------------------------------------------
addRowMetaData <- function(list.cg, tbl.rowmd, hc.rows, r) {

    print(list.cg)
    
    list.cg <- for(i in r) {
        i = i - 1
        print(i)
        print("row meta data test")

        #sprintf("cat-%d", i) <- paste(sep='', colnames(tbl.rowmd[1]), ': ', tbl.rowmd[,1][hc.rows$order])
        list.cg <- list.cg$row_nodes$sprintf("cat-%d", i) #<- paste(sep='', colnames(tbl.rowmd[1]), ': ', tbl.rowmd[,1][hc.rows$order])
        #list.cg <- list.cg$row_nodes$i
        }
    print(list.cg)
    
    browser()
    #list.cg$row_nodes$"cat-0" <- paste(sep='', colnames(tbl.rowmd), ': ', tbl.rowmd[,1][hc.rows$order])

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
                            "Jail Time"=c("20 Years", "1-5 Years", "N/A", "5-10 Years"),
                            stringsAsFactors=FALSE)

    list.cg <- matrixToClustergrammerList(mtx, tbl.rowmd)#, tbl.colmd)

    return(list.cg)
    
    }#USArrestsExample
#--------------------------------------------------------------------------------
test_simpleMatrix <- function(mtx) {

    printf("=== test_simpleMatrix")
    
    list.cg <- matrixToClustergrammerList(mtx)
    hc.rows <- hclust(dist(mtx))
    hc.cols <- hclust(dist(t(mtx)))

    checkTrue(is.list(list.cg))
    checkEquals((list.cg$row_nodes$name), rownames(mtx[hc.rows$order, hc.cols$order]))
    checkEquals((list.cg$col_nodes$name), colnames(mtx[hc.rows$order, hc.cols$order]))
    
    }# test_simpleMatrix
#--------------------------------------------------------------------------------
test_addRowMetaData <- function(mtx) {

    mtx <- matrix(c(1:9), nrow=3, dimnames=list(c("R1","R2","R3"), c("C1","C2","C3")))
    printf("=== test_addRowMetaData")
    
    list.cg <- matrixToClustergrammerList(mtx)
    hc.rows <- hclust(dist(mtx))
    hc.cols <- hclust(dist(t(mtx)))


    tbl.rowmd <- data.frame(row.names=c("R1","R2","R3"), Placement=c("First", "Second", "Third"), stringsAsFactors=FALSE)
    list.cg <- addRowMetaData(list.cg, tbl.rowmd, hc.rows, r)

    checkTrue(is.list(list.cg))
    checkEquals((list.cg$row_nodes$name), rownames(mtx[hc.rows$order, hc.cols$order]))
    checkEquals((list.cg$col_nodes$name), colnames(mtx[hc.rows$order, hc.cols$order]))
    
    }#test_addRowMetaData
#--------------------------------------------------------------------------------
test_addColumnMetaData <- function() {

    printf("=== test_addColumnMetaData")
    
    mtx <- matrix(c(1:9), nrow=3, dimnames=list(c("R1","R2","R3"), c("C1","C2","C3")))
    list.cg <- matrixToClustergrammerList(mtx)
    tbl.colmd <- data.frame(row.names=c("C1","C2","C3"),
                            Placement=c("One", "Two", "Three"),
                            Colors=c("Red", "Green", "Blue"),
                            Test=c("w", "x", "y"),
                            stringsAsFactors=FALSE)
    
    #for(i in ncol(tbl.colmd)) { list.cg[[i]] <- addColumnMetaData(list.cg, tbl.colmd, i) }
    list.cg1 <- addColumnMetaData(list.cg, tbl.colmd, 1)
    list.cg <- addColumnMetaData(list.cg1, tbl.colmd, 2)
    list.cg <- addColumnMetaData(list.cg, tbl.colmd, 3)
    
    print((list.cg))
    checkTrue(is.list(list.cg))
    checkEquals((ncol(tbl.colmd)), ncol(list.cg$col_nodes) - 3)
    
    }#test_addColumnMetaData
#--------------------------------------------------------------------------------
test_rowFill <- function(mtx) {

    printf("=== test_rowFill")
    
    hc.rows <- hclust(dist(mtx))
    treeMtx.rows <- cutree(hc.rows, k=1:3) #change based on size of matrix, max k-value = 11

    checkTrue(is.list(rowFill(treeMtx.rows, hc.rows)))
    
    }#test_rowFill
#--------------------------------------------------------------------------------
test_colFill <- function(mtx) {

    printf("=== test_colFill")
    
    hc.cols <- hclust(dist(t(mtx)))
    treeMtx.cols <- cutree(hc.cols, k=1:3)
    
    checkTrue(is.list(colFill(treeMtx.cols, hc.cols)))

    }#test_colFill
#--------------------------------------------------------------------------------
