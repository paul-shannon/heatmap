library(gplots)
library(jsonlite)
library(RUnit)
#--------------------------------------------------------------------------------
smallDemo <- function() {

   printf("=== smallDemo")
    
    mtx <- as.matrix(longley) #US GNP data
    tbl.rowmd <- data.frame(row.names=row.names(mtx),
                            Decade=c("Forties", "Forties", "Forties", "Fifties", "Fifties", "Fifties", "Fifties", "Fifties", "Fifties", "Fifties", "Fifties", "Fifties", "Fifties", "Sixties",  "Sixties", "Sixties"),
                            stringsAsFactors=FALSE)
    tbl.colmd <- data.frame(row.names=colnames(mtx),
                            "Population Stats"=c("NA", "NA", "Employment", "Employment", "Total Population", "NA", "Employment"),
                            Metric=c("GNP", "GNP", "Pop", "Pop", "Pop", "NA", "Pop"),
                            stringsAsFactors=FALSE)
    
    list.cg <- matrixToListWithMetaData(mtx, tbl.rowmd, tbl.colmd)

    text <- sprintf("matrixDemo = %s", toJSON(list.cg))
    writeLines(text, "matrixDemo.js")
    browseURL("http://localhost:8090/index.html")

    }#demo
#--------------------------------------------------------------------------------
largeDemo <- function() {

    printf("=== largeDemo")

    print(load("mtx.microglial.RData"))
    mtx <- mtx.microglial
    
    tbl.colMeta <- read.table("MayoRNAseq_RNAseq_TCX_covariates.csv", sep=",", as.is=TRUE, header=TRUE)

    Diagnosis <- tbl.colMeta$Diagnosis
    Age_at_Death <- tbl.colMeta$AgeAtDeath
    Gender <- tbl.colMeta$Gender
    
    colGroups <- data.frame(row.names=tbl.colMeta$ID,
                         Diagnosis,
                         Age_at_Death,
                         Gender,
                         stringsAsFactors=FALSE)

    print(load("tbl.GO.RData"))

    tbl.rowMeta <- tbl.GO
    Process <- tbl.rowMeta$process

    rowGroups <- data.frame(row.names=row.names(tbl.rowMeta),
                            Process,
                            stringsAsFactors=FALSE)
    
    tbl.rowmd <- rowGroups
    tbl.colmd <- colGroups

    list.cg <- matrixToListWithMetaData(mtx, tbl.rowmd, tbl.colmd)
    
    text <- sprintf("matrixDemo = %s", toJSON(list.cg))
    writeLines(text, "matrixDemo.js")
    browseURL("http://localhost:8090/index.html")
    
    }#largeDemo
#--------------------------------------------------------------------------------
runTests <- function() {

    test_rowFill()
    test_colFill()
    test_matrixToClustergrammerList()
    test_addRowMetaData()
    test_addColumnMetaData()
    test_smallMatrixToListWithMetaData()
    test_mediumMatrixToListWithMetaData()
    test_largeMatrixToListWithMetaData()

    }#runTests
#--------------------------------------------------------------------------------
matrixToListWithMetaData <- function(mtx, tbl.rowmd=NULL, tbl.colmd=NULL) {

    list.cg <- matrixToClustergrammerList(mtx)
    
    if(!is.null(tbl.rowmd)) {
        for(i in 1:ncol(tbl.rowmd)) {
            list.cg <- addRowMetaData(list.cg, tbl.rowmd, i)
            }#forloop
        }#row meta data

    if(!is.null(tbl.colmd)) {
        for(i in 1:ncol(tbl.colmd)) {
            list.cg <- addColumnMetaData(list.cg, tbl.colmd, i)
            }#forloop
        }#column meta data

    return(list.cg)
    
    }#matrixToListWithMetaData
#--------------------------------------------------------------------------------
matrixToClustergrammerList <- function(mtx) {
    
    hc.rows <- hclust(dist(mtx))
    hc.cols <- hclust(dist(t(mtx)))

    r <- nrow(mtx)
    c <- ncol(mtx)
    
    if(r > 11) {
        r = 11
        } #optimal k-value = 11
    if(c > 11) {
        c = 11
        } #optimal k-value = 11
 
    treeMtx.rows <- cutree(hc.rows, k=1:r)
    treeMtx.cols <- cutree(hc.cols, k=1:c)

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

    return(list.cg)
    
    }#matrixToClustergrammerList
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
addColumnMetaData <- function(list.cg, tbl.colmd, c) {

    desired.order <- match(list.cg$col_nodes$name, rownames(tbl.colmd))
    tbl.colmd <- tbl.colmd[desired.order,,drop=FALSE]

    category.name <- sprintf("cat-%d", c-1)    
    list.cg$col_nodes[category.name] <- paste(sep='',
                                              colnames(tbl.colmd)[c],
                                              ': ',
                                              tbl.colmd[,c])
    
    return(list.cg)
    
    }#addColumnMetaData
#--------------------------------------------------------------------------------
addRowMetaData <- function(list.cg, tbl.rowmd, r) {
    
    desired.order <- match(list.cg$row_nodes$name, rownames(tbl.rowmd))
    tbl.rowmd <- tbl.rowmd[desired.order,,drop=FALSE]

    category.name <- sprintf("cat-%d", r-1)        
    list.cg$row_nodes[category.name] <- paste(sep='',
                                              colnames(tbl.rowmd)[r],
                                              ': ',
                                              tbl.rowmd[,r])

    return(list.cg)
    
    }#addRowMetaData
#--------------------------------------------------------------------------------

# ============================== TESTS ==============================

test_smallMatrixToListWithMetaData <- function() {

    printf("=== test_smallMatrixToListWithMetaData")
    
    mtx <- matrix(c(1:9), nrow=3, dimnames=list(c("R1","R2","R3"), c("C1","C2","C3")))
    tbl.rowmd <- data.frame(row.names=c("R1","R2","R3"),
                            Placement=c("First", "Second", "Third"),
                            Shape=c("Triangle", "Square", "Circle"),
                            stringsAsFactors=FALSE)
    tbl.colmd <- data.frame(row.names=c("C1","C2","C3"),
                            Placement=c("One", "Two", "Three"),
                            Color=c("Red", "Green", "Blue"),
                            Test=c("w", "x", "y"),
                            stringsAsFactors=FALSE)

    list.cg <- matrixToListWithMetaData(mtx, tbl.rowmd, tbl.colmd)

    checkTrue(is.list(list.cg))
    
    checkEquals(ncol(tbl.rowmd), ncol(list.cg$row_nodes) - 3)
    checkEquals(list.cg$row_nodes$name, c("R3", "R1", "R2"))
    checkEquals(list.cg$row_nodes$clust, c(1,2,3))
    checkEquals(list.cg$row_nodes$"cat-0", c("Placement: Third", "Placement: First", "Placement: Second"))
    checkEquals(list.cg$row_nodes$"cat-1", c("Shape: Circle", "Shape: Triangle", "Shape: Square"))

    checkEquals((ncol(tbl.colmd)), ncol(list.cg$col_nodes) - 3)
    checkEquals(list.cg$col_nodes$name, c("C3", "C1", "C2"))
    checkEquals(list.cg$col_nodes$clust, c(1,2,3))
    checkEquals(list.cg$col_nodes$"cat-0", c("Placement: Three", "Placement: One", "Placement: Two"))
    checkEquals(list.cg$col_nodes$"cat-1", c("Color: Blue", "Color: Red", "Color: Green"))
    checkEquals(list.cg$col_nodes$"cat-2", c("Test: y", "Test: w", "Test: x"))
    
    }#test_smallMatrixToListMetaData
#--------------------------------------------------------------------------------
test_mediumMatrixToListWithMetaData <- function() {

    printf("=== test_mediumMatrixToListWithMetaData")
    
    set.seed(37)
    rows.of.interest <- sample(1:nrow(USArrests), size=3)
    mtx <- as.matrix(USArrests[rows.of.interest,]) #small sample from USArrests dataset
    
    tbl.rowmd <- data.frame(row.names=c("Nevada", "Arkansas", "New York"),
                            "Region"=c("South West", "South", "North East"),
                            stringsAsFactors=FALSE)
    tbl.colmd <- data.frame(row.names=c("Murder", "Assault", "UrbanPop", "Rape"),
                            Severity=c("Terrible", "Pretty Bad", "N/A", "Terrible"),
                            "Jail Time"=c("20 Years", "1-5 Years", "N/A", "5-10 Years"),
                            stringsAsFactors=FALSE)

    list.cg <- matrixToListWithMetaData(mtx, tbl.rowmd, tbl.colmd)

    checkTrue(is.list(list.cg))
    
    checkEquals(ncol(tbl.rowmd), ncol(list.cg$row_nodes) - 3) # 3 is number of original columns; name, clust, group
    checkEquals(list.cg$row_nodes$name, c("Arkansas", "Nevada", "New York"))
    checkEquals(list.cg$row_nodes$clust, c(1,2,3))
    checkEquals(list.cg$row_nodes$"cat-0", c("Region: South", "Region: South West", "Region: North East"))

    checkEquals(list.cg$col_nodes$name, c("Assault", "UrbanPop", "Murder", "Rape"))
    checkEquals(list.cg$col_nodes$clust, c(1,2,3,4))
    checkEquals(list.cg$col_nodes$"cat-0", c("Severity: Pretty Bad", "Severity: N/A", "Severity: Terrible", "Severity: Terrible"))
    checkEquals(list.cg$col_nodes$"cat-1", c("Jail.Time: 1-5 Years", "Jail.Time: N/A", "Jail.Time: 20 Years", "Jail.Time: 5-10 Years"))
    
    }#test_mediumMatrixToListWithMetaData
#--------------------------------------------------------------------------------
test_largeMatrixToListWithMetaData <- function() {

    printf("=== test_largeMatrixToListWithMetaData")

    mtx <- as.matrix(USArrests) #full USArrests dataseto
    tbl.colmd <- data.frame(row.names=c("Murder", "Assault", "UrbanPop", "Rape"),
                            Severity=c("Terrible", "Pretty Bad", "N/A", "Terrible"),
                            "Jail Time"=c("20 Years", "1-5 Years", "N/A", "5-10 Years"),
                            stringsAsFactors=FALSE)

    list.cg <- matrixToListWithMetaData(mtx, tbl.rowmd=NULL, tbl.colmd)

    checkTrue(is.list(list.cg))

    checkEquals(length(list.cg$row_nodes$name), 50)

    checkEquals(list.cg$col_nodes$name, c("Assault", "UrbanPop", "Murder", "Rape"))
    checkEquals(list.cg$col_nodes$clust, c(1,2,3,4))
    checkEquals(list.cg$col_nodes$"cat-0", c("Severity: Pretty Bad", "Severity: N/A", "Severity: Terrible", "Severity: Terrible"))
    checkEquals(list.cg$col_nodes$"cat-1", c("Jail.Time: 1-5 Years", "Jail.Time: N/A", "Jail.Time: 20 Years", "Jail.Time: 5-10 Years"))
    
    }#test_largeMatrixToListWithMetaData
#--------------------------------------------------------------------------------
test_matrixToClustergrammerList <- function() {

    printf("=== test_matrixToClustergrammerList")

    mtx <- matrix(c(1:9), nrow=3, dimnames=list(c("R1","R2","R3"), c("C1","C2","C3")))
    list.cg <- matrixToClustergrammerList(mtx)
    hc.rows <- hclust(dist(mtx))
    hc.cols <- hclust(dist(t(mtx)))

    checkTrue(is.list(list.cg))
    checkEquals((list.cg$row_nodes$name), rownames(mtx[hc.rows$order, hc.cols$order]))
    checkEquals((list.cg$col_nodes$name), colnames(mtx[hc.rows$order, hc.cols$order]))
    
    }# test_simpleMatrix
#--------------------------------------------------------------------------------
test_addRowMetaData <- function() {
    
    printf("=== test_addRowMetaData")
    
    mtx <- matrix(c(1:9), nrow=3, dimnames=list(c("R1","R2","R3"), c("C1","C2","C3")))
    list.cg <- matrixToClustergrammerList(mtx)
    tbl.rowmd <- data.frame(row.names=c("R1","R2","R3"),
                            Placement=c("First", "Second", "Third"),
                            Shape=c("Triangle", "Square", "Circle"),
                            stringsAsFactors=FALSE)
    
    for(i in 1:ncol(tbl.rowmd)) {
        list.cg = addRowMetaData(list.cg, tbl.rowmd, i)
        }
    
    checkTrue(is.list(list.cg))
    checkEquals(ncol(tbl.rowmd), ncol(list.cg$row_nodes) - 3) # 3 is number of original columns; name, clust, group
    checkEquals(list.cg$row_nodes$name, c("R3", "R1", "R2"))
    checkEquals(list.cg$row_nodes$clust, c(1,2,3))
    checkEquals(list.cg$row_nodes$"cat-0", c("Placement: Third", "Placement: First", "Placement: Second"))
    checkEquals(list.cg$row_nodes$"cat-1", c("Shape: Circle", "Shape: Triangle", "Shape: Square"))
    
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
    
    for(i in 1:ncol(tbl.colmd)) {
        list.cg = addColumnMetaData(list.cg, tbl.colmd, i)
        }

    checkTrue(is.list(list.cg))
    checkEquals((ncol(tbl.colmd)), ncol(list.cg$col_nodes) - 3) # 3 is number of original groups; name, clust, group
    checkEquals(list.cg$col_nodes$name, c("C3", "C1", "C2"))
    checkEquals(list.cg$col_nodes$clust, c(1,2,3))
    checkEquals(list.cg$col_nodes$"cat-0", c("Placement: Three", "Placement: One", "Placement: Two"))
    checkEquals(list.cg$col_nodes$"cat-1", c("Colors: Blue", "Colors: Red", "Colors: Green"))
    checkEquals(list.cg$col_nodes$"cat-2", c("Test: y", "Test: w", "Test: x"))
    
    }#test_addColumnMetaData
#--------------------------------------------------------------------------------
test_rowFill <- function(mtx) {

    printf("=== test_rowFill")

    mtx <- matrix(c(1:9), nrow=3, dimnames=list(c("R1","R2","R3"), c("C1","C2","C3")))
    hc.rows <- hclust(dist(mtx))
    treeMtx.rows <- cutree(hc.rows, k=1:3) #change based on size of matrix, max k-value = 11

    checkTrue(is.list(rowFill(treeMtx.rows, hc.rows)))
    
    }#test_rowFill
#--------------------------------------------------------------------------------
test_colFill <- function(mtx) {

    printf("=== test_colFill")

    mtx <- matrix(c(1:9), nrow=3, dimnames=list(c("R1","R2","R3"), c("C1","C2","C3")))
    hc.cols <- hclust(dist(t(mtx)))
    treeMtx.cols <- cutree(hc.cols, k=1:3)
    
    checkTrue(is.list(colFill(treeMtx.cols, hc.cols)))

    }#test_colFill
#--------------------------------------------------------------------------------
