library(gplots)
library(jsonlite)
library(RUnit)
#--------------------------------------------------------------------------------
matrixToClustergrammer <- function(mtx, colCatGroups=NULL) {

    hc1 <<- hclust(dist(mtx))
    hc2 <<- hclust(dist(t(mtx)))

    treeMtx <<- cutree(hc1, k=1:3) #change based on size of matrix, max k-value = 11
    treeMtx2 <<- cutree(hc2, k=1:3) # " "

    rowname <- hc1$labels[hc1$order]
    rowclust <- 1:nrow(treeMtx)
    #rowrank <- hc1$order

    row_nodes <- data.frame(name=rowname,
                            clust=rowclust,
  		 	    #rank=rowrank,
 			    group=rowFill(), #function that fills list called rowgroup with rows from treeMtx
                            stringsAsFactors=FALSE,
                            check.names=FALSE)

    colname <- hc2$labels[hc2$order]
    colclust <- 1:nrow(treeMtx2)
    #colrank <- hc2$order

    #forloop for colGroups here
    if(!is.null(colCatGroups)) {
        colCatGroups <<- colCatGroups
        catCol <- list()
        catCol <- catColFill()
        }

    col_nodes <- data.frame(name=colname,
                            clust=colclust,
 			    #rank=colrank,
                            group=colFill(), #function that fills list called colgroup with rows from treeMtx2
                            #"cat-0"= paste(sep=' ',
                             #              'Diagnosis:',
                              #             catCol),
                            stringsAsFactors=FALSE,
                            check.names=FALSE)


    mat <- unname(mtx, force=FALSE)
    mat <- mat[hc1$order, hc2$order]

    rawList <- list(row_nodes=row_nodes, col_nodes=col_nodes, mat=mat)
    listToJson <- toJSON(rawList)

    return(rawList)  

    }#matrixToClustergrammer
#--------------------------------------------------------------------------------
rowFill <- function() {
    
              rowgroup <- list()
              for(i in 1:nrow(treeMtx)) {
                 rowgroup[[i]] <- rev(treeMtx[hc1$order[i],])
                 }
              return(I(rowgroup))
              
              }#row names
#--------------------------------------------------------------------------------
colFill <- function() {
    
              colgroup <- list()
              for(i in 1:nrow(treeMtx2)) {
                 colgroup[[i]] <- rev(treeMtx2[hc2$order[i],])
                 }
              return(I(colgroup))

              }#column names
#--------------------------------------------------------------------------------
catColFill <- function() {
                 catCol <- list()
                 for(i in 1:nrow(treeMtx2)) {
                    catCol[[i]] <- colCatGroups[[hc2$labels[hc2$order[i]]]]
                    }
                 return(I(catCol))
                 }#column categories
#--------------------------------------------------------------------------------
USArrestsExample <- function() {
    
    set.seed(37)
    rows.of.interest <- sample(1:nrow(USArrests), size=3)
    mtx <- as.matrix(USArrests[rows.of.interest,])
    
    tbl.rowmd <- data.frame(row.names=c("Nevada", "Arkansas", "New York"), "N-States"=c("True", "False", "True"), stringsAsFactors=FALSE)
    tbl.colmd <- data.frame(row.names=c("Murder", "Assault", "UrbanPop", "Rape"), Crime=c("True", "True", "False", "True"), Severity=c("Terrible", "Pretty Bad", "N/A", "Terrible"), stringsAsFactors=FALSE)

    colCatGroups <- list()

    for(i in 1:length(names(tbl.colmd))) {
        colCatGroups[[i]] <- tbl.colmd[i]
        }
    
    print(colCatGroups[[1]]$Crime)
    print(colCatGroups[[2]]$Severity)    

    x <- matrixToClustergrammer(mtx, colCatGroups)
    y <- toJSON(x) # jsonify 
    z <- fromJSON(y) # parse jsonified string
    print(paste(sep=' ',
                'Number of Characters:',
                nchar(y))
                )
    print(y)
    
    checkTrue(is.matrix(z$mat))
    checkTrue(is.data.frame(z$col_nodes))
    checkTrue(is.data.frame(z$col_nodes))
        
    }#USArrestsExample
#--------------------------------------------------------------------------------
test_simpleMatrix <- function() {
    mtx <- matrix(c(1:9), nrow=3, dimnames=list(c("R1","R2","R3"), c("C1","C2","C3")))
    list.cg <- matrixToClustergrammer(mtx)
    checkTrue(is.list(list.cg))

    tbl.meta <- data.frame(row.names=c("R1","R2","R3"), Placement=c("First", "Second", "Third"))
    print(tbl.meta)
    
    
    }# test_simpleMatrix
#--------------------------------------------------------------------------------
