library(gplots)
library(jsonlite)
#--------------------------------------------------------------------------------
classSorter <- function() {

   #creating matrix from USArrests Data
   print(dim(USArrests))
   set.seed(37)
   rows.of.interest <<- sample(1:nrow(USArrests), size=3)
   mtx <<- as.matrix(USArrests[rows.of.interest,])
   #heatmap.2(mtx, trace="none", col=rev(heat.colors(10)), margins=c(20, 20))
   hc1 <<- hclust(dist(mtx))
   hc2 <<- hclust(dist(t(mtx)))

   treeMtx <<- cutree(hc1, k=1:3)
   treeMtx2 <<- cutree(hc2, k=1:3)

   #sorting the labels and order of the hierarchical cluster into a readable string using paste()
   forJson <<- paste(sep = "",
               '{',
	       "'",
               "row_nodes",
               "'",
	       ":",
	       '{',
	       "'",
               "names",
               "'",
	       ":",
	       list(hc1$labels[1], hc1$labels[2], hc1$labels[3]),
	       ',',
	       "'",
               "clust",
               "'",
	       ":",
	       list(3, 4, 5),
	       '}',
	       '}'
               )
   print(forJson)
#==================================================

   #" " using list()
   lst <<- list("names", hc1$labels[1], hc1$labels[2], hc1$labels[3])
   print(toJSON(lst))

   print(paste(sep = " ",
         lst, "this is the list pasted"))

   print(paste(hc1$labels, collapse=' '))

#==================================================

   doubleList <<-
      list(
         paste(sep = "",
               "{",
	       "'",
	       "row_nodes",
	       "'",
	       ":"),
               toJSON(list(
	              hc1$labels[1],
		      hc1$labels[2],
		      hc1$labels[3])
                      )#second list
         )#first list

   print(paste(sep = "",
               doubleList))

#==================================================

   finalAttempt <<-
      list(
         paste(sep = "",
         for(i in 1:3) {
            paste(sep = "",
               "{",
	       "'",
	       "name",
	       "'",
	       ":",
	       hc1$labels[i]
	       )#second paste
	     }#for i
	   )#first paste
	  )#first list

   print(finalAttempt)
   x <<- (toJSON(finalAttempt))
   print(x)
   print(fromJSON(x))
   print(treeMtx)
   fish <<- NULL
   print(treeMtx)
   
   #THIS KIND OF WORKS!
   for(i in 1:3) {
   fish[i] <<- paste(sep = "",
               "{",
	       "'",
	       "name",
	       "'",
	       ":",
	       "'",
	       hc1$labels[i],
	       "'",
	       ",",
	       "'",
	       "clust",
	       "'",
	       ":",
	       treeMtx[i,3],
	       ",",
	       "'",
	       "rank",
	       "'",
	       ":",
	       treeMtx[i, 3],
	       ",",
	       "'",
	       "group",
	       "'",
	       ":"
	       
	       )
	     }
   print(fish)
   print(toJSON(fish))


   }#classSorter

#--------------------------------------------------------------------------------

final <- function() {
   #creating matrix from USArrests Data
   print(dim(USArrests))
   set.seed(37)
   rows.of.interest <<- sample(1:nrow(USArrests), size=3)
   mtx <<- as.matrix(USArrests[rows.of.interest,])
   #heatmap.2(mtx, trace="none", col=rev(heat.colors(10)), margins=c(20, 20))
   hc1 <<- hclust(dist(mtx))
   hc2 <<- hclust(dist(t(mtx)))

   treeMtx <<- cutree(hc1, k=1:3)
   treeMtx2 <<- cutree(hc2, k=1:3)

   obj <<- fromJSON("teny.js")
   obj$row_nodes[,2] <- hc1$order
   print(obj$row_nodes)
   
   }#final

#--------------------------------------------------------------------------------