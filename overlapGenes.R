overlapGenes <- function(){
  library(dplyr)
  library(devtools)
  library(plyr)
  library(gplots)
  
  setwd("~/Desktop") #Set directory to where data is stored
  
  
  #heatmapOne="heatmap_ORIGINAL_100genes.csv" #Enter filename with heatmap data
  #heatmapTwo="heatmap_SMOOTHED_100genes.csv" #Enter filename with heatmap data
  
  heatmapOne="IPS_genes.txt" #Enter filename with heatmap data
  heatmapTwo="Processed_Data_KipnisMeningeal.csv" #Enter filename with heatmap data
  
  heatmap.dat.1 <-read.table(heatmapOne,header=TRUE, sep="\t", dec = ".",check.names=FALSE)
  heatmap.dat.2 <- read.csv(heatmapTwo,header=TRUE) #Store gene data from second heatmap
  
  genes1<- heatmap.dat.1$GENE #Retrieve gene names from first heatmap
  genes2<- heatmap.dat.2$User_ID.sorted.by.SD #Retrieve gene names from second heatmap
  
  commonGenes<-intersect(genes1,genes2)
  
  length(commonGenes)
  
  write.csv(commonGenes,file="commonGenes.csv",row.names=FALSE) #Write subsetted data into a csv file
}
