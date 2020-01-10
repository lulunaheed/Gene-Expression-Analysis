geneCorr <- function(countMatrixFile,mutationsFile,geneList){
  setwd("~/Desktop") #Set directory to where data is stored
  #install.packages("Hmisc")
  library(Hmisc)
  
  countMatrixFile<-"TCGA-BLCA_CountMatrix_full_SID.csv"
  mutationsFile<- "TCGA-BLCA_RB1_mutations_presenceInfo_SID.csv"
  
  genesOfInterest<-read.csv("nanostring_Cancer_Hallmark_Reference.csv",header=TRUE)
  genesOfInterest<-genesOfInterest$GENE
  geneList<-c("AURKA","TPX2",as.character(genesOfInterest))
  
  #Upload Count Matrix
  countMatrix<- read.csv(countMatrixFile,header=TRUE)
  countMatrix[,1]<-toupper(countMatrix[,1])
  countMatrix<-countMatrix[!duplicated(countMatrix[,1]),]
  rownames(countMatrix)<-countMatrix[,1]
  numberColumns<-ncol(countMatrix)
  countMatrix<-countMatrix[,2:numberColumns]
  
  #Upload Mutations Information
  mutations<- read.csv(mutationsFile,header=TRUE)
  mutations[,1]<-toupper(mutations[,1])
  mutations<-mutations[!duplicated(mutations[,1]),]
  rownames(mutations)<-mutations[,1]
  numberColumns<-ncol(mutations)
  mutations<-mutations[,2:numberColumns]
  
  corrMatrix<-data.frame()
  pValMatrix<-data.frame()
  mutationValues<-mutations$presence
  my_data<-data.frame(mutationValues=mutationValues)
  
  for (j in 1:length(geneList)){
    currGene<-geneList[j]
    expressionValues<-as.numeric(countMatrix[rownames(countMatrix)==currGene,])
    
    my_data[1:length(expressionValues),2]<-expressionValues
    res2 <- rcorr(as.matrix(my_data))
    
    corrMatrix[1,j]<-res2$r[1,2]
    pValMatrix[1,j]<-res2$P[1,2]
  }
  
 colnames(corrMatrix)<-geneList
 colnames(pValMatrix)<-geneList
 
corrVals<-as.numeric(corrMatrix[1,])
pValNums<-as.numeric(pValMatrix[1,])
n<-1:length(geneList)

corrHighInds<-n[abs(corrVals)>0.2] #Minimum correlation value shown
pValHighInds<-n[pValNums<0.05] #Cutoff for significance

impInds<-intersect(corrHighInds,pValHighInds)
geneGoodInds<-(!duplicated(geneList[impInds]))
 vals<-as.matrix(corrVals[impInds])
 vals<-vals[geneGoodInds]
 goodGenes<-geneList[impInds][geneGoodInds]
 #a<-plot(1:1:length(geneList),corrMatrix[1,])
 library(RColorBrewer)
 coul <- brewer.pal(5, "Set1")

 b<-barplot(vals[1:27], main="RB1 Mutation vs. Gene Expression Correlation Values (p<0.05) in BLCA", horiz=TRUE,names.arg=goodGenes,las=1,cex.names=0.5,col="blue",cex.main=0.8)
 c<-barplot(vals[32:62], main="RB1 Mutation vs. Gene Expression Correlations (p<0.05) in HNSC", horiz=TRUE,names.arg=geneList[impInds][32:62],las=1,cex.names=0.5,col="blue",cex.main=0.8)

 
 
 
 
 
 
 
 
 
}