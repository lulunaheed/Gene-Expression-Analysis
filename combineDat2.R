combineDat2<-function(fileName,wantGenesFile){
  setwd("~/Desktop/") #Set directory to where data is stored
  
  ##Download csv files
  dat<- as.data.frame(read.csv(fileName,header=TRUE)) #Read count data
  originalDat<-dat
  fileGeneNames<-as.data.frame(read.csv(wantGenesFile))
  geneNames<-fileGeneNames[,1] #Download gene names
  numRows<-dim(geneNames)[1] #Find number of rows
  geneNames<-unlist(geneNames[1:(numRows),])#Turn chart with gene names into vector of names, excluding titles at top
  geneNamesNoRepeats<-unique(as.character(geneNames)) #Remove all repeated genes
  
  allInds<-vector() #Initialize vector that will store row numbers where gene count data is located
  origInds<-vector()
  for (h in 1:length(geneNamesNoRepeats)){
    currentGene<-geneNamesNoRepeats[h] #Current gene within list to find in count data file
    geneInds<-grep(currentGene,dat[,1],ignore.case=TRUE,value=FALSE) #Find indices where string matches
    if (length(geneInds)==0){ #If gene could not be found, print its name
      print("Could not find:")
      print(currentGene)
    }
    geneNames<-grep(currentGene,dat[,1],ignore.case=TRUE,value=TRUE) #Find names that match
    geneLen<-nchar(currentGene) #Find the number of characters in original gene name
    onlyGeneInd<-geneInds[geneLen==nchar(geneNames)] #Pick the name that matches exactly based on the length of original gene name
    if (length(onlyGeneInd)>0){
      origInds<-c(origInds,h)
    }
    allInds<-c(allInds,onlyGeneInd) #Record row index in the vector
  }
  
  newDat<-dat[allInds,] 
  newDat<-Originaldat[1,]#Collect count data in the rows that correspond to the desired genes
  firstFileCols<-dim(dat)[2]
  after<-firstFileCols+1
  endCol<-firstFileCols+(dim(fileGeneNames)[2]-1)
  newDat[,after:endCol]<-fileGeneNames[origInds,-1]
  
 
  write.csv(newDat,file="subsettedCountData.csv",row.names=FALSE) #Write subsetted data into a csv file
}