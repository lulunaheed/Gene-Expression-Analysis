removeZerosChart <- function(fileName){
  #Input to this function should be the name of the excel sheet containing data
  
  #sudo R CMD javarecon
  
  setwd("~/Desktop")
  
  library("xlsx")
  
  ##Download Excel sheet consisting of number of reads mapping to each gene for each sample
  
  dat<- read.csv(fileName,header=TRUE,sep=",")
  
  noZeroDat<-dat[apply(dat[,-1],1,function(x) !all(x==0)),] #Disregarding the first column ([,-1]), remove all rows that contain all 0's
  
  ##Write new matrix without rows of 0's into excel sheet
  
  write.csv(noZeroDat,file="noRowZeros.csv",row.names=FALSE)
}