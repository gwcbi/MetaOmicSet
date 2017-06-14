
getLineage<-function(taxaID){
  if(!exists("id2lineage")){
    #this variable is loaded as global variable for the session if is not exist
    id2lineage<<-read.csv(gzfile("R/taxaID2lineage.csv.gz"),header = T,row.names = 1,check.names = F)
  }
  taxaID<-as.character(taxaID)
  return(id2lineage[taxaID,])

}

#if the id doesn't exist, the function return NA
#example<-c(20,40,50)
#getLineage(example)
