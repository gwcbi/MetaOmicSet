library(taxize)
library(plyr)

uid2lineage<-function(x){
  
  #define the main error in case of the fetch fail
  tmp<-c("Error in FUN(X[[i]], ...) : Gateway Timeout (HTTP 504)")
  
  #define the number of retrys per id in case of fail
  retry<-10
  
  #adding one slice of security
  while (grepl("Gateway Timeout",tmp) && retry>0) {
    tryCatch({tmp<-classification(x, db = 'ncbi')}, error = function(e){
      print(e);print("retrying");tmp<-"Gateway Timeout"})
    retry=retry-1
  }
  
  #get the df that contain the lineage
  tmpdf<-as.data.frame(tmp[[1]])
  if(nrow(tmpdf)>1){
    superk<-ifelse(identical("character(0)",superk<-as.character(tmpdf[which(
      tmpdf[2]=="superkingdom"),][1])),"NA",superk)
    phylum<-ifelse(identical("character(0)",phylum<-as.character(tmpdf[which(
      tmpdf[2]=="phylum"),][1])),"NA",phylum)
    class<-ifelse(identical("character(0)",class<-as.character(tmpdf[which(
      tmpdf[2]=="class"),][1])),"NA",class)
    order<-ifelse(identical("character(0)",order<-as.character(tmpdf[which(
      tmpdf[2]=="order"),][1])),"NA",order)
    family<-ifelse(identical("character(0)",family<-as.character(tmpdf[which(
      tmpdf[2]=="family"),][1])),"NA",family)
    genus<-ifelse(identical("character(0)",genus<-as.character(tmpdf[which(
      tmpdf[2]=="genus"),][1])),"NA",genus)
    species<-ifelse(identical("character(0)",species<-as.character(tmpdf[which(
      tmpdf[2]=="species"),][1])),"NA",species)
    name<-as.character(tmpdf[nrow(tmpdf),][1])
    out<-data.frame(ID=x,Superkingdom=superk,Phylum=phylum,Class=class,
                    Order=order,Family=family,Genus=genus,Species=species,
                    `Last Rank`=name)
  }else{
    out<-data.frame(ID=x,Superkingdom="NA",Phylum="NA",Class="NA",
                                    Order="NA",Family="NA",Genus="NA",Species="NA",
                                    Last.Rank="NA") 
  }
  return(out)
  
}
dflineage<-function(x){
  out<-ldply(lapply(thelist,uid2lineage),data.frame)
  out[out=="NA"]<-NA
  return(out)
}

#############
###EXAMPLE###
#############

thelist<-c(39313,2,48381,95953,4923,11111,111111111)
lineages<-dflineage(thelist)
lineages


