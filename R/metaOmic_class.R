rm(list = ls())

setwd("/Users/mrigank/Desktop/metaOmicSet/data")

colData <- read.csv("Data-PRJNA255523.csv", row.names = 1, skip = 4, header = T)

for(i in rownames(colData)){
  df <- read.csv(paste0(i,".filtered.fastq-sam-report.tsv"),
                 skip = 1, header = T, stringsAsFactors = F, sep = "\t")
  if (i == rownames(colData)[1]){
    metaGenome <- data.frame(cbind(df[,1], df[,4]))
  }else{
    new_idx <- union(metaGenome[,1],df[,1])
    meta_match_idx <- match(new_idx,metaGenome[,1])
    df_match_idx <- match(new_idx,df[,1])
    metaGenome <- cbind(new_idx,metaGenome[meta_match_idx,2:ncol(metaGenome)])
    metaGenome <- cbind(metaGenome,df[,4][df_match_idx])
  }
}
rownames(metaGenome) <- metaGenome[,1]
metaGenome <- metaGenome[,-1]
colnames(metaGenome) <- rownames(colData)
