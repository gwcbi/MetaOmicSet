#' Extract Sample IDs from list of files
#' @param directory the path to directory that contains the sample files
#' @param pattern_def the pattern string, the part after the end of the sample IDs part
#' @param split_def the character that is used to split words in file names
#' @return A list of 2 elements, (1)the sample list & (2)column position with sample IDs in names
#' @export
get_sampleIds <- function(directory=".",
                            pattern_def=".tsv",
                            split_def="[.]") {
    ## It is a directory of .tsv files
    l <- list.files(path = directory, pattern = pattern_def)
    s <- strsplit(l, split = split_def)
    s1 <- data.frame(s, stringsAsFactors = FALSE)
    colnames(s1) <- c(1:length(s))
    s1 <- t(s1)
    for (i in 1:ncol(s1)){
      if ( length(unique(s1[,i])) == nrow(s1)){
        sample_list <- unique(s1[,i]) #Extracting Sample IDs
        ucol <- i # Column number of the Sample IDs in split string
        break
      }
    }
  return(list(sample_list, ucol))
}

#' Read Pathoscope output
#' @param dir_file path for the directory with all sample files
#' @param s_list a list of all the sample IDs involved in the experiment
#' @param smeta_file a csv/tsv file containing the sample meta data
#' @param is.dir a flag parameter, if the "dir_file" parameter is a directory(1) or a file (0)
#' @param pattern0 & pattern1 these are the pattern of strings before and after the sample IDs
#' @param splitPoint the character that is used to split words in file names
#' @return A list of "Count data matrix" and "Sample metadata data frame" and the "TaxIDs for the OTUs"
#' @export

read_pathoscope <- function(dir_file = ".",
                              s_list = NULL,
                              smeta_file = "sample_metadata.csv",
                              is.dir = 1,
                              pattern0 = "",
                              pattern1 = ".tsv",
                              splitPoint = "[.]"){
  colData <- read.csv(paste(dir_file,smeta_file, sep="/"),
                      row.names = 1,
                      skip = 4,
                      header = T)
  for(i in rownames(colData)){
    df <- read.csv(paste0(dir_file, "/", pattern0, i, pattern1),
                   skip = 1,
                   header = T,
                   stringsAsFactors = F,
                   sep = "\t")
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
  taxids <- gsub("ti\\|", "", rownames(metaGenome))
  lineage <- uid2lineage(taxids)
  lineage[ ,1] <- rownames(metaGenome)
  class(metaGenome) <- "numeric"
  return(list(metaGenome, colData, lineage))
}