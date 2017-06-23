# In case the user wants to create the object from multiple files in a directory
# This function will extract the Sample IDs from the file names in the directory which can be further used to _______________
get_sampleIds <- function(directory=".",
                            file=0,
                            pattern_def=".tsv",
                            split_def="[.]") {
  if (file==0){
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
  }
  return(list(sample_list, ucol))
}

params <- 
  get_sampleIds(directory ="/Users/mrigank/Desktop/pathoStat/data_object",
                  pattern_def = ".filtered.fastq-sam-report.tsv")

#read_pathoscope() <- function()
