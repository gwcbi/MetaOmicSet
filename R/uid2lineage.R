
.RANKS.PROK <- c('superkingdom', 'phylum', 'class', 'order', 'family', 'genus',
                'species')

.RANKS.EUK <- c('superkingdom', 'kingdom', 'phylum', 'class', 'order', 'family',
               'genus', 'species')

.RANKS.VIRUS <- c('superkingdom', 'order', 'family', 'genus', 'species')


#' Download lineage for NCBI taxonomy ID
#' 
#' Uses taxize to download the lineage for given taxon IDs
#' 
#' @param x Vector of taxa names (character) or IDs (character or numeric) to 
#'   query. Passed to \code{\link[taxize]{classification}}.
#' @param ranks Vector of rank labels for lineage.
#' 
#' @return A data.frame with the lineage for each supplied taxonomy ID.
#' @export
#'
#' @examples
#' taxids <-c(39313, 2, 48381, 95953, 4923, 11111, 111111111)
#' lineage.df <- uid2lineage(taxids)
#' 
uid2lineage <- function(x, 
                        ranks=c('superkingdom', 'phylum', 'class', 'order',
                                 'family', 'genus', 'species')
                        )
{
  
  #define the main error in case of the fetch fail
  tmp<-c("Error in FUN(X[[i]], ...) : Gateway Timeout (HTTP 504)")
  
  #define the number of retrys per id in case of fail
  retry<-10
  
  #adding one slice of security
  while (grepl("Gateway Timeout",tmp) && retry>0) {
    tryCatch({tmp<-taxize::classification(x, db = 'ncbi')}, error = function(e){
      print(e);print("retrying");tmp<-"Gateway Timeout"})
    retry=retry-1
  }
  
  missing.row <- c(sapply(ranks, function(.z) NA), Last.Rank=NA)
  lapply(names(tmp), function(tid) {
     if(is.null(nrow(tmp[[tid]]))) return(c(ID=tid, missing.row))
      # Process data frame
     ret <- tmp[[tid]]$name
     names(ret) <- tmp[[tid]]$rank
     ret <- ret[ranks]
     names(ret) <- ranks
     # Get the name of query
     qname <- tmp[[tid]][tmp[[tid]]$id == tid, ]$name
     c(ID=tid, ret, Last.Rank=qname)
  }) %>%
      do.call(rbind, .) %>%
      data.frame(stringsAsFactors=F)
}
