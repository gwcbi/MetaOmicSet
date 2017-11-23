### =========================================================================
### Testing:
### Restart R (Session -> Restart R)
### devtools::load_all()
### x <- new("genericOmicSet", name='myset')
### dim(x)


### =========================================================================
### GenericOmicSet objects
### -------------------------------------------------------------------------
###

##
# Defining "genericOmicSet" class
#' @slot name name of the object
#' @slot smeta Observation (i.e. samples, etc.)
#' @slot fmeta Features (i.e. genes, OTUs, etc.)
#' @slot assays count matrix
#' @export genericOmicSet
##

setClass("genericOmicSet",
         slots = c(name="character",
                   smeta="DataFrame",
                   fmeta="DataFrame",
                   assays="Assays"),
         prototype=list(name=NA_character_,
                        assays=SummarizedExperiment::Assays()
         )
)

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Defining Accessor methods.
###

setMethod("length", "genericOmicSet",
          function(x) nrow(x@fmeta)
)

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Validity.
###

setValidity("genericOmicSet",function(object){
  msg <- NULL
  valid <- TRUE
  ##Empty feature_metadata is valid as data could be loaded later...
  if(nrow(object@assays) != length(object) && length(object) != 0){
    valid <- FALSE
    msg <- c(msg,
             "number of feature data and metadata rows must match (if metadata not empty).")
  }
  if(ncol(object@assays) != nrow(object@smeta)){
    valid <- FALSE
    msg <- c(msg,
             "number of sample data and metadata column must match.")
  }
  if(!identical(colnames(object@assays[[1]]),rownames(object@smeta))){
    valid <- FALSE
    msg <- c(msg,
             "sample data and metadata names must match perfectly.")
  }
  if(!identical(rownames(object@assays[[1]]), rownames(object@fmeta)) && length(object) != 0){
    valid <- FALSE
    msg <- c(msg,
             "feature data and non-empty metadata names must match perfectly.")
  }
  
  if (valid) TRUE else msg
})


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Low-level constructor (not exported).
###

# new_GenericOmicSet <- function(assays, names, rowData, colData,
#                                      metadata)
# {
#     if (!is(assays, "Assays"))
#         assays <- Assays(assays)
#     if (is.null(rowData)) {
#         if (is.null(names))
#             nrow <- nrow(assays)
#         else
#             nrow <- length(names)
#         rowData <- S4Vectors:::make_zero_col_DataFrame(nrow)
#     } else {
#         rownames(rowData) <- NULL
#     }
#     new("SummarizedExperiment", NAMES=names,
#         elementMetadata=rowData,
#         colData=colData,
#         assays=assays,
#         metadata=as.list(metadata))
# }


##
# Defining "metaGenomicSet" class
#' @slot assays the OTU count matrix
#' @slot fmeta OTUs metadata
#' @slot smeta sample meta data as provided by the user
#' @export metaGenomicSet
##
metaGenomicSet <- setClass("metaGenomicSet",
                           slots = c(assays = "Assays",
                                     fmeta = "DataFrame",
                                     smeta = "DataFrame"),
                           prototype=list(name=NA_character_,
                                          assays=SummarizedExperiment::Assays()
                           )
)


##
# Setting the "show" Generic method
#' @export
##
setMethod("show",
          signature = "metaGenomicSet",
          definition = function(object){
            cat("An object of class", class(object), "\n", sep = "")
            cat(" ",
                nrow(object@assays), " OTUs by ",
                ncol(object@assays), " samples.\n",
                sep = "")
            invisible(NULL)
          })

##
# Defining Accessor for the "Assays" slot of the metaGenomicSet

##
setGeneric("assays", function(object, ...) standardGeneric("assays"))

#' @export
setMethod("assays", "metaGenomicSet",
          function(object) object@assays)

##
# Defining Accessor for the "fmeta" slot of the metaGenomicSet

##
setGeneric("fmeta", function(object, ...) standardGeneric("fmeta"))

#' @export
setMethod("fmeta", "metaGenomicSet",
          function(object) object@fmeta)

##
# Defining Accessor for the "smeta" slot of the metaGenomicSet
##
setGeneric("smeta", function(object, ...) standardGeneric("smeta"))

#' @export
setMethod("smeta", "metaGenomicSet",
          function(object) object@smeta)

##
# Defining the sub-setting operation function
#' @export
##
# setMethod("[",
#           signature = "metaGenomicSet",
#           function(x, i, j, drop="missing"){
#             .assays <- x@assays[i,j]
#             .fmeta <- x@fmeta[i,]
#             .smeta <- x@smeta[j,]
#             metaGenomicSet(assays = .assays,
#                            fmeta = .fmeta,
#                            smeta = .smeta)
#           })

##
# Validity methods for metaGenomicSet object
#' @export
##
setValidity("metaGenomicSet",function(object){
              msg <- NULL
              valid <- TRUE
              if(nrow(assays(object)) != nrow(fmeta(object))){
                valid <- FALSE
                msg <- c(msg, "Number of data & feature meta-data rows must be identical")
              }
              if(ncol(assays(object)) != nrow(smeta(object))){
                valid <- FALSE
                msg <- c(msg, "Number of data rows & sample meta-data columns must be identical")
              }
              if(!identical(rownames(assays(object)[[1]]), rownames(fmeta(object)))){
                valid <- FALSE
                msg <- c(msg, "Data & feature meta-data row names must be identical")
              }
              if(!identical(colnames(assays(object)[[1]]), rownames(smeta(object)))){
                valid <- FALSE
                msg <- c(msg, "Data row-names & sample meta-data columns names must be identical")
              }
              if(valid) TRUE else msg
            })