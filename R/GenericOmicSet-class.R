### =========================================================================
### Testing:
### Restart R (Session -> Restart R)
### devtools::load_all()
### x <- new("GenericOmicSet", name='myset')
### dim(x)


### =========================================================================
### GenericOmicSet objects
### -------------------------------------------------------------------------
###

setClass("GenericOmicSet",
         slots = c(name="character",
                   sampleMetadata="DataFrame",    # Observations (i.e. samples)
                   featureMetadata="DataFrame",        # Features (i.e. genes, OTUs, etc.)
                   assays="Assays"),
         prototype=list(name=NA_character_,
                        assays=SummarizedExperiment::Assays()
         )
)

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Validity.
###

setValidity("GenericOmicSet",function(object){
  msg <- NULL
  valid <- TRUE
  if(ncol(object@assays) != dim(object)[[2]]){
    valid <- FALSE
    msg <- c(msg,
             "number of sample data and metadata column must match.")
  }
  ##Empty feature_metadata is valid as data could be loaded later...
  if(nrow(object@assays) != length(object) && length(object) != 0){
    valid <- FALSE
    msg <- c(msg,
             "number of feature data and non-empty metadata rows must match.")
  }
  if(!identical(ncol(object@assays),dim(object)[[2]])){
    valid <- FALSE
    msg <- c(msg,
             "sample data and metadata names must match perfectly.")
  }
  if(!identical(nrow(object@assays),length(object)) && length(object) != 0){
    valid <- FALSE
    msg <- c(msg,
             "feature data and non-empty metadata names must match perfectly.")
  }
  
  if (valid) TRUE else msg
})


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Getters and setters.
###

setMethod("length", "GenericOmicSet",
          function(x) nrow(x@featureMetadata)
)

setMethod("dim", "GenericOmicSet",
          function(x) c(length(x), nrow(x@sampleMetadata))
)


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
### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### constructors for different inputs.
###

readinphyloseq <- function(phylobject){
  if (isClass("phyloseq",phylobject)){
    sampleMetadata <- as(as(phyloseq::sample_data(phylobject),"data.frame"),"DataFrame")
    featureMetadata <- as(as(phyloseq::tax_table(phylobject),"matrix"),"DataFrame")
    assay=SummarizedExperiment::Assays(S4Vectors::SimpleList(as(phyloseq::otu_table(phylobject),"matrix")))
    genericomicset <- new("GenericOmicSet",name="phyloIn",sampleMetadata=sampleMetadata,featureMetadata=featureMetadata,assays=assay)
  }
}

readinbiom <- function(biomobject){
  if(isClass("biom",biomobject)){
    sampleMetadata <- as(biomformat::sample_metadata(biomobject),"DataFrame")
    featureMetadata <- as(biomformat::observation_metadata(biomobject),"DataFrame")
    assay=SummarizedExperiment::Assays(S4Vectors::SimpleList(as(biomformat::biom_data(biomobject),"matrix")))
    genericomicset <- new("GenericOmicSet",name="biomIn",sampleMetadata=sampleMetadata,featureMetadata=featureMetadata,assays=assay)
  }
}

##
# Defining "metaGenomicSet" class
#' @slot Assays the OTU count matrix
#' @slot fmeta OTUs metadata
#' @slot smeta sample meta data as provided by the user
#' @export metaGenomicSet
##
metaGenomicSet <- setClass("metaGenomicSet",
                           slots = c(Assays = "matrix",
                                     fmeta = "data.frame",
                                     smeta = "data.frame"))

##
# Setting the "show" Generic method
#' @export
##
setMethod("show",
          signature = "metaGenomicSet",
          definition = function(object){
            cat("An object of class", class(object), "\n", sep = "")
            cat(" ",
                nrow(object@Assays), " OTUs by ",
                ncol(object@Assays), " samples.\n",
                sep = "")
            invisible(NULL)
          })

##
# Defining Accessor for the "Assays" slot of the metaGenomicSet

##
setGeneric("Assays", function(object, ...) standardGeneric("Assays"))

#' @export
setMethod("Assays", "metaGenomicSet",
          function(object) object@Assays)

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
setMethod("[",
          signature = "metaGenomicSet",
          function(x, i, j, drop="missing"){
            .Assays <- x@Assays[i,j]
            .fmeta <- x@fmeta[i,]
            .smeta <- x@smeta[j,]
            metaGenomicSet(Assays = .Assays,
                           fmeta = .fmeta,
                           smeta = .smeta)
          })

##
# Validity methods for metaGenomicSet object
#' @export
##
setValidity("metaGenomicSet",
            function(object){
              msg <- NULL
              valid <- TRUE
              if(nrow(Assays(object)) != nrow(fmeta(object))){
                valid <- FALSE
                msg <- c(msg, "Number of data & feature meta-data rows must be identical")
              }
              if(ncol(Assays(object)) != nrow(smeta(object))){
                valid <- FALSE
                msg <- c(msg, "Number of data rows & sample meta-data columns must be identical")
              }
              if(!identical(rownames(Assays(object)), fmeta(object)[,1])){
                valid <- FALSE
                msg <- c(msg, "Data & feature meta-data row names must be identical")
              }
              if(!identical(colnames(Assays(object)), rownames(smeta(object)))){
                valid <- FALSE
                msg <- c(msg, "Data row-names & sample meta-data columns names must be identical")
              }
              if(valid) TRUE else msg
            })