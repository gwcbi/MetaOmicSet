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
    representation(
        name="character",
        sampleMetadata="DataFrame",    # Observations (i.e. samples)
        featureMetadata="DataFrame",        # Features (i.e. genes, OTUs, etc.)
        assays="Assays"
    ),
    prototype(name=NA_character_,
              assays=SummarizedExperiment::Assays()
    )
)

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Validity.
###



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

metaGenomicSet <- setClass("metaGenomicSet",
                           slots = c(Assays = "matrix",
                                     fmeta = "data.frame",
                                     smeta = "data.frame"))
##
# defining the "show" Generic method
##
setMethod("show",
          signature = "metaGenomicSet",
          definition = function(object){
            cat("An object of class", class(object), "\n", sep = "")
            cat(" ",
                nrow(object@Assays), " OTUs by",
                ncol(object@Assays), " samples.\n",
                sep = "")
            invisible(NULL)
          })

##
# Defining Accessor for the "Assays" slot of the metaGenomicSet
##
setGeneric("Assays", function(object, ...) standardGeneric("Assays"))

setMethod("Assays", "metaGenomicSet",
          function(object) object@Assays)

##
# Defining Accessor for the "fmeta" slot of the metaGenomicSet
##
setGeneric("fmeta", function(object, ...) standardGeneric("fmeta"))

setMethod("fmeta", "metaGenomicSet",
          function(object) object@fmeta)

##
# Defining Accessor for the "smeta" slot of the metaGenomicSet
##
setGeneric("smeta", function(object, ...) standardGeneric("smeta"))

setMethod("smeta", "metaGenomicSet",
          function(object) object@smeta)


