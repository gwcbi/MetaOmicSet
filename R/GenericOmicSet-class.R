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

new_GenericOmicSet_phyloinput <- function(phylobject){
  if (isClass(phylobject,"phyloseq")){
    sampleMetadata <- as(sample_data(phyle),"data.frame")
    featureMetadata <- as(tax_table(phyle),"data.frame")
    ##to be completed...
  }
}