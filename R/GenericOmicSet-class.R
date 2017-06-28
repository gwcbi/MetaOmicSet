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

read_phyloseq <- function(phylobject){
  library(phyloseq) ##perhaps this needs to be somewhere else?
  if (isClass(phylobject,"phyloseq")){
    sampleMetadata <- as(as(sample_data(phylobject),"data.frame"),"DataFrame")
    featureMetadata <- as(as(tax_table(phylobject),"matrix"),"DataFrame")
    assay=Assays(SimpleList(as(otu_table(phylobject),"matrix")))
    genericomicset <- new("GenericOmicSet",name="phyloseq",sampleMetadata=sampleMetadata,featureMetadata=featureMetadata,assays=assay)
  }
}