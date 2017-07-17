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
