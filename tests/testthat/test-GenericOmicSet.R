context("Check GenericOmicSet")

# x <- new("GenericOmicSet", name='myset')

test_that("read_pathoscope", {
  example_dir <- system.file("extdata/asthma", package = "MetaOmicSet")
  list(ex_Metagenome, ex_colData) <- read_pathoscope(dir_file = example_dir,
                                               smeta_file = "Data-PRJNA255523.csv",
                                               pattern1 = ".filtered.fastq-sam-report.tsv",
                                               splitPoint = "[.]")
  
})