context("Check GenericOmicSet")

# x <- new("GenericOmicSet", name='myset')

test_that("read_pathoscope", {
  example_dir <- system.file("extdata/asthma", package = "MetaOmicSet")
  a <- read_pathoscope(dir_file = example_dir,
                                               smeta_file = "Data-PRJNA255523.csv",
                                               pattern1 = ".filtered.fastq-sam-report.tsv.gz",
                                               splitPoint = "[.]")
  expect_equal(dim(a[[1]]), c(1508, 14))
  expect_equal(dim(a[[2]]), c(14, 6))
  expect_equal(dim(a[[3]]), c(1508, 9))
})