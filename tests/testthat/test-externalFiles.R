context("Check phyloseq file input")

test_that("read_phyloseq_RDS", {
  example_file <- system.file("extdata/asthma/phyloAsthma.rds", package = "MetaOmicSet")
  a <- read_phyloseq_RDS(example_file)
  expect_equal(a[[1]], c(1471, 14))
  expect_equal(a[[2]], c(14, 6)) 
  expect_equal(a[[3]],c(1471, 7))
})

test_that("read_biom_file", {
  example_file <- system.file("extdata/asthma/BiomAsthma.biom", package = "MetaOmicSet")
  a <- read_biom_file(example_file)
  expect_equal(a[[1]], c(1471, 14))
  expect_equal(a[[2]], c(14, 6)) 
  expect_equal(a[[3]],c(1471, 7))
})