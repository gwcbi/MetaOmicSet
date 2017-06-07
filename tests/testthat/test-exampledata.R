context("Check example data")

asthma.acc <- c('SRR1528344', 'SRR1528346', 'SRR1528348', 'SRR1528420', 
                'SRR1528426', 'SRR1528430', 'SRR1528434', 'SRR1528456',
                'SRR1528458', 'SRR1528460', 'SRR1528462', 'SRR1528464',
                'SRR1528466', 'SRR1528468')

for(x in asthma.acc) {
    test_that( paste0("example data for ", x, " is found"), {
        expect_false(
            system.file("extdata/asthma",
                        paste0(x, ".filtered.fastq-sam-report.tsv.gz"),
                        package="MetaOmicSet") == ""
        )
    })
}
