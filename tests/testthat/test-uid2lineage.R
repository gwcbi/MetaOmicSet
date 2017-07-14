context("Check uid2lineage functions")



test_that("Checking lineages with standard ranks.", {
    thelist <- c(39313, 2, 48381, 95953, 4923, 11111, 111111111, 9606)
    lineage <- uid2lineage(thelist)
    
    expect_length(lineage[,1], length(thelist))
    expect_length(lineage[1,], length(.RANKS.PROK) + 2)
    expect_named(lineage, c('ID', .RANKS.PROK, 'Last.Rank'))
    
    expect_match(lineage[lineage$ID==39313,]$superkingdom, 'Eukaryota')
    expect_match(lineage[lineage$ID==39313,]$Last.Rank, 'Gonyosoma oxycephalum')
    expect_match(lineage[lineage$ID==2,]$superkingdom, 'Bacteria')
    expect_match(lineage[lineage$ID==2,]$Last.Rank, 'Bacteria')
    expect_true(is.na(lineage[lineage$ID==2,]$species))
    expect_match(lineage[lineage$ID==9606,]$superkingdom, 'Eukaryota')
    expect_match(lineage[lineage$ID==9606,]$Last.Rank, 'Homo sapiens')
    expect_match(lineage[lineage$ID==9606,]$species, 'Homo sapiens')
    expect_match(lineage[lineage$ID==9606,]$order, 'Primates')
    expect_true(all(is.na(lineage[lineage$ID==111111111, 2:9])))
})

test_that("Checking lineages with specific ranks.", {
    thelist <- c(39313, 2, 48381, 95953, 4923, 11111, 111111111, 9606)
    newranks <- c('kingdom', 'superorder', 'species')
    lineage <- uid2lineage(thelist, ranks=newranks)
    
    expect_length(lineage[,1], length(thelist))
    expect_length(lineage[1,], length(newranks) + 2)
    expect_named(lineage, c('ID', newranks, 'Last.Rank'))
    
    expect_match(lineage[lineage$ID==9606,]$species, 'Homo sapiens')
    expect_match(lineage[lineage$ID==9606,]$superorder, 'Euarchontoglires')
    expect_match(lineage[lineage$ID==9606,]$kingdom, 'Metazoa')
    expect_match(lineage[lineage$ID==95953,]$kingdom, 'Viridiplantae')
    expect_true(is.na(lineage[lineage$ID==95953,]$superorder))
})


test_that("Checking lineages for asthma sample.", {
    x <- 'SRR1528460'
    f <- system.file("extdata/asthma",
        paste0(x, ".filtered.fastq-sam-report.tsv.gz"),
        package="MetaOmicSet")
    
    taxids <- read.table(f, stringsAsFactors=F, header=T, sep='\t', skip=1)[,1]
    taxids <- gsub('ti\\|', '', taxids)
    # Add a couple nonsense taxids
    nonsense1 <- 1423541234346
    nonsense2 <- 23421232442
    taxids <- c(taxids, nonsense1, nonsense2)
    # Get the lineage
    lineage <- uid2lineage(taxids)
    
    expect_length(lineage[,1], length(taxids))
    expect_length(lineage[1,], length(.RANKS.PROK) + 2)
    expect_true(all(is.na(lineage[lineage$ID==nonsense1, 2:9])))
    expect_true(all(is.na(lineage[lineage$ID==nonsense2, 2:9])))
    expect_match(lineage[lineage$ID==511145,]$Last.Rank,
                 'Escherichia coli str. K-12 substr. MG1655')
    expect_match(lineage[lineage$ID==511145,]$species,
                 'Escherichia coli')
    expect_match(lineage[lineage$ID==511145,]$genus,
                 'Escherichia')
    expect_match(lineage[lineage$ID==511145,]$family,
                 'Enterobacteriaceae')
    expect_match(lineage[lineage$ID==511145,]$order,
                 'Enterobacterales')
    expect_match(lineage[lineage$ID==511145,]$class,
                 'Gammaproteobacteria')    
    expect_match(lineage[lineage$ID==511145,]$phylum,
                 'Proteobacteria')    
    expect_match(lineage[lineage$ID==511145,]$superkingdom,
                 'Bacteria')
})

# 1236608
