makeOrgPackage(gene_info=gene_info, 
               go=gene2go, 
               version="0.0.1",
               maintainer='', 
               author='',
               outputDir=".", 
               tax_id="", 
               genus="", 
               species="",
               goTable="go")

save(pathway2gene, pathway2name, file = "KEGG_database.Rdata")