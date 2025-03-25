library(clusterProfiler)
library(dplyr)
library(stringr)
library(AnnotationForge)
library(jsonlite)
library(purrr)
library(RCurl)
options(stringsAsFactors = F)

egg  <-  read.delim("emapper.annotations.tsv", header = T)
egg[egg==""]<-NA

gene_info <- egg %>%
  dplyr::select(GID = Query, GENENAME = seed_ortholog) %>% 
  na.omit()

goterms <- egg %>%
  dplyr::select(Query, GOs) %>% 
  na.omit() %>% 
  filter(str_detect(GOs,"GO"))

all_go_list <- str_split(goterms$GOs, ",")
gene2go <- data.frame(GID = rep(goterms$Query, times = sapply(all_go_list, length)), GO = unlist(all_go_list), EVIDENCE = "IEA") %>% 
  filter(str_detect(GO,"GO"))