kegg <- fromJSON("ko00001.json")

gene2ko <- egg %>%
  dplyr::select(GID = Query, Ko = KEGG_ko) %>%
  na.omit() %>%
  filter(str_detect(Ko,"ko"))

#########提取通路(Pathway)与通路名称(Name)信息#########
pathway2name <- tibble(Pathway = character(), Name = character())
ko2pathway <- tibble(Ko = character(), Pathway = character())
for (a in seq_along(kegg[["children"]][["children"]])) {
  A <- kegg[["children"]][["name"]][[a]]
  for (b in seq_along(kegg[["children"]][["children"]][[a]][["children"]])) {
    B <- kegg[["children"]][["children"]][[a]][["name"]][[b]] 
    for (c in seq_along(kegg[["children"]][["children"]][[a]][["children"]][[b]][["children"]])) {
      pathway_info <- kegg[["children"]][["children"]][[a]][["children"]][[b]][["name"]][[c]]
      pathway_id <- str_match(pathway_info, "ko[0-9]{5}")[1]
      pathway_name <- str_replace(pathway_info, " \\[PATH:ko[0-9]{5}\\]", "") %>% str_replace("[0-9]{5} ", "")
      pathway2name <- rbind(pathway2name, tibble(Pathway = pathway_id, Name = pathway_name))
      kos_info <- kegg[["children"]][["children"]][[a]][["children"]][[b]][["children"]][[c]][["name"]]
      kos <- str_match(kos_info, "K[0-9]*")[,1]
      ko2pathway <- rbind(ko2pathway, tibble(Ko = kos, Pathway = rep(pathway_id, length(kos))))
    }
  }
}

#########组建通路(Pathway)与蛋白名称(Query)信息#########
ko2gene <- tibble(Ko=character(),GID=character())#由于此时存在一蛋白对应多个KO,因此将其拆成一对一的多列储存进新的dataframe中
for (Query in gene2ko$GID){
  ko_list <- strsplit(gene2ko$Ko[which(gene2ko[,1]==Query)],split = ',')
  for (ko in ko_list){
    if (length(which(ko2gene[,1]==ko))==0){
      tmp <- data.frame(Ko=ko,GID=Query)
      ko2gene <- rbind(ko2gene,tmp)
    }
    else{
      old_Query <- ko2gene$GID[which(ko2gene[,1]==ko)]
      ko2gene$GID[which(ko2gene[,1]==ko)] <- paste(old_Query,Query,sep = ',')
    }
  }
}

pathway2gene <- tibble(Pathway = character(), GID = character())
for (ko in ko2pathway$Ko){
  pathway_list <- ko2pathway$Pathway[which(ko2pathway[,1]==ko)]
  for (pathway in pathway_list){
    if (paste('ko:',ko,sep='') %in% ko2gene$Ko){
      ko <- paste('ko:',ko,sep='')
      if (length(which(pathway2gene[,1]==pathway))==0 ){
        ko2gene$GID[which(ko2gene[,1]==ko)]
        tmp <- data.frame(pathway=pathway,GID=ko2gene$GID[which(ko2gene[,1]==ko)])
        pathway2gene <- rbind(pathway2gene,tmp)
      }
      else{
        old_Query <- pathway2gene$GID[which(pathway2gene[,1]==pathway)]
        Query <- ko2gene$GID[which(ko2gene[,1]==ko)]
        pathway2gene$GID[which(pathway2gene[,1]==pathway)] <- paste(old_Query,Query,sep=',')
      }
    }
  }
}
