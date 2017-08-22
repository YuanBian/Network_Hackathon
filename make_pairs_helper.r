# compute the semantic distance between pair of words

make_McRae_pairs <- function(words_list) {
  #Get the McRae features
  features <-
    read_delim(paste(getwd(), "/in_files/", "features.csv", sep = ""), delim = ",") %>%
    select(Concept, Feature, WB_Label, BR_Label) %>%
    rename(uni_lemma=Concept)
  #Intersection of Wordbank with McRae concepts
  
  #List of word types in WordBank
  wb <- words_list %>%
    select(item, uni_lemma)
  
  # List of word bank with feature
  #Here I should deal with words in parenthesis (homophones?), e.g., chiken (animal) vs. chiken (food), etc...
  
  item_feat <- wb %>%
    left_join(features) %>%
    rename(item.definition = uni_lemma,
           item_feat = Feature) %>%
    filter(!is.na(item_feat)) %>%
    select(item, item.definition, item_feat)
  
  item_feat_list <- (wb %>%
                       filter(item %in% item_feat$item) %>%
                       select(item))$item
  
  # List these words pair-wise and compate the number of shared feature for each pair
  
  item_pair <- expand.grid(item = item_feat_list,
                           pair = item_feat_list)
  
  pair_feat <- item_feat %>%
    rename(pair = item,
           pair_feat = item_feat,
           pair.definition = item.definition)
  
  item_feat_pair <- left_join(item_feat, item_pair)
  
  item_pair_feat <- item_feat_pair %>%
    left_join(pair_feat)
  
  item_pair_shared <- item_pair_feat %>%
    group_by(item, item.definition, pair, pair.definition) %>%
    summarise(sem_value = sum(pair_feat == item_feat)) %>%
    filter(item!=pair)
  
  return(item_pair_shared)
}


##############################################################################################################################
make_McRae_distinct_pairs<- function(words_list){
  #Intersection of Wordbank with McRae concepts
  features <-
    read_delim(paste(getwd(), "/in_files/", "features.csv", sep = ""), delim = ",") %>%
    select(Concept, Feature, WB_Label, BR_Label) %>%
    filter(BR_Label=="visual-form_and_surface") %>%
    rename(uni_lemma=Concept)
  #List of word types in WordBank
  wb <- words_list %>%
    select(item, uni_lemma)
  
  # List of word bank with feature
  #Here I should deal with words in parenthesis (homophones?), e.g., chiken (animal) vs. chiken (food), etc...
  
  item_feat <- wb %>%
    left_join(features) %>%
    rename(item.definition = uni_lemma,
           item_feat = Feature) %>%
    filter(!is.na(item_feat)) %>%
    select(item, item.definition, item_feat)
  
  item_feat_list <- (wb %>%
                       filter(item %in% item_feat$item) %>%
                       select(item))$item
  
  # List these words pair-wise and compate the number of shared feature for each pair
  
  item_pair <- expand.grid(item = item_feat_list,
                           pair = item_feat_list)
  
  pair_feat <- item_feat %>%
    rename(pair = item,
           pair_feat = item_feat,
           pair.definition = item.definition)
  
  item_feat_pair <- left_join(item_feat, item_pair)
  
  item_pair_feat <- item_feat_pair %>%
    left_join(pair_feat)
  
  item_pair_shared <- item_pair_feat %>%
    group_by(item, item.definition, pair, pair.definition) %>%
    summarise(shared= sum(pair_feat == item_feat)) %>%
    filter(item!=pair)
  
  feat_num<- item_feat %>% 
    group_by(item) %>%
    summarise(n=n())
  
  distinctiveness<- item_pair_shared %>% 
    mutate(sem_value=feat_num$n[which(feat_num$item==item)]+feat_num$n[which(feat_num$item==pair)]-shared*2) %>%
    select(-shared)
  
  return(distinctiveness)
}


##############################################################################################################################

McRae_threshold<- function(item.pair.shared, threshold){
  sem_feat<-item.pair.shared %>%
    filter(item!=pair) %>%
    mutate(link=as.numeric(sem_value>=threshold)) %>%
    select(-sem_value)
  return(sem_feat)
}

##############################################################################################################################
make_assoc_pairs <- function(lemma_list) {
  
  cue_target<- read.csv("in_files/association_cue_target.csv", as.is = T)
  # filter until words in lemma_list remain
  lemma_list<- lemma_list %>% filter((uni_lemma %in% cue_target$cue) | (uni_lemma %in% cue_target$target))
  lemma<- lemma_list$uni_lemma
  cue_target<- cue_target %>% 
    filter(cue %in% lemma, 
           target %in% lemma, 
           normed=="YES") %>% 
    select(cue, target) %>% 
    mutate(link=1)
  
  assoc_table<- expand.grid(cue= lemma, target= lemma) %>% 
    left_join(cue_target) %>% 
    mutate(link=if_else(is.na(link),0,link))
  
  #make a association network dataframe with item number
  #rename stuffs so it could conform to the format PAT_generator needs
  #item corresponds to target ;  pair corresponds to cue
  assoc_link <- assoc_table %>%
    rename(pair.definition = cue) %>%
    left_join(lemma_list, c("pair.definition" = "uni_lemma")) %>%
    rename(pair = item, item.definition = target) %>%
    left_join(lemma_list, c("item.definition" = "uni_lemma")) %>%
    select(item, item.definition, pair, pair.definition, link) %>%
    arrange(item, pair) %>%
    filter(item!=pair)
  
  return(assoc_link)
}

##############################################################################################################################
make_IPA_pairs<- function(def_list, lang){

  IPA_items<- def_list %>% 
    select(item, definition) %>%
    trim_definition() %>% 
    rowwise() %>%
    mutate(IPA= Speak(lang=lang, word=definition)) %>%
    delete_space() %>%
    ungroup()
  
  item_IPA_join <- IPA_items %>% 
    select(definition, IPA)
  only_IPA <- IPA_items %>% 
    select(IPA)
  
  # make word pairs with phonological distance
  IPA_pairs <- expand.grid(only_IPA$IPA, only_IPA$IPA) %>%
    rowwise() %>%
    mutate(dist = adist(Var1, Var2)) %>%
    ungroup() %>%
    left_join(item_IPA_join, by = c("Var1" = "IPA"))  %>%
    rename(W1 = definition) %>%
    left_join(item_IPA_join, by = c("Var2" = "IPA")) %>%
    rename(W2 = definition) %>%
    select(W1, W2, dist) %>%
    filter(W1!=W2) %>%
    arrange(W1, W2)

  return(IPA_pairs)
}