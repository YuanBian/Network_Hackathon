# This file contains functions used to get item data, admin data, etc. , and generate dataframes for analysis
import_all_library<- function(){
  library(tidyr)
  library(purrr)
  library(readr)
  library(ggplot2)
  library(langcog)
  library(boot)
  library(lazyeval)
  library(dplyr)
  library(wordbankr)
  library(directlabels)
  library(scales)
  library(stringr)
  library(lmtest)
}
######################################################################################################################
get_lang_item_data <- function(lang, lang_form = "WS", lex_class = "nouns") {
  #Every English item
  item_data <- get_item_data(language = lang, form = lang_form) %>%
    select(num_item_id, definition, type, lexical_class, uni_lemma) %>%
    filter(type == "word", lexical_class == lex_class) %>%
    rename(item = num_item_id)
  
  #Initialize every item as NA (NOT yet learnt)
  lang_item <- item_data %>%
    select(item, definition, uni_lemma) %>%
    mutate(age = NA)
  
  return(lang_item)
}

######################################################################################################################
# get only "useful" admin data (where production is not NA)
get_lang_admin_data <- function(lang, lang_form = "WS") {
  #get the kids' data_id from every age
  admin_data <- get_administration_data() %>%
    filter(form == lang_form, !is.na(production), language == lang) %>%
    select(data_id, age) %>%
    arrange(age)
  
  return(admin_data)
}

######################################################################################################################
# See how many kids we have in every age
get_kids_by_age <- function(admin_data) {
  #get number of kids by age
  nkids_by_age <- admin_data %>%
    group_by(age) %>%
    summarise(n = n())
  return(nkids_by_age)
}

######################################################################################################################
#get "produces" instrument data of certain language
get_lang_instr_data <- function(lang, lang_form = "WS") {
  instr_data <- get_instrument_data(instrument_language = lang,
                                    instrument_form = lang_form) %>%
    filter(value == "produces") %>%
    arrange(num_item_id) %>%
    rename(item = num_item_id)
  
  return(instr_data)
}

######################################################################################################################
#calculate the age of acquisition
get_lang_aoa <- function(item_data, admin_data, instr_data) {
  nkids_by_age <- get_kids_by_age(admin_data)
  ages<- nkids_by_age$age
  
  for (cur_age in ages) {
    rem_item <- item_data %>% filter(is.na(age))
    current_age_id <- admin_data %>% filter(age == cur_age)
    current_instr <- instr_data %>% 
      filter(data_id %in% current_age_id$data_id)
    for (w in rem_item$item) {
      proportion <- sum(current_instr$item == w) / nkids_by_age$n[which(ages==cur_age)]
      if (proportion >= 0.5) {
        item_data$age[which(item_data$item == w)] = cur_age
      }
    }
  }
  word_aoa<- item_data %>%
    mutate(age=ifelse(is.na(age),"no",age))
  return(word_aoa)
}


######################################################################################################################
trim_definition<-function(def_list){
  def_list<- def_list %>%
    mutate(definition= gsub(" \\s*\\([^\\)]+\\)","", definition)) %>%
    mutate(definition= gsub("[*].*$","", definition)) %>%
    mutate(definition= gsub("\\/.*", "", definition)) %>%
    mutate(definition= gsub("[[:punct:]]", "", definition)) 
  return(def_list)
}
######################################################################################################################
write_out_csv<-function(name){
  write.csv(paste(getwd(),"/out_files/",name, sep = "" ))
}
######################################################################################################################
write_out_csv<- function(var, lang, type){
  write.csv(var, paste(getwd(),"/out_files/",lang, "_",type,".csv",sep = ""), row.names = F)
}
