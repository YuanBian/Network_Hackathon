######################################################################################################
# Espeak function
# CAVEAT !!! I didn't include all languages in WB (Only the languages that are said to be "reliable" in the Espeak website)
Speak<- function(lang, word){
  langs<- c("English (British)","English (American)", "Spanish", "Croatian", 
            "Italian", "Danish", "Norwegian", "Turkish", "Slovak", "Swedish", "Russian", "German")
  abbr <- c("en-rp", "en-us", "es", "hr", "it", "da", "no", "tr","sk", "sv","ru","de")
  lang_abbr<- abbr[which(langs==lang)]
  IPA<- system(paste("speak -q -v ", lang_abbr, " --ipa ", "\"", word, "\"",sep=""), intern=T)
  return(IPA)
}

######################################################################################################
# delete space in IPA
delete_space<- function(IPA_list){
  IPA_list<- IPA_list %>%
    mutate(IPA= gsub(" ","", IPA))
  return(IPA_list)
}

######################################################################################################
# trim IPA
trim_IPA<- function(IPA_list){
  IPA_list<- IPA_list %>%
    mutate(IPA= gsub(" ","", IPA)) %>%
    mutate(IPA= gsub("[[:punct:]]", "",IPA))
  return(IPA_list)
}


######################################################################################################
# set threshold for IPA distance
IPA_threshold<- function(IPA_list, threshold){
  IPA_list<- IPA_list %>%
    mutate(link=as.numeric(dist<=threshold))
  return(IPA_list)
}

