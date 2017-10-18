require(tidyverse)
require(tidytext)
require(stringr)

bigram_replace <- function(text){
  
  require(tidyverse)
  require(tidytext)
  require(stringr)
  
  ## basic cleaning exercises
  text  =  gsub("<.*?>", " ", text)	# drop html junk
  
  text = text %>%   	# v cool. mke this part of std cleanup procs in text-an
    str_to_lower %>% # make text lower case
    #      str_replace_all("[^[:alnum:]]", " ") %>%  # remove non-alphanumeric symbols
    str_replace_all("\\s+", " ")  	 # collapse multiple spaces
  
  ## drop particular stop_words from raw text corpus - of , the, at, 
  # stop1 = apply(as.data.frame(stop_words$word), 1, function(x) paste0(" ", x, " "))   # overly long. Pointless
  stop2 = c(" of ", " the ", " at ")    # get rid of connectors inside proper-noun strings
  for (i1 in 1:length(stop2)){ text = gsub(stop2[i1], "", text) }
  
  
  ## now build bigrams list
  textdf = data_frame(text)	# convert to tibble
  
  bigram_df <- textdf %>%
    unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
    separate(bigram, c("word1", "word2"), sep = " ") %>%
    
    dplyr::filter(!word1 %in% stop_words$word) %>%
    dplyr::filter(!word2 %in% stop_words$word) %>%		
    
    count(word1, word2, sort = TRUE) %>%
    filter(n > 2) %>%
    unite(bigram1, word1, word2, sep = " ", remove = FALSE) %>%
    unite(bigram2, word1, word2, sep = "_") 
  # bigram_df
  
  ## replace bigrams with single tokens
  for (i1 in 1:nrow(bigram_df)){ 
    text = gsub(bigram_df$bigram1[i1], bigram_df$bigram2[i1], text) }
  
  # output processed corpus
  return(text)	
  
} # bigram_replace() ends

### +++ new func to cast DTMs outta processed corpora +++ ###

dtm_cast <- function(text){
  
  ## basic cleaning exercises
  text  =  gsub("<.*?>", " ", text)	# drop html junk
  
  text = text %>%   	# v cool. mke this part of std cleanup procs in text-an
    str_to_lower %>% # make text lower case
    #      str_replace_all("[^[:alnum:]]", " ") %>%  # remove non-alphanumeric symbols but removes sentence delimiters too
    str_replace_all("\\s+", " ")  	 # collapse multiple spaces
  
  textdf = data_frame(text)
  textdf1 = textdf %>%
    mutate(doc = row_number()) %>%    # row_number() is v useful.
    group_by(doc) %>%
    unnest_tokens(word, text) %>%
    anti_join(stop_words) %>%
    count(word, sort = TRUE)
  
  # cast into a Matrix object
  m <- textdf1 %>%
    cast_sparse(doc, word, n)
  #  class(m)
  
  return(m) }  # dtm_cast() ends