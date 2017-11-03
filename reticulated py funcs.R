# NLP functions from py in R using reticulate

try(require(reticulate)|| install.packages("reticulate"))
library(reticulate) 
library(stringr)

# defining a purely clean_text op
clean_text <- function(text, lower=FALSE, alphanum=FALSE){
  
  require(stringr)
  text  =  str_replace_all(text, "<.*?>", " ")   # drop html junk
  text = text %>%   
    # str_to_lower %>% 	# make text lower case
    # str_replace_all("[^[:alnum:]]", " ") %>%  # remove non-alphanumeric symbols
    str_replace_all("\\\\s+", " ")  # collapse multiple spaces
  
  if (lower=="TRUE") {text = text %>% str_to_lower()}
  if (alphanum=="TRUE") {text = text %>% str_replace_all("[^[:alnum:]]", " ")}
  
  return(text)    }  # clean_text() ends

### Func 1: write an R func to POSTag with py
py.postag <- function(text){
  
  require(reticulate)
  
  nltk = import("nltk")   # Import nltk
  tokens = nltk$word_tokenize(text)    # first tokenizing raw text
  
  tagged_list = nltk$pos_tag(tokens)   # next, POStagging
  
  # Write paired list as a R dataframe
  r_df = data.frame(slnum = 1:length(tagged_list),
                    token = sapply(tagged_list, `[[`, 1),    # list-extract opr `[[` used
                    pos_tag = sapply(tagged_list, `[[`, 2))  # extract all second elems in the list
  
  return(r_df)    }    # py.postag() func ends

### Func 2: py.NER in R
py.ner <- function(text){
  
  require(reticulate)
  nltk = import("nltk")   # Import nltk
  
  ne_chunk <- nltk$ne_chunk
  tree2tag <- nltk$chunk$tree2conlltags
  
  b0 = nltk$chunk$tree2conlltags(
    nltk$ne_chunk(
      nltk$pos_tag(
        nltk$word_tokenize(text))))
  
  ne_df = data.frame(slnum = 1:length(b0),
                     token = sapply(b0, `[[`, 1),
                     pos_tag = sapply(b0, `[[`, 2),
                     named_entity = sapply(b0, `[[`, 3))
  
  return(ne_df)    } # py.ner() func ends

# Func 3: merge penn_treebank desc onto the outp df 
# https://www.ling.upenn.edu/courses/Fall_2003/ling001/penn_treebank_pos.html

postag_desc <- function(postag_df){  
  
  penn_treebank = read.csv('https://raw.githubusercontent.com/sudhir-voleti/sample-data-sets/master/penn_treebank.csv', 
                           header=TRUE)
  
  penn_treebank = penn_treebank[,-1]
  
  b0 = merge(postag_df, penn_treebank,
             by.x = 'pos_tag', by.y = 'Tag')
  
  b1 = b0[with(b0, order(doc_num, slnum)),]
  
  return(b1) } # postag_desc() func ends


## Func 4: lemmatize with py in R
py.lemmatize <- function(text){
  
  require(reticulate)
  nltk = import("nltk")   # Import nltk
  
  nltk.stem <- import("nltk.stem")
  wordnet_lemm = nltk.stem$WordNetLemmatizer()
  
  tokens = nltk$word_tokenize(text)     # chr vector
  
  # wordnet_lemm$lemmatize('walking', pos ='v')   # hello, hello, mike testing.
  system.time({ a2 = sapply(tokens, wordnet_lemm$lemmatize) })    # 3.67 secs for 1 doc. slow.
  
  a0 = data.frame(names(a2), a2, stringsAsFactors = FALSE)
  colnames(a0) = c("original", "lemmatized")
  
  return(a0)     }   # py.lemmatize() func ends

## Func 5: One func to bind them all.

# create master func to link through py funcs 1-3 for corpus inputs
py.annotate <- function(corpus, ner = FALSE){
  
  require(reticulate)
  nltk = import("nltk")   # Import nltk
  
  clean_corpus = clean_text(corpus)
  
  if (ner == "TRUE") {text_list = lapply(clean_corpus, function(x) {py.ner(x)})} else { 
    text_list = lapply(clean_corpus, function(x) {py.postag(x)})}
  
  for (doc in 1:length(text_list)){ text_list[[doc]]$doc_num = doc    }
  text_df = bind_rows(pgp_list)
  text_annotated_df = pgp_df %>% postag_desc()
  
  return(text_annotated_df) }    # py.annotate() func ends
