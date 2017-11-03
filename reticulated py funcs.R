# NLP functions from py in R using reticulate

try(require(reticulate)|| install.packages("reticulate"))
library(reticulate) 
library(stringr)
require(tidyverse)

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
  text_df = bind_rows(text_list)
  text_annotated_df = text_df %>% postag_desc()
  
  return(text_annotated_df) }    # py.annotate() func ends

### +++ Build [noun | verb] phrase detector (& later, extractor) func

# defining phrase components
verb_phrase = c("RB", "RBR", "RBS", "VB", "VBD", "VBG", "VBN", "VBP", "VBZ", "PRP", "PRP$")
noun_phrase = c("JJ", "JJR", "JJS", "NN", "NNP", "NNS", "NNPS")

phrase_detector <- function(doc_df, noun=TRUE){   # one document in df form
  
  if (noun == "FALSE"){phrase_compts = verb_phrase} else {phrase_compts = noun_phrase}
  
  serial_num = seq(1:nrow(doc_df))
  phrase_index = rep(0, nrow(doc_df))
  doc_df1 = data.frame(serial_num, phrase_index, doc_df)
  logical_vec = (doc_df$pos_tag %in% phrase_compts)
  doc_df_subsetted = doc_df1[logical_vec,]
  # head(doc_df_subsetted, 10)
  
  # drop all rows with F-T-F pattern
  n1 = nrow(doc_df_subsetted)
  try(if(n1 < 2) stop("not enough df rows"))
  if (n1 > 5) { 
    current = doc_df_subsetted$serial_num
    before = c(0, doc_df_subsetted$serial_num[1:(n1-1)])  
    after = c(doc_df_subsetted$serial_num[2:n1], 0)
    drop = ((current - before) != 1)*((after-current) != 1); # sum(drop)
    
    doc_df_subsetted = doc_df_subsetted[(!drop),]
    # head(doc_df_subsetted, 15)
  } # if ends
  
  try(if(nrow(doc_df_subsetted) <3) stop("not enough df rows"))
  if(nrow(doc_df_subsetted) <3)  {b0 = NULL} else {
    
    # build loop for detecting phrases    
    index = 1
    for (i1 in 2:nrow(doc_df_subsetted)){
      if ((doc_df_subsetted$serial_num[i1] - doc_df_subsetted$serial_num[i1-1]) == 1) {
        doc_df_subsetted$phrase_index[i1] = index;  
        doc_df_subsetted$phrase_index[i1-1] = index} else {index = index+1}
    } # i1 loop ends
    
    # paste0(doc_df_subsetted$token[(doc_df_subsetted$phrase_index == 2)], collapse = " ")  
    
    b0 = sapply(seq(2:max(doc_df_subsetted$phrase_index)),
                function(x) {
                  paste0(doc_df_subsetted$token[(doc_df_subsetted$phrase_index == x)], collapse = " ") }) # 0.02 secs
    
  } # else ends
  
  return(b0)    } # phrase_detector() func ends

## build a corpus-level wrapper func around phrase-detector()
extract_phrases <- function(corpus_df, noun=TRUE){   # output from py.annotate()

    corpus_split_list = split(corpus_df, corpus_df$doc_num)
  
  if (noun == "FALSE"){phrase_list = lapply(corpus_split_list, function(x) {phrase_detector(x, noun=FALSE)})} else 
  {phrase_list = lapply(corpus_split_list, function(x) {phrase_detector(x)})}  
  
  return(phrase_list)    }   # extract_phrases() func ends
