# NLP functions from py in R using reticulate

if(!require(reticulate)) {install.packages("reticulate")}
library(reticulate) 
library(stringr)
require(tidyverse)

penn_treebank = read.csv('https://raw.githubusercontent.com/sudhir-voleti/sample-data-sets/master/penn_treebank.csv', 
                           header=TRUE)

# defining a purely clean_text op
clean_text <- function(text, lower=FALSE, alphanum=FALSE, drop_num=FALSE){
  text  =  str_replace_all(text, "<.*?>", " ")   # drop html junk
 
  if (lower=="TRUE") {text = text %>% str_to_lower()}
  if (alphanum=="TRUE") {text = text %>% str_replace_all("[^[:alnum:]]", " ")}
  if (drop_num=="TRUE") {text = text %>% str_replace_all("[:digit:]", "")}

 # collapse multiple spaces
  text = text %>%   
    str_replace_all("\\\\s+", " ")  

  return(text) } # clean_text() ends

# sentence-tokenizing routine (coz tidytext's sent-tokenizer ain't great)
py.sent_tokenize = function(text) {
 
  require(reticulate)
  require(dplyr)
  nltk = import("nltk")
  
  sent_list = vector(mode="list", length=length(text))  
  counter = 0

  for (i in 1:length(text)){  
    sents = nltk$tokenize$sent_tokenize(text[i])
    sent_list[[i]] = data.frame(docID = i, 
	sentID = counter + seq(1:length(sents)), 
	text = sents, 
  	stringsAsFactors=FALSE)

   counter = max(sent_list[[i]]$sentID)   }    # i ends

  sent_df = bind_rows(sent_list)   
  return(sent_df)  }   # func ends

### Func 1: write an R func to POSTag with py
py.postag <- function(text){
	
# If py.annotate fails, goto CMD prompt & type the following (using #0 as line break)
#0 python  #0 import nltk  #0 nltk.download()
# In the open downloader window, download 'punkt' for 'Punkt tokenizer models', 'average perceptron tagger', etc.
  
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

postag_desc <- function(postag_df, penn_treebank){  
  
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
  text_annotated_df = text_df %>% postag_desc(penn_treebank)
  
  return(text_annotated_df) }    # py.annotate() func ends

### +++ Build [noun | verb] phrase detector (& later, extractor) func

phrase_detector <- function(doc_df, noun=TRUE){   # one annotated document in df form
	
  if (!("pos_tag" %in% colnames(doc_df))) {doc_df = py.annotate(doc_df)}	
  
  # defining phrase components
  verb_phrase = c("RB", "RBR", "RBS", "VB", "VBD", "VBG", "VBN", "VBP", "VBZ", "PRP", "PRP$")
  noun_phrase = c("JJ", "JJR", "JJS", "NN", "NNP", "NNS", "NNPS")
	
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

    if (!("pos_tag" %in% colnames(corpus_df))) {corpus_df = py.annotate(corpus_df)}	
    corpus_split_list = split(corpus_df, corpus_df$doc_num)
  
  if (noun == "FALSE"){phrase_list = lapply(corpus_split_list, function(x) {phrase_detector(x, noun=FALSE)})} else 
  {phrase_list = lapply(corpus_split_list, function(x) {phrase_detector(x)})}  
  
  return(phrase_list)    }   # extract_phrases() func ends

      
### +++ Build [person | Org | GPE] entity detector (& later, extractor) func

## func to do doc-wise entity collection          
entity_list_doc <- function(doc_df){ 
  
  # code exceptions for insufficient df rows
  if (nrow(doc_df) == 0) {b0 = NULL}
  if (nrow(doc_df) < 3) {
    b0 = paste0(doc_df$token, collapse = " ")
  } else {
    
    index = 1  
    for (i1 in 2:nrow(doc_df)){
      
      # not needed now since entity_type = "PERSON" is prespecified
      # a01 = (doc_df$entity_type[i1] == doc_df$entity_type[i1-1]) 
      
      a02 = (doc_df$chunk_type[i1-1] == "I")*(doc_df$chunk_type[i1] == "B")
      if (!a02) {doc_df$index_col[i1] = index; 
      doc_df$index_col[i1-1] = index} else 
      { index = index + 1;
      doc_df$index_col[i1] = index}
    } # i1 loop ends
    
    b0 = sapply(seq(1:max(doc_df$index_col)),
                function(x) {
                  paste0(doc_df$token[(doc_df$index_col == x)], collapse = " ") })
  } # else ends
  
  return(b0)    }    # entity_list_doc() func ends

      
## build wrapper func around entity_list_doc() entity extractor  
extract_entity <- function(text_df,   # annotated df with ner=TRUE 
                           entity = "PERSON"){    # Alternately, "ORGANIZATION", "GPE"  
  
  require(tidyverse)
  
  # retain only relevant rows w.r.t. named_entity field
  match_term1 = paste0("B-", entity)
  match_term2 = paste0("I-", entity)
  logi_vec = (text_df$named_entity == match_term1 | text_df$named_entity == match_term2)
  text_df_subsetted = text_df[logi_vec,]
  head(text_df_subsetted, 10)
  
  # narrowing analysis df to only relevant colms
  selected_df = text_df_subsetted %>% 
    select(token, named_entity, doc_num) %>%    # first, keep relevant colms
    mutate(serial_num = row_number()) %>%     # build a serial_num colm
    mutate(index_col = 0) %>%
    separate(named_entity, c("chunk_type", "entity_type")) # %>% head()
  head(selected_df)
  
  docs = unique(selected_df$doc_num)
  
  outp_list = lapply(docs, 
                     function(x){entity_list_doc(selected_df[(selected_df$doc_num == x),])})
  
  # doc_df = selected_df[(selected_df$doc_num == 1),]
  
  return(outp_list)    }    # extract_entity() func ends

# example is below, commented out based on ibm corpus
# text_df = py.annotate(ibm_corpus, ner = TRUE); ibm_orgs_list = extract_entity(text_df, entity = "ORGANIZATION") 
###      

### === Reticulate an R func to snowball.stem a dtm's colnames ===
py.dtm_stemmer <- function(dtm1){

 tokens = colnames(dtm1)
 require(reticulate)
 nltk = import("nltk")   # import nltk
 nltk.stem = nltk$stem	
 snowb_stemmer = nltk.stem$SnowballStemmer   # from nltk.stem.snowball import SnowballStemmer
 # snowb_stemmer('english')$stem('technologies') # example
 b0 = sapply(tokens, snowb_stemmer('english')$stem)    # 2.44 secs for 11k tokens

 ## deconstruct the dtm and reassemble later
 b1 = mapply(identical, b0, colnames(dtm1))   # see ?mapply 
 dtm1_preserve = dtm1[, b1]    # 1k x 4412
 dtm1_old = dtm1[, (!b1)]    # 1k x 6597
 
 b2 = setdiff(b0, colnames(dtm1))   # asymm set difference
 dtm1_new = dtm1[,1:length(b2)] 
 colnames(dtm1_new) = NULL
 b3 = vector(length=length(b2), mode="list")
	
 for (i1 in 1:length(b2)){
    b00 = which((b0 == b2[i1]))
    dtm1_new[, i1] = apply(as.matrix(dtm1[,b00]), 1, sum)
    b3[i1] = colnames(dtm1)[b00[1]]		 
    if ((i1 %% 100) == 0) { cat(i1, "\n")}
	 }    # hah.

 colnames(dtm1_new) = b3
 
 dtm2 = cbind(dtm1_preserve, dtm1_new)

 return(dtm2)    }     # func ends

# try on nokia dataset
# dtm_nokia = nokia %>% clean_text(lower=TRUE, alphanum=TRUE) %>% bigram_replace() %>%
#		dtm_cast() %>% preprocess_dtm()  # 1.65 secs
# dim(dtm_nokia)    # 120x443
# dtm_nokia = py.dtm_stemmer(dtm_nokia)   # 0.55 secs
# dim(dtm_nokia)    # 120x400. ~10% drop
      
