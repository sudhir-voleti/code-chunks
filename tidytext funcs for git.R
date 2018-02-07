require(tidyverse)
require(tidytext)
require(stringr)
require(text2vec)   # for tfidf transform in the preprocessing dtm func
require(Matrix)	  # in dtm_merge() func to bind sparse matricies
library(widyr)   # for pairwise_count() func in building tidy based cogs
library(ggraph)  # for tidy based cogs

# +++ defining a purely clean_text op
clean_text <- function(text, lower=FALSE, alphanum=FALSE, drop_num=FALSE){
  text  =  str_replace_all(text, "<.*?>", " ")   # drop html junk
 
  if (lower=="TRUE") {text = text %>% str_to_lower()}
  if (alphanum=="TRUE") {text = text %>% str_replace_all("[^[:alnum:]]", " ")}
  if (drop_num=="TRUE") {text = text %>% str_replace_all("[:digit:]", "")}

 # collapse multiple spaces
  text = text %>%   
    str_replace_all("\\\\s+", " ")  

  return(text) } # clean_text() ends

# +++
replace_bigram <- function(text, min_freq = 2){
  
  require(tidyverse)
  require(tidytext)
  require(stringr)
   
  ## drop particular stop_words from raw text corpus - of , the, at, 
  # stop1 = apply(as.data.frame(stop_words$word), 1, function(x) paste0(" ", x, " "))   # overly long. Pointless
  stop2 = c(" of ", " the ", " at ")    # get rid of connectors inside proper-noun strings

  b0 = sapply(stop2, function(x) {text = str_replace_all(text, x, " ")});   # rm(b0)
  
  ## now build bigrams list
  textdf = data_frame(text)	# convert to tibble
  
  bigram_df <- textdf %>%
    unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
    separate(bigram, c("word1", "word2"), sep = " ") %>%
    
    dplyr::filter(!word1 %in% stop_words$word) %>%
    dplyr::filter(!word2 %in% stop_words$word) %>%
    
    count(word1, word2, sort = TRUE) %>%
    filter(n > min_freq) %>%     # threshold kicks in here
    unite(bigram1, word1, word2, sep = " ", remove = FALSE) %>%
    unite(bigram2, word1, word2, sep = "_") 
  # bigram_df
  
  ## replace bigrams with single tokens
  if (nrow(bigram_df) > 0) { 
	b0 = sapply(bigram_df$bigram1, function(x) {text = str_replace_all(text, x, str_replace(x, " ", "_"))})
	} else {text = text};    # rm(b0)	
	
  return(text)	
  
} # replace_bigram() ends

 ## === small pipe-able routine to clean DTMs of empty rows/colms ===
 nonempty_dtm <- function(dtm){

	# drop empty rows from dtm
	a100 = apply(dtm, 1, sum); a101 = (a100 == 0)
		dtm = dtm[!(a101), ]

	# dropempty colms from dtm
	a200 = apply(dtm, 2, sum); a201 = (a200 == 0)
		dtm = dtm[, !(a201)]

  return(dtm) }

### +++ new func to cast DTMs outta processed corpora +++ ###

casting_dtm <- function(text,    	 # text is raw corpus 
			tfidf=FALSE,     
			use.stopwords=TRUE,    # whether to use stopwords at all 
			additional.stopwords=NULL){    # which additional stopwords to add

  ## basic cleaning exercises. Obviates need for clean_text()
  text  =  gsub("<.*?>", " ", text)	# drop html junk
  text = text %>%   	# v cool. mke this part of std cleanup procs in text-an
    str_to_lower %>% # make text lower case
    str_replace_all("\\s+", " ")  	 # collapse multiple spaces

  ## tokenizing the corpus
   textdf = data_frame(text)
   textdf1 = textdf %>%
    mutate(docID = row_number()) %>%    # row_number() is v useful.
    group_by(docID) %>%
    unnest_tokens(word, text) %>%
    count(word, sort = TRUE) %>% ungroup()

  ## make stop.words list
   stop.words = data.frame(word = as.character(unique(c(additional.stopwords, stop_words$word))),
				stringsAsFactors=FALSE)	

    if (use.stopwords == "TRUE"){ textdf1 = textdf1 %>% anti_join(stop.words) }

  ## cast into a Matrix object
  if (tfidf == "TRUE") {
	textdf2 = textdf1 %>% group_by(docID) %>% 
		count(word, sort=TRUE) %>% ungroup() %>%
		bind_tf_idf(word, docID, nn) %>% 
		rename(value = tf_idf)} else { textdf2 = textdf1 %>% rename(value = n)  }

  m <- textdf2 %>% cast_sparse(docID, word, value)
  #  class(m)

  # reorder dtm to have sorted rows by doc_num and cols by colsums	
  m = m[order(as.numeric(rownames(m))),]    # reorder rows	
  b0 = apply(m, 2, sum) %>% order(decreasing = TRUE)
  m = m[, b0]

  return(m) }  # func casting_dtm() ends

 # testing the func
  # speech = readLines('https://raw.githubusercontent.com/sudhir-voleti/sample-data-sets/master/PM%20speech%202014.txt')
  # system.time({ speech_dtm_tf = speech %>% casting_dtm() })    # 0.05 secs
  # system.time({ speech_dtm_idf = speech %>% casting_dtm(tfidf=TRUE) })   # 0.07 secs 


### +++ new func to preprocess n prune DTMs +++ ###

require(text2vec)
# require(tm)

preprocess_dtm <- function(dtm, min_occur = 0.01, max_occur = 0.50){
  
  require(text2vec)
  
  # build tfidf wala dtm first
  dtm_tfidf = fit_transform(dtm, TfIdf$new())
    
  # filter out irrelevant tokens based on idf colsums
  a0 = apply(dtm_tfidf, 2, sum) %>% summary() 
  idf_thresh = as.numeric(a0[2])
  a1 = (a0 > idf_thresh)
  dtm = dtm[, a1];    # dim(dtm)
  rm(a0, a1)
  
  # drop tokens failing a min or max doc_occurrence threshold
  a0 = apply(dtm, c(1,2), function(x) ifelse(x>0, 1, 0))
  a1 = apply(a0, 2, sum);    summary(a1)
  min_thresh = min_occur*nrow(dtm)    # drop if token occurs in < 1% of docs
  a2 = (a1 > min_thresh)
  a2_dtm = dtm[, a2];    # dim(a2_dtm)
  
  max_thresh = max_occur*nrow(dtm)     # drop if token occurs in > 50% of docs 
  a1 = apply(a2_dtm, 2, sum)
  a3 = (a1 <= max_thresh)
  a3_dtm = a2_dtm[, a3];    # dim(a3_dtm) 
  a0 = order(as.numeric(rownames(a3_dtm)))
  a3_dtm = a3_dtm[a0,]          
  # rm(a0, a1, a2, a3, a2_dtm)
  
  return(a3_dtm)    # pre-processed dtm output
  
    }  # func ends

# example try
# system.time({ nokia_dtm_processed = preprocess_dtm(nokia_dtm) })    # 1.48 secs
# dim(nokia_dtm_processed)
	     
## === func to merge DTMs (needed when iterating) === ##
merge_dtm <- function(dtm1, dtm2){
require(Matrix)	
colnames1 = colnames(dtm1)	
colnames2 = colnames(dtm2)

# first merge cols that do match b/w both DTMs
union_cols = intersect(colnames1, colnames2)
	a00 = (colnames(dtm1) %in% union_cols)
	dtm1_union = dtm1[, a00]

	a01 = (colnames(dtm2) %in% union_cols)
	dtm2_union = dtm2[, a01]

	# reorder colnames to make colnames identical
	a02 = order(colnames(dtm1_union))
	a03 = order(colnames(dtm2_union))

	dtm1_union = dtm1_union[, a02]	
	dtm2_union = dtm2_union[, a03]	

if (identical(colnames(dtm1_union), colnames(dtm2_union))) { 
	merged_dtm = rbind(dtm1_union, dtm2_union) } else {cat("colnames mismatch b/w DTMs")}

# divide non-intersecting portion of merged_dtm into 4 quadrants starting top-left clockwise: 2 & 4 being zero matrices
unique_cols = setdiff(unique(c(colnames1, colnames2)), intersect(colnames1, colnames2))
	a00 = (colnames(dtm1) %in% unique_cols)    
	dtm1_unique = dtm1[, a00]	# quadrant 1 ready
	quad_4 = Matrix(0, nrow=nrow(dtm2), 
			ncol=ncol(dtm1_unique), sparse=TRUE) 	# quadrant 4 ready

	a01 = (colnames(dtm2) %in% unique_cols)
	dtm2_unique = dtm2[, a01]	# quadrant 3 ready
	quad_2 = Matrix(0, nrow=nrow(dtm1), 
			ncol=ncol(dtm2_unique), sparse=TRUE) 	# quadrant 2 ready

	colnames(quad_4) = colnames(dtm1_unique)
	rownames(quad_4) = rownames(dtm2_unique)
	colnames(quad_2) = colnames(dtm2_unique)
	rownames(quad_2) = rownames(dtm1_unique)

# start binding the pieces together now ...
quad_14 = rbind(dtm1_unique, quad_4)
quad_23 = rbind(quad_2, dtm2_unique)

merged_dtm = cbind(merged_dtm, quad_14, quad_23) # dtm1_unique, quad_2)  # worx.

	return(merged_dtm) 	} # merge_dtm() func ends

# testing above func on nokia data
# nokia = readLines('https://github.com/sudhir-voleti/sample-data-sets/raw/master/text%20analysis%20data/amazon%20nokia%20lumia%20reviews.txt')
# nokia1 = sample(nokia[1:60], 25);    nokia2 = sample(nokia[61:120], 25)
# dtm1 = nokia1 %>% clean_text(lower=TRUE, alphanum=TRUE) %>%  bigram_replace() %>% dtm_cast()
# dtm2 = nokia2 %>% clean_text(lower=TRUE, alphanum=TRUE) %>% bigram_replace() %>% dtm_cast()
# system.time({ nokia_merged_dtm = merge_dtm(dtm1, dtm2) })    # 0.08 secs	     

## === funcs tying in above funcs with iterators for large corpora ===
## dtm-builder-seq func
dtm_seq <- function(text, lower1=TRUE, alphanum1=TRUE,
                    min_occur1=0.02, max_occur1=0.75){
  
  dtm1 = text %>% clean_text(lower=lower1, alphanum=alphanum1) %>%  
    replace_bigram() %>% dtm_cast() %>% 
    preprocess_dtm(min_occur=min_occur1, max_occur=max_occur1)
  
    return(dtm1)}   # dtm_seq() func ends

## build an iterator func
iterate <- function(files, bite_size = 50){
  
  n1 = length(files)
  seq1 = seq(from=1, to=n1, by=bite_size)
  n2 = length(seq1)
  if (n1 > max(seq1)){ if ((n1 - max(seq1)) < 0.5*bite_size) {seq1[n2] = n1} else {
    seq1 = c(seq1, n1+1);  n2 = length(seq1) }}
  
  start = seq1[1:(n2-1)]
  stop = seq1[2:n2]-1; 
  stop[(n2-1)]=n1
  df1 = data.frame(start, stop); df1
  
  outp_list = vector(mode="list", length=(n2-1))
  for (i1 in 1:(n2-1)){ outp_list[[i1]] = files[df1$start[i1]:df1$stop[i1]] }
  return(outp_list)    }    # func ends. Yields a list output of iterated data shards

## build iterated dtm builder func
build_dtm_iterated <- function(text_col, bite_size1 = 25,   # iterate defaults
                               lower1=TRUE, alphanum1=TRUE, # dtm_seq defaults
                               min_occur1=0.02, max_occur1=0.75){    # preprocess_dtm defaults
  
  outp_list = iterate(text_col, bite_size=bite_size1) # 0.03 secs for 1k docs
  # text_col = filt_bigrm_corp$chunk
  
  # system.time({ dtm_list = lapply(outp_list, dtm_seq) })   # takes way too long.
  dtm_list = vector(mode = "list", length = length(outp_list))
  for (i2 in 1:length(dtm_list)){
    dtm_list[[i2]] = dtm_seq(outp_list[[i2]], 
                             lower=lower1, alphanum=alphanum1,
                             min_occur1, max_occur1)    # dtm_seq() func used with defaults
    
    cat("Processed list element no. ", i2,"\n")  }  # t < 214 secs
  
  dtm1 = dtm_list[[1]]
  for (i1 in 2:length(dtm_list)){ dtm1 = merge_dtm(dtm1, dtm_list[[i1]])  } # 1.12 secs
  dtm1 = preprocess_dtm(dtm1, min_occur=min_occur1, max_occur=max_occur1) # 21 secs
  return(dtm1)    }    # build_dtm_iterated() func ends

## trial above on Nokia before uploading to git
# nokia = readLines('https://github.com/sudhir-voleti/sample-data-sets/raw/master/text%20analysis%20data/amazon%20nokia%20lumia%20reviews.txt')
# dtm_nokia = build_dtm_iterated(nokia, bite_size=15)   # 0.89 secs
	     
	     

concordance.r <- function(text1,  # corpus
                          word1,  # focal word for whcih context is sought
                          k){     # context window length in words on either side
  
  require(magrittr)
  require(tidytext)
  require(dplyr)
  require(tidyr)
  
  text1 = gsub('<.*?>', "", text1)   # drop html junk
  
  text_df <- data_frame(text1) %>% 
    unnest_tokens(word, text1) %>% 
    
    # build an index for word positions in the corpus
    mutate(index = 1) %>% mutate(wordnum = 1:sum(index)) %>% dplyr::select(-index) #%>%
  
  text_df
  
  # locate context words for each instance of the focal word
  a0 = which(text_df$word == word1)
  a1 = matrix(0, nrow = length(a0), ncol = 3)
  colnames(a1) = c("start", "focal", "stop")
  for (i1 in 1:nrow(a1)){a1[i1, 1] = max(0, a0[i1]-k) 
  a1[i1, 2] = a0[i1]
  a1[i1, 3] = min(nrow(text_df), a0[i1]+k)  }
  head(a1)
  
  require(stringi)
  # creat a list to store the contexts or concordances of word1  
  list0 = vector("list", length = length(a0))
  for (i2 in 1:length(list0)){
    list0[[i2]] = stri_join(text_df$word[a1[i2,1]:a1[i2, 3]], collapse=" ") 
    list0[[i2]] = gsub(word1, paste0('*', word1, '*', collapse=""), list0[[i2]])   # gsub(pattern, replacement, x)
  } # i2 ends
  list0[[2]]
  
  # read list into dataframe for easier display of output  
  list_df = data.frame(NULL)
  for (i2 in 1:length(a0)){list_df[i2,1] = list0[[i2]]}
  colnames(list_df) = 'concordance'
  
return(list_df) } # func ends
	     
# +++
	     
## tidytext based wordcloud + COG combo
build_cog_ggraph <- function(corpus,   # text colmn only
                             max_edges = 150, 
                             drop.stop_words=TRUE,
                             new.stopwords=NULL){
  
  # invoke libraries
  library(tidyverse)
  library(tidytext)
  library(widyr)
  library(ggraph)
  library(igraph)
  
  # build df from corpus
  corpus_df = data.frame(docID = seq(1:length(corpus)), text = corpus, stringsAsFactors=FALSE)
  
  # eval stopwords condn
  if (drop.stop_words == TRUE) {stop.words = unique(c(stop_words$word, new.stopwords)) %>% 
    as_tibble() %>% rename(word=value)} else {stop.words = stop_words[2,]}
  
  # build word-pairs
  tokens <- corpus_df %>% 
    
    # tokenize, drop stop_words etc
    unnest_tokens(word, text) %>% anti_join(stop.words)
    
    # pairwise_count() counts #token-pairs co-occuring in docs
  word_pairs = tokens %>% pairwise_count(word, docID, sort = TRUE, upper = FALSE)# %>% # head()
  
  word_counts = tokens %>% count( word,sort = T) %>% dplyr::rename( wordfr = n)
  
  word_pairs = word_pairs %>% left_join(word_counts, by = c("item1" = "word"))
  
  row_thresh = min(nrow(word_pairs), max_edges)
  
  # now plot
  set.seed(1234)
  # windows()
  plot_d <- word_pairs %>%
    filter(n >= 3) %>%
    top_n(row_thresh) %>%   igraph::graph_from_data_frame() 
  
  dfwordcloud = data_frame(vertices = names(V(plot_d))) %>% left_join(word_counts, by = c("vertices"= "word"))
  
  plot_obj = plot_d %>%   # graph object built!
    
    ggraph(layout = "fr") +
    geom_edge_link(aes(edge_alpha = n, edge_width = n), edge_colour = "cyan4")  +
    # geom_node_point(size = 5) +
    geom_node_point(size = log(dfwordcloud$wordfr)) +
    geom_node_text(aes(label = name), repel = TRUE, 
                   point.padding = unit(0.2, "lines"),
                   size = 1 + log(dfwordcloud$wordfr)) +
    theme_void()
  
  return(plot_obj)    # must return func output
  
}  # func ends

# quick example for above func. uncomment & run.
# speech = readLines('https://raw.githubusercontent.com/sudhir-voleti/sample-data-sets/master/PM%20speech%202014.txt')
# b0 = build_cog_ggraph(speech); b0    # < 1 second
