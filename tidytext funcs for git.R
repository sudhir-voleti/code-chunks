require(tidyverse)
require(tidytext)
require(stringr)
require(text2vec)   # for tfidf transform in the preprocessing dtm func
require(Matrix)	  # in dtm_merge() func to bind sparse matricies

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

  b0 = sapply(stop2, function(x) {text = str_replace_all(text, x, " ")});   rm(b0)
  
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
	} else {text = text};    rm(b0)	
	
  return(text)	
  
} # replace_bigram() ends

### +++ new func to cast DTMs outta processed corpora +++ ###

dtm_cast <- function(text){   # text is corpus; tidytext::cast_dtm() is already taken
  
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

  # reorder dtm to have sorted rows by doc_num and cols by colsums	
  m = m[order(as.numeric(rownames(m))),]    # reorder rows	
  b0 = apply(m, 2, sum) %>% order(decreasing = TRUE)
  m = m[, b0]; rm(b0)	      
	      
  return(m) }  # dtm_cast() ends

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
	     
	     
