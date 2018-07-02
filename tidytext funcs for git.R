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

# +++ below is workflow to keyword filter a corpus, as follows: 
# py.sent_tokenize() %in% keyword_filt_corpus() %in% iterated_keyword_filt()
## === sentence tokenizing using py
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

## +++
# keyword filtering a corpus - efficient implementation (sentence based). Needs py!
keyword_filter_corpus <- function(raw_corpus, wordlist=NULL, half.window.size=1, delimiter=" "){

  library(tidyverse)
  library(tidytext)

  # first, sentence tokenize the corpus
  sent_df = py.sent_tokenize(raw_corpus)  # colms are {docID, sentID, text (of sentences)}
  
  # routine to pre-process wordlist
  if (is.null(wordlist)) {print("Enter valid wordlist"); stop}
  wlist_df = wordlist %>% data_frame() %>% rename("word" = ".") %>% unique() %>% mutate(wlist = word)
    wlist_df
	
  # word-tokenize corpus by sentence & merge with wlist_df
  corpus_df = sent_df %>% unnest_tokens(word, text) 
  corpus_df1 = left_join(corpus_df, wlist_df, by=c("word" = "word"))  

  # Use logical-colms to ID sentences for extraction.
  a100 = which(!(is.na(corpus_df1$wlist)))    # row_nums of keywords found
  a101 = corpus_df[a100,] %>% select(docID, sentID) 
  a101 %>% head()

    a102 = a101 %>% select(sentID) %>% unique()   # unique sentences containing keywords
    a102e = a102$sentID   # focal sentence with keyword in it
    a102a = a102e + half.window.size    # sentences following the focal keyword
    a102b = a102e - half.window.size    # sentences preceding the focal keyword

      # boundary exception handling
      if (max(a102a) > max(corpus_df$sentID)) {a102a[length(a102a)] = max(corpus_df$sentID)}
      if (min(a102b) ==0) {a102b[1] = 1}

    a102x = data.frame(start=a102b, stop=a102a); head(a102x)      
    a102c = apply(a102x, 1, function(x) {seq(from=x[1], to=x[2])}) %>% unlist() %>% unique()	# unique superset of all sentences to ID         
    a102c = a102c[order(a102c)]  # vector of ordered a102c elements

    # find doc boundaries
    a103a = corpus_df$docID
    a103b = c(a103a[2:length(a103a)], a103a[length(a103a)])	
    a103 = which(!(a103a == a103b))    # row_nums representing doc boundaries
    
    a104 = corpus_df$sentID[a103]+1    # sentences right after doc boundaries. 
    
   # sentences to extract
   a105 = which(!(a102c %in% a104))
   a106 = a102c[a105]
     a106a = order(a106)
     a106 = a106[a106a]    # list of sentences to extract

  # build list to store extracted sents doc-wise
  sent_df1 = sent_df[a106,]
  a107 = unique(sent_df1$docID)  
    out_list = vector(mode="list", length=length(a107))
    # out_df = data.frame(docID = numeric(), filt.text = character(), stringsAsFactors=FALSE)
    counter = 0
    for (i in a107){ 
        counter = counter+1
	b100 = sent_df1[sent_df1$docID == i,] 
	b101 = str_c(b100$text, collapse=delimiter)
	out_list[[counter]] = data.frame(docID = i, filt.text = b101, stringsAsFactors=FALSE)
	} # i ends
      
  out_df = bind_rows(out_list)

  return(out_df)  }   # keyword_filter_corpus() func ends

 # testing keyword_filter_corpus() func
 # wordlist = c("service", "services", "solution", "solutions", "subscription", "subscribe", "utility", "API", "cloud",
 #		"consult", "consulting", "consultancy")
 # raw_corpus = readRDS("C:\\Users\\20052\\Dropbox\\teaching related\\Data An for FPM 2018\\session 9 topic modeling\\bd.df.2009.Rds")
 # system.time({ out_df = keyword_filter_corpus(raw_corpus[1:100], wordlist, half.window.size=2, delimiter="+++") })    # 1.47 secs for 100 RF docs

 ## code an iterated version of above and test
 iterated_keyword_filt <- function(raw_corpus, wordlist=NULL, bite.size=100, half.window.size=1){

  # build iterator sequence
  file.seq = seq(from=1, to=length(raw_corpus), by=bite_size) 
    if (max(file.seq) < length(raw_corpus)) { file.seq[length(file.seq)] = length(raw_corpus)}
    file.seq
  
  # build and populate list
  n1 = length(file.seq)
  out_list = vector(mode="list", length=n1)
  for (i1 in 1:(n1-1))  {
	start = file.seq[i1]; stop = file.seq[i1+1]-1
	out_list[[i1]] = raw_corpus[start:stop]}

  out_list[[n1]] =  raw_corpus[file.seq[n1-1]:file.seq[n1]]

  out_list1 = lapply(out_list, function(x) keyword_filter_corpus(x, wordlist=wordlist, half.window.size=half.window.size))  

  out_corpus = bind_rows(out_list1)

  return(out_corpus) }   # iterated_keyword_filt() func ends
 
  # testing above for 1 year full BD corpus
  # system.time({ out_corpus = iterated_keyword_filt(bd.2009$bd.text, wordlist)})    # 16.65 secs for full corpus! Almost linear scaling.

		     
## == brew efficient func to build bigrams (and upto trigrams).
  replace_bigram <- function(corpus, min_freq = 2){  # corpus has 1 unnamed character colm

 library(tidyverse)
 library(tidytext)
 library(stringr)
  
  # first filter out stopwords - c("of ", "the ", " and").
   corpus_df = corpus %>% data_frame() %>% rename(text=".") %>%
			mutate( text = str_replace_all(text, " of ", " "),
				text = str_replace_all(text, " and ", " "),
				text = str_replace_all(text, " [Tt]he ", " "),
				text = str_replace_all(text, "\\\\s+", "\\s"))

  textdf = data.frame(docID=seq(1:nrow(corpus_df)), text=corpus, stringsAsFactors=FALSE)

  # Unnesting bigrams
  a0 = textdf %>% 	
	 # bigram-tokenize, count and filter by freq
	 unnest_tokens(ngram, text, token = "ngrams", n = 2) 
   head(a0)

   # creating frequent bigrams for replacement
   a1 = a0 %>% 
	   count(ngram, sort=TRUE) %>% filter(n >= min_freq) %>% 
	   separate(ngram, c("word1", "word2"), sep=" ", remove=FALSE) %>% 

		# drop all stopwords in the bigrams of interest
    		dplyr::filter(!word1 %in% stop_words$word) %>%
		dplyr::filter(!word2 %in% stop_words$word) %>%
	   	
	   unite(bigram1, c("word1", "word2"), sep="_")	 %>% 
	   dplyr::select(ngram, bigram1)    # dplyr:: coz MASS also has select()
   a1
	 
  # merging the 2 above dfs
  a2 = left_join(a0, a1, by=c("ngram" = "ngram")) %>%
	 separate(ngram, c("word1", "word2"), sep=" ", remove=FALSE) %>%
	 dplyr::select(-ngram) # %>% mutate(out_colm = bigram1)
  head(a2)

  ## using logical colms to solve repeats wala problem
  a400 = (is.na(a2$bigram1))
    # a400a = which(!a400)   # orig bigram locations
    a2$bigram1[a400] = a2$word1[a400]
  head(a2)
  
  a401 = which(!a400)  # orig bigram locations
  a402 = a401 + 1
	if (max(a402) > nrow(a2)) { a402[length(a402)] = nrow(a2) }

  a403 = (a2$docID[a401] == a2$docID[a402])   # is bigram inside the document vs at its boundary?

  # what if there are consecutive bigrams?  
  a403a = (a403)*(!(a402 %in% a401))  # bigrams are inside docs and NOT consecutive

  a404 = a402*a403a
  a405 = a404[(a404 > 0)]  # these are the extra terms or repeats to be dropped.

  a2$bigram1[a405] = ""

  # use logical-colms to solve token-repeats in consecutive bigrams
  a403b = (a403)*(a402 %in% a401)  # consec bigrams inside docs, logical colm
  a404b = a402*a403b       
  a405b = a404b[(a404b > 0)]    # row_nums of consec, inside bigrams
 
	# subroutine to drop middle-wala repeating token
	a405c = a405b -1    # first bigram ka location
	newgram = a2$bigram1
	newgram[a405c] = paste(a2$word1[a405c], a2$word1[a405b], a2$word1[a405b+1], sep="_")
	newgram[a405b] = ""
	a2$bigram1 = newgram

  # using colm-logicals to solve last-word-dropoff wala problem
  a500 = a2$docID
    a501 = c(a500[2:length(a500)], a500[length(a500)])	
    a502 = which(a500 != a501)    # docID boundaries
    a503 = !(a502 %in% a401)   # these are the ones to insert
    a503a = a502[a503]    

    a2$bigram1[a503a] = paste(a2$bigram1[a503a], a2$word2[a503a])
	
  # rebuilding corpus, now at doc layer
  doc_corpus = data.frame(docID = numeric(), text = character(), stringsAsFactors=FALSE)
  a201 = unique(a2$docID)

  for (i2 in a201){
    a200 = a2[a2$docID == i2,] 	
    doc_corpus[i2, 1] = a200$docID[1]	
    doc_corpus[i2, 2] = str_c(a200$bigram1, collapse=" ")

    if (i2 %% 1000 == 0) {cat(i2, " docs processed\n")}
    }    # i2 ends
 
  return(doc_corpus) }

 # testing above on speech data
 # speech = readLines('https://raw.githubusercontent.com/sudhir-voleti/sample-data-sets/master/PM%20speech%202014.txt')
 # system.time({ bigrammed_corpus = replace_bigram(speech, min_freq = 2) }) # 0.09 secs
 # bigrammed_corpus[1:5,]

## +++ writing an iterated version of above for efficiency since it doesn't scale linearly with corpus size
 iterated_bigram_replace <- function(raw_corpus, bite_size=100, min_freq=3){

  # build iterator sequence
  file.seq = seq(from=1, to=length(raw_corpus), by=bite_size) 
    if (max(file.seq) < length(raw_corpus)) { file.seq[length(file.seq)] = length(raw_corpus)}
    file.seq
  
  # build and populate list
  n1 = length(file.seq)
  out_list = vector(mode="list", length=n1)
  for (i1 in 1:(n1-1))  {
	start = file.seq[i1]; stop = file.seq[i1+1]-1
	out_list[[i1]] = raw_corpus[start:stop]}

  out_list[[n1]] =  raw_corpus[file.seq[n1-1]:file.seq[n1]]

  out_list1 = lapply(out_list, function(x) replace_bigram(x, min_freq = min_freq))  

  out_corpus = bind_rows(out_list1)

  return(out_corpus) }    # func ends

 # testing on large iterated corpus for efficiency gains
 # bd.2009 = readRDS("C:\\Users\\20052\\Dropbox\\teaching related\\Data An for FPM 2018\\session 9 topic modeling\\bd.df.2009.Rds")		     
 # system.time({ out_corpus = iterated_bigram_replace(bd.2009$bd.text) })    # < 50 secs for 1 year full corpus. O(n+).

 ### +++ new func to cast DTMs outta processed corpora +++ ###
casting_dtm <- function(text_as_df,    	 # text_as_df is single df colm 
			tfidf=FALSE,     
			use.stopwords=TRUE,    # whether to use stopwords at all 
			additional.stopwords=NULL){    # which additional stopwords to add

  ## tokenizing the corpus
   textdf1 = text_as_df %>% 
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

  # reorder dtm to have sorted rows by doc_num and cols by colsums	
  m = m[order(as.numeric(rownames(m))),]    # reorder rows	
  b0 = apply(m, 2, sum) %>% order(decreasing = TRUE)
  m = m[, b0]

  return(m) }    # func ends

 # testing the func
 # mydata = readLines('https://raw.githubusercontent.com/sudhir-voleti/sample-data-sets/master/Technolgy%20pitches%20from%20kickstarter.txt')
 # Piping a workflow based on 4 sourced funcs
 # my_dtm = mydata %>% clean_text(lower=TRUE) %>% replace_bigram(min_freq=2) %>% select(text) %>% casting_dtm(tfidf=FALSE)
  
## === small pipe-able routine to clean DTMs of empty rows/colms ===
 nonempty_dtm <- function(dtm){

	# drop empty rows from dtm
	a100 = apply(dtm, 1, sum); a101 = (a100 == 0)
		dtm = dtm[!(a101), ]

	# dropempty colms from dtm
	a200 = apply(dtm, 2, sum); a201 = (a200 == 0)
		dtm = dtm[, !(a201)]

  return(dtm) }

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

# === stand-alone func to convert dtm directly to tfidf
dtm_to_tfidf <- function(dtm){  # ideally in dgCMatrix form
  require(text2vec)
  require(Matrix)	
     if (class(dtm) != "dgCMatrix") {dtm = Matrix(dtm, sparse=TRUE)}	
	
  model_tfidf = TfIdf$new()
  dtm_tfidf = model_tfidf$fit_transform(dtm)	
 return(dtm_tfidf)}
	     
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

## === func 2 build cog_ggraph from dtm directly ===
dtm.2.ggraph <- function(dtm, 
			max_tokens = 30,
			max_edges = 150, 
                        drop.stop_words=TRUE,
                        new.stopwords=NULL)
{

 require(tidyverse)
 require(tidytext)

 dtm = as.matrix(dtm)	
	
# elim stopwords from dtm
 stop.words1 = c(tidytext::stop_words$word, new.stopwords);    # length(stop.words1) 
 logi.vec1 = (colnames(dtm) %in% stop.words1);   #  logi.vec1 %>% head();    sum(logi.vec1)
 if (sum(logi.vec1) >0) {dtm = dtm[, !(logi.vec1)]}  # drop DTM colmns which are stopwords

# choose a sample of say top max_tokens words that remain and continue
 dtm.col.sum = apply(dtm, 2, sum)
   a0 = sort(dtm.col.sum, decreasing=TRUE, index.return=TRUE)
   dtm = dtm[,(a0$ix[1:max_tokens])];  #  dim(nokia_dtm2)
   dtm = as.matrix(dtm)
   tt.mat = t(dtm) %*% dtm;  #  dim(tt.mat)

# build word-pair type df now based on tt.mat lower triangular
 diag(tt.mat) = 0
 n = ncol(tt.mat); n1 = n*(n-1)/2
 word_pairs = data.frame(item1=character(n1), item2=character(n1), n=numeric(n1), stringsAsFactors=FALSE)

 i0 = 0
 for (i1 in 1:(n-1)){
   for (i2 in (i1+1):n){

	i0 = i0+1
	word_pairs$item1[i0] = colnames(tt.mat)[i1]
	word_pairs$item2[i0] = rownames(tt.mat)[i2]
	word_pairs$n[i0] = tt.mat[i2, i1]

	}}

# Insert wordfreq as new colmn to word_pairs
 dtm.col.sum = apply(dtm, 2, sum) 
   word_counts = data.frame(word=colnames(dtm), wordfr = dtm.col.sum, stringsAsFactors=FALSE)
   word_pairs = word_pairs %>% left_join(word_counts, by = c("item1" = "word"))
   row_thresh = min(nrow(word_pairs), max_edges)

# now plot
  set.seed(1234)
  # windows()
  plot_d <- word_pairs %>% 
	    filter(n >= 3) %>%
	    top_n(row_thresh) %>%   igraph::graph_from_data_frame() # graph object built!

  dfwordcloud = data_frame(vertices = names(V(plot_d))) %>% 
		left_join(word_counts, by = c("vertices"= "word"))

 # build ggraph now
  plot_obj = plot_d %>%   
	    ggraph(layout = "fr") +
	    geom_edge_link(aes(edge_alpha = n, edge_width = n), edge_colour = "cyan4")  +
	    # geom_node_point(size = 5) +
	    geom_node_point(size = log(dfwordcloud$wordfr)) +
	    geom_node_text(aes(label = name), repel = TRUE, 
                   point.padding = unit(0.2, "lines"),
                   size = 1 + log(dfwordcloud$wordfr)) +
	    theme_void()

 return(plot_obj) 

 } # dtm.2.ggraph() func ends

 # check with sample dataset
 # ibm = readLines("https://raw.githubusercontent.com/sudhir-voleti/sample-data-sets/master/International%20Business%20Machines%20(IBM)%20Q3%202016%20Results%20-%20Earnings%20Call%20Transcript.txt")
 # source("https://raw.githubusercontent.com/sudhir-voleti/code-chunks/master/cba%20tidytext%20funcs%20for%20git%20upload.R")
 # dtm.ibm = ibm %>% 
 #		text.clean(., remove_numbers=FALSE) %>% 
 #		dtm_build(.) %>% 
 #		streamline_dtm(., min_occur=0.01, max_occur=0.80)

  # dtm.2.ggraph(dtm.ibm, max_tokens=50)	
	     
