#=======================================
# func 1 is text.clean(x)
#=======================================

text.clean = function(x){                    # x = text data
  
  require("tm")
  x  =  gsub("<.*?>", " ", x)               # regex for removing HTML tags
  x  =  iconv(x, "latin1", "ASCII", sub="") # Keep only ASCII characters
  x  =  gsub("[^[:alnum:]]", " ", x)        # keep only alpha numeric 
  x  =  tolower(x)                          # convert to lower case characters
  #  x  =  removeNumbers(x)                    # removing numbers
  x  =  stripWhitespace(x)                  # removing white space
  x  =  gsub("^\\s+|\\s+$", "", x)          # remove leading and trailing white space
  
  # Read Stopwords list
  stpw1 = readLines('https://raw.githubusercontent.com/sudhir-voleti/basic-text-analysis-shinyapp/master/data/stopwords.txt')# stopwords list
  stpw2 = tm::stopwords('english')      # tm package stop word list; tokenizer package has the same name function, hence 'tm::'
  comn  = unique(c(stpw1, stpw2))         # Union of two list
  stopwords = unique(gsub("'"," ",comn))  # final stop word lsit after removing punctuation
  
  x  =  removeWords(x,stopwords)            # removing stopwords created above
  x  =  stripWhitespace(x)                  # removing white space
  #  x  =  stemDocument(x)                   # can stem doc if needed.
  
  return(x)
}


#=======================================
# func 2 is dtm_tcm builder in text2vec
#=======================================

dtm.tcm.creator <- function(text,
                            id = "",
                            std.clean = TRUE,
                            #                          std.stop.words = TRUE,
                            #                          stop.words.additional = c('a','b'),
                            bigram.encoding = TRUE,
                            bigram.min.freq = 2,
                            min.dtm.freq = 2,
                            skip.grams.window = 5) {    # for TCM local window half-length
  
  if (class(text) != "character" | length(text) < 3){
    stop("data format Not correct. Make sure it's a character verctor of length above 3")
  }
  
  if ((id == "")[1]){
    id = 1:length(text)
  }
  
  require(tm)
  if (std.clean == TRUE) {
    print("Performing Standard Text Cleaning")
    
    text = text.clean(text)
  }
  
  #    if (std.stop.words == TRUE){
  #      print("Removing Stop Words")
  #      stop.words.f = unique(c(stpw3,stop.words.additional))
  #      text = removeWords(text,stop.words.f)            # removing stopwords created above
  #      text = stripWhitespace(text)                  # removing white spacestop.words.additional
  #    }
  
  require(text2vec)
  tok_fun = word_tokenizer  # using word & not space tokenizers
  
  if (bigram.encoding == TRUE){
    
    # data = data.frame(id = 1:length(text),text = text, stringsAsFactors = F)
    
    print("finding bi-grams for encoding with selected criteria")
    
    it_0 = itoken( text,
                   tokenizer = tok_fun,
                   ids = id,
                   progressbar = T)
    
    vocab = create_vocabulary(it_0, ngram = c(2L, 2L))
    pruned_vocab = prune_vocabulary(vocab, term_count_min = bigram.min.freq)
    replace_list = pruned_vocab$vocab$terms[order(pruned_vocab$vocab$terms_counts, decreasing = T)]
    
    if (length(replace_list) > 0){
      text = paste("",text,"")
      
      pb <- txtProgressBar(min = 1, max = (length(replace_list)), style = 3) ; i = 0
      
      print(paste("Encoding",length(replace_list),"bi-grams as unigram"))
      for (term in replace_list){
        i = i + 1
        focal.term = gsub("_", " ",term)        # in case dot was word-separator
        replacement.term = term
        text = gsub(paste("",focal.term,""),paste("",replacement.term,""), text)
        setTxtProgressBar(pb, i)
      }                  
    } else {
      print("No bigram to encode with selected criteria")}
  }
  
  print("Creating Document Term Matrix")
  # Create DTM
  it_m = itoken(text,
                tokenizer = tok_fun,
                ids = id,
                progressbar = T)
  
  vocab = create_vocabulary(it_m)
  pruned_vocab = prune_vocabulary(vocab,
                                  term_count_min = min.dtm.freq)
  
  vectorizer = vocab_vectorizer(pruned_vocab)
  
  dtm_m  = create_dtm(it_m, vectorizer)
  dtm = as.DocumentTermMatrix(dtm_m, weighting = weightTf)
  
  print("Creating Term Co-occurrence Matrix")
  
  vectorizer = vocab_vectorizer(pruned_vocab,
                                grow_dtm = FALSE,
                                skip_grams_window = skip.grams.window)
  
  tcm = create_tcm(it_m, vectorizer) # func to build a TCM
  
  print("Done!!")
  out = list(dtm = dtm, tcm = tcm)
  
  return(out)
}

### +++ tidytext based cast DTM +++ ###
require(tidytext)
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

#=================================
# func 3 is wordcloud builder
#=================================

dtm.word.count <- function(dtm) {
  
  if (ncol(dtm) > 1000) {
    tst = round(ncol(dtm)/100)  # divide DTM's cols into 100 manageble parts
    a = rep(tst,99)
    b = cumsum(a);rm(a)
    b = b[-which(b >= ncol(dtm))]
    b = c(0,b,ncol(dtm))
    
    ss.col = c(NULL)
    for (i in 1:(length(b)-1)) {
      tempdtm = dtm[,(b[i]+1):(b[i+1])]
      s = colSums(as.matrix(tempdtm))
      ss.col = c(ss.col,s)
    }
  } else {
    ss.col = colSums(as.matrix(dtm))
  }
  
  tsum = ss.col
  tsum = tsum[order(tsum, decreasing = T)]       #terms in decreasing order of freq
  return(tsum)
}

# example below
# dtm = out[[1]]; tsum = dtm.word.count(dtm)


#_____________________________________________________#
#_____________________________________________________#

dtm.word.cloud <- function(count = count, title = "Title", max_words){
  
  require(wordcloud)
  
  if (class(count)[1] == "DocumentTermMatrix"|class(count)[1] == "simple_triplet_matrix")
  {
    tsum = dtm.word.count(count)
  } else {
    tsum = count
  }
  
  if (class(tsum) != "numeric") stop("Give input as wordcount or DocumentTermMatrix")
  
  wordcloud(names(tsum), tsum,     # words, their freqs 
            scale = c(4, 0.5),     # range of word sizes
            1,                     # min.freq of words to consider
            max.words = max_words,       # max #words
            colors = brewer.pal(8, "Dark2"))    # Plot results in a word cloud 
  title(sub = title)     # title for the wordcloud display
}   

# example dtm.word.cloud(dtm, "wordcloud from dtm", 100)

#=================================
# func 4 is COG builder
#=================================

distill.cog.tcm = function(mat1, # input TCM or DTM MAT
                           title, # title for the graph
                           s,    # no. of central nodes
                           k1){  # max no. of connections  
  require(igraph)
  
  mat1 = as.matrix(mat1)
  mat1 = t(mat1) %*% mat1
  
  
  if (ncol(mat1) > 1000) {
    tst = round(ncol(mat1)/100)  # divide mat1's cols into 100 manageble parts
    a = rep(tst,99)
    b = cumsum(a);rm(a)
    b = b[-which(b >= ncol(mat1))]
    b = c(0,b,ncol(mat1))
    
    ss.col = c(NULL)
    for (i in 1:(length(b)-1)) {
      tempmat1 = mat1[,(b[i]+1):(b[i+1])]
      su = colSums(as.matrix(tempmat1))
      ss.col = c(ss.col,su);rm(su)
    }
  } else {
    ss.col = colSums(as.matrix(mat1))
  }
  
  # a = colSums(mat1) # collect colsums into a vector obj a
  a = ss.col
  b = order(-a)     # nice syntax for ordering vector in decr order  
  
  mat2 = mat1[b, b]     # order both rows and columns along vector b
  
  diag(mat2) =  0
  
  ## +++ go row by row and find top k adjacencies +++ ##
  
  wc = NULL
  
  for (i1 in 1:s){ 
    thresh1 = mat2[i1,][order(-mat2[i1, ])[k1]]
    mat2[i1, mat2[i1,] < thresh1] = 0   # neat. didn't need 2 use () in the subset here.
    mat2[i1, mat2[i1,] > 0 ] = 1
    word = names(mat2[i1, mat2[i1,] > 0])
    mat2[(i1+1):nrow(mat2), match(word,colnames(mat2))] = 0
    wc = c(wc,word)
  } # i1 loop ends
  
  mat3 = mat2[match(wc, colnames(mat2)), match(wc, colnames(mat2))]
  ord = colnames(mat2)[which(!is.na(match(colnames(mat2), colnames(mat3))))]  # removed any NAs from the list
  mat4 = mat3[match(ord, colnames(mat3)), match(ord, colnames(mat3))]
  
  graph <- graph.adjacency(mat4, mode = "undirected", weighted=T)    # Create Network object
  
  
  graph = simplify(graph) 
  V(graph)$color[1:s] = "green"
  V(graph)$color[(s+1):length(V(graph))] = "pink"
  
  graph = delete.vertices(graph, V(graph)[ degree(graph) == 0 ]) # delete singletons?
  
  plot(graph, 
       layout = layout.kamada.kawai, 
       main = title)
} 

# distill.cog.tcm(dtm, "DTM based cog", 4, 5)
