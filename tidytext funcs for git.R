require(tidyverse)
require(tidytext)
require(stringr)
require(text2vec)   # for tfidf transform in the preprocessing dtm func

bigram_replace <- function(text, min_freq = 2){
  
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
    filter(n > min_freq) %>%     # threshold kicks in here
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

## +++  write func to do LMD fit metrics for topicmodels +++
try(require(Rmpfr) || install.packages("Rmpfr")); library(Rmpfr)
harmonicMean <- function(logLikelihoods, precision=2000L) {
  
  require(Rmpfr)
  llMed <- median(logLikelihoods)
  as.double(llMed - log(mean(exp(-mpfr(logLikelihoods,
                                       prec = precision) + llMed))))
  }    # harmonicMean() func ends

LMD_fit <- function(dtm, seq_k,
                    burnin = 1000, iter = 1000, keep = 50){
  
  require(topicmodels)
  require(Rmpfr)
  
  # apply LDA for each k
  fitted_many <- lapply(seq_k, 
                        function(k) LDA(dtm, k = k, method = "Gibbs",
                                        control = list(burnin = burnin, iter = iter, keep = keep) ))
  
  # extract logliks from each topic
  logLiks_many <- lapply(fitted_many, function(L)  L@logLiks[-c(1:(burnin/keep))])
  
  # compute harmonic means
  hm_many <- sapply(logLiks_many, function(h) harmonicMean(h))
  
  return(hm_many)
  }   # LMD_fit() func ends

# downstream analyses for above LMD func
# plot(seq_k1, hm_many1, type = "l", xlab = "num_topics", ylab = "Log Marginal Density")
# seq_k1[which.max(hm_many1)]    # compute optimum number of topics                    

## +++ Want to try perplexity based fit as a func? +++ ##
# func to compute perplexity scores for both trg & validn samples
perpl_fit <- function(train_set, valid_set, k,
                      burnin = 1000, iter = 1000, keep = 50){
  
  require(topicmodels)
  # define fitted model below
  fitted <- topicmodels::LDA(train_set, 
                             k = k, method = "Gibbs",
                             control = list(burnin = burnin, iter = iter, keep = keep))
  
  a0 = topicmodels::perplexity(fitted, newdata = as.matrix(train_set)) # about 567
  a1 = topicmodels::perplexity(fitted, newdata = as.matrix(valid_set)) # about 928
  
  return(list(a0, a1))
  
} # perpl_fit() func ends

# func for perplexity calcs in m-fold cross-validn setting, for seq_k
perpl_cv_mfold <- function(dtm, folds = 5, seq_k){
  
  # build df to store perplexity results in	
  num_topic = rep(seq_k, folds)
  fold_num = NULL; for (i1 in 1:folds){fold_num = c(fold_num, rep(i1, length(seq_k)))}	
  trg_perpl = c(0)
  test_perpl = c(0)
  store_df = data.frame(num_topic, fold_num, trg_perpl, test_perpl)
  
  # split data into cross-validated trg n test samples
  splitfolds <- sample(1:folds, nrow(dtm), replace = TRUE)
  for(i in 1:folds){
    train_set <- dtm[splitfolds != i , ]
    valid_set <- dtm[splitfolds == i, ]
    
    a00 = lapply(seq_k, function(k) 
    {perpl_fit(train_set, valid_set, k)})
    
    a01 = matrix(unlist(a00), length(a00), 2, byrow=TRUE)
    a02 = (store_df$fold_num == i) 				
    store_df$trg_perpl[a02] = a01[, 1]
    store_df$test_perpl[a02] = a01[, 2]
    
    cat(i, "\n")		
    
  } # i loop ends
  
  # ggplotting the results
  ggplot(store_df, aes(x = num_topic, y = test_perpl)) +
    geom_point() +
    geom_smooth(se = FALSE) +
    
    ggtitle("5-fold cross-validation of topic modelling with the Nokia dataset",
            "(ie 5 different models fit for each candidate number of topics)") +
    
    labs(x = "Candidate number of topics", y = "Perplexity when fitting the trained model to the hold-out set")
  
  
  return(store_df)
  
} # perpl_cv_mfold() func ends
                    
