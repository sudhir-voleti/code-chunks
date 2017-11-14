## === funcs to run LDA, fit best model, and post-process results === ##

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

## === Analysing the LTM output objects ===

## func to extract top loading tokens per topic
top_n_tokens <- function(Gibbs,   # Gibbs is the Gibbs outp obj
                         n=15,     # num of top loading words to show
                         exclusive=FALSE){   # top tokens exclusive or shared across topics? 
  
  if (class(Gibbs) != "LDA_Gibbs") {stop("Error - Wrong Input. Not an LDA object")}
  
  # first extract the theta = tokens x topics matrix
  terms = slot(Gibbs, "terms")   # vector of tokens
  beta = slot(Gibbs, "beta")    # matrix obj of log parms of the token distribution for each topic
  beta = t(beta)   # to make it tokens x topix
  n1 = ncol(beta)   # num_topics
  rownames(beta) = terms
  
  theta = apply(beta, 1, function(x) {x - log(sum(exp(x)))}); dim(theta) 
  if (ncol(theta) != n1) {theta = t(theta)}   # always check after apply() func family when axis=1
  if (theta[1,1] <0) { theta_exp = apply(theta, c(1,2), exp) }  # log-scaled probabilities 
  # if (ncol(theta_exp) != n1) {theta_exp = t(theta_exp)}
  
  # evaluate exclusvitiy condition. i.e., 1 token maps to 1 topic only
  if (exclusive == "TRUE") {theta1 = apply(theta_exp, 1, function(x) {x*(x == max(x))}) } else
  { theta1 = theta_exp }
  if (ncol(theta1) != n1) {theta1 = t(theta1)}
  
  # now populate list with top_n tokens
  top_tokens_list = vector(mode="list", length=n1)
  for (i1 in 1:n1){ 
    b00 = theta1[(order(-theta1[, i1])[1:n]), i1]
    top_tokens_list[[i1]] = data.frame(top_tokens = names(b00), 
                                       values = as.numeric(b00))   } # i1 loop ends
  
  return(top_tokens_list)   }  # top_n_tokens() func ends

# test an example for func above
# top_tokens_list = top_n_tokens(Gibbs, exclusive=TRUE)   # <1 sec
# top_tokens_list[[1]]; top_tokens_list[[10]]; 

## === Func to extract top loading docs  === ##
top_n_docs <- function(Gibbs, n=15, doc_names=NULL){
  
  if (class(Gibbs) != "LDA_Gibbs") {stop("Error - Wrong Input. Not an LDA object")}
  
  gamma = slot(Gibbs, "gamma")    # matrix obj of posterior topic distribution for each document
  if (is.null(doc_names)) {doc_names=rownames(gamma)} else {rownames(gamma) = doc_names}
  
  # now populate list with top_n tokens
  top_docs_list = vector(mode="list", length=ncol(gamma))
  for (i1 in 1:ncol(gamma)){ 
    b00 = gamma[(order(-gamma[, i1])[1:n]), i1]
    top_docs_list[[i1]] = data.frame(top_docs = names(b00), 
                                     values = as.numeric(b00))   } # i1 loop ends
  
  return(top_docs_list)    }  # top_n_docs() func ends

# test an example 
# top_docs_list = top_n_docs(Gibbs, n=20, doc_names=doc_names)    # 0.04 secs


## === build a show_wordcloud func for df input ===
show_wordcloud <- function(input_df){   # first colm is words, next is freq
  
  require(wordcloud)
  if (ncol(input_df)<2 | is.null(ncol(input_df))) {stop("Func input Error")}
  words = input_df[,1]
  freq = input_df[,2]
  freq1 = freq/max(freq)   # to give it decent range
  
  # now build wordcloud
  windows()   # open in new window
  suppressWarnings(
    wordcloud(words, freq1, scale=c(4, .5), min.freq=2) ) 
}  # show_wordcloud() func ends

# try example
#     top_tokens_list = top_n_tokens(Gibbs, n=100)
#     show_wordcloud(top_tokens_list[[16]])   # <2 secs

# building input for show_cog()
tokens = as.character(top_tokens_list[[16]]$top_tokens)
dtm1=readRDS("E:\\R miscell\\dtm1.Rds")
cols=colnames(dtm1)
a0 = (cols %in% tokens); sum(a0)
dtm1_subset = dtm1[, a0]; dim(dtm1_subset)

# == general cog builder func that we'll invoke later ==
build_cog <- function(dtm1, # input DTM
                      title="co-occurrence graph (COG)", # title for the graph
                      num_central_nodes=4,    # no. of central nodes s
                      num_connections=5){     # max no. of connections k1  
  
  require(igraph) 
  mat1 = as.matrix(dtm1)
  mat1 = tcrossprod(t(mat1))
  
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
  
  a = ss.col
  b = order(-a)     # nice syntax for ordering vector in decr order  
  mat2 = mat1[b, b]     # order both rows and columns along vector b  
  diag(mat2) =  0
  
  ## +++ go row by row and find top k adjacencies +++ ##  
  wc = NULL  
  for (i1 in 1:num_central_nodes){ 
    thresh1 = mat2[i1,][order(-mat2[i1, ])[num_connections]]    # k1^th biggest num in i1^th row
    mat2[i1, mat2[i1,] < thresh1] = 0   # neat. didn't need 2 use () in the subset here.
    mat2[i1, mat2[i1,] > 0 ] = 1
    word = names(mat2[i1, mat2[i1,] > 0])
    mat2[(i1+1):nrow(mat2), match(word,colnames(mat2))] = 0    # drop links to all other nodes
    wc = c(wc,word)    # central nodes & their peripherals?
  } 
  
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
}   # build_cog(dtm) func ends


# == wrapper func around build_cog() ==
show_cog <- function(tokens_list,  # tokens_list is outp from top_n_tokens() for n>=100
                     topic_k = 1, # choose which topic number to show  
                     dtm1,   # input full dtm in
                     title = "main title here", 
                     num_central_nodes=4,    # no. of central nodes s
                     num_connections=5){     # max no. of connections k1  
  
  # Extract relevant dtm subset based on topic_k chosen
  tokens = as.character(tokens_list[[topic_k]]$top_tokens)
  a0 = (colnames(dtm1) %in% tokens); 
  dtm1_subset = dtm1[, a0]; # dim(dtm1_subset)
  
  # now build cog
  windows()
  build_cog(dtm1_subset, title=title, 
            num_central_nodes = num_central_nodes, 
            num_connections = num_connections)
}   # show_cog() ends

# example for show_cog() above
# show_cog(top_tokens_list, topic_k=8, dtm1, title="COG for topic 8")
# system.time({ show_cog(top_tokens_list, topic_k=18, dtm1, title="COG for topic 18") }) # 0.09 secs

# build an iterator func over corpora ==
iterator_seq <- function(corpus, bite_size = 50){
  if (class(corpus) != "data.frame") {corpus = data_frame(corpus)} 
  iter_seq = seq(from = 1, to = (nrow(corpus)+1), by = bite_size)      
  if (max(iter_seq) < nrow(corpus)) {iter_seq = c(iter_seq, (nrow(corpus)+1))} 
  
  # edit last bite. If small, merge with previous bite
  n1 = length(iter_seq)
  if ((iter_seq[n1] - iter_seq[n1-1]) < 0.5*bite_size) {
    iter_seq[n1-1] = iter_seq[n1]; iter_seq = iter_seq[1:(n1-1)]}
  
  return(iter_seq)    }   # corpus_iterator() func ends

## === Build show_trajectory() func ===

## == defining pre-req funcs for show_movements func ==

## calc_dist() func for a given firm
calc_dist <- function(dtm_subset, cumul=FALSE){
  dist_mat = matrix(0, nrow=nrow(dtm_subset), ncol=1)
  if (cumul == "TRUE") {
    for (i2 in 2:nrow(dist_mat)){ dist_mat[i2,1] = sqrt(sum((dtm_subset[i2,]-dtm_subset[(i2-1),])^2)) +  dist_mat[(i2-1),1] }
    colnames(dist_mat) = "cumul_distance"	} else {
      for (i2 in 2:nrow(dist_mat)){ dist_mat[i2,1] = sqrt(sum((dtm_subset[i2,]-dtm_subset[(i2-1),])^2)) }
      colnames(dist_mat) = "total_distance" }
  return(dist_mat) }    # func ends

## calc_disp() func for a given firm
calc_disp <- function(dtm_subset){
  disp_mat = matrix(0, nrow=nrow(dtm_subset), ncol=1)
  for (i2 in 2:nrow(disp_mat)){ disp_mat[i2,1] = sqrt(sum((dtm_subset[i2,]-dtm_subset[1,])^2))  }
  colnames(disp_mat) = "net_displacement"
  return(disp_mat) }    # func ends

## func to extract firm indices of interest
show_firm_indices <- function(firm_name1="yahoo "){ which(str_detect(firm_detail$firm_name, firm_name1)) }

## multiplot() needed for below
# http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
      
    }}}


## === building a show_movements() func for any focal firms list ===
show_movements <- function(firms_sample=firms_sample,   # list of firms to see movements for
                           firm_names_short=NULL,   # since partial matching can net different firms
                           by_variable=FALSE){
  
  require(ggplot2)
  
  if (is.null(firm_names_short)) {firm_names_short = firms_sample}
  
  # define empty vector for master-df
  df0 = data.frame(year=factor(), firm = character(), variable=character(), value=numeric(), stringsAsFactors=FALSE) 
  
  a02 = lapply(firms_sample, function(x){show_firm_indices(x)}); # a02[[2]]
  
  for (i1 in 1:length(a02)){
    year = firm_detail[a02[[i1]], 1] 
    firm_name = firm_names_short[i1]
    locations = gamma[a02[[i1]],]
    cumul_dists = calc_dist(locations, cumul=TRUE)
    step_dists = calc_dist(locations)
    disps = calc_disp(locations)
    
    df1 = data.frame(year, firm=rep(firm_name,length(year)), variable=rep("cumul_distance",length(year)), value=as.numeric(calc_dist(locations, cumul=TRUE)), stringsAsFactors=FALSE)
    df2 = data.frame(year, firm=rep(firm_name,length(year)), variable=rep("step_distance",length(year)), value=as.numeric(calc_dist(locations)), stringsAsFactors=FALSE)
    df3 = data.frame(year, firm=rep(firm_name,length(year)), variable=rep("displacement",length(year)), value=as.numeric(calc_disp(locations)), stringsAsFactors=FALSE)
    df0 = suppressWarnings(bind_rows(df0, df1, df2, df3))
    
  } # i1 ends. master-df df0 built
  
  if (by_variable == FALSE){
    # building separate plots by firm
    firms = names(table(df0$firm))
    plots = vector(mode="list", length=length(firms))
    for (i1 in 1:length(firms)){
      firm_focal = firms[i1]
      plots[[i1]] = ggplot(data=df0[df0$firm == firm_focal,]) + geom_line(aes(x=year, y=value, group=variable, color=variable, size=0.8)) +
        labs(title=firm_focal, subtitle="Movements in Pdt-Mkt space")
    } # i1 ends
    
    # use multiplot() to plot on same page
    multiplot(plotlist=plots, cols=2) 
    
  } else {
    
    # building separate plots by variable
    variables = names(table(df0$variable))
    plots = vector(mode="list", length=length(variables))
    for (i1 in 1:length(variables)){
      variable_focal = variables[i1]
      plots[[i1]] = ggplot(data=df0[df0$variable == variable_focal,]) + geom_line(aes(x=year, y=value, group=firm, color=firm, size=0.8)) +
        labs(title=variable_focal, subtitle="Movements in Pdt-Mkt space")
    } # i1 ends
    
    # use multiplot() to plot on same page
    multiplot(plotlist=plots, cols=2) 
  } # else ends
  
}  # show_movements() func ends

# examples of above show_movements() func
# firms_sample = c("microsoft ", "yahoo ", "google ", "apple ", "international business machines ")
# firm_names_short = c("microsoft", "yahoo", "google", "apple", "ibm")
# show_movements(firms_sample=firms_sample, firm_names_short=firm_names_short, by_variable=TRUE) # <8 secs