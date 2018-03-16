library("text2vec")
library("data.table")
data("movie_review")  
###############设置数据的唯一主键，划分训练集和测试集############
setDT(movie_review)
setkey(movie_review, id)
set.seed(2016L)
all_ids <- movie_review$id
train_ids <- sample(all_ids, 4000)
test_ids <- setdiff(all_ids, train_ids) 
train <- movie_review[J(train_ids)]
test <- movie_review[J(test_ids)]
###############文档向量化，分词迭代器，构建DTM矩阵############
prep_fun <- tolower 
tok_fun <- word_tokenizer # 代表词语划分到什么程度
# 步骤一，设置分词迭代器
it_train <- itoken(train$review, 
                   preprocessor <- prep_fun,
                   tokenizer <- tok_fun, 
                   ids <- train$id, 
                   progressbar <- FALSE)
# 步骤二，分词消除停用词
stop_words <- c("i", "me", "my", "myself", "we", "our", 
                "ours", "ourselves", "you", "your", "yours")
# 分词函数
vocab <- create_vocabulary(it_train, stopwords <- stop_words)
# 对低词频的修建
pruned_vocab = prune_vocabulary(vocab,   
                                term_count_min = 10,   #词频，低于10个都删掉
                                doc_proportion_max = 0.5,  
                                doc_proportion_min = 0.001)
# 步骤3.设置形成语料文件
vectorizer = vocab_vectorizer(pruned_vocab)
# 进行hash化，提效降内存#2-ngrams增加文字信息量
#h_vectorizer = hash_vectorizer(hash_size = 2 ^ 14, ngram = c(1L, 2L))  

#步骤4.构建DTM矩阵
dtm_train = create_dtm(it_train, vectorizer)
#=========================================
#优化方法#标准化，加入惩罚项
#dtm_train_l1_norm = normalize(dtm_train, "l1")
#转为TFIDF步骤
#1.设置TFIDF编译器#tfidf = TfIdf$new()  
#2.转换成TFIDF格式
#dtm_train_tfidf = fit_transform(dtm_train, tfidf)
#dtm_test_tfidf  = create_dtm(it_test, vectorizer) %>%   
#  transform(tfidf)
#或者写为
#dtm_test_tfidf  = create_dtm(it_test, vectorizer) %>%   
#  transform(tfidf)

###############基于Logistic的情感标注##########################
library("glmnet")  
NFOLDS = 4  
glmnet_classifier = cv.glmnet(x = dtm_train, 
                              y = train[['sentiment']],   
                              family = 'binomial',   
                              # L1 penalty  
                              alpha = 1,  
                              # interested in the area under ROC curve  
                              type.measure = "auc",  
                              # 5-fold cross-validation  
                              nfolds = NFOLDS,  
                              # high value is less accurate, but has faster training  
                              thresh = 1e-3,  
                              # again lower number of iterations for faster training  
                              maxit = 1e3)  
plot(glmnet_classifier)

###############验证集效果######################################
it_test = test$review %>%   
  prep_fun %>%   
  tok_fun %>%   
  itoken(ids = test$id,   
         # turn off progressbar because it won't look nice in rmd  
         progressbar = FALSE)  
dtm_test = create_dtm(it_test, vectorizer)  
preds = predict(glmnet_classifier, dtm_test, type = 'response')[,1] 
glmnet:::auc(test$sentiment, preds) 

###############Glove词嵌入######################################
text8_file = "./text8"
if (!file.exists(text8_file)) {
  download.file("http://mattmahoney.net/dc/text8.zip", "./text8.zip")
  unzip ("./text8.zip", files = "text8", exdir = "./")}
wiki = readLines(text8_file, n = 1, warn = FALSE)
# Create iterator over tokens
tokens <- space_tokenizer(wiki)
# Create vocabulary. Terms will be unigrams (simple words).
it = itoken(tokens, progressbar = FALSE)
vocab <- create_vocabulary(it)
vocab <- prune_vocabulary(vocab, term_count_min = 5L)
# Use our filtered vocabulary
vectorizer <- vocab_vectorizer(vocab, 
                               # don't vectorize input
                               grow_dtm = FALSE, 
                               # use window of 5 for context words
                               skip_grams_window = 5L)
tcm <- create_tcm(it, vectorizer)
#RcppParallel::setThreadOptions(numThreads = 4)
glove = GlobalVectors$new(word_vectors_size = 50, 
                          vocabulary = vocab, x_max = 10)
glove$fit(tcm, n_iter = 20)
#glove = GlobalVectors$new(word_vectors_size = 50, vocabulary = vocab, x_max = 10)
# `glove` object will be modified by `fit()` call !
fit(tcm, glove, n_iter = 20)
#now we get the word vectors:
word_vectors <- glove$get_word_vectors()
berlin <- word_vectors["paris", , drop = FALSE] - 
  word_vectors["france", , drop = FALSE] + 
  word_vectors["germany", , drop = FALSE]
cos_sim = sim2(x = word_vectors, y = berlin, method = "cosine", norm = "l2")
head(sort(cos_sim[,1], decreasing = TRUE), 5)

###############主题模型LDA######################################
tokens = movie_review$review %>% 
  tolower %>% 
  word_tokenizer
# turn off progressbar because it won't look nice in rmd
it = itoken(tokens, ids = movie_review$id, progressbar = FALSE)
v = create_vocabulary(it) %>% 
  prune_vocabulary(term_count_min = 10, doc_proportion_max = 0.2)
vectorizer = vocab_vectorizer(v)
dtm = create_dtm(it, vectorizer, type = "lda_c")
#前面步骤与词向量操作一致，后面运用LDA函数构建主题模型
lda_model = 
  LDA$new(n_topics = 10, vocabulary = v, 
          doc_topic_prior = 0.1, topic_word_prior = 0.01)
doc_topic_distr = 
  lda_model$fit_transform(dtm, n_iter = 1000, convergence_tol = 0.01, 
                          check_convergence_every_n = 10)

###############相似性度量######################################
library(stringr)
library(text2vec)
data("movie_review")
# select 500 rows for faster running times
movie_review = movie_review[1:500, ]prep_fun = function(x) {
  x %>% 
    # make text lower case
    str_to_lower %>% 
    # remove non-alphanumeric symbols
    str_replace_all("[^[:alnum:]]", " ") %>% 
    # collapse multiple spaces
    str_replace_all("\\s+", " ")}
movie_review$review_clean = prep_fun(movie_review$review)
doc_set_1 = movie_review[1:300, ]
it1 = itoken(doc_set_1$review_clean, progressbar = FALSE)
# specially take different number of docs in second set
doc_set_2 = movie_review[301:500, ]
it2 = itoken(doc_set_2$review_clean, progressbar = FALSE)
it = itoken(movie_review$review_clean, progressbar = FALSE)
v = create_vocabulary(it) %>% 
  prune_vocabulary(doc_proportion_max = 0.1, term_count_min = 5)
vectorizer = vocab_vectorizer(v)
# Jaccard similarity
# they will be in the same space because we use same vectorizer# hash_vectorizer will also work fine
dtm1 = create_dtm(it1, vectorizer)
dim(dtm1)
dtm2 = create_dtm(it2, vectorizer)
dim(dtm2)
d1_d2_jac_sim = sim2(dtm1, dtm2, method = "jaccard", norm = "none")
# Cosine similarity
d1_d2_cos_sim = sim2(dtm1, dtm2, method = "cosine", norm = "l2")
# Euclidean distance
x = dtm_tfidf_lsa[1:300, ]
y = dtm_tfidf_lsa[1:200, ]
m1 = dist2(x, y, method = "euclidean")
# RWMD
data("movie_review") 
tokens = movie_review$review %>%  
  tolower %>%  
  word_tokenizer  
v = create_vocabulary(itoken(tokens)) %>%  
  prune_vocabulary(term_count_min = 5, doc_proportion_max = 0.5) 
corpus = create_corpus(itoken(tokens), vocab_vectorizer(v, skip_grams_window = 5)) 
dtm = get_dtm(corpus) 
tcm = get_tcm(corpus) 
glove_model = GloVe$new(word_vectors_size = 50, vocabulary = v, x_max = 10)
wv = glove_model$fit(tcm, n_iter = 10)  
rwmd_model = RWMD(wv) 
rwmd_dist = dist2(dtm[1:10, ], dtm[1:100, ], method = rwmd_model, norm = 'none')



