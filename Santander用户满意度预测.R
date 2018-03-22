#########XGBoost和随机特征子集选择学习法#########################
### 载入数据包
library('readr')
library('magrittr')
library('Matrix')
library('xgboost')

### 载入数据
dt.train <- readr::read_csv('train_1.csv',
                           col_types = paste(rep('d', 371), collapse = ''))
dt.train <- readr::read_csv('test_1.csv', 
                           col_types = paste(rep('d', 370), collapse = ''))

### 转换数据
label.name <- 'TARGET'
y <- as.integer(dt.train[[label.name]])

### 调整样本权重
class.freq <- table(y) %>% prop.table
row.wt <- ifelse(y == 0, 1/class.freq[1], 1/class.freq[2])

### 数据清洗
col.names <- names(dt.train) %>% setdiff(c('ID', label.name))
zero.rate <- sapply(dt.train[col.names], function(dt.col){
  sum(dt.col == 0)/length(dt.col)
})
keep.cols <- col.names[zero.rate < 0.9]

### 设置XGBoost参数
xgb.paras <- list(
  "booster" = "gbtree",
  "eta" = 1e-2,
  "max_depth" = 4,
  "subsample" = 0.7,
  "colsample_bytree" = 0.7,
  "min_child_weight" = 1,
  "objective" = "binary:logistic",
  "eval_metric" = "auc",
  "silent" = 1,
  "nthread" = 4
)

### 开始训练
n.models <- 5
n.features <- 50
n.folds <- 3
model.perf <- numeric(n.models)
meta.tr <- vector('list', n.models)
meta.te <- vector('list', n.models)
for(i in 1:n.models){
  cat(paste('/n### Model', i, '###/n'))
}

  ## 特征选择
  sel.cols <- sample(keep.cols, n.features)
  x.tr <- Matrix(as.matrix(dt.train[sel.cols]), sparse = TRUE)
  dtrain <- xgb.DMatrix(x.tr, label = y, weight = row.wt)
  x.te <- Matrix(as.matrix(dt.test[sel.cols]), sparse = TRUE)
  dtest <- xgb.DMatrix(x.te)
  
  ## 交叉验证
  cv.out <- xgb.cv(params = xgb.params, data = dtrain, nrounds = 1500, 
                   nfold = n.folds, prediction = TRUE, stratified = TRUE, 
                   verbose = FALSE, early.stop.round = 15, maximize = TRUE)
  model.perf[i] <- max(cv.out$dt$test.auc.mean)
  best.iter <- which.max(cv.out$dt$test.auc.mean)
  meta.tr[[i]] <- cv.out$pred
  
  ## 再次训练
  xgb.model <- xgb.train(data = dtrain, params = xgb.params, 
                         nrounds = best.iter);
  
  ## 生成测试数据
  meta.te[[i]] <- predict(xgb.model, dtest)
  
  cat(paste('\nAUC:', model.perf[i]))
  
### 保存数据
  model.names <- paste('Model', 1:n.models, sep = '')
  names(meta.tr) <- model.names
  meta.tr$Id <- as.integer(dt.train$ID)
  meta.tr$Wt <- row.wt
  meta.tr$Target <- y
  write.csv(meta.tr, 'meta_train.csv', row.names = FALSE, quote = FALSE)
  
### 测试
  names(meta.te) <- model.names
  meta.te$Id <- as.integer(dt.test$ID)
  write.csv(meta.te, 'meta_test.csv', row.names = FALSE, quote = FALSE)
