#!/usr/bin/env Rscript

DIR = commandArgs(trailingOnly = TRUE)[[1]]
if (!dir.exists(DIR)) dir.create(DIR, recursive = TRUE)

library(Matrix)
library(xgboost)
data(agaricus.train, package = "xgboost")
features = colnames(agaricus.train$data)

#----------------------------------------
# train xgboost
#----------------------------------------
bst = xgboost(data = agaricus.train$data,
              label = agaricus.train$label,
              max_depth = 2, eta = 1,
              nrounds = 5,
              save_name = normalizePath(file.path(DIR, "model.xgb")),
              objective = "binary:logistic")
#----------------------------------------
# create function which transforms
# raw data to the correct representation
#----------------------------------------
model_matrix_constructor = function(features) {
  n_features = length(features)
  function(x) {
    # read feature_name - feature_value pairs from json
    key_val = jsonlite::fromJSON(x, simplifyVector = FALSE)
    # convert feature_name <-> feature_value pairs to "key=value" strings
    # as it was in original feature space
    for (i in length(key_val)) {
      kv = key_val[[i]]
      key_val[[i]] = paste(kv$featureName, kv$featureValue, sep = "=")
    }
    # find the column index for a given feature
    col_index = match(key_val, features)
    # if we've received unseen priperties - ignore them
    col_index = col_index[!is.na(col_index)]
    # since request contains information about single observation -
    # there should be 1 row in the matrix
    row_index = rep(1, length(col_index))
    # cosntruct sparse matrix as it is required by xgboost
    sparseMatrix(i = row_index,
                 j = col_index,
                 x = 1,
                 dims = c(1, n_features))

  }
}

create_model_matrix = model_matrix_constructor(features)
saveRDS(create_model_matrix, file.path(DIR, "create_model_matrix.rds"))

#----------------------------------------
# generate sample request:
#----------------------------------------
# nolint start
request = '[{"featureName": "stalk-color-below-ring", "featureValue": "red"},{"featureName": "cap-shape","featureValue": "conical"}]'
# nolint end
writeLines(request, file.path(DIR, "request.json"))
#----------------------------------------
