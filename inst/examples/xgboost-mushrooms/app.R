library(RestRserve)
library(xgboost)
library(Matrix)

DIR = commandArgs(trailingOnly = TRUE)[[1]]

create_model_matrix = readRDS(file.path(DIR, "create_model_matrix.rds"))
bst = xgb.load(file.path(DIR, "model.xgb"))

pred_xgb_post = function(request, response) {
  #' ---
  #' description: predict whether mushroom edible or poisonous
  #' operationId: postXgbPredict
  #' requestBody:
  #'   description: "mushroom properties"
  #'   required: true
  #'   content:
  #'     application/json:
  #'       schema:
  #'         type: array
  #'         items:
  #'           required:
  #'             - featureName
  #'             - featureValue
  #'           properties:
  #'             featureName:
  #'               type: string
  #'               example: "stalk-color-below-ring"
  #'             featureValue:
  #'               type: string
  #'               example: "red"
  #' responses:
  #'   200:
  #'     description: API response
  #'     content:
  #'       text/plain:
  #'         schema:
  #'           type: numeric
  #'           example: 0.5
  #' ---
  x = rawToChar(request$body)
  x = create_model_matrix(x)
  response$body = as.character(predict(bst, x))
  response$content_type = "text/plain"
  response$headers = character(0)
  response$status_code = 200L
}
pred_xgb_get = function(request, response) {
  #' ---
  #' description: predict whether mushroom edible or poisonous
  #' operationId: getXgbPredict
  #' parameters:
  #'   - name: "mushrom-properties"
  #'     description: >
  #'       String of the 'key=value' pairs of mushroom properties.
  #'       Several paramenters can be provided - use semicolon ';'
  #'       to concatenate them
  #'     in: query
  #'     schema:
  #'       type: string
  #'     example: "stalk-color-below-ring=red;cap-shape=conical"
  #' responses:
  #'   200:
  #'     description: API response
  #'     content:
  #'       text/plain:
  #'         schema:
  #'           type: numeric
  #'           example: 0.5
  #' ---

  # create json in order to be able to re-use create_model_matrix() function
  # just for demonstration - by far not ideal solution!
  x = strsplit(request$parameters_query[["mushrom-properties"]], ";", fixed = TRUE)[[1]]
  x = strsplit(x, "=", fixed = TRUE)
  x = jsonlite::toJSON (lapply(x, function(z) list(featureName = z[[1]], featureValue = z[[2]])), auto_unbox = TRUE)
  # create feature matrix
  x = create_model_matrix(x)
  response$body = as.character(predict(bst, x))
  response$content_type = "text/plain"
  response$headers = character(0)
  response$status_code = 200L
}
#------------------------------------------------------------------------------------------
# create application
#------------------------------------------------------------------------------------------
app = RestRserve::Application$new()
app$add_post(path = "/predict", FUN = pred_xgb_post)
app$add_get(path = "/predict", FUN = pred_xgb_get)
app$add_openapi()
app$add_swagger_ui("/")

backend = BackendRserve$new()
backend$start(app, 8080)
