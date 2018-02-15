library(RestRserve)
library(xgboost)
library(Matrix)

create_model_matrix = readRDS("create_model_matrix.rds")
bst = xgb.load("model.xgb")

pred_xgb_post = function(request) {
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
  RestRserve::create_response(body = as.character(predict(bst, x)),
                              content_type = "text/plain",
                              headers = character(0),
                              status_code = 200L)
}
pred_xgb_get = function(request) {
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
  x = strsplit(request$query[["mushrom-properties"]], ";", fixed = TRUE)[[1]]
  x = strsplit(x, "=", fixed = TRUE)
  x = jsonlite::toJSON (lapply(x, function(z) list(featureName = z[[1]], featureValue = z[[2]])), auto_unbox = TRUE)
  # create feature matrix
  x = create_model_matrix(x)
  RestRserve::create_response(body = as.character(predict(bst, x)),
                              content_type = "text/plain",
                              headers = character(0),
                              status_code = 200L)
}
#------------------------------------------------------------------------------------------
# create application
#------------------------------------------------------------------------------------------
RestRserveApp = RestRserve::RestRserveApplication$new()
RestRserveApp$add_post(path = "/predict", FUN = pred_xgb_post)
RestRserveApp$add_get(path = "/predict", FUN = pred_xgb_get)
RestRserveApp$add_openapi()
RestRserveApp$add_swagger_ui("/")
