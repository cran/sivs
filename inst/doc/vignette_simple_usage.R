## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

#save(list = c("train_x", "train_y", "validation_x", "validation_y", "sivs_obj"), file = "vignette_v0.2.1.RData")
load(url("https://seafile.utu.fi/f/13e0ef294b374549b499/?dl=1"))

## -----------------------------------------------------------------------------
library("varhandle")

knitr::kable(varhandle::inspect.na(d = train_x, barplot = F))
knitr::kable(varhandle::inspect.na(d = validation_x, barplot = F))

## -----------------------------------------------------------------------------
train_x <- train_x[, -10001]
validation_x <- validation_x[, -10001]

## -----------------------------------------------------------------------------
knitr::kable(varhandle::var.info(regex = "_[xy]$"))

## ----run_sivs, include = FALSE------------------------------------------------
library("sivs")
# bypass CRAN's error on number of used cores! Apparently on CRAN you have limit your code to only 2 cores!
#chk <- Sys.getenv("_R_CHECK_LIMIT_CORES_", "")
#
#if ((nzchar(chk) && chk == "TRUE") || (!identical(Sys.getenv("NOT_CRAN", unset = "true"), "true"))) {
#    # use 2 cores in CRAN/Travis/AppVeyor
#    sivs_obj <- sivs::sivs(x = train_x, y = factor(train_y), verbose = "none", progressbar = FALSE, parallel.cores = 2)
#}else{
#    sivs_obj <- sivs::sivs(x = train_x, y = factor(train_y), verbose = "none", progressbar = FALSE)
#}

## ----plotting_sivs, fig.height=9, fig.width=8---------------------------------
layout(mat = matrix(c(1,2,
                      3,3),
                    nrow = 2,
                    byrow = T))
{
    plot(sivs_obj)
    layout(1)
}


## ----sivs_suggest-------------------------------------------------------------
sivs::suggest(sivs_obj, strictness = 0.5)

## ----glmnet_vs_sivs, fig.height=10, fig.width=5-------------------------------
library("glmnet")
library("pROC")

# build a model without SIVS
set.seed(12345)
glmnet_model <- glmnet::cv.glmnet(x = data.matrix(train_x),
                                  y = factor(train_y),
                                  family = "binomial")

# build a model with SIVS
set.seed(12345)
sivs_glmnet_model <- glmnet::cv.glmnet(x = data.matrix(train_x[, sivs::suggest(sivs_obj, strictness = 0.5)]),
                                       y = factor(train_y),
                                       family = "binomial")

# predict both training set and validation sets
glmnet_train_pred <- predict(object = glmnet_model,
                             newx = data.matrix(train_x),
                             s = "lambda.min",
                             type = "response")
glmnet_validation_pred <- predict(object = glmnet_model,
                                  newx = data.matrix(validation_x),
                                  s = "lambda.min",
                                  type = "response")

sivs_glmnet_train_pred <- predict(object = sivs_glmnet_model,
                                  newx = data.matrix(train_x[, sivs::suggest(sivs_obj, strictness = 0.5)]),
                                  s = "lambda.min",
                                  type = "response")
sivs_glmnet_validation_pred <- predict(object = sivs_glmnet_model,
                                       newx = data.matrix(validation_x[, sivs::suggest(sivs_obj, strictness = 0.5)]),
                                       s = "lambda.min",
                                       type = "response")




glmnet_train_roc <- pROC::roc(response = factor(train_y),
                              predictor = as.numeric(glmnet_train_pred))
glmnet_validation_roc <- pROC::roc(response = factor(validation_y),
                                   predictor = as.numeric(glmnet_validation_pred))

sivs_glmnet_train_roc <- pROC::roc(response = factor(train_y),
                                   predictor = as.numeric(sivs_glmnet_train_pred))
sivs_glmnet_validation_roc <- pROC::roc(response = factor(validation_y),
                                        predictor = as.numeric(sivs_glmnet_validation_pred))

layout(mat = matrix(1:2, nrow = 2))
{
    plot(glmnet_train_roc, col = "salmon", main = "Performance on training data")
    plot(sivs_glmnet_train_roc, col = "cornflowerblue", add = T)
    legend("bottomright",
           fill = c("salmon", "cornflowerblue"),
           legend = c(paste0("glmnet (AUROC=",
                             round(pROC::auc(glmnet_train_roc),
                                   digits = 4),
                             ")"),
                      paste0("SIVS + glmnet (AUROC=",
                             round(pROC::auc(sivs_glmnet_train_roc),
                                   digits = 4),
                             ")")))
    

    plot(glmnet_validation_roc, col = "salmon", main = "Performance on validation data")
    plot(sivs_glmnet_validation_roc, col = "cornflowerblue", add = T)
    legend("bottomright",
           fill = c("salmon", "cornflowerblue"),
           legend = c(paste0("glmnet (AUROC=",
                             round(pROC::auc(glmnet_validation_roc),
                                   digits = 4),
                             ")"),
                      paste0("SIVS + glmnet (AUROC=",
                             round(pROC::auc(sivs_glmnet_validation_roc),
                                   digits = 4),
                             ")")))
    layout(1)
}



## ----compare_feature_count----------------------------------------------------
# extract the coefficients
sum(coef(glmnet_model) != 0)
sum(coef(sivs_glmnet_model) != 0)

