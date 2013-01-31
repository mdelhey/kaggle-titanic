rmse <- function(mod, data, r = resid2) {
  sqrt(mean(r(mod, data) ^ 2, na.rm = TRUE))
}

rd <- function(mod, data, r = resid2) {
  quantile(abs(r(mod, data)), c(0.25, 0.5, 0.75, 0.9), na.rm = TRUE)
}

resid2 <- function(mod, data = NULL) {
  # If no data, and model has residuals method, use that
  if (is.null(data)) {
    r <- residuals(mod)
    if (!is.null(r)) {
      return(r)
    } else {
      stop("No data and no-built in residuals", call. = FALSE)
    }
  }

  # Otherwise return difference between actual value and predicted
  resp(mod, data) - predict(mod, data)
}

cv_train <- function(n, train = 0.9) {
  nkeep <- round(n * train)
  sample(rep(c(TRUE, FALSE), c(nkeep, n - nkeep)))
}

cv_rmse <- function(mod, data, n = 10, train = 0.9) {
  replicate(n, {
    test <- cv_train(nrow(data), train)
    mod_cv <- my_update(mod, data = data[test, ])
    rmse(mod_cv, data[!test, ])
  })
}

my_update <- function(mod, data) {
  call <- getCall(mod)
  if (is.null(call)) {
    stop("Model object does not support updating (no call)", call. = FALSE)
  }
  term <- terms(mod)
  if (is.null(term)) {
    stop("Model object does not support updating (no terms)", call. = FALSE)
  }

  call$data <- data
  env <- attr(term, ".Environment")

  eval(call, env)
}

seq_range <- function(x, n, trim = NULL) {
  if (!is.null(trim)) {
    rng <- quantile(x, c(trim / 2, 1 - trim / 2), na.rm = TRUE)
  } else {
    rng <- range(x, na.rm = TRUE)
  }

  seq(rng[1], rng[2], length = n)
}

mod_grid <- function(data, ...) {
  call <- substitute(expand.grid(...,
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE))

  eval(call, data, parent.frame())
}

resp <- function(model, data) {
  eval(term_vars(model)$resp, data)
}
pred <- function(model, data) {
  eval(term_vars(model)$pred, data)
}



term_vars <- function(model) {
  t <- terms(model)
  vars <- as.list(attr(t, "variables")[-1])
  resp <- attr(t, "response")

  list(
    resp = vars[[resp]],
    pred = as.call(c(as.name("data.frame"), vars[-resp]))
  )
}
