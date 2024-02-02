# FUNCTIONS
compute_total <- function(row, H) {
  print(row)
  from <- as.Date(row["from"])
  to <- as.Date(row["to"])
  subset_dt <- window(data, start = from, end = to)
  print(min(index(subset_dt)))
  print(max(index(subset_dt)))
  est_lag1 <- vars::VAR(subset_dt, p = 1, type = "const")
  sp_lag <- spilloverDY12(est_lag1, n.ahead = H, no.corr = F)
  return(sp_lag)
}
adcc_output <- function(mat_list, data = data,
                        statistics = c("mean", "median", "sd", "min", "max"),
                        type = "weight") {
  list_subset <- mat_list
  z <- array(unlist(list_subset), c(dim(list_subset[[1]]), length(list_subset)))
  colnames(z) <- colnames(data)
  rownames(z) <- colnames(data)
  temp <- data.frame(matrix(0, ncol = length(statistics)))
  for (stat in statistics) {
    fun_mat <- apply(z, 1:2, stat)
    fun_arr <- mat_to_long(fun_mat, type = type, stat = stat)
    if (dim(temp)[1] == 1) {
      temp <- fun_arr
    } else {
      temp <- left_join(temp, fun_arr, by = "name")
    }
  }
  final <- temp[, -1]
  ratio <- temp[, 1]
  final <- cbind(ratio, final)
  return(final)
}
mat_to_long <- function(mat, type = "weight", stat = "mean") {
  vals <- as.vector(mat)
  df <- data.frame(matrix(ncol = 3))
  counter <- 1
  for (j in 1:ncol(mat)) {
    for (i in 1:nrow(mat)) {
      nm1 <- rownames(mat)[i]
      nm2 <- colnames(mat)[j]
      if (i != j) {
        df[counter, ] <- c(nm1, nm2, mat[i, j])
        counter <- counter + 1
      }
    }
  }
  clean_df <- df
  if (type == "weight") {
    cols <- c(1, 2)
    zz <- df[, cols]
    for (i in 1:nrow(df)) {
      zz[i, ] <- sort(df[i, cols])
    }
    clean_df <- df[!duplicated(zz), ]
    rownames(clean_df) <- NULL
  }
  clean_df <- cbind(paste0(clean_df[, 1], " ", clean_df[, 2]), clean_df[, 3])
  clean_df <- as.data.frame(clean_df)
  clean_df[, 2] <- as.numeric(clean_df[, 2])
  colnames(clean_df) <- c("name", stat)
  return(clean_df)
}
weightMat <- function(table, dt) {
  n <- length(dt)
  nms <- colnames(dt)
  weight_mat <- data.frame(matrix(ncol = n, nrow = n))
  colnames(weight_mat) <- nms
  rownames(weight_mat) <- nms

  for (i in 1:nrow(weight_mat)) {
    for (j in 1:length(weight_mat)) {
      h_jj <- table[j, j]
      h_ij <- table[i, j]
      h_ii <- table[i, i]
      w_ij <- (h_jj - h_ij) / (h_ii - 2 * h_ij + h_jj)
      weight_mat[i, j] <- w_ij
    }
  }
  return(weight_mat)
}

hedgeMat <- function(table, dt) {
  n <- length(dt)
  nms <- colnames(dt)
  hedge_mat <- data.frame(matrix(ncol = n, nrow = n))
  colnames(hedge_mat) <- nms
  rownames(hedge_mat) <- nms

  for (i in 1:nrow(hedge_mat)) {
    for (j in 1:length(hedge_mat)) {
      h_jj <- table[j, j]
      h_ij <- table[i, j]
      h_ii <- table[i, i]
      hedge_ij <- h_ij / h_jj
      hedge_mat[i, j] <- hedge_ij
    }
  }
  return(hedge_mat)
}



compute_total <- function(row, H) {
  print(row)
  from <- as.Date(row["from"])
  to <- as.Date(row["to"])
  subset_dt <- window(data, start = from, end = to)
  print(min(index(subset_dt)))
  print(max(index(subset_dt)))
  est_lag1 <- vars::VAR(subset_dt, p = 1, type = "const")
  sp_lag <- spilloverDY12(est_lag1, n.ahead = H, no.corr = F)
  return(sp_lag)
}
adcc_output <- function(mat_list, data = data,
                        statistics = c("mean", "median", "sd", "min", "max"),
                        type = "weight") {
  list_subset <- mat_list
  z <- array(unlist(list_subset), c(dim(list_subset[[1]]), length(list_subset)))
  colnames(z) <- colnames(data)
  rownames(z) <- colnames(data)
  temp <- data.frame(matrix(0, ncol = length(statistics)))
  for (stat in statistics) {
    fun_mat <- apply(z, 1:2, stat)
    fun_arr <- mat_to_long(fun_mat, type = type, stat = stat)
    if (dim(temp)[1] == 1) {
      temp <- fun_arr
    } else {
      temp <- left_join(temp, fun_arr, by = "name")
    }
  }
  final <- temp[, -1]
  ratio <- temp[, 1]
  final <- cbind(ratio, final)
  return(final)
}
mat_to_long <- function(mat, type = "weight", stat = "mean") {
  vals <- as.vector(mat)
  df <- data.frame(matrix(ncol = 3))
  counter <- 1
  for (j in 1:ncol(mat)) {
    for (i in 1:nrow(mat)) {
      nm1 <- rownames(mat)[i]
      nm2 <- colnames(mat)[j]
      if (i != j) {
        df[counter, ] <- c(nm1, nm2, mat[i, j])
        counter <- counter + 1
      }
    }
  }
  clean_df <- df
  if (type == "weight") {
    cols <- c(1, 2)
    zz <- df[, cols]
    for (i in 1:nrow(df)) {
      zz[i, ] <- sort(df[i, cols])
    }
    clean_df <- df[!duplicated(zz), ]
    rownames(clean_df) <- NULL
  }
  clean_df <- cbind(paste0(clean_df[, 1], " ", clean_df[, 2]), clean_df[, 3])
  clean_df <- as.data.frame(clean_df)
  clean_df[, 2] <- as.numeric(clean_df[, 2])
  colnames(clean_df) <- c("name", stat)
  return(clean_df)
}
weightMat <- function(table, dt) {
  n <- length(dt)
  nms <- colnames(dt)
  weight_mat <- data.frame(matrix(ncol = n, nrow = n))
  colnames(weight_mat) <- nms
  rownames(weight_mat) <- nms

  for (i in 1:nrow(weight_mat)) {
    for (j in 1:length(weight_mat)) {
      h_jj <- table[j, j]
      h_ij <- table[i, j]
      h_ii <- table[i, i]
      w_ij <- (h_jj - h_ij) / (h_ii - 2 * h_ij + h_jj)
      weight_mat[i, j] <- w_ij
    }
  }
  return(weight_mat)
}

hedgeMat <- function(table, dt) {
  n <- length(dt)
  nms <- colnames(dt)
  hedge_mat <- data.frame(matrix(ncol = n, nrow = n))
  colnames(hedge_mat) <- nms
  rownames(hedge_mat) <- nms

  for (i in 1:nrow(hedge_mat)) {
    for (j in 1:length(hedge_mat)) {
      h_jj <- table[j, j]
      h_ij <- table[i, j]
      h_ii <- table[i, i]
      hedge_ij <- h_ij / h_jj
      hedge_mat[i, j] <- hedge_ij
    }
  }
  return(hedge_mat)
}

# move dates if it was on weekend on holiday
find_next_date <- function(date_orig) {
  next_date <- date.range$Date[date.range$Date >= date_orig][1]
  return(next_date)
}
to_long <- function(data) {
  xts_df <- data %>% as.data.frame()
  xts_df$Date <- as.Date(index(data))
  rownames(xts_df) <- NULL
  df_long <- melt(xts_df, id.vars = "Date")
  return(df_long)
}

plot_sub_spill <- function(df, type = "Net") {
  ncol <- length(unique(net_sp$variable))
  plt <- ggplot(df, aes(x = Date, y = value)) +
    geom_line(col = "black") +
    geom_area(fill = "black") +
    facet_wrap(~variable, scales = "free", nrow = ncol) +
    theme_bw() +
    labs(x = "Year", y = "Spillover") +
    DCA_list +
    -
    scale_y_continuous(limits = c(-20, 20))
  return(plt)
}

TCI_to_df <-function(TCI,name){
  df <- data.frame(date = as.Date(rownames(TCI)),
                   TCI = coredata(TCI),
                   dca_type = rep(name, nrow(TCI)),
                   row.names = NULL)
  return (df)
}

bootstrap_data <- function(j) {
  # step 2
  for (i in 1:(nrow(X_init) - 1)) {
    # copy initial X
    # sample residuals - non parametrically TOO SLOW
    # u_b <- apply(u_hat, 2, sample, size = 1)
    # sample residuals - parametrically
    u_b <- rnorm(ncols, mean = rep(0, ncols), sd = sd_hat)
    # get old row
    x_b <- X_init[i, ] %*% A_hat + u_b
    # add constant
    x_b[ncols + 1] <- 1
    # sub new rows
    X_init[i + 1, ] <- x_b
  }
  # remove constant
  X_init <- X_init[, -(ncols + 1)]
  
  # step 3
  res <- VAR(X_init,
             p = p,
             type = "const",
             ic = c("AIC")
  )
  S <-
    overall(spilloverDY12(res, n.ahead = H, no.corr = F))[[1]][[1]]
  return(S)
}

find_first_null_index <- function(lst) {
  for (i in seq_along(lst)) {
    if (is.null(lst[[i]])) {
      return(i)
    }
  }
  return(NULL)
}


bootstrap_window <- function(i,
                             p = 1,
                             w = 100,
                             H = 100,
                             B = 100) {
  data <- all[i:(i + w - 1), ]
  last_date <- data %>%
    tail(1) %>%
    dplyr::select(Date) %>%
    pull()
  # step 1
  # estimate VAR model
  res <- VAR(data[-1],
             p = p,
             type = "const",
             ic = c("AIC")
  )
  # save A_hat
  coefs <- coef(res)
  A_hat <- as.matrix(as.data.frame(lapply(coefs, "[", , 1)))
  # save u_hat
  u_hat <- resid(res)
  sd_hat <- apply(u_hat, 2, sd)
  # save S_hat
  S_hat <-
    overall(spilloverDY12(res, n.ahead = H, no.corr = F))[[1]]
  # save initial X
  X_init <-
    data %>%
    dplyr::select(-Date) %>%
    mutate(const = rep(1, nrow(data))) %>%
    as.matrix()
  
  clusterExport(
    cl = cl,
    varlist = c("sd_hat", "A_hat", "X_init"),
    envir = environment()
  )
  
  
  # step 2+3 - get S_b
  S_b <- parSapply(cl, 1:B, FUN = bootstrap_data)
  S_b_init_mean <- mean(S_b)
  
  
  # step 4 - get bias
  bias <- S_b_init_mean - S_hat
  
  # step 5 - get bias corrected boot straps
  S_b <- parSapply(cl, 1:B, FUN = bootstrap_data)
  
  return(
    list(
      "index" = i,
      "date" = last_date,
      "S_init" = S_hat,
      "S_prior_mean" = S_b_init_mean,
      "bias" = bias,
      "S_posterior_mean" = mean(S_b - bias),
      "S" = S_b - bias
    )
  )
}


add_j <- function(events, j = 1, perc = 0.95) {
  colnm <- paste(c("j_", j), collapse = "")
  colnm_perc <- paste(c(colnm, "_perc"), collapse = "")
  
  res <- lapply(2:(length(b_data) - j), eval_date, j = j)
  Date <- as.Date(sapply(res, `[[`, 1))
  value <- sapply(res, `[[`, 2)
  test_res <- data.frame(Date, value)
  above_X <- test_res %>%
    filter(value > perc) %>%
    dplyr::select(Date) %>%
    pull()
  wanted_rnks <- events[events$moved_date %in% above_X, "rank"]
  events[[colnm]] <-
    with(events, ifelse(rank %in% wanted_rnks, 1, 0))
  colnames(test_res) <- c("Date", colnm_perc)
  events <-
    events %>% dplyr::left_join(test_res, by = c("moved_date" = "Date"))
  return(events)
}

every_nth <- function(n) {
  return(function(x) {
    x[c(TRUE, rep(FALSE, n - 1))]
  })
}

eval_date <- function(i = w, j = 0) {
  S_pre <- mean(b_data[i - 1][[1]]$S)
  S_post <- b_data[i + j][[1]]$S
  prob <- sum((S_post - S_pre) > 0) / length(S_post)
  date <- b_data[i][[1]]$date
  return(list("date" = date, "prob" = prob))
}


get_post_mean<-function(i=w,j=0){
  colnm = paste(c("post_mean_", j), collapse = "")
  post_mean_val<- mean(b_data[i+j][[1]]$S)
  date<-b_data[i][[1]]$date
  return (list("date" = date,'post_mean'= post_mean_val))
}

get_pre_mean<-function(i=w){
  prior_mean <- mean(b_data[i-1][[1]]$S)
  date<-b_data[i][[1]]$date
  return (list("date" = date,"prior_mean" = prior_mean))
}

