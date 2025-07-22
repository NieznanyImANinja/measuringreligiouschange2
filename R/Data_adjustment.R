
#old distribution list for testing
#dist_list <- list(c(2, 3, 4), c(2, 3, 4, 5), c(2, 3, 4), c(3, 4, 5, 6),
                  #c(2, 3, 4), c(2, 3, 4, 5), c(2, 3, 4), c(3, 4, 5, 6))

################################################################################

#function for clearing NA in input data

multvector <- function(vector) {
  i <- 1
  for (number in vector)
  i <- i * number
  return(i)
}

clear_na <- function(retention_data) {
  if (nrow(retention_data) == 0)
    return(retention_data)
  remove_keep <- rep(TRUE, nrow(retention_data))
  for (i in 1:nrow(retention_data)) {
    if (sum(is.na(retention_data[i, 4:ncol(retention_data)])) == 0)
      next
    if (sum(is.na(retention_data[i, 4:ncol(retention_data)])) == 1) {
      for (k in 4:ncol(retention_data))
      if (is.na(retention_data[i, k]))
        retention_data[i, k] <- 1 - sum(retention_data[i,
                                                       4:ncol(retention_data)],
                                        na.rm =  TRUE)
      next
    }
    remove_keep[i] <- FALSE
  }
  return(retention_data[remove_keep, ])
}

#function for creating all combinations of age group, sex and religion at birth,
#adds blank rows (p = 0) for those not included in input data
#user can adjust age_groups and sex_labels so it would match the input data

ras_combination_creator <- function(retention_data, age_groups = c("15-54"),
                                    sex_labels = c(1, 2)) {
  for (re in names(retention_data[4:ncol(retention_data)])) {
    for (ag in age_groups) {
      for (se in sex_labels) {
        if (nrow(retention_data[(retention_data[[1]] == re &
                                   retention_data[[2]] == ag &
                                   retention_data[[3]] == se), ]) != 0)
          next
        tibble <- as.data.frame(matrix(data =
                                         append(list(re, ag, se),
                                                as.list(rep(0,
                                                            ncol(retention_data)
                                                            - 3))),
                                       nrow = 1, ncol = ncol(retention_data)))
        colnames(tibble) <- colnames(retention_data)

        tibble[, 1:2] <- sapply(tibble[, 1:2], as.character)
        tibble[, 3:ncol(tibble)] <- sapply(tibble[, 3:ncol(tibble)], as.numeric)

        retention_data <- retention_data %>%
          add_row(tibble)
      }
    }
  }
  return(retention_data)
}

#function for adjusting switch probability,
#if switching probability in the row sums to 0
#then childhood religion gets p = 1

switch_prob_adjustment <- function(retention_data) {
  for (i in 1:nrow(retention_data)) {
    if (sum(retention_data[i, 4:ncol(retention_data)]) != 0) {
      next
    } else {
      retention_data[i, unlist(retention_data[i, 1])] <- 1
    }
  }
  return(retention_data)
}


#function for sorting retention data

sorting <- function(retention_data) {
  retention_data <- retention_data %>%
    arrange(retention_data$Religion, retention_data$Sex, retention_data$Age)
}


#function for sorting retention data by religions colnames order

sorting_colorder_religions <- function(retention_data) {
  religion_names <- c(colnames(retention_data[4:ncol(retention_data)]),
                      sapply(colnames(retention_data[4:ncol(retention_data)]),
                             paste, "_post_switch", sep = ""))
  for (i in 1:nrow(retention_data)){
    retention_data[i, 1] <- as.character(which(religion_names ==
                                                 unlist(retention_data[i, 1])))
  }
  retention_data <- sorting(retention_data)
  class(retention_data[[1]]) <- "double"
  vec <- c()
  for (i in 1:nrow(retention_data)){
    vec <- c(vec, religion_names[unlist(retention_data[i, 1])])
  }
  retention_data[[1]] <- as.numeric(retention_data[[1]])
  return(retention_data)
}


#function for adjusting input retention data

retention_data_adjustment <- function(retention_data) {
  retention_data <- as.data.frame(retention_data)
  retention_data <- clear_na(retention_data)
  retention_data <- ras_combination_creator(retention_data,
                                            age_groups = c("15-54"))
  retention_data <- switch_prob_adjustment(retention_data)
  retention_data <- sorting_colorder_religions(retention_data)
  return(retention_data)
}


################################################################################


#decompose retention vector for one religion using distribution vector

decomposition <- function(retention, distribution) {
  base <- retention ^ (1 / sum(distribution))
  result <- c()
  for (i in 1:length(distribution))
    result <- c(result, base ^ distribution[i])
  return(result)
}

#decompose retention vector for one religion using any distribution

anydistdecomposition <- function(retention, vector, iterations = 10000) {
  check <- (vector > 1 | vector < 0)
  if (sum(check) > 0 || sum(vector) > 1) {
    log_print("wrong distribution")
    return(NULL)
  }
  for (i in 1:iterations){
    c <- (multvector(1 - vector)) / retention
    vector <- vector * c
  }
  return(1-vector)
}

#decompose retention

decomposition_vector <- function(retentionvector, distribution,
                                 religion, exponentials = FALSE) {
  result <- data.frame(matrix(data = 0, nrow = length(distribution),
                              ncol = length(retentionvector)))
  colrel <- data.frame(anydistdecomposition(retentionvector[religion],
                                            distribution))
  for (i in 1:length(distribution)){
    result[i, religion] <- colrel[1, i]
  }
  for (i in 1:length(retentionvector)){
    if (i == religion)
      next
    if (retentionvector[i] == 0)
      next
    for (k in 1:nrow(result)) {
      result[k, i] <- (1 - result[k, religion]) * retentionvector[i] /
        (1 - retentionvector[religion])
    }
  }
  return(result)
}


#final function for decomposing retention data
#nosw_start and nosw_end must be adjusted to original age groups,
#for 15-54 we include 3 first and 9 last age groups
#that are not included in retention

switching_matrix_full <- function(retention_data, distribution_list, religion,
                                  differencial = FALSE, nosw_start = 3,
                                  nosw_end = 9) {
  retcol <- ncol(retention_data)
  distlen <- length(distribution_list)
  result <- matrix(data = 0, nrow = nosw_start, ncol = retcol * 2)
  result[, religion] <- 1
  for (i in 1:(distlen / 2)){
    dist <- length(distribution_list[[i]])
    colrel <- anydistdecomposition(retention_data[i, religion], distribution_list[[i]])
    chunk <- matrix(data = 0, nrow = dist, ncol = retcol * 2)
    chunk[, religion] <- colrel
    for (n in 1:retcol){
      if (retention_data[i, religion] == 1)
        next
      if (n == religion)
        next
      mult <- retention_data[i, n] / (1 - retention_data[i, religion])
      for (k in 1:dist){
        chunk[k, n] <- (1 - chunk[k, religion]) * mult
      }
    }
    result <- rbind(result, chunk)
  }
  buffer <- matrix(data = 0, nrow = nosw_start + nosw_end, ncol = retcol * 2)
  buffer[, religion] <- 1
  result <- rbind(result, buffer)
  for (i in (distlen / 2 + 1):distlen){
    dist <- length(distribution_list[[i]])
    colrel <- anydistdecomposition(retention_data[i, religion], distribution_list[[i]])
    chunk <- matrix(data = 0, nrow = dist, ncol = retcol * 2)
    chunk[, religion] <- colrel
    for (n in 1:retcol){
      if (retention_data[i, religion] == 1)
        next
      if (n == religion)
        next
      mult <- retention_data[i, n] / (1 - retention_data[i, religion])
      for (k in 1:dist){
        chunk[k, n] <- (1 - chunk[k, religion]) * mult
      }
    }
    result <- rbind(result, chunk)
  }
  buffer <- matrix(data = 0, nrow = nosw_end, ncol = retcol * 2)
  buffer[, religion] <- 1
  result <- rbind(result, buffer)
  if (differencial) {
    for (i in 1:(ncol(result) / 2)) {
      if (i == religion) {
        result[, i + (ncol(result) / 2)] <- 0
        next
      }
      result[, i + (ncol(result) / 2)] <- result[, i]
      result[, i] <- 0
    }
  } else {
    result <- result[, 1:(ncol(result) / 2)]
  }
  return(result)
}


#function for decomposing the retention data

dataframe_decomposition <- function(retention_data_adjusted, distribution_list,
                                    post_switch = TRUE) {
  retention_full_data <- list()

  for (i in 4:ncol(retention_data_adjusted)) {
    little_matrix <- retention_data_adjusted[1:length(distribution_list),
                                             4:ncol(retention_data_adjusted)]
    working_matrix <- switching_matrix_full(little_matrix, dist_list,
                                            i - 3, post_switch)
    retention_full_data[[i - 3]] <- data.matrix(working_matrix)
    retention_data_adjusted <- retention_data_adjusted[-c(1:length(distribution_list)), ]
  }
  if (post_switch) {
    len <- length(retention_full_data)
    for (i in (len + 1):(2 * len)) {
      temp <- matrix(data = 0, nrow = nrow(retention_full_data[[1]]),
                     ncol = ncol(retention_full_data[[1]]))
      temp[, i] <- 1
      retention_full_data[[i]] <- temp
    }

    cnames <- c(colnames(retention_data_adjusted)[4:ncol(retention_data_adjusted)],
                sapply(colnames(retention_data_adjusted)[4:ncol(retention_data_adjusted)],
                       paste, "_post_switch", sep = ""))
    for (i in 1:length(retention_full_data)){
      colnames(retention_full_data[[i]]) <- cnames
    }
    names(retention_full_data) <- cnames
  } else {
    names(retention_full_data) <- colnames(retention_data_adjusted)[4:ncol(retention_data_adjusted)]
  }
  return(retention_full_data)
}

#function for labeling decomposed data

labeling <- function(retention_data_decomposed, religion_data) {
  for (i in 1:length(retention_data_decomposed)) {
    rownames(retention_data_decomposed[[i]]) <- rownames(religion_data)
  }
  names(retention_data_decomposed) <- colnames(religion_data)[1:length(names(retention_data_decomposed))]
  return(retention_data_decomposed)
}


#function for joining decomposed data with labels

final_decomposition <- function(retention_data, distribution_list,
                                religion_data, post_switch = FALSE) {
  retention_data_adjusted <- retention_data_adjustment(retention_data)
  retention_data_decomposed <- dataframe_decomposition(retention_data_adjusted,
                                                       distribution_list,
                                                       post_switch)
  retention_data_decomposed <- labeling(retention_data_decomposed,
                                        religion_data)
  return(retention_data_decomposed)
}



################################################################################



survivability_adjustment_country <- function(mortality_full_data, code = "blank") {
  if (code != "blank") {
    mortality_full_data <- mortality_full_data[mortality_full_data[[1]] == code, ]
  }
  mortality_full_data <- mortality_full_data %>% arrange(mortality_full_data[[3]])
  colnames <- c("Country Code", "Sex", unique(mortality_full_data[[3]]))
  data_frame_mortality <- data.frame(matrix(data = 0, nrow = 0, ncol = 16))
  timeslist <- unique(mortality_full_data[[3]])
  for (c in unique(mortality_full_data[[1]])) {
    working_matrix <- data.frame(matrix(data = 0, nrow = 42, ncol = 0))
    c_code <- mortality_full_data[1:42, 1]
    sex_vec <- rep(unique(mortality_full_data[[4]]), each = 21)
    for (i in timeslist) {
      if (mortality_full_data[1,1] == c) {
        vector_mortality <- unlist(c(mortality_full_data[1:21, 7], mortality_full_data[23:43, 7]))
        working_matrix <- cbind(working_matrix, vector_mortality)
        mortality_full_data <- mortality_full_data[-(1:44), ]
      } else {
        working_matrix <- cbind(working_matrix, data.frame(matrix(data = 0, nrow = 42, ncol = 1)))
      }
    }
    working_matrix <- cbind(c_code, sex_vec, working_matrix)
    data_frame_mortality <- rbind(data_frame_mortality, working_matrix)
  }
  colnames(data_frame_mortality) <- colnames
  return(data_frame_mortality)
}


survivability_adjustment <- function(survivability_data) {
  frame <- data.frame(matrix(data = 0, nrow = 0, ncol = 20))
  codes <- unique(survivability_data[[1]])
  for (i in codes) {
    matrix <- survivability_adjustment_country(survivability_data, i)
    frame <- rbind(frame, matrix)
  }
  return(frame)
}
