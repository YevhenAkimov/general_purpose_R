
MinMax=function (data, min, max) {
  data2 <- data
  data2[data2 > max] <- max
  data2[data2 < min] <- min
  return(data2)
}


MinMaxQuant=function(data, q=0.1 , low_q=NULL, high_q=NULL){
  if (!is.null(q)){
    if (sign(0.5-q)==-1) {
      low_q=1-q
      high_q=q
    } else {
      low_q=q
      high_q=1-q
    }
    
    if (is.null(low_q)){
      low_q=q
    }
    if (is.null(high_q)){
      high_q=q
    }
  } else if (!is.null(low_q) & !is.null(high_q)){
    ## check if low_q < high_q
    if (low_q > high_q){
      stop("Error low_q > high_q")
    }
    
  } else {
    stop("Error: q or low_q and high_q should be provided")
  }
  # print(low_q)
  # print(high_q)
  # 
  min=quantile(data,low_q,na.rm = T)
  max=quantile(data,high_q,na.rm = T)
  # 
  # print(paste("min:",min))
  # print(paste("max:",max))
  
  data[data > max] <- max
  data[data < min] <- min
  return(data)
  
}

min_max_normalization <- function(x, new_min=0, new_max=1) {
if (length(x) == 1) {
    return(new_min)
  }
  normalized_x <- (x - min(x,na.rm = T)) / (max(x,na.rm = T) - min(x,na.rm = T)) * (new_max - new_min) + new_min
  return(normalized_x)
}

find_min_except_zero <- function(mat) {
  non_zero_elements <- mat[mat != 0]
  min_val <- min(non_zero_elements,na.rm = T)
  return(min_val)
}
cv <-function(x, ..., aszero=FALSE, na.rm=FALSE)  {
  #  R function to compute the coefficient of variation (expressed as a percentage)
  # if there is only a single value, stats::sd = NA. However, one could argue that cv =0. 
  # and NA may break the code that receives it.
  #The function returns NA if(aszero=FALSE)   else a value of 0 is returned.
  x <- c(x, ...)
  z <- x[!is.na(x)]
  if (length(z) == 0) { 
    return(NA) 
  } else if (na.rm == FALSE & (length(z) < length(x))) { 
    return(NA)	 
  } else if (length(z) == 1 & aszero) { 
    return(0)
  } else {
    # abs to avoid very small (or zero) mean with e.g. -5:5
    x <- mean(abs(z))  
    if (x == 0) {# all values are 0
      return(0)
    } else {
      return(100 * stats::sd(z) / x)
    }
  }	
}


# summarize_columns.R
#' Summarise rows of a data frame by cluster labels
#'
#' `summarize_columns()` groups the rows of *data* using a cluster assignment
#' vector and applies an aggregation function (default: `mean`) to all numeric
#' columns.  Non-numeric columns are collapsed to a single representative value
#' (first unique value) per cluster.  Optionally, the function adds a list
#' column containing the original row indices for each cluster.
#'
#' This helper is designed to live in a GitHub repository or R package.  The
#' implementation uses only base R, is pipe-free, and includes roxygen2 docs

#'
#' @param data           A **data.frame**.  Must have row names if
#'                       `order_rows = TRUE`.
#' @param cluster_vector A vector (factor, character, or numeric) giving the
#'                       cluster assignment for each row of *data*.
#' @param fun            Aggregation function applied to numeric columns
#'                       (default `mean`).  The function must accept a numeric
#'                       vector and return a scalar.
#' @param order_rows     Logical.  If `TRUE` the rows of *data* and
#'                       *cluster_vector* are matched via row names to ensure
#'                       proper alignment (default `FALSE`).
#' @param indices        Logical.  If `TRUE` a list column `indices`
#'                       is added that stores the 1-based row indices (in *data*)
#'                       belonging to each cluster (default `FALSE`).
#'
#' @return A data frame where each row corresponds to a cluster.  The row names
#'   are the cluster labels.  Column order is: numeric summaries, non-numeric
#'   summaries, optional `indices` list column.
#'
#' @details
#' * **Numeric columns** – aggregated with *fun*.
#' * **Non-numeric columns** – the first unique value in the cluster is kept.
#'   A warning is emitted if more than one unique value exists.
#' * **Indices column** – useful for back-tracking which original rows belong
#'   to which cluster after summarisation.
#'
#' @seealso [aggregate()], [tapply()].
#' @export
#'
#' @examples
#' df <- data.frame(a = rnorm(6),
#'                  b = runif(6),
#'                  label = LETTERS[1:6],
#'                  row.names = paste0("cell", 1:6))
#' cl <- c("x", "x", "y", "y", "y", "z")
#' summarize_columns(df, cl)
#'
#' ## Different summary function and enable indices
#' summarize_columns(df, cl, fun = median, indices = TRUE)
#'
#' ## Non-numeric column with inconsistent values triggers a warning
#' df$label[1] <- "Z"  # now label differs within cluster "x"
#' summarize_columns(df, cl)
summarize_columns <- function(data,
                              cluster_vector,
                              fun        = mean,
                              order_rows = FALSE,
                              indices    = FALSE) {
  
  ## ---- Sanity checks --------------------------------------------------
  if (!is.data.frame(data)) {
    stop("data must be a data frame.")
  }
  
  if (order_rows) {
    if (is.null(rownames(data))) {
      stop("data must have row names when order_rows = TRUE.")
    }
    if (is.null(names(cluster_vector))) {
      stop("cluster_vector must have names when order_rows = TRUE.")
    }
    cluster_vector <- cluster_vector[rownames(data)]
  }
  
  ## Convert cluster vector to factor for consistent ordering
  cluster_vector <- as.factor(cluster_vector)
  
  ## Identify numeric and non-numeric columns
  numeric_cols     <- vapply(data, is.numeric, logical(1))
  non_numeric_cols <- !numeric_cols
  
  summarized_list <- list()
  
  ## ---- Numeric aggregation -------------------------------------------
  if (any(numeric_cols)) {
    summarized_numeric <- aggregate(data[, numeric_cols, drop = FALSE],
                                    by = list(cluster = cluster_vector),
                                    FUN = fun)
    summarized_list$numeric <- summarized_numeric
  }
  
  ## ---- Non-numeric aggregation ---------------------------------------
  if (any(non_numeric_cols)) {
    
    summarize_non_numeric <- function(vec) {
      u <- unique(vec)
      if (length(u) > 1) {
        warning("Non-numeric column has multiple values within a cluster; taking first.")
      }
      u[1]
    }
    
    summarized_non_numeric <- aggregate(data[, non_numeric_cols, drop = FALSE],
                                        by  = list(cluster = cluster_vector),
                                        FUN = summarize_non_numeric)
    summarized_list$non_numeric <- summarized_non_numeric
  }
  
  if (!length(summarized_list)) {
    stop("No columns to summarise.")
  }
  
  ## ---- Combine numeric + non-numeric ----------------------------------
  summarized_data <- Reduce(function(x, y) merge(x, y, by = "cluster"),
                            summarized_list)
  
  ## ---- Indices column -------------------------------------------------
  if (indices) {
    ind <- tapply(seq_along(cluster_vector), cluster_vector, identity)
    ind_df <- data.frame(cluster = names(ind), indices = I(unname(ind)),
                         row.names = NULL)
    summarized_data <- merge(summarized_data, ind_df, by = "cluster")
  }
  
  rownames(summarized_data) <- summarized_data$cluster
  summarized_data$cluster <- NULL
  summarized_data
}

summarize_over_rows_given_colgroup <- function(mat, groups, func1,retain_colnames=F) {
  # Validate that 'mat' is a matrix
  if (!is.matrix(mat)) {
    stop("The first argument 'mat' must be a matrix.")
  }
  
  # Validate that 'groups' is a vector with length equal to the number of columns in 'mat'
  if (length(groups) != ncol(mat)) {
    stop("Length of 'groups' must match the number of columns in 'mat'.")
  }
  
  # Identify unique groups
  unique_groups <- unique(groups)
  
  # Initialize a matrix to store the summarized results
  # Number of rows remains the same, number of columns equals the number of unique groups
  result <- matrix(NA, nrow = nrow(mat), ncol = length(unique_groups))

# Assign column names based on unique groups for clarity
colnames(result) <- unique_groups

# Iterate over each row of the matrix
for (i in 1:nrow(mat)) {
  # Extract the current row
  row_values <- mat[i, ]
  
  # Apply the summary function to each group within the row

  summarized_values <- sapply(unique_groups, function(g) {
    func1(row_values[groups == g])
  })
  
  # Store the summarized values in the result matrix
  result[i, ] <- summarized_values
}

if (retain_colnames) {
 
if (!is.null(rownames(mat))) {
  rownames(result) <- rownames(mat)
} }

return(result)
}


rowCVs <- function(df) {
  apply(df, 1, function(x) sd(x, na.rm = TRUE) / mean(x, na.rm = TRUE) * 100)
}
