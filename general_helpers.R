min_max_normalization <- function(x, new_min=0, new_max=1) {
if (length(x) == 1) {
    return(new_min)
  }
  normalized_x <- (x - min(x,na.rm = T)) / (max(x,na.rm = T) - min(x,na.rm = T)) * (new_max - new_min) + new_min
  return(normalized_x)
}

