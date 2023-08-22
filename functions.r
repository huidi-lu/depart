# `depart` is the main function collect the arguments that the user provides
# and then execute the full algorithm.
# The `depart` function takes four inputs:
#      1. the week/day number
#      2. the actual price series
#      3. the threshold for distinguishing short-term discounts and long-term
#       regular price changes, corresponding to N_min in the paper
#      4. the maximum number of splits (for robustness testing purpose: 
#       see Web Appendix WD). It should be set to Inf for our proposed algorithm.
# `depart` returns a partition of the price series. Regular prices can be obtained
# by choosing the maximum price within each sub-series.

depart <-
  function(weeks,
           raw_price,
           MinWeeksDiscount,
           MaxNumSplits) {
    NWEEKS <- length(weeks)
    partitions <- list(weeks)
    splits <- numeric()
    flag_maxsplit <- FALSE
    
    # Execute the recursive binary splitting procedure. Stop when RSS cannot be
    # further improved (Equation 1 in the paper).
    while (TRUE) {
      NPART <- length(partitions)
      
      # For each existing sub-partition, use `find_split` to identify a new split.
      # Store the new split in a list.
      for (ip in 1:NPART) {
        week_series <- unlist(partitions[[ip]])
        new_split <-
          find_split(week_series, raw_price[week_series], MinWeeksDiscount)
        splits <- c(splits, new_split)
        if (length(splits) >= MaxNumSplits) {
          flag_maxsplit <- TRUE
          break
        }
      }
      
      # In each loop, update the partitions based on currently identified splits.
      # Stop the procedure when no new splits are proposed (i.e., RSS cannot be
      # further improved).
      if (NPART == length(splits) + 1) {
        break
      } else {
        splits <- sort(splits)
        loc_splits <- which(weeks %in% splits)
        subpart_sizes <- diff(c(0, loc_splits, NWEEKS))
        partitions <-
          lapply(seq_along(subpart_sizes), function(i)
            weeks[(sum(subpart_sizes[1:i - 1]) + 1):sum(subpart_sizes[1:i])])
      }
      
      if (flag_maxsplit) {
        break
      }
    }
    return(list(partitions = partitions, splits = splits))
  }

# The `find_split` function loops over each period in the price series, test whether
# RSS can be improved and calls `refine_split` to locate the best split.
find_split <- function(weeks, raw_price, MinLeafSize) {
  prior_resid <- raw_price - mean(raw_price, na.rm = TRUE)
  prior_rss <- sum(prior_resid ^ 2, na.rm = TRUE)
  post_rss <- rep(NA, length(raw_price) - 1)
  
  for (i in 1:(length(raw_price) - 1)) {
    w <- weeks[i]
    left <- raw_price[weeks <= w]
    right <- raw_price[weeks > w]
    post_rss[i] <-
      sum((left - mean(left, na.rm = TRUE)) ^ 2, na.rm = TRUE) +
      sum((right - mean(right, na.rm = TRUE)) ^ 2, na.rm = TRUE)
  }
  
  delta_rss <- prior_rss - post_rss # Equation 1 in paper
  split_week <-
    refine_split(delta_rss, weeks, raw_price, MinLeafSize, MinLeafSize)
  return(split_week)
}

# `refine_split` implements our two-stage rule to choose the best split (section 3.2).
refine_split <-
  function(delta_rss,
           weeks,
           raw_price,
           frontMinLeafSize,
           backMinLeafSize) {
    split_week <- NULL
    max_delta_rss <- max(delta_rss, na.rm = TRUE)
    # Equation 3: The potential split location that ignores the minimum length condition.
    i <- which.max(delta_rss) 
    
    if (max_delta_rss > 1e-6) {# for numeric stability
      # In the first case where the minimum length condition is satisfied, we
      # directly propose the split.
      if ((weeks[i] - weeks[1] + 1) >= frontMinLeafSize &&
          (weeks[length(weeks)] - weeks[i] + 1) >= backMinLeafSize) {
        split_week <- weeks[i]
      # The following two cases are when the potential split is close to 
      # the right boundary, corresponding to case ii in the paper.
        ## case ii-a: when left price is lower, we just ignore the length condition.
      } else if ((weeks[i] - weeks[1] + 1) >= frontMinLeafSize &&
                 (weeks[length(weeks)] - weeks[i] + 1) < backMinLeafSize &&
                 mean(raw_price[1:i]) < mean(raw_price[(i + 1):length(raw_price)])) {
        split_week <- weeks[i]
        ## case ii-b: when left price is higher, we revert to the basic ART solution.
      } else if ((weeks[i] - weeks[1] + 1) >= frontMinLeafSize &&
                 (weeks[length(weeks)] - weeks[i] + 1) < backMinLeafSize &&
                 mean(raw_price[1:i]) >= mean(raw_price[(i + 1):length(raw_price)]) &&
                 1 <= length(raw_price) - backMinLeafSize - 1) {
        split_week <-
          refine_split(delta_rss[1:(length(raw_price) - backMinLeafSize - 1)],
                       weeks[1:(length(raw_price) - backMinLeafSize)],
                       raw_price[1:(length(raw_price) - backMinLeafSize)],
                       frontMinLeafSize, 1)
      # The following two cases are when the potential split is close to 
      # the left boundary, corresponding to case iii in the paper.
        ## case iii-a: when left price is higher, we just ignore the length condition.
      } else if ((weeks[i] - weeks[1] + 1) < frontMinLeafSize &&
                 (weeks[length(weeks)] - weeks[i] + 1) >= backMinLeafSize &&
                 mean(raw_price[1:i]) > mean(raw_price[(i + 1):length(raw_price)])) {
        split_week <- weeks[i]
        ## case iii-b: when left price is lower, we revert to the basic ART solution.
      } else if ((weeks[i] - weeks[1] + 1) < frontMinLeafSize &&
                 (weeks[length(weeks)] - weeks[i] + 1) >= backMinLeafSize &&
                 mean(raw_price[1:i]) <= mean(raw_price[(i + 1):length(raw_price)]) &&
                 frontMinLeafSize <= length(raw_price) - 1) {
        split_week <-
          refine_split(delta_rss[frontMinLeafSize:(length(raw_price) - 1)],
                       weeks[frontMinLeafSize:length(weeks)],
                       raw_price[frontMinLeafSize:length(raw_price)],
                       1, backMinLeafSize)
      }
    }
    
    return(split_week)
  }
