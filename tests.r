# Loading DEPART functions
source("functions.r")

# The `depart` function takes four inputs:
#      1. the week/day number
#      2. the actual price series
#      3. the threshold for distinguishing short-term discounts and long-term
#       regular price changes, corresponding to N_min in the paper
#      4. the maximum number of splits (for robustness testing purpose: 
#       see Web Appendix WD). It should be set to Inf for our proposed algorithm.
# `depart` returns a partition of the price series. Regular prices can be obtained
# by choosing the maximum price within each sub-series.

# A quick example with 15 weeks.
# In the first 10 weeks, the product regular price equals 1. 
# There are temporary discounts in weeks 4 and 5.
# In weeks 11-15, regular prices equal 1.2.
# `depart` returns two breaks at week 5 and 10 and 3 resulting partitions.
# Using the max price within each sub-series, we correctly recover regular prices.
depart(1:15,
       c(1,1,1,0.8,0.8,1,1,1,1,1,1.2,1.2,1.2,1.2,1.2),
       4, 
       Inf)

# Long Example 1.
# Actual and regular prices of a product spanning 77 days, saved in `sample1.csv`.
# The regular price is fixed at 1.39, but there were discounts in some weeks.
# `depart` takes in the actual prices data and returns a partition of the series
# (one split at week 49). Using the max price in each period, we correctly 
# recover regular prices.
sample <- read.csv(file = 'sample1.csv')
depart(sample$t, sample$actual, 4*7, Inf)

# Long Example 2.
# Actual and regular prices of a product spanning 784 days, saved in `sample2.csv`.
# There are changes in both actual and regular prices. `depart` returns a 
# partition of the price series, with splits in weeks 42, 294, 574, 616, 721 and 749.
# We can proceed to recover regular prices by using the max price within each sub-series.
sample <- read.csv(file = 'sample2.csv')
depart(sample$t, sample$actual, 4*7, Inf)
  
# Long Example 3.
# Actual and regular prices of a product spanning 861 days, saved in `sample3.csv`.
# There are changes in both actual and regular prices. `depart` returns a 
# partition of the price series, with splits in weeks 427, 455, 490, 511, 560, 714 and 763.
# We can proceed to recover regular prices by using the max price within each sub-series.
sample <- read.csv(file = 'sample3.csv')
depart(sample$t, sample$actual, 4*7, Inf)