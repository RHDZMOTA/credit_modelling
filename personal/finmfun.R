
# Basic financial mathematic functions 
# by: Rodrigo Hernández Mota

# Functions  --------------------------------------------------------------

# Functions to calculate:
# - future_value
# - present_value
# - interest_rate 

future_value <- function(M, i, n){
  # desc.
  # This function returns the future value of an investment given the 
  # annual interest rate, the initial capital and the # of months. 
  # 
  # I N P U T S:
  # * M: initial capital
  # * i: annual interest rate with monthly capitalization
  # * n: number of months required
  # 
  # O U T P U T S:
  # * FN: final/future value of capital M given the time and interest. 
  
  FN <- M * (1 + i/12) ^ n
  
  return(FN)
}

present_value <- function(FN, i, n){
  # desc. 
  # This function returns the present value of an investment given the
  # annual interest rate, the final caputal and the # of months. 
  # 
  # I N P U T S:
  # * FN: final/future expected value of capital M
  # * i: annual interest rate with monthly capitalization
  # * n: number of months required
  # 
  # O U T P U T S:
  # * M: initial capital required to fullfil the... 
  
  M <- Fn * (1 + i/12) ^ (-n)
  
  return(M)
}

interest_rate <- function(M, FN, n){
  # desc. 
  # This function returns the annual interest rate with monthly
  # capitalization of an investment of initial capital M in n months
  # to return a final capital FN. 
  # ...
  
  i <- (FN / M) ^ (1/n) - 1
  i <- 12 * i
  
  return(i)
}

eq_intrate <- function(init, cap){
  # desc. 
  # This function returns the equivalent annual interest rate with monthly
  # capitalization of a given initial annual interest rate with arbitrary
  # capitalization. 
  # 
  # I N P U T S:
  # * init = annual interest rate with arbitrary capitalization. 
  # * cap = number of capitalizations per year (e.g. daily capitalization is cap = 360)
  # 
  # O U T P U T S:
  
  i <- 12 * ( (1 + init / cap) ^ (cap / 12) - 1 )
  
  return(i)
}
