NOREMARK <- function(marked_available, marked_seen, unmarked_seen, 
                     confidence = 0.95, interval_max = 20000) {
  
  # Input validation
  if (length(marked_available) != length(marked_seen) || 
      length(marked_seen) != length(unmarked_seen)) {
    stop("All input vectors must have the same length")
  }
  
  occ <- length(marked_seen)
  total_seen <- marked_seen + unmarked_seen
  
  # Calculate minimum number known alive
  min_alive <- max(marked_available) + max(unmarked_seen)
  
  # CORRECTED Chapman estimator with exact NOREMARK variance formula
  chapman_estimator <- function(M, m, n, confidence = 0.95) {
    # Chapman estimator
    N_hat <- ((M + 1) * (n + 1)) / (m + 1) - 1
    
    # NOREMARK's exact variance formula for Chapman estimator
    # This matches the manual output exactly
    var_N <- ( (M + 1) * (n + 1) * (M - m) * (n - m) ) / 
      ( (m + 1) * (m + 1) * (m + 2) )
    
    # Normal approximation confidence intervals (matches NOREMARK manual output)
    if (var_N > 0 && is.finite(var_N)) {
      se <- sqrt(var_N)
      z <- qnorm(1 - (1 - confidence) / 2)
      
      lower <- N_hat - z * se
      upper <- N_hat + z * se
    } else {
      lower <- NA
      upper <- NA
    }
    
    return(list(estimate = N_hat, lower = lower, upper = upper))
  }
  
  # Calculate Chapman estimates for each occasion
  chapman_results <- list()
  for (i in 1:occ) {
    chapman_results[[i]] <- chapman_estimator(
      M = marked_available[i],
      m = marked_seen[i], 
      n = total_seen[i],
      confidence = confidence
    )
  }
  
  # Joint Hypergeometric Likelihood Function
  joint_hypergeometric_loglik <- function(N) {
    ll_value <- 0
    for (i in 1:occ) {
      M_i <- marked_available[i]
      m_i <- marked_seen[i]
      u_i <- unmarked_seen[i]
      n_i <- total_seen[i]
      
      if (n_i == 0) next
      if (M_i > N || m_i > M_i || u_i > (N - M_i) || m_i > n_i || u_i > n_i) {
        return(-1e20)
      }
      
      ll_value <- ll_value + lchoose(M_i, m_i) + lchoose(N - M_i, u_i) - lchoose(N, n_i)
    }
    return(ll_value)
  }
  
  # Find MLE using optimization
  lower_bound <- max(min_alive, max(total_seen) + 1)
  
  mle_result <- optimize(
    function(N) -joint_hypergeometric_loglik(N),
    interval = c(lower_bound, interval_max),
    maximum = FALSE,
    tol = 1e-8
  )
  
  N_mle <- round(mle_result$minimum)
  max_loglik <- -mle_result$objective
  
  # Profile Likelihood Confidence Interval
  chi_critical <- qchisq(confidence, df = 1)
  target_loglik <- max_loglik - chi_critical / 2
  
  lower_ci <- tryCatch({
    optimize(
      function(N) (joint_hypergeometric_loglik(N) - target_loglik)^2,
      interval = c(lower_bound, N_mle),
      tol = 1e-8
    )$minimum
  }, error = function(e) min_alive)
  
  upper_ci <- tryCatch({
    optimize(
      function(N) (joint_hypergeometric_loglik(N) - target_loglik)^2,
      interval = c(N_mle, interval_max),
      tol = 1e-8
    )$minimum
  }, error = function(e) interval_max)
  
  # Extract Chapman estimates for data frame
  chapman_estimates <- sapply(chapman_results, function(x) x$estimate)
  chapman_lower <- sapply(chapman_results, function(x) x$lower)
  chapman_upper <- sapply(chapman_results, function(x) x$upper)
  
  occasion_stats <- data.frame(
    occasion = 1:occ,
    marked_available = marked_available,
    marked_seen = marked_seen,
    unmarked_seen = unmarked_seen,
    total_seen = total_seen,
    sighting_rate = ifelse(marked_available > 0, marked_seen / marked_available, NA),
    chapman_estimate = round(chapman_estimates, 1),
    chapman_lower = round(chapman_lower, 1),
    chapman_upper = round(chapman_upper, 1)
  )
  
  valid_chapman <- chapman_estimates[is.finite(chapman_estimates)]
  mean_chapman <- ifelse(length(valid_chapman) > 0, mean(valid_chapman), NA)
  
  list(
    minimum_alive = min_alive,
    population_estimate = N_mle,
    confidence_interval = c(lower = round(lower_ci, 1), upper = round(upper_ci, 1)),
    confidence_level = confidence,
    max_loglikelihood = max_loglik,
    occasion_data = occasion_stats,
    mean_chapman_estimate = round(mean_chapman, 1),
    method = "Joint Hypergeometric Maximum Likelihood Estimator (JHE)"
  )
}
