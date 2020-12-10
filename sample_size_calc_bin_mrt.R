library(mrtbincalc)

if (0) {
    total_T <- 30
    p10 <- 0.5
    pT0 <- 0.5
    p11 <- 0.6
    pT1 <- 0.6
    alpha_shape <- "constant"
    beta_shape <- "constant"
    rand_prob <- rep(0.4, total_T)
    avail_pattern <- rep(0.7, total_T)
    typeIerror <- 0.05
    
    calculateSampleSizeBinaryMRT_wrapper(p10, pT0, p11, pT1,  total_T,
                                         alpha_shape, 
                                         beta_shape, 
                                         rand_prob, 
                                         avail_pattern,
                                         typeIerror,
                                         0.8)
    
    calculatePowerBinaryMRT_wrapper(p10, pT0, p11, pT1,  total_T,
                                         alpha_shape, 
                                         beta_shape, 
                                         rand_prob, 
                                         avail_pattern,
                                         typeIerror,
                                         35)
}

# Calculate sample size with given power: wrapper function
calculateSampleSizeBinaryMRT_wrapper <- function(p10, pT0, p11, pT1,
                                                 total_T,
                                                 alpha_shape = c("constant", "loglinear"),
                                                 beta_shape = c("constant", "loglinear"),
                                                 rand_prob,  ## p_t
                                                 avail_pattern, ## E[I_t]  # TQ: will assume this is vector of length T
                                                 typeIerror,
                                                 power) {
    alpha_shape <- match.arg(alpha_shape)
    beta_shape <- match.arg(beta_shape)
    
    alpha_beta_ate <- compute_alpha_beta_from_prob(p10, pT0, p11, pT1, total_T, alpha_shape, beta_shape)
    
    if (alpha_shape == "constant") {
        g_t <- matrix(1, nrow = total_T, ncol = 1)
    } else if (alpha_shape == "loglinear") {
        g_t <- cbind(1, 1:total_T)
    }
    
    if (beta_shape == "constant") {
        f_t <- matrix(1, nrow = total_T, ncol = 1)
    } else if (beta_shape == "loglinear") {
        f_t <- cbind(1, 1:total_T)
    }
    
    sample_size <- calculate_mrt_bin_samplesize_f(avail_pattern = avail_pattern,
                                        f_t = f_t,
                                        g_t = g_t,
                                        beta = alpha_beta_ate$beta,
                                        alpha = alpha_beta_ate$alpha,
                                        p_t = rand_prob,
                                        gamma = typeIerror,
                                        b = 1 - power,
                                        exact = FALSE)
    return(sample_size)
}


# Calculate sample size with given sample size: wrapper function
calculatePowerBinaryMRT_wrapper <- function(p10, pT0, p11, pT1,
                                      total_T,
                                      alpha_shape = c("constant", "loglinear"),
                                      beta_shape = c("constant", "loglinear"),
                                      rand_prob,  ## p_t
                                      avail_pattern, ## E[I_t]  # TQ: will assume this is vector of length T
                                      typeIerror,
                                      sample_size) {
    alpha_shape <- match.arg(alpha_shape)
    beta_shape <- match.arg(beta_shape)
    
    alpha_beta_ate <- compute_alpha_beta_from_prob(p10, pT0, p11, pT1, total_T, alpha_shape, beta_shape)
    
    if (alpha_shape == "constant") {
        g_t <- matrix(1, nrow = total_T, ncol = 1)
    } else if (alpha_shape == "loglinear") {
        g_t <- cbind(1, 1:total_T)
    }
    
    if (beta_shape == "constant") {
        f_t <- matrix(1, nrow = total_T, ncol = 1)
    } else if (beta_shape == "loglinear") {
        f_t <- cbind(1, 1:total_T)
    }
    
    power <- calculate_mrt_bin_power_f(avail_pattern = avail_pattern,
                             f_t = f_t,
                             g_t = g_t,
                             beta = alpha_beta_ate$beta,
                             alpha = alpha_beta_ate$alpha,
                             p_t = rand_prob,
                             gamma = typeIerror,
                             sample_size = sample_size)
    return(power)
}


## For computing the generative model from user inputs
compute_alpha_beta_from_prob <- function(p10, pT0, p11, pT1, total_T,
                                         alpha_shape = c("constant", "loglinear"),
                                         beta_shape = c("constant", "loglinear")) {
    
    #log(p11) = alpha_0 + alpha_1 + beta_0 + beta_1 
    #log(p10) = alpha_0 + alpha_1
    #log(pT1) = alpha_0 + total_T * alpha_1 + beta_0 + total_T * beta_1
    #log(pT0) = alpha_0 + total_T * alpha_1)
    
    #alpha_0 + alpha_1 = log(p10) => alpha_0 = log(p10) - alpha_1
    #log(pt0) = log(p10) - alpha_1 + total_T * alpha_1 = (total_T - 1)*alpha_1 + log(p10) => alpha_1 = log(pt0/p10) / (total_T - 1)
    
    alpha_shape <- match.arg(alpha_shape)
    beta_shape <- match.arg(beta_shape)
    
    alpha_1 <- log(pT0/p10) / (total_T - 1)
    alpha_0 <- log(p10) - alpha_1
    
    #beta_0 = log(p11) - (alpha_1 + alpha_0 + beta_1)
    #beta_0 = log(pT1) - alpha_0 - total_T * alpha_1 - total_T * beta_1
    
    eq <- function(beta_1) {
        log(pT1) - alpha_0 - total_T * alpha_1 - total_T * beta_1 - (log(p11) - (alpha_1 + alpha_0 + beta_1))
    }
    
    beta_1 <- uniroot(eq, interval = c(-4, 5), extendInt = "yes")$root
    beta_0 <- log(p11) - (alpha_1 + alpha_0 + beta_1)
    ate <-  sum(exp(alpha_0 + alpha_1 * (1:total_T) + beta_0 + beta_1 * (1:total_T))) /
        sum(exp(alpha_0 + alpha_1 * (1:total_T)))
    
    if (alpha_shape == "constant") {
        alpha <- alpha_0
    } else if (alpha_shape == "loglinear") {
        alpha <- c(alpha_0, alpha_1)
    }
    
    if (beta_shape == "constant") {
        beta <- beta_0
    } else if (beta_shape == "loglinear") {
        beta <- c(beta_0, beta_1)
    }
    
    return(list(alpha = alpha, beta = beta, ate = ate))
}