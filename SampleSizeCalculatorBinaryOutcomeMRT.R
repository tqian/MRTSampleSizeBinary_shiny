
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
    
    sample_size <- MRT_bin_samplesize.F(avail_pattern = avail_pattern,
                                        f_t = f_t,
                                        g_t = g_t,
                                        beta = alpha_beta_ate$beta,
                                        alpha = alpha_beta_ate$alpha,
                                        p_t = rand_prob,
                                        gamma = typeIerror,
                                        b = 1 - power)
    return(ceiling(sample_size))
}


# Calculate sample size with given power
MRT_bin_samplesize.F <- function(avail_pattern,  ## E[I_t]  # TQ: will assume this is vector of length T
                                 f_t,             ## f(t)    # TQ: will assume this f_t object is matrix of dimension T * p
                                 g_t,             ## g(t)    # TQ: will assume this g_t object is matrix of dimension T * q
                                 beta,            ## beta_0
                                 alpha,           ## alpha_0
                                 p_t,             ## p_t (randomization probability)
                                 gamma,          ## gamma (type I error rate),
                                 b)               ## b (Type II error rate)
{
    
    p <- length(beta)
    q <- length(alpha)
    
    M_and_Sigma <- computeMSigma(avail_pattern, f_t, g_t, beta, alpha, p_t)
    M.Matrix <- M_and_Sigma$M
    Sigma.Matrix <- M_and_Sigma$Sigma
    
    ## Setting up the function that we will ultimately solve to get the sample size
    powerF <- function(n){
        ## Lambda as a function of x (i.e., the sample size)
        lambda <- function(x){
            as.numeric(x * t(beta) %*% solve(solve(M.Matrix) %*% Sigma.Matrix %*% t(solve(M.Matrix))) %*% beta)
        }
        
        righthand.side <- pf(q=qf(p=(1-gamma), df1=p, df2=n-q-p), df1=p, df2=n-q-p, ncp=lambda(n))
        lefthand.side = b
        return(righthand.side - lefthand.side)
    }
    
    sample_size <- uniroot(powerF, lower=p+q+1, upper=1000000)$root
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
    
    power <- MRT_bin_power.F(avail_pattern = avail_pattern,
                             f_t = f_t,
                             g_t = g_t,
                             beta = alpha_beta_ate$beta,
                             alpha = alpha_beta_ate$alpha,
                             p_t = rand_prob,
                             gamma = typeIerror,
                             sample_size = sample_size)
    return(power)
}

# Calculate the power with given sample size
MRT_bin_power.F <- function(avail_pattern,  ## E[I_t]  # TQ: will assume this is vector of length T
                            f_t,             ## f(t)    # TQ: will assume this f_t object is matrix of dimension T * p
                            g_t,             ## g(t)    # TQ: will assume this g_t object is matrix of dimension T * q
                            beta,            ## beta_0
                            alpha,           ## alpha_0
                            p_t,             ## p_t (randomization probability)
                            gamma,          ## gamma (type I error),
                            sample_size)     ## sample size
{
    
    p <- length(beta)
    q <- length(alpha)
    
    M_and_Sigma <- computeMSigma(avail_pattern, f_t, g_t, beta, alpha, p_t)
    M.Matrix <- M_and_Sigma$M
    Sigma.Matrix <- M_and_Sigma$Sigma
    
    ## Setting up the function that we will ultimately solve to get the sample size
    n <- sample_size
    lambda <- as.numeric(n * t(beta) %*% solve(solve(M.Matrix) %*% Sigma.Matrix %*% t(solve(M.Matrix))) %*% beta)
    b <- pf(q=qf(p=(1-gamma), df1=p, df2=n-q-p), df1=p, df2=n-q-p, ncp=lambda)
    return(1 - b)
}


computeMSigma <- function(avail_pattern,  ## E[I_t]  # TQ: will assume this is vector of length T
                          f_t,             ## f(t)    # TQ: will assume this f_t object is matrix of dimension T * p
                          g_t,             ## g(t)    # TQ: will assume this g_t object is matrix of dimension T * q
                          beta,            ## beta_0
                          alpha,           ## alpha_0
                          p_t              ## p_t
) {
    p <- length(beta)
    q <- length(alpha)
    
    ## The M and Sigma matrices (needed to compute lambda)
    M.Matrix <- matrix(data=0, nrow=length(beta), ncol=length(beta))
    Sigma.Matrix <- matrix(data=0, nrow=length(beta), ncol=length(beta))
    for (i in 1:length(p_t)){## For each decision point (T = total # of decision points is taken as the length of p_t, for now)
        
        # breaking down steps to identify bug and improve robustness of code
        this_f_t <- as.numeric(f_t[i, ])
        this_g_t <- as.numeric(g_t[i, ])
        
        stopifnot(length(this_f_t) == p)
        stopifnot(length(this_g_t) == q)
        
        this_f_t_times_beta <- sum(this_f_t * beta) # both are vectors, so use this way to calculate inner product
        this_g_t_times_alpha <- sum(this_g_t * alpha)
        
        this_f_t_f_t <- outer(this_f_t, this_f_t) # this is f_t %*% f_t^T
        
        ThisM <- as.numeric(avail_pattern[i] * (exp(p_t[i] * this_f_t_times_beta)) * exp(this_g_t_times_alpha) * (1-p_t[i]) * p_t[i]) * this_f_t_f_t
        stopifnot("matrix" %in% class(ThisM) & all(dim(ThisM) == c(p, p))) # added a check of dimension
        
        M.Matrix <- M.Matrix + ThisM  ## A running sum so that we end up with each entry being the sum of that entry across all time points
        
        ThisSigma <- as.numeric(avail_pattern[i] * (exp(2 * p_t[i] * this_f_t_times_beta)) * exp(this_g_t_times_alpha) * (1-p_t[i]) * p_t[i] * ((1-p_t[i]) * exp(-1 * this_f_t_times_beta) + p_t[i] - exp(this_g_t_times_alpha))) * this_f_t_f_t
        Sigma.Matrix <- Sigma.Matrix + ThisSigma
        stopifnot("matrix" %in% class(ThisSigma) & all(dim(ThisSigma) == c(p, p)))
    }
    
    return(list(M = M.Matrix, Sigma = Sigma.Matrix))
    
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