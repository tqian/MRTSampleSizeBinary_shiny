library(shiny)
source("sample_size_calc_bin_mrt.R")

shinyServer(function(input,output,session){
    
    ### Calculate total number of decision points based on # Days and # Decision points per day
    
    total_decision_points <- reactive({
        
        validate(
            need(input$days == round(input$days),
                 "Error: Please enter integer values for the number of days"),
            need(input$days > 0 ,
                 "Error: Please specify the number of days greater than 0"),
            need(input$occ_per_day == round(input$occ_per_day),
                 "Error: Please enter integer values for the number of decision points per day"),
            need(input$occ_per_day > 0 ,
                 "Error: Please specify the number of decision points per day greater than 0")
        )
        
        input$days * input$occ_per_day
    })
    
    ### Reading the file for time-varying randomization probability###
    
    #### Reading the file of decision times ###
    P_inter_dec <- reactive({    
        
        inFile <- input$file1
        
        if (is.null(inFile))
            return(NULL)
        
        read.csv(inFile$datapath, header = TRUE, sep = ',')
    })
    
    #### Output the first five rows of the table reading from the file with respect to decision times 
    ### and output warnings if the format of the file is not correct
    output$P_inter_table_dec <- renderTable({       
        delta <- as.vector(P_inter_dec()$Randomization.Probability)
        validate(
            need(!is.null(input$file1), "Warning: No file is uploaded"),
            need(is.null(input$file1) || length(data) == input$days * input$occ_per_day, 
                 "Error: the number of decision times doesn't match."),
            need(is.null(input$file1) || max(data) <= 1, 
                 "Error: some value of randomization prob
                 ability is bigger than 1"),
            need(is.null(input$file1) || min(data) >= 0, 
                 "Error: some value of randomization probability is less than 0")
        )
        head(P_inter_dec(), n = 5)
    })
    
    ### Reading the file with respect to days ###
    P_inter_days <- reactive({     
        
        inFile <- input$file2
        
        if (is.null(inFile))
            return(NULL)
        
        read.csv(inFile$datapath, header = TRUE, sep = ',')
    })
    
    #### Output the first five rows of the table reading from the file with respect to days
    ### and output warnings if the format of the file is not correct
    output$P_inter_table_days <- renderTable({      #### Output the first five rows of the table reading from the file for days
        delta <- as.vector(P_inter_days()$Randomization.Probability)
        
        validate(
            need(!is.null(input$file2), "Warning: No file is uploaded"),
            need(is.null(input$file2) || length(data) == input$days , 
                 "Error: the number of days doesn't match."),
            need(is.null(input$file2) || max(data) <= 1, 
                 "Error: some value of randomization probability is bigger than 1"),
            need(is.null(input$file2) || min(data) >= 0, 
                 "Error: some value of randomization probability is less than 0")
        )
        
        head(P_inter_days(), n = 5)
    })
    
    #### Calculating baseline success probability ####
    
    alpha_input <- reactive({
        # Initialize some value to avoid some internal error when running locally
        # This part should have not effect on the UI.
        result <- 0.5
        if (input$alpha_choices == "constant") {
        
            validate(
                need(input$alpha_constant_mean > 0, 
                     "Error: Please specify the baseline success probability greater than 0")
            )
        
            result <- rep(input$alpha_constant_mean, total_decision_points())
        
        } else if (input$alpha_choices == "loglinear") {
            
            validate(
                need(input$alpha_loglinear_initial > 0, 
                     "Error: Please specify the initial value of baseline success probability greater than 0"),
                need(input$alpha_loglinear_final > 0, 
                     "Error: Please specify the final value of baseline success probability greater than 0")
            )
            
            initial_log <- log(input$alpha_loglinear_initial)
            final_log <- log(input$alpha_loglinear_final)
            result_log <- seq(from = initial_log, 
                              to = final_log, 
                              length.out = total_decision_points())
            
            result <- exp(result_log)
        }
        validate(
            need(min(result) > 0,
                 "Warning: Some values of baseline success probability are less than or equal to 0"),
            need(max(result) <= 1,
                 "Warning: Some values of baseline success probability are greater than 1")
        )
        result
    })
    
    ### plot of the graphs for the baseline success probability ###
    
    output$alpha_graph <- renderPlot({
        plot(alpha_input(), 
             xlab = "Decision Point", 
             ylab = "Baseline Success Probability", 
             ylim = c(0, 1), 
             type = "o",
             pch = 16, 
             cex = 0.8, 
             col = 4)
    })
    
    
    #### Calculating proximal treatment effect (relative risk) ####
    
    beta_input <- reactive({
        # Initialize some value to avoid some internal error when running locally
        # This part should have not effect on the UI.
        result <- 1
        if (input$beta_choices == "constant") {
        
            validate(
                need(input$beta_constant_mean > 0, 
                     "Error: Please specify the proximal treatment effect greater than 0")
            )
        
            result <- rep(input$beta_constant_mean, total_decision_points())
        
        } else if (input$beta_choices == "loglinear") {
            validate(
                need(input$beta_loglinear_initial > 0, 
                     "Error: Please specify the initial value of proximal treatment effect greater than 0"),
                need(input$beta_loglinear_final > 0, 
                     "Error: Please specify the final value of proximal treatment effect greater than 0")
            )
        
            initial_log <- log(input$beta_loglinear_initial)
            final_log <- log(input$beta_loglinear_final)
            result_log <- seq(from = initial_log, to = final_log, 
                              length.out = total_decision_points())
        
            result <- exp(result_log)
        }
        validate(
            need(min(result * alpha_input()) > 0,
                 "Warning: Some values of success probability are less than or equal to 0"),
            need(max(result * alpha_input()) <= 1,
                 "Warning: Some values of success probability are greater than 1")
        )
        result
    })
    
    ### plot of the graphs for the proximal treatment effect ###
    
    output$beta_graph <- renderPlot({
        plot(alpha_input() * beta_input(), 
             xlab = "Decision Point", 
             ylab = "Success Probability", 
             ylim = c(0, 1), 
             type = "o",
             pch = 16, 
             cex = 0.8, 
             col = 2)
        
        points(x = 1:length(alpha_input()), 
               y = alpha_input(), 
               type = "o", 
               pch = 16, 
               cex = 0.8, 
               col = 4)
        
        legend("topleft", 
               cex = 1, 
               legend=c('Alternate Hypothesis', 'Null Hypothesis'),
               col = c(2,4),
               lty = c(1,1), 
               pch=c(16,16),
               bty = "n")
    })
    
    
    ### p10, p11, pT0, pT1 ###
    
    p10 <- reactive({alpha_input()[1]})
    
    pT0 <- reactive({alpha_input()[total_decision_points()]})
    
    p11 <- reactive({alpha_input()[1] * beta_input()[1]})
    
    pT1 <- reactive({alpha_input()[total_decision_points()] *
            beta_input()[total_decision_points()]})
    
    #### Expected Availability ####
    
    avail_input <- reactive({
        # Initialize some value to avoid some internal error when running locally
        # This part should have not effect on the UI.
        result <- 0.5
        result <- 0.5
        if (input$avail_choices == "constant") {
            
            validate(
                need(input$avail_constant_mean > 0, 
                     "Error: Please specify the average availability greater than 0")
            )
            
            result <- rep(input$avail_constant_mean, total_decision_points())
        
        } else if (input$avail_choices == "linear") {
            
            validate(
                need(input$avail_linear_initial > 0, 
                     "Error: Please specify the initial value of expected availability greater than 0"),
                need(input$avail_linear_final > 0, 
                     "Error: Please specify the final value of expected availability greater than 0")
            )
            
            result <- seq(from = input$avail_linear_initial, 
                          to = input$avail_linear_final, 
                          length.out = total_decision_points())
        } 
        
        validate(
            need(min(result) > 0,
                 "Warning: Some values of expected availability are less than or equal to 0"),
            need(max(result) <= 1,
                 "Warning: Some values of expected availability are greater than 1")
        )
        result
    })
    
    ### Plot the graph for expected availability ###
    
    output$avail_graph <- renderPlot({
        plot(avail_input(), 
             xlab = "Decision Point", 
             ylab = "Expected Availability", 
             ylim = c(0, 1), 
             type = "o",
             pch = 16,
             cex = 0.8, 
             col = 4)
        abline(h = mean(avail_input()), lty = 2)
        legend("topleft", 
               legend=c('Availability','Average Availability'), 
               col = c(4,1),
               lty = c(1,2), 
               pch=c(16,NA),bty = "n")
    })
    
    ##### Calculate Sample Size #####
    
    sample_size <- eventReactive(input$button_calculate_sample_size, {
        # The determination of randomization probability is not well-implemented.
        # Need to think more carefully, because there are three sources of rand. prob.
        rand_prob <- rep(input$rand_prob_const, total_decision_points())
        
        if (!is.null(input$file1)) {
            rand_prob <- rep(P_inter_days()$Randomization.Probability, 
                             each = input$occ_per_day)
        }
        
        if (!is.null(input$file2)) {
            rand_prob <- P_inter_dec()$Randomization.Probability
        }
        
        calculateSampleSizeBinaryMRT_wrapper(p10 = p10(),
                                             pT0 = pT0(),
                                             p11 = p11(),
                                             pT1 = pT1(),
                                             total_T = total_decision_points(),
                                             alpha_shape = input$alpha_choices,
                                             beta_shape = input$beta_choices,
                                             rand_prob = rand_prob,  ## p_t
                                             avail_pattern = avail_input(), ## E[I_t]  # TQ: will assume this is vector of length T
                                             typeIerror = input$sig_level,
                                             power = input$power)
        
        
    })
    
    output$sample_size <- renderUI({
        if (sample_size() > 10) {
            HTML(paste("<h4 style = 'color:blue';> The required sample size is ",
                       sample_size(), 
                       "to attain", 
                       input$power*100,
                       "% power when the significance level is",input$sig_level,".")) 
        } else {
            ### if the calculated sample size is less than 10, we won't output the exact sample size ###
            HTML(paste("<h4 style = 'color:blue';> The required sample size is less than or equal to 10 to attain", 
                       input$power*100,
                       "% power when the significance level is",
                       input$sig_level,
                       ". Please refer to the result section in the left column for suggestions.")) 
        }
    })
    
    
    ##### Calculate Power #####
    
    power <- eventReactive(input$button_calculate_power, {
        # The determination of randomization probability is not well-implemented.
        # Need to think more carefully, because there are three sources of rand. prob.
        rand_prob <- rep(input$rand_prob_const, total_decision_points())
        if (!is.null(input$file1)) {
            rand_prob <- rep(P_inter_days()$Randomization.Probability, 
                             each = input$occ_per_day)
        }
        if (!is.null(input$file2)) {
            rand_prob <- P_inter_dec()$Randomization.Probability
        }
        
        
        calculatePowerBinaryMRT_wrapper(p10 = p10(),
                                        pT0 = pT0(),
                                        p11 = p11(),
                                        pT1 = pT1(),
                                        total_T = total_decision_points(),
                                        alpha_shape = input$alpha_choices,
                                        beta_shape = input$beta_choices,
                                        rand_prob = rand_prob,  ## p_t
                                        avail_pattern = avail_input(), ## E[I_t]  # TQ: will assume this is vector of length T
                                        typeIerror = input$sig_level,
                                        sample_size = input$sample_size)
    })
    
    output$power <- renderUI({
        if (power() >= 0.4) {
            HTML(paste("<h4 style = 'color:blue';> The power we get is ", 
                       round(power(), 3)*100,
                       "% with sample size", 
                       input$sample_size,
                       "when the significance level is",
                       input$sig_level,"."))
        } else {
            ### If the calculated power is less than 40% ###
            HTML(paste("<h4 style = 'color:blue';> The power we get is less than 40% with sample size", 
                       input$sample_size, 
                       "when the significance level is",
                       input$sig_level,"."))
        }
    })
    
    
    ### Create history table for power calculation and sample size calculation
    
    sample_size_history <- reactiveValues(avail_pattern = c(), 
                                          avail_init = c(), 
                                          avail_final = c(),
                                          rand_prob = c(),
                                          alpha_shape = c(), 
                                          p10 = c(), 
                                          pT0 = c(),
                                          beta_shape = c(), 
                                          p11 = c(), 
                                          pT1 = c(),
                                          sample_size = c(),
                                          power = c(),
                                          sig_level = c())
    
    observeEvent(input$button_calculate_sample_size, {
        sample_size_history$avail_pattern <- c(sample_size_history$avail_pattern, 
                                               input$avail_choices)
        
        sample_size_history$avail_init <- c(sample_size_history$avail_init, 
                                            avail_input()[1])
        
        sample_size_history$avail_final <- c(sample_size_history$avail_final, 
                                             avail_input()[total_decision_points()])
        
        sample_size_history$rand_prob <- c(sample_size_history$rand_prob, 
                                           input$rand_prob_const)
        
        sample_size_history$alpha_shape <- c(sample_size_history$alpha_shape, 
                                             input$alpha_choices)
        
        sample_size_history$p10 <- c(sample_size_history$p10, p10())
        
        sample_size_history$pT0 <- c(sample_size_history$pT0, pT0())
        
        sample_size_history$beta_shape <- c(sample_size_history$beta_shape, 
                                            input$beta_choices)
        
        sample_size_history$p11 <- c(sample_size_history$p11, p11())
        
        sample_size_history$pT1 <- c(sample_size_history$pT1, pT1())
        
        sample_size_history$sample_size <- c(sample_size_history$sample_size, 
                                             sample_size())
        
        sample_size_history$power <- c(sample_size_history$power, 
                                       input$power)
        
        sample_size_history$sig_level <- c(sample_size_history$sig_level, 
                                           input$sig_level)
    })
    
    output$sample_size_history_table <- renderTable({
        data.frame("Sample Size" = sample_size_history$sample_size,
                   "Power" = sample_size_history$power,
                   "Sig Level" = sample_size_history$sig_level,
                   "Rand Prob" = sample_size_history$rand_prob,
                   "Succ Prob No Trt Shape" = sample_size_history$alpha_shape,
                   "Trt Eff Shape" = sample_size_history$beta_shape,
                   "p10" = sample_size_history$p10,
                   "pT0" = sample_size_history$pT0,
                   "p11" = sample_size_history$p11,
                   "pT1" = sample_size_history$pT1,
                   "Avail Pattern" = sample_size_history$avail_pattern,
                   "Avail Init" = sample_size_history$avail_init,
                   "Avail Final" = sample_size_history$avail_final)
    })
    
    power_history <- reactiveValues(avail_pattern = c(), 
                                    avail_init = c(), 
                                    avail_final = c(),
                                    rand_prob = c(),
                                    alpha_shape = c(), 
                                    p10 = c(), 
                                    pT0 = c(),
                                    beta_shape = c(), 
                                    p11 = c(), 
                                    pT1 = c(),
                                    sample_size = c(),
                                    power = c(),
                                    sig_level = c())

    observeEvent(input$button_calculate_power, {
        power_history$avail_pattern <- c(power_history$avail_pattern, 
                                         input$avail_choices)
        power_history$avail_init <- c(power_history$avail_init, 
                                      avail_input()[1])
        power_history$avail_final <- c(power_history$avail_final, 
                                       avail_input()[total_decision_points()])
        power_history$rand_prob <- c(power_history$rand_prob, 
                                     input$rand_prob_const)
        power_history$alpha_shape <- c(power_history$alpha_shape, 
                                       input$alpha_choices)
        power_history$p10 <- c(power_history$p10, p10())
        power_history$pT0 <- c(power_history$pT0, pT0())
        power_history$beta_shape <- c(power_history$beta_shape, 
                                      input$beta_choices)
        power_history$p11 <- c(power_history$p11, p11())
        power_history$pT1 <- c(power_history$pT1, pT1())
        power_history$sample_size <- c(power_history$sample_size, 
                                       input$sample_size)
        power_history$power <- c(power_history$power, power())
        power_history$sig_level <- c(power_history$sig_level, 
                                     input$sig_level)
    })
    
    output$power_history_table <- renderTable({
        data.frame("Sample Size" = power_history$sample_size,
                   "Power" = power_history$power,
                   "Sig Level" = power_history$sig_level,
                   "Rand Prob" = power_history$rand_prob,
                   "Succ Prob No Trt Shape" = power_history$alpha_shape,
                   "Trt Eff Shape" = power_history$beta_shape,
                   "p10" = power_history$p10,
                   "pT0" = power_history$pT0,
                   "p11" = power_history$p11,
                   "pT1" = power_history$pT1,
                   "Avail Pattern" = power_history$avail_pattern,
                   "Avail Init" = power_history$avail_init,
                   "Avail Final" = power_history$avail_final)
    })
})
