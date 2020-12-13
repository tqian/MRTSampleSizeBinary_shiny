library(shiny)
library(DT)
source("sample_size_calc_bin_mrt.R")

shinyServer(function(input,output,session){
    
    rv <- reactiveValues(data=NULL)
    
    rv$ss_clicked <- FALSE

    rv$power_clicked <- FALSE
    
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
        
        ceiling(input$days * input$occ_per_day)
    })
    
    ### Reading the file for time-varying randomization probability###
    
    #### Reading the file of decision times ###
    P_inter_dec <- reactive({    
        
        inFile <- input$file2
        
        if (is.null(inFile))
            return(NULL)
        
        read.csv(inFile$datapath, header = TRUE, sep = ',')
    })
    
    #### Output the first five rows of the table reading from the file with respect to decision times 
    ### and output warnings if the format of the file is not correct
    output$P_inter_table_dec <- renderDataTable({      
        print("p inter dec table")
        print(P_inter_dec())
        delta <- as.vector(P_inter_dec()$Randomization.Probability)
        validate(
            need(!is.null(input$file2), "Warning: No file is uploaded"),
            need(is.null(input$file2) || length(delta) == input$days * input$occ_per_day, 
                 "Error: the number of decision times doesn't match."),
            need(is.null(input$file2) || max(delta) <= 1, 
                 "Error: some value of randomization prob
                 ability is bigger than 1"),
            need(is.null(input$file2) || min(delta) >= 0, 
                 "Error: some value of randomization probability is less than 0")
        )
        head(P_inter_dec(), n = 5)
    })
    
    ### Reading the file with respect to days ###
    P_inter_days <- reactive({    
        

        
        inFile <- input$file1
        
        if (is.null(inFile)) {
            return(NULL)
        }
        
        read.csv(inFile$datapath, header = TRUE)
    })
    
    #### Output the first five rows of the table reading from the file with respect to days
    ### and output warnings if the format of the file is not correct
    #### Output the first five rows of the table reading from the file for days
    output$P_inter_table_days <- renderDataTable({     
        print("p inter days table")
        delta <- as.vector(P_inter_days()$Randomization.Probability)
        print(P_inter_days())
        validate(
            need(!is.null(input$file1), "Warning: No file is uploaded"),
            need(is.null(input$file1) || length(delta) == input$days , 
                 "Error: the number of days doesn't match."),
            need(is.null(input$file1) || max(delta) <= 1, 
                 "Error: some value of randomization probability is bigger than 1"),
            need(is.null(input$file1) || min(delta) >= 0, 
                 "Error: some value of randomization probability is less than 0")
        )
        
        head(P_inter_days(), n = 5)
    })
    
    
    # trying this outside
    rand_prob <- reactive({
    
        if (input$rand_prob_choices == "constant"){
            rand_prob <- rep(input$rand_prob_const, total_decision_points())
            rv$rp_shape <- "constant"
        } else if (input$rand_prob_choices == "tv_days") {
            rand_prob <- rep(P_inter_days()$Randomization.Probability, 
                         each = input$occ_per_day)
            rv$rp_shape <- "time-varying"
        } else if (input$rand_prob_choices == "tv_dec_pts") {
            rand_prob <- P_inter_dec()$Randomization.Probability
            rv$rp_shape <- "time-varying"
        }
        
        rand_prob
    })
    
    #### Templates for randomization probability #####
    
    days_df <- reactive({
        col_names <- c("Days", "Randomization Probability")
        temp_df <- data.frame(cbind(1:input$days, rep(0.4, input$days)))
        colnames(temp_df) <- col_names
        temp_df
    })
    
    
    output$days_template <- downloadHandler(
        filename = function() {
            paste0("rand_prob_", input$days, "_days.csv")
        },
        content = function(file){
            write.csv(days_df(), file, row.names=FALSE)
        }
    )
    
    dec_pts_df <- reactive({
        col_names <- c("Dec Times", "Randomization Probability")
        dec_pts <- ceiling(input$days * input$occ_per_day)
        temp_df <- data.frame(cbind(1:dec_pts, rep(0.4, dec_pts)))
        colnames(temp_df) <- col_names
        temp_df
    })
    
    output$dec_pts_template <- downloadHandler(
        filename = function() {
            paste0("rand_prob_", 
                   round(input$days*input$occ_per_day), 
                   "_dec_pts_over_",
                   input$days,
                   "days.csv")
        },
        content = function(file){
            write.csv(dec_pts_df(), file, row.names=FALSE)
        }
    )
    
    
    #### Calculating baseline success probability ####
    
    a_mat <- reactive({
        # Initialize some value to avoid some internal error when running locally
        # This part should have not effect on the UI.
        result <- 0.5
        if (input$alpha_choices == "constant") {
            print('const amat')
            validate(
                need(input$alpha_constant_mean > 0, 
                     "Error: Please specify the baseline success probability greater than 0")
            )
            
            #result <- rep(input$alpha_constant_mean, total_decision_points())
            
            result <- matrix(log(input$alpha_constant_mean))
            
        } else if (input$alpha_choices == "loglinear") {
            print("loglinear amat")
            validate(
                need(input$alpha_loglinear_initial > 0, 
                     "Error: Please specify the initial value of baseline success probability greater than 0"),
                need(input$alpha_loglinear_final > 0, 
                     "Error: Please specify the final value of baseline success probability greater than 0")
            )
            
            initial_log <- log(input$alpha_loglinear_initial)
            final_log <- log(input$alpha_loglinear_final)
           # result_log <- seq(from = initial_log, 
        #                      to = final_log, 
          #                    length.out = total_decision_points())
            
            slope <- (final_log - initial_log) / total_decision_points() 
            
            result <- matrix(c(initial_log - slope, slope ), ncol=1)
            
            # result <- exp(result_log)
        } else if (input$alpha_choices == "logquadratic"){
            print("logquad amat")
           validate(
                need(input$alpha_logquad_initial > 0, 
                     "Error: Please specify the initial value of baseline success probability greater than 0"),
                need(input$alpha_logquad_change_val > 0, 
                     "Error: Please specify the final value of baseline success probability greater than 0"),
                need(input$alpha_logquad_change_pt > 0, 
                     "Error: Please specify the final value of baseline success probability greater than 0")
            )
           print('pass validation lq')
            
            k1 <- log(input$alpha_logquad_initial)
            k2 <- input$alpha_logquad_change_pt
            k3 <- log(input$alpha_logquad_change_val)
            
            validate(need(k2 != 0, "Error: Change point cannot be starting point"))
            
            a2 <- (2 * k1 * k2 - 2 * k3 * k2) / (2 * k2 * (1 - k2/2) - 1)
            a1 <- k1 - k3 - (1 - k2/2) * a2
            a3 <- k1 - a1 - a2
            
            #t_mat <- cbind(rep(1, times=total_decision_points()),
             #              1:total_decision_points(),
            #               (1:total_decision_points())^2)
            
            a_mat <- as.matrix(c(a1, a2, a3), ncol=1)
            
            #result <- exp(t_mat %*% a_mat) 
            #print(result)
            #print(a_mat)
            
            result <- a_mat
            print('exit lq')
        }
        # validate(
        #     need(min(result) > 0,
        #          "Warning: Some values of baseline success probability are less than or equal to 0"),
        #     need(max(result) <= 1,
        #          "Warning: Some values of baseline success probability are greater than 1")
        # )
        result
    })
    
    g_t <- reactive({
      #  if(!is.null(a_mat())) {
        print('in gt')
            if (input$alpha_choices == "constant") {
                
                t_mat <- as.matrix(rep(1, times = total_decision_points()))
                return(t_mat)
            } else if (input$alpha_choices == "loglinear"){
                
                t_mat <- as.matrix(cbind(rep(1, times = total_decision_points()),
                               1:total_decision_points()))
                return(t_mat)
            
            } else if (input$alpha_choices == "logquadratic"){
                print(total_decision_points())
                print(rep(1, times=total_decision_points()))
                print(1:total_decision_points())
                print((1:total_decision_points())^2)
                
                one <- rep(1, times = total_decision_points())
                two <- 1:total_decision_points()
                three <- two^2
                t_mat <- as.matrix(cbind(one, two, three))
                
                print(t_mat)

                
                return(t_mat)
            }
            
            
        #}
    })
    
    alpha_input <- reactive({
        print('alpha input')
        print(g_t())
        print(a_mat())
        if(!is.null(a_mat()) && !is.null(g_t())) {
            print("amat and gt are fine")
            return(exp(g_t() %*% a_mat()))
        } else {
            return(.5)
        }
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

    f_t <- reactive({
        #if(!is.null(b_mat())) {
            if (input$beta_choices == "constant") {
                
                t_mat <- as.matrix(rep(1, times = total_decision_points()))
                return(t_mat)
            } else if (input$beta_choices == "loglinear"){
                
                t_mat <- as.matrix(cbind(rep(1, times = total_decision_points()),
                               1:total_decision_points()))
                return(t_mat)
                
            } else if (input$beta_choices == "logquadratic"){
                one <- rep(1, times = total_decision_points())
                two <- 1:total_decision_points()
                three <- two^2
                t_mat <- as.matrix(cbind(one, two, three))

                return(t_mat)
            }
            

       # }
    })
    
    
    
    b_mat <- reactive({
        if (!is.null(alpha_input())){
            print('bet input not alpha null')
            print(alpha_input())
        # Initialize some value to avoid some internal error when running locally
        # This part should have not effect on the UI.
        result <- 1
        if (input$beta_choices == "constant") {
            
            validate(
                need(input$beta_constant_mean > 0, 
                     "Error: Please specify the proximal treatment effect greater than 0")
            )
            
            #result <- rep(input$beta_constant_mean, total_decision_points())
            
            result <- matrix(c(log(input$beta_constant_mean)), ncol=1)
        
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
            
            #result <- exp(result_log)
            
            slope <- (final_log - initial_log)/total_decision_points()
            
            result <- matrix(c(initial_log - slope, slope), ncol=1)
            
        } else if (input$beta_choices == "logquadratic"){
            
            print('beta quad')
            validate(
                need(input$beta_logquad_initial > 0, 
                     "Error: Please specify the initial value of baseline success probability greater than 0"),
                need(input$beta_logquad_change_val > 0, 
                     "Error: Please specify the final value of baseline success probability greater than 0"),
                need(input$beta_logquad_change_pt > 0, 
                     "Error: Please specify the final value of baseline success probability greater than 0")
            )
            
            print('beta put')
            
            k1 <- log(input$beta_logquad_initial)
            k2 <- input$beta_logquad_change_pt
            k3 <- log(input$beta_logquad_change_val)
            
            validate(need(k2 != 0, "Error: Change point cannot be starting point"))
            
            b2 <- (2 * k1 * k2 - 2 * k3 * k2) / (2 * k2 * (1 - k2/2) - 1)
            b1 <- k1 - k3 - (1 - k2/2) * b2
            b3 <- k1 - b1 - b2
            
            t_mat <- cbind(rep(1, times=total_decision_points()),
                           1:total_decision_points(),
                           (1:total_decision_points())^2)
            
            b_mat <- as.matrix(c(b1, b2, b3), ncol=1)
            
            result <- exp(t_mat %*% b_mat) 
            print(result)
            print(b_mat)
            
            result <- b_mat
        }
        # validate(
        #     need(min(result * alpha_input()) > 0,
        #          "Warning: Some values of success probability are less than or equal to 0"),
        #     need(max(result * alpha_input()) <= 1,
        #          "Warning: Some values of success probability are greater than 1")
        # )
        result
        }
    })
    
    beta_input <- reactive({
        if(!is.null(b_mat()) && !is.null(f_t())){
            return(exp(f_t() %*% b_mat()))
        }
    })
    
    ### plot of the graphs for the proximal treatment effect ###
    
    output$beta_graph <- renderPlot({
        if(!is.null(alpha_input()) && !is.null(beta_input())){
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
        }
    })
    
    
    ### p10, p11, pT0, pT1 ###
    
    p10 <- reactive({NULL}) #reactive({alpha_input()[1]})
    
    pT0 <- reactive({NULL}) #reactive({alpha_input()[total_decision_points()]})
    
    p11 <- reactive({NULL}) #reactive({alpha_input()[1] * beta_input()[1]})
    
    pT1 <- reactive({NULL}) #reactive({alpha_input()[total_decision_points()] * beta_input()[total_decision_points()]})
    
    #### Expected Availability ####
    
    avail_input <- reactive({
        # Initialize some value to avoid some internal error when running locally
        # This part should have not effect on the UI.
        result <- 0.5

        if (input$avail_choices == "constant") {
            
            validate(
                need(input$avail_constant_mean > 0, 
                     "Error: Please specify the average availability greater than 0")
            )
            
            result <- rep(input$avail_constant_mean, total_decision_points())
            
            rv$ea_shape <- "constant"
            
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
            
            rv$ea_shape <- "linear"
            
        } else if (input$avail_choices == "tv_days") {
            print('hello days')
            print(ea_inter_days())
            validate(need(!is.null(ea_inter_days()),
                     "Error: No file uploaded."))
            print('pass validation')
            result <- rep(ea_inter_days()$Expected.Availability, 
                          each = input$occ_per_day)
            

            print(length(result))
            print(total_decision_points())
            validate(need(length(result)==total_decision_points(),
                     "Error: Number of days does not match"))
            
            rv$ea_shape <- "time-varying"
            
            
        } else if (input$avail_choices == "tv_dec_pts") {
            print('hello dec pts')
            validate(need(!is.null(ea_inter_dec()), "Error: No file uploaded"),
                     need("Expected.Availability" %in% colnames(ea_inter_dec()), 
                          "Error: No column of expected availability. See template")) 
            result <- ea_inter_dec()$Expected.Availability
            print('midway dec pts')
            validate(need(length(result)==total_decision_points()),
                     "Error: Number of decision points does not match")
            
            rv$ea_shape <- "time-varying"
            print('bye dec pts')
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
        validate(need(!(is.null(avail_input())), "Error: No availability input"))
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
    
    
    
    
    ### Reading the file with respect to days for expected availability ###
    ea_inter_days <- reactive({     
        
        inFile <- input$file0
        
    
        if (is.null(inFile)) {
            return(NULL)
        }
        print('ea inter days')
        print('file0a')
        print(input$file0a)
        print('file0')
        print(input$file0)

        print('ea inter dec')
        print(ea_inter_dec())
        print(input$file0)
        read.csv(inFile$datapath, header = TRUE)
        
    
    })
    
    ea_inter_dec <- reactive({     
        
        inFile <- input$file0a
        #inFile <- input$file0a
        
        if (is.null(inFile)) {
            return(NULL)
        }
        print('ea inter ddec')
        
        
        read.csv(inFile$datapath, header = TRUE)
        
        
    })
    

    
    
    #### Output the first five rows of the table reading from the file with respect to days
    ### and output warnings if the format of the file is not correct
    #### Output the first five rows of the table reading from the file for days
    output$ea_inter_table_days <- renderDataTable({      
        delta <- as.vector(ea_inter_days()$Expected.Availability)
        print('ea inter table days')
        print(ea_inter_days())
        validate(
            need(!is.null(input$file0), "Warning: No file is uploaded"),
            need(is.null(input$file0) || "Expected.Availability" %in% colnames(ea_inter_days()),
                 "Error: need a column titled 'Expected Availability'; see template"),
            need(is.null(input$file0) || length(delta) == input$days , 
                 "Error: the number of days doesn't match."),
            need(is.null(input$file0) || max(delta) <= 1, 
                 "Error: some value of expected availability is bigger than 1"),
            need(is.null(input$file0) || min(delta) >= 0, 
                 "Error: some value of expected availability is less than 0")
        )
        print('exit ea inter table days')
        head(ea_inter_days(), n = 5)
    })
    
    output$ea_inter_table_dec <- renderDataTable({    
        print("ea inter table dec")
        delta <- as.vector(ea_inter_dec()$Expected.Availability)
        validate(
            need(!is.null(input$file0a), "Warning: No file is uploaded"),
            need(is.null(input$file0a) || "Expected.Availability" %in% colnames(ea_inter_dec()),
                 "Error: need a column titled 'Expected Availability'; see template"),
            need(is.null(input$file0a) || length(delta) == input$days * input$occ_per_day, 
                 "Error: the number of decision times doesn't match."),
            need(is.null(input$file0a) || max(delta) <= 1, 
                 "Error: some value of expected availability is bigger than 1"),
            need(is.null(input$file0a) || min(delta) >= 0, 
                 "Error: some value of expected availability is less than 0")
        )
        head(ea_inter_dec(), n = 5)
    })
    
    ### expected availability templates ###
    ea_days_df <- reactive({
        col_names <- c("Days", "Expected Availability")
        temp_df <- data.frame(cbind(1:input$days, rep(0.7, input$days)))
        colnames(temp_df) <- col_names
        temp_df
    })
    
    
    output$ea_days_template <- downloadHandler(
        filename = function() {
            paste0("exp_avail_", input$days, "_days.csv")
        },
        content = function(file){
            write.csv(ea_days_df(), file, row.names=FALSE)
        }
    )
    
    
    ea_dec_pts_df <- reactive({
        col_names <- c("Dec Times", "Expected Availability")
        dec_pts <- ceiling(input$days * input$occ_per_day)
        temp_df <- data.frame(cbind(1:dec_pts, rep(0.7, dec_pts)))
        colnames(temp_df) <- col_names
        temp_df
    })
    
    output$ea_dec_pts_template <- downloadHandler(
        filename = function() {
            paste0("exp_avail_", 
                   round(input$days*input$occ_per_day), 
                   "_dec_pts_over_",
                   input$days,
                   "days.csv")
        },
        content = function(file){
            write.csv(ea_dec_pts_df(), file, row.names=FALSE)
        }
    )
    

    
    
    ##### Calculate Sample Size #####
    
    sample_size <- eventReactive(input$button_calculate_sample_size, {
        # The determination of randomization probability is not well-implemented.
        # Need to think more carefully, because there are three sources of rand. prob.
        # rand_prob <- rep(input$rand_prob_const, total_decision_points())
        # 

        
        # if (input$rand_prob_choices == "constant"){
        #     rand_prob <- rep(input$rand_prob_const, total_decision_points())
        #     rv$rp_shape <- "constant"
        # } else if (input$rand_prob_choices == "tv_days") {
        #     rand_prob <- rep(P_inter_days()$Randomization.Probability, 
        #                      each = input$occ_per_day)
        #     rv$rp_shape <- "time-varying"
        # } else if (input$rand_prob_choices == "tv_dec_pts") {
        #     rand_prob <- P_inter_dec()$Randomization.Probability
        #     rv$rp_shape <- "time-varying"
        # }
       
        print(rand_prob()) 
        rv$ss_clicked <- TRUE
        # calculate_mrt_bin_samplesize_wrapper(p10 = p10(),
        #                                      pT0 = pT0(),
        #                                      p11 = p11(),
        #                                      pT1 = pT1(),
        #                                      total_T = total_decision_points(),
        #                                      alpha_shape = input$alpha_choices,
        #                                      beta_shape = input$beta_choices,
        #                                      alpha_input = alpha_input(),
        #                                      beta_input = beta_input(),
        #                                      rand_prob = rand_prob(),  ## p_t
        #                                      avail_pattern = avail_input(), ## E[I_t]  # TQ: will assume this is vector of length T
        #                                      typeIerror = input$sig_level,
        #                                      power = input$power)
        print("ft/gt")
        print(f_t())
        print(g_t())
        calculate_mrt_bin_samplesize_f(avail_pattern = avail_input(),
                                       f_t = f_t(),
                                       g_t = g_t(),
                                       beta = b_mat(),
                                       alpha = a_mat(),
                                       p_t = rand_prob(),
                                       gamma = input$sig_level,
                                       b = 1-input$power)
        
        
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
        
        # if (input$rand_prob_choices == "constant"){
        #     rand_prob <- rep(input$rand_prob_const, total_decision_points())
        #     rv$rp_shape <- "constant"
        # } else if (input$rand_prob_choices == "tv_days") {
        #     rand_prob <- rep(P_inter_days()$Randomization.Probability, 
        #                      each = input$occ_per_day)
        #     rv$rp_shape <- "time-varying"
        # } else if (input$rand_prob_choices == "tv_dec_pts") {
        #     rand_prob <- P_inter_dec()$Randomization.Probability
        #     rv$rp_shape <- "time-varying"
        # }
        # 
        
        # 
        # calculate_mrt_bin_power_wrapper(p10 = p10(),
        #                                 pT0 = pT0(),
        #                                 p11 = p11(),
        #                                 pT1 = pT1(),
        #                                 total_T = total_decision_points(),
        #                                 alpha_shape = input$alpha_choices,
        #                                 beta_shape = input$beta_choices,
        #                                 rand_prob = rand_prob(),  ## p_t
        #                                 avail_pattern = avail_input(), ## E[I_t]  # TQ: will assume this is vector of length T
        #                                 typeIerror = input$sig_level,
        #                                 sample_size = input$sample_size)
        calculate_mrt_bin_power_f(avail_pattern = avail_input(),
                                  f_t = f_t(),
                                  g_t = g_t(),
                                  beta = b_mat(),
                                  alpha = a_mat(),
                                  p_t = rand_prob(),
                                  gamma = input$sig_level,
                                  n = input$sample_size)
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
    
    # sample_size_history <- reactiveValues(avail_pattern = c(), 
    #                                       avail_init = c(), 
    #                                       avail_final = c(),
    #                                       rand_prob_shape = c(),
    #                                       alpha_shape = c(), 
    #                                       p10 = c(), 
    #                                       pT0 = c(),
    #                                       beta_shape = c(), 
    #                                       p11 = c(), 
    #                                       pT1 = c(),
    #                                       sample_size = c(),
    #                                       power = c(),
    #                                       sig_level = c(),
    #                                       tot_dec_pts = c())
    
    sample_size_history <- reactiveValues(data=NULL)
    
    observeEvent(input$button_calculate_sample_size, {
        sample_size_history$avail_pattern <- c(sample_size_history$avail_pattern, 
                                               input$avail_choices)
        
        sample_size_history$avail_init <- c(sample_size_history$avail_init, 
                                            avail_input()[1])
        
        sample_size_history$avail_final <- c(sample_size_history$avail_final, 
                                             avail_input()[total_decision_points()])
        
        sample_size_history$rand_prob_shape <- c(sample_size_history$rand_prob_shape, 
                                           rv$rp_shape)
        
        sample_size_history$alpha_shape <- c(sample_size_history$alpha_shape, 
                                             input$alpha_choices)
        
        #sample_size_history$p10 <- c(sample_size_history$p10, p10())
        
        #sample_size_history$pT0 <- c(sample_size_history$pT0, pT0())
        
        sample_size_history$beta_shape <- c(sample_size_history$beta_shape, 
                                            input$beta_choices)
        
        #sample_size_history$p11 <- c(sample_size_history$p11, p11())
        
        #sample_size_history$pT1 <- c(sample_size_history$pT1, pT1())
        
        sample_size_history$sample_size <- c(sample_size_history$sample_size, 
                                             sample_size())
        
        sample_size_history$power <- c(sample_size_history$power, 
                                       input$power)
        
        sample_size_history$sig_level <- c(sample_size_history$sig_level, 
                                           input$sig_level)
        
        sample_size_history$tot_dec_pts <- c(sample_size_history$tot_dec_pts,
                                             total_decision_points())
    })
    
   # ss_hist_tab <- renderDataTable({
    #ss_hist_tab <- renderDataTable({
        samp_size_hist <- reactive({data.frame(
            "Sample Size" = sample_size_history$sample_size,
                   "Power" = sample_size_history$power,
                   "Sig Level" = sample_size_history$sig_level,
                   "Rand Prob Shape" = sample_size_history$rand_prob_shape,
                   "Total Dec Pts" = sample_size_history$tot_dec_pts,
                   "Succ Prob No Trt Shape" = sample_size_history$alpha_shape,
                   "Trt Eff Shape" = sample_size_history$beta_shape,
                   #"p10" = sample_size_history$p10,
                   #"pT0" = sample_size_history$pT0,
                   #"p11" = sample_size_history$p11,
                   #"pT1" = sample_size_history$pT1,
                   "Avail Pattern" = sample_size_history$avail_pattern,
                   "Avail Init" = sample_size_history$avail_init,
                   "Avail Final" = sample_size_history$avail_final)})
        #ssht
    #})
   

    output$sample_size_history_table <- renderDataTable({samp_size_hist()})
    

    
    output$samp_size_dl <- downloadHandler(
        filename = function() {
            paste0("sample_size_history.csv")
        },
        content = function(file){
            write.csv(samp_size_hist(), file, row.names=FALSE)
        }
    )
    
    output$download_ss <- renderUI({
        if(rv$ss_clicked) {
            downloadButton('samp_size_dl', 'Sample Size History')
        }
    })
    
    #output$sample_size_history_table <- renderDataTable({ss_hist_tab()})

    
    
    
    
    ###### Power vs Sample Size plot #####
    pow_vs_n_plot1 <- eventReactive(input$button_calculate_sample_size, {
        # The determination of randomization probability is not well-implemented.
        # Need to think more carefully, because there are three sources of rand. prob.
        
        rv$ss_clicked <- TRUE
        # 
        # if (input$rand_prob_choices == "constant"){
        #     rand_prob <- rep(input$rand_prob_const, total_decision_points())
        #     rv$rp_shape <- "constant"
        # } else if (input$rand_prob_choices == "tv_days") {
        #     rand_prob <- rep(P_inter_days()$Randomization.Probability, 
        #                      each = input$occ_per_day)
        #     rv$rp_shape <- "time-varying"
        # } else if (input$rand_prob_choices == "tv_dec_pts") {
        #     rand_prob <- P_inter_dec()$Randomization.Probability
        #     rv$rp_shape <- "time-varying"
        # }
        

        
        
        # power_vs_n_plot_wrapper(p10 = p10(),
        #                         pT0 = pT0(),
        #                         p11 = p11(),
        #                         pT1 = pT1(),
        #                         total_T = total_decision_points(),
        #                         alpha_shape = input$alpha_choices,
        #                         beta_shape = input$beta_choices,
        #                         rand_prob = rand_prob(),  ## p_t
        #                         avail_pattern = avail_input(), ## E[I_t]  # TQ: will assume this is vector of length T
        #                         typeIerror = input$sig_level)
        
        power_vs_n_plot(avail_pattern = avail_input(),
                                       f_t = f_t(),
                                       g_t = g_t(),
                                       beta = b_mat(),
                                       alpha = a_mat(),
                                       p_t = rand_prob(),
                                       gamma = input$sig_level)
        
    })    
    
    
    output$power_vs_n1 <- renderPlot({
        pow_vs_n_plot1()
    })
    
    pow_vs_n_plot2 <- eventReactive(input$button_calculate_power, {
        # The determination of randomization probability is not well-implemented.
        # Need to think more carefully, because there are three sources of rand. prob.
        rv$ss_clicked <- TRUE        
        # if (input$rand_prob_choices == "constant"){
        #     rand_prob <- rep(input$rand_prob_const, total_decision_points())
        #     rv$rp_shape <- "constant"
        # } else if (input$rand_prob_choices == "tv_days") {
        #     rand_prob <- rep(P_inter_days()$Randomization.Probability, 
        #                      each = input$occ_per_day)
        #     rv$rp_shape <- "time-varying"
        # } else if (input$rand_prob_choices == "tv_dec_pts") {
        #     rand_prob <- P_inter_dec()$Randomization.Probability
        #     rv$rp_shape <- "time-varying"
        # }
        
        
        # power_vs_n_plot_wrapper(p10 = p10(),
        #                         pT0 = pT0(),
        #                         p11 = p11(),
        #                         pT1 = pT1(),
        #                         total_T = total_decision_points(),
        #                         alpha_shape = input$alpha_choices,
        #                         beta_shape = input$beta_choices,
        #                         rand_prob = rand_prob(),  ## p_t
        #                         avail_pattern = avail_input(), ## E[I_t]  # TQ: will assume this is vector of length T
        #                         typeIerror = input$sig_level)  
        
        power_vs_n_plot(avail_pattern = avail_input(),
                        f_t = f_t(),
                        g_t = g_t(),
                        beta = b_mat(),
                        alpha = a_mat(),
                        p_t = rand_prob(),
                        gamma = input$sig_level)
    })    
    
    output$power_vs_n2 <- renderPlot({
        pow_vs_n_plot2()
    })
    
    #### Power Summary ######
    pow_summary1 <- eventReactive(input$button_calculate_sample_size, {
        # The determination of randomization probability is not well-implemented.
        # Need to think more carefully, because there are three sources of rand. prob.
        rv$power_clicked <- TRUE
        # if (input$rand_prob_choices == "constant"){
        #     rand_prob <- rep(input$rand_prob_const, total_decision_points())
        #     rv$rp_shape <- "constant"
        # } else if (input$rand_prob_choices == "tv_days") {
        #     rand_prob <- rep(P_inter_days()$Randomization.Probability, 
        #                      each = input$occ_per_day)
        #     rv$rp_shape <- "time-varying"
        # } else if (input$rand_prob_choices == "tv_dec_pts") {
        #     rand_prob <- P_inter_dec()$Randomization.Probability
        #     rv$rp_shape <- "time-varying"
        # }
        
        # 
        # psw_out <-power_summary_wrapper(p10 = p10(),
        #                                 pT0 = pT0(),
        #                                 p11 = p11(),
        #                                 pT1 = pT1(),
        #                                 total_T = total_decision_points(),
        #                                 alpha_shape = input$alpha_choices,
        #                                 beta_shape = input$beta_choices,
        #                                 rand_prob = rand_prob(),  ## p_t
        #                                 avail_pattern = avail_input(), ## E[I_t]  # TQ: will assume this is vector of length T
        #                                 typeIerror = input$sig_level)  
        # 
        #return(psw_out)
        power_summary(avail_pattern = avail_input(),
                        f_t = f_t(),
                        g_t = g_t(),
                        beta = b_mat(),
                        alpha = a_mat(),
                        p_t = rand_prob(),
                        gamma = input$sig_level)
    })  
    
    output$power_summary1 <- DT::renderDataTable({ pow_summary1() }) 
    
    
    pow_summary2 <- eventReactive(input$button_calculate_power, {
        # The determination of randomization probability is not well-implemented.
        # Need to think more carefully, because there are three sources of rand. prob.
        
        rv$power_clicked <- TRUE
        
        # if (input$rand_prob_choices == "constant"){
        #     rand_prob <- rep(input$rand_prob_const, total_decision_points())
        #     rv$rp_shape <- "constant"
        # } else if (input$rand_prob_choices == "tv_days") {
        #     rand_prob <- rep(P_inter_days()$Randomization.Probability, 
        #                      each = input$occ_per_day)
        #     rv$rp_shape <- "time-varying"
        # } else if (input$rand_prob_choices == "tv_dec_pts") {
        #     rand_prob <- P_inter_dec()$Randomization.Probability
        #     rv$rp_shape <- "time-varying"
        # }
        # 
        
        # power_summary_wrapper(p10 = p10(),
        #                       pT0 = pT0(),
        #                       p11 = p11(),
        #                       pT1 = pT1(),
        #                       total_T = total_decision_points(),
        #                       alpha_shape = input$alpha_choices,
        #                       beta_shape = input$beta_choices,
        #                       rand_prob = rand_prob(),  ## p_t
        #                       avail_pattern = avail_input(), ## E[I_t]  # TQ: will assume this is vector of length T
        #                       typeIerror = input$sig_level)  
        
        
        power_summary(avail_pattern = avail_input(),
                      f_t = f_t(),
                      g_t = g_t(),
                      beta = b_mat(),
                      alpha = a_mat(),
                      p_t = rand_prob(),
                      gamma = input$sig_level) 
        
        
    })  
    
    output$power_summary2 <- DT::renderDataTable({pow_summary2()}) 
    
    
    
    power_history <- reactiveValues(avail_pattern = c(), 
                                    avail_init = c(), 
                                    avail_final = c(),
                                    rand_prob_shape = c(),
                                    alpha_shape = c(), 
                                    p10 = c(), 
                                    pT0 = c(),
                                    beta_shape = c(), 
                                    p11 = c(), 
                                    pT1 = c(),
                                    sample_size = c(),
                                    power = c(),
                                    sig_level = c(),
                                    total_dec_pts = c())
    
    observeEvent(input$button_calculate_power, {
        power_history$avail_pattern <- c(power_history$avail_pattern, 
                                         input$avail_choices)
        power_history$avail_init <- c(power_history$avail_init, 
                                      avail_input()[1])
        power_history$avail_final <- c(power_history$avail_final, 
                                       avail_input()[total_decision_points()])
        power_history$alpha_shape <- c(power_history$alpha_shape, 
                                       input$alpha_choices)
        #power_history$p10 <- c(power_history$p10, p10())
        #power_history$pT0 <- c(power_history$pT0, pT0())
        power_history$beta_shape <- c(power_history$beta_shape, 
                                      input$beta_choices)
        #power_history$p11 <- c(power_history$p11, p11())
        #power_history$pT1 <- c(power_history$pT1, pT1())
        power_history$sample_size <- c(power_history$sample_size, 
                                       input$sample_size)
        power_history$power <- c(power_history$power, power())
        power_history$sig_level <- c(power_history$sig_level, 
                                     input$sig_level)
        power_history$rand_prob_shape <- c(power_history$rand_prob_shape, 
                                     rv$rp_shape)
        power_history$total_dec_pts <- c(power_history$total_dec_pts,
                                         total_decision_points())
    })
    
    pow_history_table <- reactive({

        pht <- data.frame("Sample Size" = power_history$sample_size,
                   "Power" = power_history$power,
                   "Sig Level" = power_history$sig_level,
                   "Rand Prob Shape" = power_history$rand_prob_shape,
                   "Succ Prob No Trt Shape" = power_history$alpha_shape,
                   "Trt Eff Shape" = power_history$beta_shape,
                   "Total Dec Pts" = power_history$total_dec_pts,
                  # "p10" = power_history$p10,
                  # "pT0" = power_history$pT0,
                  # "p11" = power_history$p11,
                  # "pT1" = power_history$pT1,
                   "Avail Pattern" = power_history$avail_pattern,
                   "Avail Init" = power_history$avail_init,
                   "Avail Final" = power_history$avail_final)
        

        pht
    })
    
    output$power_history_table <- renderDataTable({pow_history_table()})
    
    output$pow_dl <- downloadHandler(
        filename = function() {
            paste0("power_history.csv")
        },
        content = function(file){
            write.csv(power_history_tab(), file, row.names=FALSE)
        }
    )
    
    output$download_pow <- renderUI({
        if(rv$power_clicked) {
            downloadButton('pow_dl', 'Power History')
        }
    })
    
})
