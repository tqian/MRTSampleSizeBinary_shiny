library(shiny)
library(DT)
library(ggplot2)
library(dplyr)
library(MRTSampleSizeBinary)


shinyServer(function(input,output,session){
    
    rv <- reactiveValues(data=NULL)
    
    # flags for when to display download buttons
    rv$ss_clicked <- FALSE
    rv$power_clicked <- FALSE
    
    # flags to make sure all parameters have settings before buttons clicked

    rv$null_set <- FALSE
    rv$te_set <- FALSE
    
    
    ### Calculate total number of decision points based on # Days and # Decision points per day
    total_decision_points <- reactive({
        
        validate(
            need(input$days == round(input$days),
                 "Error: Please enter integer values for the number of days"),
            
            need(input$days > 0 ,
                 "Error: Please specify the number of days greater than 0"),
            
            need(input$occ_per_day == round(input$occ_per_day),
                 paste0("Error: Please enter integer values for the number of",
                        " decision points per day")),
            
            need(input$occ_per_day > 0 ,
                 paste0("Error: Please specify the number of decision points", 
                        " per day greater than 0"))
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
    

# Randomization probability -----------------------------------------------

    # Output first 5 rows of table reading from file wrt decision times 
    # and output warnings if the format of the file is not correct
    output$P_inter_table_dec <- renderDataTable({      
        validate(need("Randomization.Probability" %in% colnames(P_inter_dec()), 
                      "Error: No column of randomization probability. 
                              See template")) 
        
        delta <- as.vector(P_inter_dec()$Randomization.Probability)
        
        validate(
            need(!is.null(input$file2), "Warning: No file is uploaded"),
            
            need(is.null(input$file2) ||
                     length(delta) == input$days * input$occ_per_day, 
                 "Error: the number of decision times doesn't match."),
            
            need(is.null(input$file2) || max(delta) <= 1, 
                 paste0("Error: some value of randomization probability is",
                        " bigger than 1")),
            
            need(is.null(input$file2) || min(delta) >= 0, 
                 paste0("Error: some value of randomization probability", 
                        " is less than 0"))
        )

        pl <- min(nrow(P_inter_dec()), 5)
        validate(need(pl > 0, "No rows to show."))
        datatable(P_inter_dec(), 
                  options = list(pageLength = pl),
                  colnames = c("Decision Time Point" = 1,
                               "Randomization Probability" = 2),
                  rownames = FALSE)
    })
    
    ### Reading the file with respect to days ###
    P_inter_days <- reactive({    
        

        
        inFile <- input$file1
        
        if (is.null(inFile)) {
            return(NULL)
        }
        
        read.csv(inFile$datapath, header = TRUE)
    })
    
    # Output first 5 rows of table reading from file with respect to days
    # and output warnings if the format of the file is not correct
    # Output the first five rows of the table reading from the file for days
    output$P_inter_table_days <- renderDataTable({
        validate(need("Randomization.Probability" %in% colnames(P_inter_days()), 
                      "Error: No column of randomization probability. 
                              See template")) 
        
        delta <- as.vector(P_inter_days()$Randomization.Probability)
        
        validate(
            need(!is.null(input$file1), "Warning: No file is uploaded"),
            
            need(is.null(input$file1) || length(delta) == input$days , 
                 "Error: the number of days doesn't match."),
            
            need(is.null(input$file1) || max(delta) <= 1, 
                 paste0("Error: some value of randomization probability", 
                        " is bigger than 1")),
            
            need(is.null(input$file1) || min(delta) >= 0, 
                 paste0("Error: some value of randomization probability", 
                        " is less than 0"))
        )
        
        pl <- min(nrow(P_inter_days()), 5)
        
        validate(need(pl > 0, "No rows to show."))
        
        datatable(P_inter_days(), 
                  options = list(pageLength = pl), 
                  colnames = c("Day" = 1,
                               "Randomization Probability" = 2),
                  rownames = FALSE)
    })
    
    
    # vector of randomization probabilities
    rand_prob <- reactive({

        # if (input$rand_prob_choices == "constant"){
        #     
        #     rand_prob <- rep(input$rand_prob_const, total_decision_points())
        #     rv$rp_shape <- "constant"
        # 
        # 
        # } else if (input$rand_prob_choices == "tv_days") {
        #     
        #     rand_prob <- rep(P_inter_days()$Randomization.Probability, 
        #                  each = input$occ_per_day)
        #     rv$rp_shape <- "time-varying"
        #     
        # } else if (input$rand_prob_choices == "tv_dec_pts") {
        #     
        #     rand_prob <- P_inter_dec()$Randomization.Probability
        #     rv$rp_shape <- "time-varying"
        #     
        # }
        rand_prob <- rep(input$rand_prob_const, total_decision_points())
        rv$rp_shape <- "constant"
        rand_prob
    })
    
    #### Templates for csv for randomization probability
    
    days_df <- reactive({
        col_names <- c("Day", "Randomization Probability")
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
        col_names <- c("Dec Time", "Randomization Probability")
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
    
    
# Calculating success probability null curve ------------------------------------------
    # alpha vector
    a_mat <- reactive({
        # Initialize a value to avoid some internal error when running locally
        # This part should have not effect on the UI.
        result <- 0.5
        if (input$alpha_choices == "constant") {
            
            validate(
                need(input$alpha_constant_mean > 0, 
                     paste0("Error: Please specify the success probability under the null", 
                            " to be greater than 0"))
            )
          
            result <- matrix(log(input$alpha_constant_mean))
            
            rv$null_set <- TRUE
            
        } else if (input$alpha_choices == "loglinear") {


            validate(
                need(input$alpha_loglinear_initial > 0, 
                     paste0("Error: Please specify the initial value of", 
                            " the success probability null curve to be greater than 0")),
                
                need(input$alpha_loglinear_initial < 1, 
                     paste0("Error: Please specify the initial value of", 
                            " the success probability null curve to be less than 1")),
                
                need(input$alpha_loglinear_final > 0, 
                     paste0("Error: Please specify the final value of", 
                            " the success probability null curve to be greater than 0")),
                
                need(input$alpha_loglinear_final < 1, 
                     paste0("Error: Please specify the final value of", 
                            " the success probability null curve to be less than 1"))
            )
          cat(file=stderr(), "postcheck", "\n")
            initial_log <- log(input$alpha_loglinear_initial)
            
            final_log <- log(input$alpha_loglinear_final)

            
            slope <- (final_log - initial_log) / total_decision_points() 
            
            result <- matrix(c(initial_log - slope, slope ), ncol=1)

            
            rv$null_set <- TRUE
            
        } else if (input$alpha_choices == "logquadratic"){
           
            validate(
                need(input$alpha_logquad_initial > 0, 
                     paste0("Error: Please specify the initial value of", 
                            " the success probability null curve to be greater than 0")),
                
                need(input$alpha_logquad_initial < 1, 
                     paste0("Error: Please specify the initial value of", 
                            " the success probability null curve to be less than 1")),
                
                need(input$alpha_logquad_change_val > 0, 
                     paste0("Error: Please specify the success probability null curve value", 
                     " at change point to be greater than 0")),
                
                need(input$alpha_logquad_change_val < 1, 
                     paste0("Error: Please specify the success probability null curve value", 
                            " at change point to be less than 1")),
                
                need(input$alpha_logquad_change_pt > 1, 
                     paste0("Error: Please specify the change point location",
                            " (on the scale of decision point) to be greater than 1"))
            )
            
            k1 <- log(input$alpha_logquad_initial)
            k2 <- input$alpha_logquad_change_pt
            k3 <- log(input$alpha_logquad_change_val)
            
            a2 <- (2 * k1 * k2 - 2 * k3 * k2) / (2 * k2 * (1 - k2/2) - 1)
            a1 <- k1 - k3 - (1 - k2/2) * a2
            a3 <- k1 - a1 - a2
            
            a_mat <- as.matrix(c(a3, a2, a1), ncol=1) # intercept in 1st entry

            result <- a_mat
            rv$null_set <- TRUE
        }

        result
    })
    
    # gmatrix
    g_t <- reactive({
            if (input$alpha_choices == "constant") {
                
                t_mat <- as.matrix(rep(1, times = total_decision_points()))
                
                return(t_mat)
            } else if (input$alpha_choices == "loglinear"){
                
                t_mat <- as.matrix(
                            cbind(
                              rep(1, times = total_decision_points()),
                              1:total_decision_points()))
                return(t_mat)
            
            } else if (input$alpha_choices == "logquadratic"){

                
                one <- rep(1, times = total_decision_points())
                two <- 1:total_decision_points()
                three <- two^2
                t_mat <- as.matrix(
                                cbind(one, two, three))
                

                
                return(t_mat)
            }
            
    })
    
    alpha_input <- reactive({

        if(!is.null(a_mat()) & !is.null(g_t())) {
            return(exp(g_t() %*% a_mat()))
        } else {
            return(.5)
        }
    })
    
    ### plot of the graphs for the baseline success probability ###
    
    output$alpha_graph <- renderPlot({

      if (!is.null(alpha_input())){

        validate(need(max(alpha_input()) < 1 & min(alpha_input()) > 0,
                    "Current settings lead to invalid probabilities."))   

        ## ggplot in the works
        y1 = alpha_input()
        x1 = seq(1:length(alpha_input()) )
        df_alpha <- data.frame(x1, y1)
        
        df_alpha<- data.frame(apply(df_alpha, 2, unclass))
        
        ggplot(df_alpha) +
          ggtitle("Success Probability Null Curve") +
          geom_line(aes(y = alpha_input(), 
                        x = seq(1:length(alpha_input()))),
                    size = 1, 
                    color = "deepskyblue3", group = 1) +
          
          xlab("Decision Point") + 
          ylab("Success Probability Under H0") +
          ylim(0,1) +
          theme(axis.text  = element_text(size = 12),
                axis.title = element_text(size = 14)) 
        
          
      }
    })
    
    
# Calculate proximal treatment effect -------------------------------------
    # f matrix
    f_t <- reactive({
            if (input$beta_choices == "constant") {
                
                t_mat <- as.matrix(rep(1, times = total_decision_points()))
                rv$te_set <- TRUE
                
                return(t_mat)
            } else if (input$beta_choices == "loglinear"){
                
                t_mat <- as.matrix(
                            cbind(rep(1, times = total_decision_points()),
                                  1:total_decision_points()))
                rv$te_set <- TRUE
                
                return(t_mat)
                
            } else if (input$beta_choices == "logquadratic"){
                
                one <- rep(1, times = total_decision_points())
                two <- 1:total_decision_points()
                three <- two^2
                
                t_mat <- as.matrix(cbind(one, two, three))
                
                rv$te_set <- TRUE
                
                return(t_mat)
            }
            

    })
    
    
    # beta vector
    b_mat <- reactive({
        if (!is.null(alpha_input())){
        # Initialize a value to avoid some internal error when running locally
        # This part should have no effect on the UI.
        result <- 1
        if (input$beta_choices == "constant") {
            
            validate(
                need(input$beta_constant_mean > 1, 
                     paste0("Error: Please specify the constant proximal treatment", 
                     " effect (relative risk) to be greater than 1 (i.e., a positive treatment effect)"))
            )
            
            
            result <- matrix(c(log(input$beta_constant_mean)), ncol=1)
        
        } else if (input$beta_choices == "loglinear") {
            validate(
                need(input$beta_loglinear_initial > 0, 
                     paste0("Error: Please specify the initial value of", 
                            " the proximal treatment effect to be greater than 0")),
                need(input$beta_loglinear_final > 0, 
                     paste0("Error: Please specify the final value of",
                     " the proximal treatment effect to be greater than 0"))
            )
            
            initial_log <- log(input$beta_loglinear_initial)
            final_log <- log(input$beta_loglinear_final)
            result_log <- seq(from = initial_log, to = final_log, 
                              length.out = total_decision_points())
            
            
            slope <- (final_log - initial_log)/total_decision_points()
            
            result <- matrix(c(initial_log - slope, slope), ncol=1)
            
        } else if (input$beta_choices == "logquadratic"){
            
            validate(
                need(input$beta_logquad_initial > 0, 
                     paste0("Error: Please specify the initial value of", 
                            " the proximal treatment effect to be greater than 0")),
                
                need(input$beta_logquad_change_val > 0, 
                     paste0("Error: Please specify the value of", 
                            " the proximal treatment effect at the change point to be greater than 0")),
                
                need(input$beta_logquad_change_pt > 1, 
                     paste0("Error: Please specify the change point location",
                            " (on the scale of decision point) to be greater than 1"))
            )
            
            
            k1 <- log(input$beta_logquad_initial)
            k2 <- input$beta_logquad_change_pt
            k3 <- log(input$beta_logquad_change_val)
            
            b2 <- (2 * k1 * k2 - 2 * k3 * k2) / (2 * k2 * (1 - k2/2) - 1)
            b1 <- k1 - k3 - (1 - k2/2) * b2
            b3 <- k1 - b1 - b2
            
            b_mat <- as.matrix(c(b3, b2, b1), ncol=1) # intercept in the 1st entry
            
            result <- b_mat
        }

        result
        }
    })
    

    
    ### plot of the graphs for the proximal treatment effect ###
    beta_input <- reactive({
        if(!is.null(b_mat()) & !is.null(f_t())){
            return(exp(f_t() %*% b_mat()))
        }
    })
    
    output$beta_graph <- renderPlot({
    
      if(!is.null(alpha_input()) & !is.null(beta_input())){
        ab_product <- alpha_input() * beta_input()  
          
        validate(need(max(alpha_input()) < 1 & min(alpha_input()) > 0,
                        paste0("Current settings lead to invalid probabilities (greater than 1 or smaller than 0).",
                               " Please adjust the input of success probability null curve and/or",
                               " the proximal treatment effect so that all success probabilities are",
                               " between 0 and 1.")))
          
        validate(need(max(ab_product) < 1 & min(ab_product) > 0,
                      paste0("Current settings lead to invalid probabilities (greater than 1 or smaller than 0).",
                             " Please adjust the input of success probability null curve and/or",
                             " the proximal treatment effect so that all success probabilities are",
                             " between 0 and 1.")))
        
        ## plot set up
        y1 <- alpha_input()
        y2 <- beta_input() * alpha_input()
        x2 <- seq(1:length(alpha_input()))
        
        df_beta <- data.frame(x2, y2,y1)
        df_beta <- data.frame(apply(df_beta, 2, unclass))

        ggplot(data = df_beta) +
          ggtitle("Success Probability of Proximal Outcome Under Null and Under Alternative") +
          
          geom_line(mapping = aes(y     = y2, 
                                  x     = x2, 
                                  color = "Alternative Hypothesis"),
                    size = 1) +
          
          geom_line(mapping = aes(y     = y1, 
                                  x     = x2, 
                                  color = "Null Hypothesis"), 
                    size = 1) +

          # geom_point(mapping = aes(y     = y2, 
          #                          x     = x2, 
          #                          color = "Alternative Hypothesis"),
          #            size = 1) +
      
          scale_color_manual(
            values = c("Null Hypothesis"        = "deepskyblue3",
                       "Alternative Hypothesis" = "red3")) +

          labs(x = "Decision Point",
               y = "Success Probability") +
          
          ylim(0, 1) +
          ylab("Success Probability") +
          xlab("Decision Point") +
          labs(color = "Legend") +
          
          theme(legend.position = 'bottom')
        
        
      }
    })
      
        
    
    ### p10, p11, pT0, pT1 ###
    
    # probability of success for under null at first time point
    p10 <- reactive({
        alpha_input()[1]
    })
    
    # probability of success under null at final time point
    pT0 <- reactive({
        alpha_input()[total_decision_points()]
    })
    
    # probability of success under alternative at first time point
    p11 <- reactive({
        alpha_input()[1] * beta_input()[1]
    })
    
    # probability of success under alternative at final time point
    pT1 <- reactive({
        alpha_input()[total_decision_points()] *
            beta_input()[total_decision_points()]
    })

# Expected availability ---------------------------------------------------
    
    avail_input <- reactive({
        
        
        if (input$avail_choices == "constant") {
            
            validate(
                need(input$avail_constant_mean > 0, 
                     paste0("Error: Please specify the average availability", 
                            " to be greater than 0")),
                
                need(input$avail_constant_mean <= 1, 
                     paste0("Error: Please specify the average availability", 
                            " to be less than or equal to 1"))
            )
            
            result <- rep(input$avail_constant_mean, total_decision_points())
            
            rv$ea_shape <- "constant"

            
            validate(
                need(min(result) > 0,
                    paste0("Warning: Some values of expected availability are", 
                        " less than or equal to 0")),
                need(max(result) <= 1,
                    paste0("Warning: Some values of expected availability are", 
                        " greater than 1"))
            )
            
            
            return(result)
            
        } else if (input$avail_choices == "linear") {
            
            validate(
                need(input$avail_linear_initial > 0, 
                     paste0("Error: Please specify the initial value of", 
                            " the expected availability to be greater than 0")),
                need(input$avail_linear_initial <= 1, 
                     paste0("Error: Please specify the initial value of", 
                            " the expected availability to be less than or equal to 1")),
                need(input$avail_linear_final > 0, 
                     paste0("Error: Please specify the final value of", 
                            " the expected availability to be greater than 0")),
                need(input$avail_linear_final <= 1, 
                     paste0("Error: Please specify the final value of", 
                            " the expected availability to be less than or equal to 1"))
            )
            
            result <- seq(from = input$avail_linear_initial, 
                          to = input$avail_linear_final, 
                          length.out = total_decision_points())
            
            rv$ea_shape <- "linear"

            
            validate(
                need(min(result) > 0,
                    paste0("Warning: Some values of expected availability are", 
                        " less than or equal to 0")),
                need(max(result) <= 1,
                    paste0("Warning: Some values of expected availability are", 
                        " greater than 1"))
            )
            
            return(result)
            
        } else if (input$avail_choices == "tv_days") {

            validate(need(!is.null(ea_inter_days()),
                     "Error: No file uploaded."))
            result <- rep(ea_inter_days()$Expected.Availability, 
                          each = input$occ_per_day)
            
            validate(need(length(result)==total_decision_points(),
                     "Error: Number of days does not match"))
            
            rv$ea_shape <- "time-varying"
            
            validate(
                need(min(result) > 0,
                    paste0("Warning: Some values of expected availability are", 
                        " less than or equal to 0")),
                need(max(result) <= 1,
                    paste0("Warning: Some values of expected availability are", 
                        " greater than 1"))
            )
            
            return(result)
            
        } else if (input$avail_choices == "tv_dec_pts") {

            validate(need(!is.null(ea_inter_dec()), "Error: No file uploaded"),
                     need("Expected.Availability" %in% colnames(ea_inter_dec()), 
                          "Error: No column of expected availability. 
                          See template")) 
            
            result <- ea_inter_dec()$Expected.Availability

            validate(need(length(result)==total_decision_points()),
                     "Error: Number of decision points does not match")
            
            rv$ea_shape <- "time-varying"
            
            validate(
                need(min(result) > 0,
                    paste0("Warning: Some values of expected availability are", 
                        " less than or equal to 0")),
                need(max(result) <= 1,
                    paste0("Warning: Some values of expected availability are", 
                        " greater than 1"))
            )
            
            return(result)
        }
        
    })
    
    
    
    ### Plot the graph for expected availability ###
    output$avail_graph <- renderPlot({

      validate(need(!(is.null(avail_input())), "Error: No availability input"))
      
      # format data for plotting
      y3 <- avail_input()
      x3 <- seq(1:length(avail_input()) )
      m <- c(rep(mean(avail_input()), length(x3)))
      
      df_avail <- data.frame(y3, x3, m)
      df_avail<- data.frame(apply(df_avail, 2, unclass))
       
       
       

      ggplot(data = df_avail) +
        
        ggtitle("Availability vs. Decision Points") +
           
        geom_line(mapping = aes(y     = m,
                                x     = x3,
                                color = "Average Availability"),
                  size = 1) +
        geom_line(mapping = aes(y     = y3,
                                x     = x3,
                                color = "Availability"), 
                  size = 1) +
           
        geom_point(mapping = aes(y     = m,
                                 x     = x3,
                                 color = "Average Availability"),
                  size = 1) +           
           
        scale_color_manual(
          values = c("Average Availability" = "grey69",
                     "Availability"         = "deepskyblue3")) +

        ylim(0, 1) + 
        ylab("Expected Availability") +
        xlab("Decision Point") +
        labs(color = "Legend") +
        theme(legend.position = 'bottom')
      
    })




       

    
    ### Reading the file with respect to days for expected availability ###
    
    # time-varying days
    ea_inter_days <- reactive({     
        
        inFile <- input$file0
        
    
        if (is.null(inFile)) {
            return(NULL)
        }

        read.csv(inFile$datapath, header = TRUE)
        
    
    })
    
    # time-varying decision points
    ea_inter_dec <- reactive({     
        
        inFile <- input$file0a

        
        if (is.null(inFile)) {
            return(NULL)
        }
        
        
        read.csv(inFile$datapath, header = TRUE)
        
        
    })
    
  
    
    # Output first 5 rows of the table reading from file with respect to days
    # and output warnings if the format of the file is not correct
    # Output the first five rows of the table reading from the file for days
    output$ea_inter_table_days <- DT::renderDataTable({     
      
        validate(need(is.null(input$file0) ||
                        "Expected.Availability" %in% colnames(ea_inter_days()),
                      paste0(
                        "Error: need a column titled 'Expected Availability';",
                        " see template")))
      
        delta <- as.vector(ea_inter_days()$Expected.Availability)
        
        validate(
            need(!is.null(input$file0), "Warning: No file is uploaded"),
            
            need(is.null(input$file0) ||
                     length(delta) == input$days , 
                 "Error: the number of days doesn't match."),
            
            need(is.null(input$file0) || max(delta) <= 1, 
                 "Error: some value of expected availability is bigger than 1"),
            
            need(is.null(input$file0) || min(delta) >= 0, 
                 "Error: some value of expected availability is less than 0")
        )
        
        datatable(ea_inter_days(), 
                  options = list(pageLength = 5),
                  colnames = c("Day", "Expected Availability"),
                  rownames = FALSE)

        

    })
    
    output$ea_inter_table_dec <- renderDataTable({
        validate(need(is.null(input$file0a) ||
                      "Expected.Availability" %in% colnames(ea_inter_dec()),
                      paste0(
                        "Error: need a column titled 'Expected Availability';",
                         " see template")))
        
        delta <- as.vector(ea_inter_dec()$Expected.Availability)
        
        validate(
            need(!is.null(input$file0a), "Warning: No file is uploaded"),
            
            need(is.null(input$file0a) || 
                     length(delta) == input$days * input$occ_per_day, 
                 "Error: the number of decision times doesn't match."),
            
            need(is.null(input$file0a) || max(delta) <= 1, 
                 "Error: some value of expected availability is bigger than 1"),
            
            need(is.null(input$file0a) || min(delta) >= 0, 
                 "Error: some value of expected availability is less than 0")
        )
        
        datatable(ea_inter_dec(), 
                  options = list(pageLength = 5),
                  colnames = c("Decision Time Point" = 1,
                               "Expected Availability" = 2),
                  rownames = FALSE)
    })
    
    ### expected availability templates for download
    ea_days_df <- reactive({
        col_names <- c("Day", "Expected Availability")
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
        col_names <- c("Dec Time", "Expected Availability")
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
    
    

# Calculate Sample Size ---------------------------------------------------


    sample_size <- eventReactive(input$button_calculate_sample_size, {
        
        # check all parameters are set before finding sample size
        validate(need(
            !is.null(avail_input()) & !is.null(rand_prob()) &
                rv$null_set & rv$te_set,
            "Provide values for all parameters first."
        ))
        

        rv$ss_clicked <- TRUE
           
           out <- tryCatch(
               {
                   
                   message("Try calculate sample size")
                   
                   mrt_binary_ss(
                       avail_pattern = avail_input(),
                       f_t           = f_t(),
                       g_t           = g_t(),
                       beta          = b_mat(),
                       alpha         = a_mat(),
                       p_t           = rand_prob(),
                       gamma         = input$sig_level,
                       b             = 1-input$power) 

               },
               error=function(cond) {
                   message("Here's the original error message:")
                   message(cond)

                   return(NA)
               },
               warning=function(cond) {
                   message("Here's the original warning message:")
                   message(cond)

                   return(NULL)
               },
               finally={
                   message("Exit sample size computation")
               }
           )    
           return(out)
    
    })
    
    # make eventReactive version of input$power and input$sig_level
    # to be used in the output
    input_power_reactive <- eventReactive(
      input$button_calculate_sample_size,input$power)
    input_sig_level_samplesize_reactive <- eventReactive(
      input$button_calculate_sample_size, input$sig_level)
    
    output$sample_size <- renderUI({
        validate(
            need(
              !is.na(sample_size()) & !is.null(sample_size()),
              paste0(
              "There was an error in the computation of the sample size.", 
               " Most likely this comes from choice of null curve and ",
               "proximal treatment effect. ",
               " Check inputs and try again. 
               See MRTSampleSizeBinary documentation for further details.")))

        
        if (sample_size() > 10) {
            HTML(
              paste0("<h5 style = 'color:black';> The required sample size is ",
                    "<span style= 'color: blue'>",
                    sample_size(),
                    "</span>",
                    " to attain ", 
                    "<span style= 'color: blue'>",
                    input_power_reactive()*100,
                    "%</span>",
                    " power when the significance level is ",
                    input_sig_level_samplesize_reactive(),".")) 
        } else {
            # if calculated sample size <=10, don't output sample size
            HTML(
              paste0("<h5 style = 'color:black';> The required sample size is ",
                     "<span style= 'color: blue'>",
                     "less than or equal to 10",
                     "</span>",
                     " to attain ", 
                     "<span style= 'color: blue'>",
                     input_power_reactive()*100,
                    "%</span>",
                    " power when the significance level is ",
                    # "<span style= 'color: blue'>",
                    input_sig_level_samplesize_reactive(),
                    # "</span>",
                    ". Sample size n <= 10 may result in inaccurate power ",
                    "calculation, because the sample size formula is based on ",
                    "an asymptotic result. See the left panel for what to do.")) 
        }
        
    })
    
    

# Calculate Power ---------------------------------------------------------
    
    power <- eventReactive(input$button_calculate_power, {
        validate(need(
            !is.null(avail_input()) & !is.null(rand_prob()) & 
              rv$null_set & rv$te_set,
            "Provide values for all parameters first."
        ))
        rv$power_clicked <- TRUE
        out <- tryCatch(
            {
                
                message("Try calculate power")
                
                mrt_binary_power(avail_pattern = avail_input(),
                                 f_t           = f_t(),
                                 g_t           = g_t(),
                                 beta          = b_mat(),
                                 alpha         = a_mat(),
                                 p_t           = rand_prob(),
                                 gamma         = input$sig_level,
                                 n             = input$sample_size)
                
            },
            error=function(cond) {
                message("Here's the original error message:")
                message(cond)
           
                return(NA)
            },
            warning=function(cond) {
                message("Here's the original warning message:")
                message(cond)
             
                return(NULL)
            },
            finally={
                message("Exit calculate power")
            }
        )    
        return(out)
        
    })
    
    # make eventReactive version of input$sample_size and input$sig_level
    # to be used in the output
    input_sample_size_reactive <- eventReactive(
      input$button_calculate_power,input$sample_size)
    input_sig_level_power_reactive <- eventReactive(
      input$button_calculate_power, input$sig_level)
    
    output$power <- renderUI({
        validate(
            need(!is.na(power()) & !is.null(power()), 
                 paste0(
                     "There was an error in the computation of the power.", 
                     " Most likely this comes from choice of null curve and",
                     " proximal treatment effect. ",
                     " Check inputs and try again. 
               See MRTSampleSizeBinary documentation for further details.")))
        
        
        if (input_sample_size_reactive() <= 10) {
          # if input sample size <=10, don't output power
          
          HTML(paste0("<h5 style = 'color:black';>",
                      "Sample size n <= 10 may result in inaccurate power ",
                      "calculation, because the power formula is based on ",
                      "an asymptotic result. See the left panel for more information.")) 
        } else if (power() >= 0.4) {

            HTML(paste0("<h5 style = 'color:black';> The power we get is ",
                       "<span style= 'color: blue'>",
                       round(power(), 3)*100,
                       "%</span>",
                       " with sample size ", 
                       "<span style= 'color: blue'>",
                       input_sample_size_reactive(),
                       "</span>",
                       " when the significance level is ",
                       input_sig_level_power_reactive(),"."))
        } else {
            ### If the calculated power is less than 40% ###

            HTML(paste0("<h5 style = 'color:black';> ", 
                       "The power we get is ", 
                       "<span style= 'color: blue'>",
                       "less than 40%",
                       "</span>",
                       " with sample size ", 
                       "<span style= 'color: blue'>",
                       input_sample_size_reactive(),
                       "</span>",
                       " when the significance level is ",
                       # "<span style= 'color: blue'>",
                       input_sig_level_power_reactive(),
                       # "</span>",
                       "."))
        }
    })
    
    


# Sample size history table -----------------------------------------------

    sample_size_history <- reactiveValues(data=NULL)
    
    observeEvent(input$button_calculate_sample_size, {
      # only update if valid sample size and all settings were made before 
      # call to compute results
      validate(need(
            !is.null(avail_input()) & !is.null(rand_prob()) & 
              rv$null_set & rv$te_set,
            FALSE
        ))
        
      validate(need(!is.null(sample_size()) & !is.na(sample_size()), FALSE))
        
      sample_size_history$avail_pattern <- c(sample_size_history$avail_pattern, 
                                               input$avail_choices)
        
      sample_size_history$avail_init <- c(sample_size_history$avail_init, 
                                            avail_input()[1])
        
      sample_size_history$avail_final <- c(
          sample_size_history$avail_final, 
          avail_input()[total_decision_points()])
        
      sample_size_history$rand_prob_shape <- c(
          sample_size_history$rand_prob_shape, 
          rv$rp_shape)
        
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
        
      sample_size_history$tot_dec_pts <- c(sample_size_history$tot_dec_pts,
                                             total_decision_points())
    })

    samp_size_hist <- reactive({
            print(sample_size_history$sample_size)
      print(sample_size_history$power)
      print(sample_size_history$sig_level)
      print(sample_size_history$rand_prob_shape)
      data.frame(
                   "Sample Size" = sample_size_history$sample_size,
                   "Power" = sample_size_history$power,
                   "Sig Level" = sample_size_history$sig_level,
                   "Rand Prob Shape" = sample_size_history$rand_prob_shape,
                   "Total Dec Pts" = sample_size_history$tot_dec_pts,
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
   

    output$sample_size_history_table <- renderDataTable({
        # check that sample size table has been initialized
        validate(need(!is.null(samp_size_hist()) & 
                                   all(dim(samp_size_hist()) > 0),
                      "No sample size calculations performed yet."))
        
        temp_tab <- samp_size_hist()[,c("Sample.Size",
                                        "Power",
                                        "Sig.Level",
                                        "Rand.Prob.Shape",
                                        "Total.Dec.Pts",
                                        "Succ.Prob.No.Trt.Shape",
                                        "Trt.Eff.Shape",
                                        "Avail.Pattern")]
        
        temp_tab %>% 
            mutate_if(is.numeric, round, digits = 4) %>% 
            datatable( 
                  options = list(pageLength = 5),
                  colnames = c("Sample Size",
                               "Power",
                               "Sig. Level",
                               "Rand. Prob.",
                               "Total Dec. Pts",
                               "Succ. Prob. No Trt Shape",
                               "Trt Eff. Shape",
                               "Avail. Pattern"),
                  caption = htmltools::tags$caption(
                      style = "caption-side: bottom; text-align: center;",
                      "Sample Size History Table: ", 
                      htmltools::em("This table holds a summary of settings 
                                     and outputs for sample size calculations.
                                     It records sample size, power, 
                                     significance level, randomization 
                                     probability shape, total number of 
                                     decision points, shape of success 
                                     probability curve under no treatment 
                                     and under treatment, and availability 
                                     pattern. More details are contained in 
                                     the downloaded version of this table.")),
                  rownames = FALSE)
    })
    

    # download button for sample size history table
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

    


# Create power vs Sample Size plots ---------------------------------------

    # this is displayed when on the sample size setting
    pow_vs_n_plot1 <- eventReactive(input$button_calculate_sample_size, {
        validate(need(
            !is.null(avail_input()) & !is.null(rand_prob()) & 
              rv$null_set & rv$te_set & sample_size() >= 10,
            FALSE
        ))
        rv$ss_clicked <- TRUE


        out <- tryCatch(
            {
                
                message("Try power vs n plot")
                
                power_vs_n_plot(avail_pattern = avail_input(),
                                    f_t       = f_t(),
                                    g_t       = g_t(),
                                    beta      = b_mat(),
                                    alpha     = a_mat(),
                                    p_t       = rand_prob(),
                                    gamma     = input$sig_level,
                                    max_n     = 200)
                
            },
            error=function(cond) {
                message("Here's the original error message:")
                message(cond)

                return(NA)
            },
            warning=function(cond) {
                message("Here's the original warning message:")
                message(cond)

                return(NA)
            },
            finally={
                message("Exit power vs n plot")
            }
        )    
        return(out)
        
    })    
    
    
    output$power_vs_n1 <- renderPlot({
        
        validate(need(!is.na(pow_vs_n_plot1()) & sample_size() > 10, 
                      FALSE))
        pow_vs_n_plot1()
    })

    # this is displayed on the power setting
    pow_vs_n_plot2 <- eventReactive(input$button_calculate_power, {
        validate(need(
            !is.null(avail_input()) & !is.null(rand_prob()) &
              rv$null_set & rv$te_set & input_sample_size_reactive() > 10,
            FALSE
        ))
        rv$ss_clicked <- TRUE        
 
        out <- tryCatch(
            {
                
                message("Try power vs n plot")
                
                power_vs_n_plot(avail_pattern = avail_input(),
                                    f_t       = f_t(),
                                    g_t       = g_t(),
                                    beta      = b_mat(),
                                    alpha     = a_mat(),
                                    p_t       = rand_prob(),
                                    gamma     = input$sig_level)
                
            },
            error=function(cond) {
                message("Here's the original error message:")
                message(cond)
                # Choose a return value in case of error
                return(NA)
            },
            warning=function(cond) {
                message("Here's the original warning message:")
                message(cond)
                # Choose a return value in case of warning
                return(NA)
            },
            finally={
                message("Exit plot power vs n")
            }
        )    
        return(out)
    })    
    
    output$power_vs_n2 <- renderPlot({
        validate(need(!is.na(pow_vs_n_plot2()) & !is.null(pow_vs_n_plot2()) &
                      input_sample_size_reactive() > 10, 
                      FALSE))
        pow_vs_n_plot2()
    })
    

# Power summary tables (beneath plot) -------------------------------------
    # case for when sample size is being calculated
    pow_summary1 <- eventReactive(input$button_calculate_sample_size, {
        validate(need(
            !is.null(avail_input()) & !is.null(rand_prob()) &
              rv$null_set & rv$te_set,
            FALSE
        ))
        rv$ss_clicked <- TRUE

        out <- tryCatch(
            {
                
                message("Enter try for power summary")
                
                power_summary(avail_pattern = avail_input(),
                              f_t           = f_t(),
                              g_t           = g_t(),
                              beta          = b_mat(),
                              alpha         = a_mat(),
                              p_t           = rand_prob(),
                              gamma         = input$sig_level)
                
            },
            error=function(cond) {
                message("Here's the original error message:")
                message(cond)
                return(NA)
            },
            warning=function(cond) {
                message("Here's the original warning message:")
                message(cond)
                return(NA)
            },
            finally={
                message("exit power summary")
            }
        )
        
        
        return(out)
    })  
    
    output$power_summary1 <- renderDataTable({

       validate(need(!is.na(pow_summary1()) & !is.null(pow_summary1()),
                      FALSE))
       
        datatable(pow_summary1(), colnames=c("Power", "Sample Size")) 
    }) 
    
    # for when power is being calculated
    pow_summary2 <- eventReactive(input$button_calculate_power, {
        validate(need(
            !is.null(avail_input()) & !is.null(rand_prob()) & 
              rv$null_set & rv$te_set,
            FALSE
        ))
        
        rv$power_clicked <- TRUE
        
        
        out <- tryCatch(
            {
                
                message("Enter try for power summary.")
                
                power_summary(avail_pattern = avail_input(),
                              f_t           = f_t(),
                              g_t           = g_t(),
                              beta          = b_mat(),
                              alpha         = a_mat(),
                              p_t           = rand_prob(),
                              gamma         = input$sig_level)
                
            },
            error=function(cond) {
                message("Here's the original error message:")
                message(cond)
                return(NA)
            },
            warning=function(cond) {
                message("Here's the original warning message:")
                message(cond)
                return(NA)
            },
            finally={
                message("Exit power summary.")
            }
        )    
        return(out)
        
        
    })  
    
    output$power_summary2 <- renderDataTable({
        validate(need(!is.null(pow_summary2()) & !is.na(pow_summary2()),
                      FALSE))
        datatable(pow_summary2(), colnames=c("Power", "Sample Size"))
    }) 
    
    

# Power history table -----------------------------------------------------
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
        validate(need(
            !is.null(avail_input()) & !is.null(rand_prob()) &
              rv$null_set & rv$te_set,
            FALSE
        ))
        # only up date if valid power calculation was performed
        validate(need(!is.null(power()) & !is.na(power()), FALSE))
        
        power_history$avail_pattern <- c(power_history$avail_pattern, 
                                         input$avail_choices)
        
        power_history$avail_init <- c(power_history$avail_init, 
                                      avail_input()[1])
        
        power_history$avail_final <- c(power_history$avail_final, 
                                       avail_input()[total_decision_points()])
        
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
        
        power_history$rand_prob_shape <- c(power_history$rand_prob_shape, 
                                     rv$rp_shape)
        
        power_history$total_dec_pts <- c(power_history$total_dec_pts,
                                         total_decision_points())
    })
    
    pow_history_table <- reactive({

        pht <- data.frame(
                   "Sample Size" = power_history$sample_size,
                   "Power" = power_history$power,
                   "Sig Level" = power_history$sig_level,
                   "Rand Prob Shape" = power_history$rand_prob_shape,
                   "Succ Prob No Trt Shape" = power_history$alpha_shape,
                   "Trt Eff Shape" = power_history$beta_shape,
                   "Total Dec Pts" = power_history$total_dec_pts,
                   "p10" = power_history$p10,
                   "pT0" = power_history$pT0,
                   "p11" = power_history$p11,
                   "pT1" = power_history$pT1,
                   "Avail Pattern" = power_history$avail_pattern,
                   "Avail Init" = power_history$avail_init,
                   "Avail Final" = power_history$avail_final)
        
        pht 
    })
    
    output$power_history_table <- renderDataTable({
        validate(need(!is.null(pow_history_table()) & 
                                   all(dim(pow_history_table()) > 0),
                      "No history calculations performed yet."))
        
        pht <- pow_history_table()[,c("Sample.Size",
                                      "Power",
                                      "Sig.Level",
                                      "Rand.Prob.Shape",
                                      "Total.Dec.Pts",
                                      "Succ.Prob.No.Trt.Shape",
                                      "Trt.Eff.Shape",
                                      "Avail.Pattern")]
        
        pht %>% 
            mutate_if(is.numeric, round, digits = 4) %>% 
            datatable(options = list(pageLength = 5),
                      colnames = c("Sample Size",
                                   "Power",
                                   "Sig. Level",
                                   "Rand. Prob. Shape",
                                   "Total Dec. Pts",
                                   "Succ. Prob. No Trt Shape",
                                   "Trt Eff. Shape",
                                   "Avail. Pattern"),
                      rownames = FALSE,
                      caption = htmltools::tags$caption(
                          style = "caption-side: bottom; text-align: center;",
                          "Power History Table: ", 
                          htmltools::em("This table holds a summary of settings 
                                         and outputs for power calculations.
                                         It records sample size, power, 
                                         significance level, randomization 
                                         probability shape, total number of 
                                         decision points, shape of success 
                                         probability curve under no treatment 
                                         and under treatment, and availability 
                                         pattern. More details are contained in 
                                         the downloaded version of this table.")
                      ))
    })
    
    # download for power history table
    output$pow_dl <- downloadHandler(
        filename = function() {
            paste0("power_history.csv")
        },
        content = function(file){
            write.csv(pow_history_table(), file, row.names=FALSE)
        }
    )
    
    output$download_pow <- renderUI({
        if(rv$power_clicked) {
            downloadButton('pow_dl', 'Power History')
        }
    })
    
    
    # download forhistory table description
    # (shiny needs duplicates for these tables to have two sep buttons)
    # inelegant solution, but it works
    output$hist_desc_dl <- downloadHandler(
      filename = function() {
        paste0("history_description.txt")
      },
      content = function(file){
        writeLines("
    - Sample Size: calculated sample size (when calculating sample size) or user-input sample size (when calculating power)\n
    - Power: user-input power (when calculating sample size) or calculated power (when calculating power)\n
    - Sig Level: user-input significance level\n
    - Rand Prob: randomization probability\n
    - Total Dec Pts: total number of decision points\n
    - Succ Prob No Trt Shape: shaped of the success probability null curve\n
    - Trt Eff Shape: shaped of the treatment effect over time\n
    - p10: outcome success probability at the first decision point under no treatment\n
    - pT0: outcome success probability at the last decision point under no treatment\n
    - p11: outcome success probability at the first decision point under treatment\n
    - pT1: outcome success probability at the last decision point under treatment\n
    - Avail Pattern: shape of the expected availability over time\n
    - Avail Init: expected availability at the first decision point\n
    - Avail Final: expected availability at the last decision point", file, sep = "")
      }
    )
    
    output$hist_desc_dl1 <- downloadHandler(
      filename = function() {
        paste0("history_description.txt")
      },
      content = function(file){
        writeLines("
    - Sample Size: calculated sample size (when calculating sample size) or user-input sample size (when calculating power)\n
    - Power: user-input power (when calculating sample size) or calculated power (when calculating power)\n
    - Sig Level: user-input significance level\n
    - Rand Prob: randomization probability\n
    - Total Dec Pts: total number of decision points\n
    - Succ Prob No Trt Shape: shaped of the success probability null curve\n
    - Trt Eff Shape: shaped of the treatment effect over time\n
    - p10: outcome success probability at the first decision point under no treatment\n
    - pT0: outcome success probability at the last decision point under no treatment\n
    - p11: outcome success probability at the first decision point under treatment\n
    - pT1: outcome success probability at the last decision point under treatment\n
    - Avail Pattern: shape of the expected availability over time\n
    - Avail Init: expected availability at the first decision point\n
    - Avail Final: expected availability at the last decision point", file, sep = "")
      }
    )
    
    output$download_tab_desc_pow <- renderUI({
      if(rv$power_clicked ) {
        downloadLink('hist_desc_dl', 'Table Description')
      }
    })

    output$download_tab_desc_hist <- renderUI({
      if(rv$ss_clicked) {
        downloadLink('hist_desc_dl1', 'Table Description')
      }
    })
    
})

