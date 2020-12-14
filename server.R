library(shiny)
library(DT)
library(ggplot2)
library(dplyr)
library(mrtbincalc)
#source("sample_size_calc_bin_mrt.R")

shinyServer(function(input,output,session){
    
    rv <- reactiveValues(data=NULL)
    
    # flags for when to display download buttons
    rv$ss_clicked <- FALSE
    rv$power_clicked <- FALSE
    
    # flags to make sure all parameters have settings before buttons clicked
    rv$ea_set <- FALSE
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
    

# Randomization probability -----------------------------------------------

    # Output first 5 rows of table reading from file wrt decision times 
    # and output warnings if the format of the file is not correct
    output$P_inter_table_dec <- renderDataTable({      
        
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
        
        datatable(P_inter_dec(), 
                  options = list(pageLength = 5),
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
        
        datatable(P_inter_days(), 
                  options = list(pageLength = 5), 
                  colnames = c("Day" = 1,
                               "Randomization Probability" = 2),
                  rownames = FALSE)
    })
    
    
    # vector of randomization probabilities
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
    
    
# Calculating null success curve ------------------------------------------
    # alpha vector
    a_mat <- reactive({
        # Initialize a value to avoid some internal error when running locally
        # This part should have not effect on the UI.
        result <- 0.5
        if (input$alpha_choices == "constant") {
            
            validate(
                need(input$alpha_constant_mean > 0, 
                     paste0("Error: Please specify the baseline success", 
                            " probability greater than 0"))
            )
            
            result <- matrix(log(input$alpha_constant_mean))
            
            rv$null_set <- TRUE
            
        } else if (input$alpha_choices == "loglinear") {
            validate(
                need(input$alpha_loglinear_initial > 0, 
                     paste0("Error: Please specify the initial value of", 
                            " baseline success probability greater than 0")),
                
                need(input$alpha_loglinear_final > 0, 
                     paste0("Error: Please specify the final value of", 
                            " baseline success probability greater than 0"))
            )
            
            initial_log <- log(input$alpha_loglinear_initial)
            
            final_log <- log(input$alpha_loglinear_final)

            
            slope <- (final_log - initial_log) / total_decision_points() 
            
            result <- matrix(c(initial_log - slope, slope ), ncol=1)
            
            rv$null_set <- TRUE
            
        } else if (input$alpha_choices == "logquadratic"){
           
            validate(
                need(input$alpha_logquad_initial > 0, 
                     paste0("Error: Please specify the initial value of", 
                     " baseline success probability greater than 0")),
                
                need(input$alpha_logquad_change_val > 0, 
                     paste0("Error: Please specify the final value of baseline", 
                     " success probability greater than 0")),
                
                need(input$alpha_logquad_change_pt > 0, 
                     paste0("Error: Please specify the final value of baseline", 
                      " success probability greater than 0"))
            )
            
            k1 <- log(input$alpha_logquad_initial)
            k2 <- input$alpha_logquad_change_pt
            k3 <- log(input$alpha_logquad_change_val)
            
            validate(need(k2 != 0, 
                          "Error: Change point cannot be starting point"))
            
            a2 <- (2 * k1 * k2 - 2 * k3 * k2) / (2 * k2 * (1 - k2/2) - 1)
            a1 <- k1 - k3 - (1 - k2/2) * a2
            a3 <- k1 - a1 - a2
            

            
            a_mat <- as.matrix(c(a1, a2, a3), ncol=1)

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
                    "Invalid probabilities. Must be between 0 and 1."))   
     
        ## ggplot in the works
        y1 = alpha_input()
        x1 = seq(1:length(alpha_input()) )
        df_alpha <- data.frame(x1, y1)
        
        df_alpha<- data.frame(apply(df_alpha, 2, unclass))
        
        ggplot(df_alpha)+
        geom_line(aes( y = alpha_input(), 
                     x = seq(1:length(alpha_input()))), size = 1, 
                color = "deepskyblue3", group=1)+
        ggtitle("Success Probability Null Curve") +
        xlab("Decision Point") + ylab("Baseline Success Probability")+
        ylim(0,1)+
        theme(axis.text = element_text(size=12),
              axis.title = element_text(size=14))
      
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
                need(input$beta_constant_mean > 0, 
                     paste0("Error: Please specify the proximal treatment", 
                     " effect greater than 0"))
            )
            
            
            result <- matrix(c(log(input$beta_constant_mean)), ncol=1)
        
        } else if (input$beta_choices == "loglinear") {
            validate(
                need(input$beta_loglinear_initial > 0, 
                     paste0("Error: Please specify the initial value of", 
                            " proximal treatment effect greater than 0")),
                need(input$beta_loglinear_final > 0, 
                     paste0("Error: Please specify the final value of",
                     " proximal treatment effect greater than 0"))
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
                            " baseline success probability greater than 0")),
                
                need(input$beta_logquad_change_val > 0, 
                     paste0("Error: Please specify the final value of", 
                            " baseline success probability greater than 0")),
                
                need(input$beta_logquad_change_pt > 0, 
                     paste0("Error: Please specify the final value of baseline", 
                            " success probability greater than 0"))
            )
            
            
            k1 <- log(input$beta_logquad_initial)
            k2 <- input$beta_logquad_change_pt
            k3 <- log(input$beta_logquad_change_val)
            
            validate(need(k2 != 0, 
                          "Error: Change point cannot be starting point"))
            
            b2 <- (2 * k1 * k2 - 2 * k3 * k2) / (2 * k2 * (1 - k2/2) - 1)
            b1 <- k1 - k3 - (1 - k2/2) * b2
            b3 <- k1 - b1 - b2
            
            t_mat <- cbind(rep(1, times=total_decision_points()),
                           1:total_decision_points(),
                           (1:total_decision_points())^2)
            
            b_mat <- as.matrix(c(b1, b2, b3), ncol=1)
            
            result <- exp(t_mat %*% b_mat) 
            
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
                        "Invalid probabilities. Must be between 0 and 1."))  
          
        validate(need(max(ab_product) < 1 & min(ab_product) > 0,
                        "Invalid probabilities. Must be between 0 and 1."))  
          
       
        
        validate(need(max(beta_input()*alpha_input()) < 1 &
                          min(beta_input()*alpha_input()) > 0,
                      "Invalid probabilities. Must be between 0 and 1."))  
      
        


        y1 = alpha_input()
        y2 = beta_input()*alpha_input()
        x2 = seq(1:length(alpha_input()) )
        df_beta <- data.frame(x2, y2,y1)
        df_beta <- data.frame(apply(df_beta, 2, unclass))
        
        ggplot(df_beta)+
          geom_line(aes( y = y2, 
                         x = x2), size = 1, 
                    color = "deepskyblue3",group=1)+
          ggtitle("Null Vs. Alternative Hypothesis ") +
                   
          xlab("Decision Point") + ylab("Success Probability")+
          ylim(0,1)+
          geom_point(aes(y= y1, x = x2), size = 1, 
                         color = "red3")+ 
          theme(axis.text = element_text(size=12),
                axis.title = element_text(size=14))

      }
      
        # if(!is.null(alpha_input()) && !is.null(beta_input())){
        # plot(alpha_input() * beta_input(), 
        #      xlab = "Decision Point", 
        #      ylab = "Success Probability", 
        #      ylim = c(0, 1), 
        #      type = "o",
        #      pch = 16, 
        #      cex = 0.8, 
        #      col = 2)
        # 
        # points(x = 1:length(alpha_input()), 
        #        y = alpha_input(), 
        #        type = "o", 
        #        pch = 16, 
        #        cex = 0.8, 
        #        col = 4)
        # 
        # legend("topleft", 
        #        cex = 1, 
        #        legend=c('Alternate Hypothesis', 'Null Hypothesis'),
        #        col = c(2,4),
        #        lty = c(1,1), 
        #        pch=c(16,16),
        #        bty = "n")
        # }
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
                            " greater than 0"))
            )
            
            result <- rep(input$avail_constant_mean, total_decision_points())
            
            rv$ea_shape <- "constant"
            rv$ea_set <- TRUE
            
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
                            " expected availability greater than 0")),
                need(input$avail_linear_final > 0, 
                     paste0("Error: Please specify the final value of", 
                            " expected availability greater than 0"))
            )
            
            result <- seq(from = input$avail_linear_initial, 
                          to = input$avail_linear_final, 
                          length.out = total_decision_points())
            
            rv$ea_shape <- "linear"
            rv$ea_set <- TRUE
            
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
            rv$ea_set <- TRUE
            
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
            rv$ea_set <- TRUE
            
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
      
      y3 <- avail_input()
      x3 <- seq(1:length(avail_input()) )
      m <- c(rep(mean(avail_input()), length(x3)))
      
      df_avail <- data.frame(y3, x3, m)
      df_avail<- data.frame(apply(df_avail, 2, unclass))
      
       ggplot(df_avail)+
        geom_line(aes( y =y3, 
                       x = x3),
                       size = 1, 
                  color = "deepskyblue3")+
        ggtitle("Availability") +
        xlab("Decision Point") + ylab("Expected Availablility")+
        ylim(0,1)+
         geom_point(aes(y= m, x = x3), size = 1, 
                    color = "red")+
        theme(axis.text = element_text(size=12),
              axis.title = element_text(size=14))
      
    })

       
        
    #     plot(avail_input(), 
    #          xlab = "Decision Point", 
    #          ylab = "Expected Availability", 
    #          ylim = c(0, 1), 
    #          type = "o",
    #          pch = 16,
    #          cex = 0.8, 
    #          col = 4)
    #     abline(h = mean(avail_input()), lty = 2)
    #     legend("topleft", 
    #            legend=c('Availability','Average Availability'), 
    #            col = c(4,1),
    #            lty = c(1,2), 
    #            pch=c(16,NA),bty = "n")
    # 
        
    #     plot( avail_input(),
    #          xlab = "Decision Point", 
    #          ylab = "Expected Availability", 
    #          ylim = c(0, 1), 
    #          type = "o",
    #          pch = 16,
    #          cex = 0.8, 
    #          col = 4)
    #     abline(h = mean(avail_input()), lty = 2)
    #     legend("topleft", 
    #            legend=c('Availability','Average Availability'), 
    #            col = c(4,1),
    #            lty = c(1,2), 
    #            pch=c(16,NA),bty = "n")
    # 
    # })
    

    


    
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
        delta <- as.vector(ea_inter_days()$Expected.Availability)
        
        validate(
            need(!is.null(input$file0), "Warning: No file is uploaded"),
            
            need(is.null(input$file0) || 
                     "Expected.Availability" %in% colnames(ea_inter_days()),
                 "Error: need a column titled 'Expected Availability';
                  see template"),
            
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
        delta <- as.vector(ea_inter_dec()$Expected.Availability)
        
        validate(
            need(!is.null(input$file0a), "Warning: No file is uploaded"),
            
            need(is.null(input$file0a) || 
                     "Expected.Availability" %in% colnames(ea_inter_dec()),
                 "Error: need a column titled 'Expected Availability'; 
                  see template"),
            
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
                   
                   calculate_mrt_bin_samplesize_f(
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
    
    output$sample_size <- renderUI({
        validate(
            need(
              !is.na(sample_size() & !is.null(sample_size())),
              paste0(
              "There was an error in the computation of the sample size.", 
               " Most likely this comes from choice of null curve and ",
               "proximal treatment effect. ",
               " Check inputs and try again. 
               See mrtbincalc documentation for further details.")))

        
        if (sample_size() > 10) {
            HTML(
              paste("<h5 style = 'color:blue';> The required sample size is ",
                    sample_size(), 
                    "to attain", 
                    input$power*100,
                    "% power when the significance level is",
                    input$sig_level,".")) 
        } else {
            # if calculated sample size <=10, don't output sample size
            HTML(
              paste("<h5 style = 'color:blue';> The required sample size is 
                     less than or equal to 10 to attain", 
                    input$power*100,
                    "% power when the significance level is",
                    input$sig_level,
                    ". Please refer to the result section in the left column 
                     for suggestions.")) 
        }
        
    })
    
    

# Calculate Power ---------------------------------------------------------
    
    power <- eventReactive(input$button_calculate_power, {
        validate(need(
            rv$ea_set & !is.null(rand_prob()) & rv$null_set & rv$te_set,
            "Provide values for all parameters first."
        ))
        rv$power_clicked <- TRUE
        out <- tryCatch(
            {
                
                message("Try calculate power")
                
                calculate_mrt_bin_power_f(avail_pattern = avail_input(),
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
    
    output$power <- renderUI({
        validate(
            need(!is.na(power()) & !is.null(power()), 
                 paste0(
                     "There was an error in the computation of the power.", 
                     " Most likely this comes from choice of null curve and ",
                     "proximal treatment effect. ",
                     " Check inputs and try again. 
               See mrtbincalc documentation for further details.")))
        
        
        if (power() >= 0.4) {
            HTML(paste0("<h5 style = 'color:blue';> The power we get is ", 
                       round(power(), 3)*100,
                       "% with sample size ", 
                       input$sample_size,
                       " when the significance level is ",
                       input$sig_level,"."))
        } else {
            ### If the calculated power is less than 40% ###
            HTML(paste0("<h5 style = 'color:blue';>", 
                       "The power we get is less than 40% with sample size ", 
                       input$sample_size, 
                       " when the significance level is ",
                       input$sig_level,"."))
        }
    })
    
    


# Sample size history table -----------------------------------------------

    sample_size_history <- reactiveValues(data=NULL)
    
    observeEvent(input$button_calculate_sample_size, {
      # only update if valid sample size and all settings were made before 
      # call to compute results
        
      validate(need(
            rv$ea_set & !is.null(rand_prob()) & rv$null_set & rv$te_set,
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
                               "Rand. Prob. Shape",
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
            rv$ea_set & !is.null(rand_prob()) & rv$null_set & rv$te_set,
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
                message("Exit power vs n plot")
            }
        )    
        return(out)
        
    })    
    
    
    output$power_vs_n1 <- renderPlot({
        validate(need(!is.na(pow_vs_n_plot1()), 
                      FALSE))
        pow_vs_n_plot1()
    })

    # this is displayed on the power setting
    pow_vs_n_plot2 <- eventReactive(input$button_calculate_power, {
        validate(need(
            rv$ea_set & !is.null(rand_prob()) & rv$null_set & rv$te_set,
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
        validate(need(!is.na(pow_vs_n_plot2()) & !is.null(pow_vs_n_plot2()), 
                      FALSE))
        pow_vs_n_plot2()
    })
    

# Power summary tables (beneath plot) -------------------------------------
    # case for when sample size is being calculated
    pow_summary1 <- eventReactive(input$button_calculate_sample_size, {
        validate(need(
            rv$ea_set & !is.null(rand_prob()) & rv$null_set & rv$te_set,
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
        validate(need(!is.na(pow_summary1() & !is.null(pow_summary1())),
                      FALSE))
        
        datatable(pow_summary1(), colnames=c("Power", "Sample Size")) 
    }) 
    
    # for when power is being calculated
    pow_summary2 <- eventReactive(input$button_calculate_power, {
        validate(need(
            rv$ea_set & !is.null(rand_prob()) & rv$null_set & rv$te_set,
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
            rv$ea_set & !is.null(rand_prob()) & rv$null_set & rv$te_set,
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
    
})

