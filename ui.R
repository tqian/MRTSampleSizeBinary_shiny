library(shiny)
library(shinyBS)
library(shinythemes)



shinyUI(fluidPage(theme = shinytheme("readable"),
  titlePanel("Sample Size Calculator for Micro-Randomized Trials with Binary 
              Outcomes"),    
  
  ####Sample Size Calculator Simple version####
  
  ### Print error validation in "red" ###
  
  tags$head(
    tags$style(HTML("
    .shiny-output-error-validation {
      color: red;
    }"))
  ),
  
  ### CSS HEADER ###
  ### Apply style attributes across the application
  tags$head( 
    #set width of numericInputs 
    tags$style(type='text/css',"input[type='number'] {width:90px}"),   
    
    #responsive modals  
    tags$style(type='text/css',includeHTML("www/css/bootstrap-modal.css")),    
    
    tags$link(rel="stylesheet", 
              href="//fonts.googleapis.com/css?family=Roboto|Roboto+Condensed"), 
    
    tags$style("body {font-family: 'Roboto', sans-serif;}  
               h1 {font-family: 'Roboto Condensed', sans-serif;}  
               h2 {font-family: 'Roboto Condensed', sans-serif;} 
               h3 {font-family: 'Roboto Condensed', sans-serif;}  
               h4 {font-family: 'Roboto Condensed', sans-serif;}  
               h5 {font-family: 'Roboto Condensed', sans-serif;}  
               h6 {font-family: 'Roboto Condensed', sans-serif;}")    
  ),
  

  # Intro of MRT bin calcon LHS ---------------------------------------------
  sidebarPanel(
    
    ### Purpose ###
    
    h3("Overview"),
    p("This applet provides a sample size calculator for Micro-randomized 
      Trials with binary outcomes. The sample size formula is developed in",
      a("Sample Size Considerations for Micro-Randomized Trials with 
        Binary Outcome", 
        href="https://sites.google.com/view/tianchen-qian/research"),
      ", which is currently a work in progress. Please contact Tianchen Qian 
      (t.qian@uci.edu) 
      if you would like a copy of the draft manuscript",
      "or if you have any questions.",
      "An R package with some additional functionality is called mrtbincalc",
      "and can be downloaded from",
      a("CRAN", href = "https://cran.r-project.org")),
    
    ### Inputs Required ###
    
    h3("Required Inputs"),
    tags$div(strong("1. Study Setup"),
             tags$ul(
               tags$li("Duration of the study in days"),
               tags$li("Number of decision points within each day."),
               tags$li("Randomization probability, i.e. the probability of 
                        assigning the treatment at a decision time point.
                        This applet only allows constant randomization probability.
                        Use the R package mrtbincalc if you have randomization probability
                        that changes over the course of the study.")
               
             )
    ),
    
    tags$div(strong("2. Availability"),
             tags$ul(
               tags$li("Treatment can only be provided when an indivudual is 
                        available. The 
                        expected availability is the probability a person is 
                        available to receive the intervention at the decision 
                        times."),
               tags$li("You need to select a time-varying pattern for the 
                        expected availability. There are 4 patterns you can 
                        choose from: constant, linear, time-varying depending
                        on days in the study, or time-varying depending on the
                        decision points in the study. For the latter two options,
                        you can upload a file for the expected availability over time.")
             ),
    tags$div(strong("3. Success Probability Null Curve"),
             tags$ul(
               tags$li("The Success Probability Null Curve at each 
                        decision point is defined as the probability of 
                        the proximal outcome equal to 1 for available 
                        individuals who are not assigned treatment."),
               tags$li("You need to provide the trend of success probability 
                        null curve. This could be either constant, log-linear, or 
                        log-quadratic over decision points."),
               tags$li("The shape of the success probability null curve needs to
                       be at least as complex as the shape of the proximal treatment
                       effect to have accurate results. For example, if the 
                       shape of the proximal treatment effect is constant,
                       then the shape of the success probability null curve can be
                       either constant, log-linear, or log-quadratic. However, if the
                       shape of the proximal treatment effect is log-linear,
                       then the shape of the success probability null curve has to be
                       either log-linear or log-quadratic, but it cannot be constant.")
                      )
             ),
    tags$div(strong("4. Proximal Treatment Effect"), 
             tags$ul(
               tags$li("The Proximal Treatment Effect (on the relative risk scale)
                        at each decision 
                        point is defined as the ratio between the proximal 
                        outcome of available individuals who are assigned a 
                        treatment versus available individuals who are not assigned 
                        treatment."),
               tags$li("You need to provide the trend of proximal treatment 
                        effects. This could be either constant, log-linear, or 
                        log-quadratic over decision points.")
               )
             )
             
    ),
    
    h3("Outputs"),
    tags$ul(
      tags$li("We provide both power and sample size calculation
               (we calculate one if you provide the other). In both 
               cases, you will need to input the desired significance level."),
      tags$li("Pressing the history button will display a list of powers and/or 
               sample sizes calculated during the current session.")
    ),
    
    br(),
    br()
  ),
  

  # Main panel on RHS -------------------------------------------------------
  mainPanel(
    tags$hr(),
    
    ### Study Setup ###
    
    h3("Study Setup"),
    br(),
    br(),
    fluidRow(
      column(3,
             numericInput("days",
                          label = "Duration of the Study (Days)",
                          value = 30)
      ),
      column(3,
             numericInput("occ_per_day",
                          label = "Number of Decision Time Points per Day",
                          value = 1)
      ),
      column(6,
             textOutput("setting_warning")  
      )                                     
    ),                                      
    br(),
    
    tags$hr(),
    
    #### Expected Availability ####
    fluidRow(
      column(6,
             h3("Expected Availability")),
      column(6,
             h3(paste("")))
    ),
    
    br(),
    br(),
    
    # Expected availability
    fluidRow(
      # choose from constant, linear, or user input
      column(
        5,
        selectizeInput(
          "avail_choices", 
          label = "Select one of the following patterns for the 
                   expected availability", 
          choices = list("Constant"                      = "constant",
                         "Linear"                        = "linear",
                         "Time-Varying: Days"            = "tv_days",
                         "Time-Varying: Decision Points" = "tv_dec_pts"),
          options = list(placeholder  = "Please select a pattern",
                         onInitialize = I('function() { this.setValue(0); }')
                            )
          ),
        
        ### Inputs for constant pattern of expected availability ###
        conditionalPanel(
          condition = "input.avail_choices =='constant' ",
          sliderInput(
            "avail_constant_mean",
            label = "Average of Expected Availability", 
            min = 0, max = 1,value = 0.7)
          ),
             
        ### Inputs for linear pattern of expected availability ###
        conditionalPanel(
          condition = "input.avail_choices == 'linear' ",
          sliderInput(
            "avail_linear_initial", 
            label = "Initial Value of Expected Availability", 
            min = 0, max = 1,value = 0.8),
          
          sliderInput(
            "avail_linear_final",
            label = "Final Value of Expected Availability", 
            min = 0, max = 1,value = 0.6
            )
          ),
        
        conditionalPanel(
          condition = "input.avail_choices == 'tv_days' ",
          fileInput(
            "file0",
            "Choose a .csv file of time-varying expected availabilitys (Days) 
             to upload", 
            accept = c('.csv')
            ),
          
          # explanation of how to use/what this setting means
          p("With this setting, the expected availability for each decision 
             point on a given day will be the same across all decision points within that day
             (if there are multiple decision points per day), but the expected
             availability can vary across days. If you want a template of a 
             .csv file, you can first download the template, revise the numbers, and then 
             upload it."
            ),
          
          downloadButton("ea_days_template", "Template"),
          
          p("In the template file, the expected availability is contantly 0.7"),
          p("The number of inputs for this file should be equal to the number 
             of days."),
          p("Showing the first 5 rows of the uploaded file."),
          
          dataTableOutput("ea_inter_table_days")
          ),
             
        conditionalPanel(
          condition = "input.avail_choices == 'tv_dec_pts' ",
          
          fileInput(
            "file0a",
            "Choose a .csv file of time-varying expected availability 
             (Decision Times) to upload",
            accept = c('.csv')
            ),
          
          p("If you want a template of a .csv file, you can first download this 
             template, revise the numbers, then upload it."),
          
          downloadButton("ea_dec_pts_template", "Template"),
          
          p("In the template file, the expected availability is contantly 0.7."),
          p("The number of inputs for this file should be equal to the number 
             of decision points."),
          p("Showing the first 5 rows of the uploaded file."),
          
          dataTableOutput('ea_inter_table_dec')
          ),
             
             
        ### Comments on constant pattern of expected availability ###
        conditionalPanel(
          condition = "input.avail_choices == 'constant'",
          p(em("Notes: "),
          " A simplistic constant availability pattern.")
                              
             ),
        
        # ### Comments on linear pattern of expected availability ###
        # conditionalPanel(
        #   condition = "input.avail_choices == 'linear'",
        #   
        #   p(em("Notes: "), 
        #     "A linearly increasing pattern of expected availability might be 
        #      used if participants will find the intervention useful and thus 
        #      more likely to turn the intervention on"),
        #   
        #   p("A linearly decreasing pattern of expected availability might be 
        #      used if participants learn more about the intervetion and get 
        #      bored through the course of the study and thus getting less likely 
        #     to turn on the invervention.")
        #   ),
      ),
      column(1),
      column(
        6,
        conditionalPanel(
          condition = "['constant', 'linear'].includes(input.avail_choices)",
          plotOutput("avail_graph"))
        )
    ),
    tags$hr(),

    
    
    
    
    
    ### Randomization Probability ####
    fluidRow(
      column(6,
             h3("Randomization Probability")),
      column(6,
             h3(paste("")))
    ),
    br(),
    br(),
    fluidRow(
      column(
        5,
        selectizeInput(
          "rand_prob_choices",
          label = "Select one of the following patterns for the randomization 
                   probability",
          choices = list("Constant"                      = "constant",
                         "Time-Varying: Days"            = "tv_days",
                         "Time-Varying: Decision Points" = "tv_dec_pts"),
          options = list(placeholder  = "Please select a pattern",
                         onInitialize = I('function() { this.setValue(0); }'))
          ),
             
        ### Inputs for constant pattern of expected availability ###
        conditionalPanel(
          condition = "input.rand_prob_choices =='constant' ",
          
          numericInput(
            "rand_prob_const",                               
            label = "Constant Randomization Probability",
            value = 0.4 ),
          
          textOutput("setting_warning_ranPro") 
          ),

        ### Inputs for linear pattern of expected availability ###
        conditionalPanel(
          condition = "input.rand_prob_choices == 'tv_days' ",
          
          fileInput(
            "file1",
            "Choose a .csv file of time-varying randomization probability 
             (Days) to upload",
                                        
            accept = c('.csv')
            ),
          
          # explanation of how to use/what this setting means
          p("With this setting randomization probabilities for decision points 
             on the same day will be the same (if there are multiple decision
             points for each day), but randomization probabilities 
             across days can differ. If you want a template of .csv file, you 
             can first download the template, revise the numbers, and then upload it."
            ),
          
          downloadButton("days_template", "Template"),
          
          p("In the template file, the randomization probability is contantly 
             0.4"),
          p("The number of inputs for this file should be equal to the number 
             of days."),
          p("Showing the first 5 rows of the uploaded file."),
          
          dataTableOutput("P_inter_table_days")
          ),

        ### Inputs for linear pattern of expected availability ###
        conditionalPanel(
          condition = "input.rand_prob_choices == 'tv_dec_pts' ",
          
          fileInput(
            "file2",
            "Choose a .csv file of time-varying randomization probability 
             (Decision Times) to upload",
            accept = c('.csv')
            ),
          
          p("If you want a template of .csv file, you can first download the 
             template, revise the numbers, and then upload it."
            ),
          
          downloadButton("dec_pts_template", "Template"),
          
          p("In the template file, the randomization probability is contantly 
             0.4"),     
          p("The number of inputs for this file should be equal to the number 
             of decision times."),
          p("Showing the first 5 rows of the uploaded file."),
          
          dataTableOutput('P_inter_table_dec')
          ),
      )
    ),
    
    tags$hr(),
    
    
    #### Specifying trend for Success Probability Null Curve ####
    
    fluidRow(
      column(
        6,
        h3("Success Probability Null Curve"))
        ),
    
    br(),
    br(),
    
    fluidRow(
      column(
        5,
        ### Three trends of success probability null curve to choose from
        ### constant, loglinear, logquadratic
        selectizeInput(
          "alpha_choices", 
          label = "Select one of the following shapes for the success 
                   probability null curve", 
          choices = list("Constant"      = "constant",
                         "Log-linear"    = "loglinear",
                         "Log-quadratic" = "logquadratic"),
          options = list(placeholder  = "Please select a trend",
                         onInitialize = I('function() { this.setValue(0); }'))),
        
        ### Inputs for constant trend of success probability null curve ###
        conditionalPanel(
          condition = "input.alpha_choices =='constant'",
          
          sliderInput(
            "alpha_constant_mean", 
            label = "Average of Success Probability Null Curve",
            min = 0, max = 1, value = 0.5),
          
          p(em("Caution: The shape here needs to be at least as \"complex\" as the shape
               for proximal treatment effect below. See the left panel for more explanation.")),
          
          p(em("Note"),
            ": The success probability under no treatment stays constant over 
             the study. Your input will be this constant."
            )),
        
        ### Inputs for loglinear trend of success probability null curve ###
        conditionalPanel(
          condition = "input.alpha_choices == 'loglinear' ",
          
          sliderInput(
            "alpha_loglinear_initial", 
            label = "Initial Value of Success Probability Null Curve", 
            min = 0, max = 1,value = 0.6),
          
          sliderInput(
            "alpha_loglinear_final",
            label = "Final Value of Success Probability Null Curve", 
            min = 0, max = 1,value = 0.4),
          
          p(em("Caution: The shape here needs to be at least as \"complex\" as the shape
               for proximal treatment effect below. See the left panel for more explanation.")),
          
          p(em("Note"),
            ": The success probability under no treatment changes log-linearly over 
             the course of the study. Your input includes the initial value (i.e., the success probability 
            under no treatment at the first decision point) and the final value (i.e.,
            the success probability under no treatment at the last decision point)."),
          
          p("The log-linearly increasing pattern of a success probability null 
             curve might be used if participants are expected to build up habit
             over time and more likely to have a successfull proximal outcome (outcome=1)
             later on in the study (compared to early on in the study) even
             when no treatment is provided."),
          
          p("The log-linearly decreasing pattern of a success probability null 
            curve might be used if participants are less likely to have a 
            successfull proximal outcome (outcome=1) later on in the study 
            (compard to early on in the study) when no treatment is provided."),
        ),
             
        ### Inputs for logquadratic trend of success probability null curve ###
        conditionalPanel(
          condition = "input.alpha_choices == 'logquadratic' ",
          
          sliderInput(
            "alpha_logquad_initial", 
            label = "Initial Value of Success Probability Null Curve", 
            min = 0, max = 1,value = 0.4),
          
          sliderInput(
            "alpha_logquad_change_val",
            label = "Change Point Value of Success Probability Null Curve", 
            min = 0, max = 1,value = 0.6),
          
          numericInput(
            "alpha_logquad_change_pt",
            label = "Change point of Success Probability Null Curve", 
            value = 10),

          p(em("Caution: The shape here needs to be at least as \"complex\" as the shape
               for proximal treatment effect below. See the left panel for more explanation.")),
          
          p(em("Note"),
            ": The success probability under no treatment changes log-quadratically over 
             the course of the study. Your input includes the initial value (i.e., the success probability 
            under no treatment at the first decision point), the success probability at the 
            change point of the log-quadratic curve, and the change point location (in terms of
            decision point) of the log-quadratic curve."),
          
          p("This can be used when it is expected that the success probability
            under no treatment will increase (decrease) at first then decrease (increase) later on
            in the study."),
          
        )
      ),
      column(1),
      column(
        6,
        conditionalPanel(
          condition =
            "['constant', 'loglinear', 'logquadratic'].
             includes(input.alpha_choices)",
          plotOutput("alpha_graph"))
      )
      
    ),
    
    tags$hr(),
    
    
    #### Specifying trend for Proximal Treatment Effect ####
    
    fluidRow(
      column(6,
             h3("Proximal Treatment Effect (relative risk)"))
    ),
    
    br(),
    br(),
    
    fluidRow(
      column(
        5,
        ### Three trends of proximal treatment effect to choose from
        ### quadratic, constant, linear
        selectizeInput(
          "beta_choices", 
          label = "Select one of the following shapes for the proximal 
                   treatment effect", 
          choices = list("Constant"      = "constant",
                         "Log-linear"    = "loglinear",
                         "Log-Quadratic" = "logquadratic"),
          options = list(placeholder = "Please select a trend",
                         onInitialize = I('function() { this.setValue(0); }'))
          ),
             
        ### Inputs for constant trend of proximal treatment effect ###
        conditionalPanel(
          condition = "input.beta_choices =='constant'",
          
          sliderInput(
            "beta_constant_mean", 
            label = "Average of Proximal Treatment Effect",
            min = 1, max = 3.0, value = 1.2, step = 0.01),
            p(em("Notes"),
              ": The proximal treatment effect (relative risk) stays constant over the study.
              Your input will be this constant.")
          ),
        
        ### Inputs for loglinear trend of proximal treatment effect ###
        conditionalPanel(
          condition = "input.beta_choices == 'loglinear' ",
          
          sliderInput(
            "beta_loglinear_initial", 
            label = "Initial Value of Proximal Treatment Effect",
            min = 0, max = 3.0, value = 1.3, step = 0.01),
          
          sliderInput(
            "beta_loglinear_final",
            label = "Final Value of Proximal Treatment Effect",
            min = 0, max = 3.0, value = 1.1, step = 0.01),
          
          p(em("Note"),
            ": The proximal treatment effect (relative risk) changes log-linearly over 
             the course of the study. Your input includes the initial value (i.e., the 
            proximal treatment effect at the first decision point) and the final value (i.e.,
            the proximal treatment effect at the last decision point)."),
          
          p("The log-linearly increasing pattern of a proximal treatment effect 
             might be used if participants will get more enthusiastically 
             engage in the apps and thus the proximal treatment effect will 
             increase over time."),
          
          p("The log-linearly decreasing pattern of a proximal treatment effect 
             might be used if participantsare likely to disengage with the app
             (e.g., due to habituation), and thus the proximal treatment effect
             will decrease over time.")
          ),
             
        ### Inputs for logquadratic trend of proximal treatment effect ###
        conditionalPanel(
          condition = "input.beta_choices == 'logquadratic' ",
          
          sliderInput(
            "beta_logquad_initial", 
            label = "Initial Value of Proximal Treatment Effect", 
            min = 0, max = 3.0, value = 1.1, step = 0.01),
          
          sliderInput(
            "beta_logquad_change_val",
            label = "Change Point Value of Proximal Treatment Effect", 
            min = 0, max = 3.0, value = 1.3, step = 0.01),
          
          numericInput(
            "beta_logquad_change_pt",
            label = "Change point of Proximal Treatment Effect", 
            value = 15),
          
          p(em("Note"),
            ": The proximal treatment effect (relative risk) changes log-quadratically over 
             the course of the study. Your input includes the initial value (i.e., the
            proximal treatment effect at the first decision point), the proximal treatment effect at the 
            change point of the log-quadratic curve, and the change point location (in terms of
            decision point) of the log-quadratic curve."),
          
        )
      ),
      
      column(1),
      column(
        6,
        conditionalPanel(
          condition = 
            "['constant', 'loglinear', 'logquadratic'].
             includes(input.beta_choices) && 
             ['constant', 'loglinear', 'logquadratic'].
             includes(input.alpha_choices)",
          
          plotOutput("beta_graph"))
      )
      
    ),
    
    tags$hr(),
    
    
    
    ### Specifying sample size or the power ###
    fluidRow(
      column(
        4,
        ### Choices to choose from "sample size" or "power" ###
        radioButtons(
          "radio_choices", 
          label    = "Would you like to calculate sample size or power?", 
          choices  = list("Sample Size" = "choice_sample_size",
                         "Power"        = "choice_power"),
          selected = "choice_sample_size")
      ),
      column(
        4,
             
        ### type in power for sample size calculation ###
        conditionalPanel(
          condition = "input.radio_choices=='choice_sample_size'",
          
          numericInput(
            "power", 
            label = HTML("Desired Power"), 
            value = 0.8)
          ),
             
        ### type in sample size if you want to calculate the power attained ###
        conditionalPanel(
          condition = "input.radio_choices=='choice_power'",
          
          numericInput(
            "sample_size", 
            label = HTML("Number of Participants"), 
            value = 40)
          ),
             
        ### type in significance level for both cases ###
        numericInput(
          "sig_level", 
          label = HTML("Significance Level"), 
          value = 0.05)
             
      ),
      
      column(
        4,
             
        ### Output warnings for wrong format in "desired power" ###
        conditionalPanel(
          condition = "input.radio_choices=='choice_sample_size'",
          
          textOutput("choice_sample_size_warning")
          ),
             
        ### Output warnings for wrong format in "Number of participants" ###
        conditionalPanel(
          condition = "input.radio_choices=='choice_power'",
          
          textOutput("choice_power_warning")
          ),
             
        ### Output warnings for wrong format in "Significance level" ###
        textOutput("significance_warning")
      )
    ),
    
    tags$hr(),
    
    ##### choice to calculate sample size(action buttons) #####
    
    tabsetPanel(
      tabPanel(
        "Current Result",
        
        conditionalPanel(
          condition = "input.radio_choices == 'choice_sample_size'",
          
          actionButton("button_calculate_sample_size", "Result"),
          
          uiOutput("sample_size"),
          
          plotOutput("power_vs_n1"),
          
          dataTableOutput("power_summary1")
          ),
        
        conditionalPanel(
          condition = "input.radio_choices == 'choice_power'",
          
          actionButton("button_calculate_power", "Result"),
          
          uiOutput("power"),
          
          plotOutput("power_vs_n2"),
          
          dataTableOutput("power_summary2")                                      
          ),
        ),
      tabPanel(
        "History",
        
        conditionalPanel(
          condition = "input.radio_choices == 'choice_sample_size'",
          
          dataTableOutput("sample_size_history_table"),
          
          uiOutput('download_ss')
          ),
               
               
        conditionalPanel(
          
          condition = "input.radio_choices == 'choice_power'",
          
          dataTableOutput("power_history_table"),
          
          uiOutput('download_pow')
          )
        )
    ),
    
    br(),
    br(),
    br(),
    br(),
    br()
    
  ),
  
  
  #### A TOOLTIPS: to give you tips for typing in numeric inputs. ####
  
  
  
  bsTooltip(id = "days", 
            title = "integer greater than 0",
            placement="right", 
            trigger = "focus"),
  
  bsTooltip(id = "occ_per_day", 
            title = "integer greater than 0",
            placement="right", 
            trigger = "focus"),
  
  bsTooltip(id = "rand_prob_const", 
            title = "Input must range from 0-1",
            placement="right", 
            trigger = "focus"),
  
  bsTooltip(id = "sig_level", 
            title = "Input must range from 0-1",
            placement="right", 
            trigger = "focus"),
  
  bsTooltip(id = "power", 
            title = "Input must range from 0-1",
            placement="right", 
            trigger = "focus"),
  
  bsTooltip(id = "beta_quadratic__initial", 
            title = "Input must be greater than or equal to 0, and less than or 
                     equal to Average Standardized Effect",
            placement="right", 
            trigger = "focus"),
  
  bsTooltip(id = "beta_quadratic__max", 
            title = "Input must be integer greater than 0",
            placement="right", 
            trigger = "focus"),
  
  bsTooltip(id = "sample_size", 
            title = "Input must be integer greater than 1",
            placement="right", 
            trigger = "focus"),
  
  bsTooltip(id = "quadratic_max", 
            title = "Input must be integer greater than 0",
            placement="right", 
            trigger = "focus"),
  
  collapsable = TRUE,
  footer = HTML("<p style = 'font-size:12px'> Please direct correspondence to 
                 <a href='mailto:t.qian@uci.edu'>t.qian@uci.edu</a></p>")
  
))
