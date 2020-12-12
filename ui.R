library(shiny)
library(shinyBS)

shinyUI(fluidPage(
  titlePanel("Sample Size Calculator for Micro-Randomized Trials with Binary Outcomes"),    
  
  ####Sample Size Calculator Simple version####
  
  ### Print error validation in "red" ###
  
  tags$head(
    tags$style(HTML("
    .shiny-output-error-validation {
      color: red;
    }
  "))
  ),
  
  ### CSS HEADER ###
  ### Apply style attributes across the application
  tags$head( 
    tags$style(type='text/css',"input[type='number'] {width:90px}"),   #set width of numericInputs 
    tags$style(type='text/css',includeHTML("www/css/bootstrap-modal.css")),   #responsive modals   
    tags$link(rel="stylesheet", href="//fonts.googleapis.com/css?family=Roboto|Roboto+Condensed"), 
    tags$style("body {font-family: 'Roboto', sans-serif;}  
                                       h1 {font-family: 'Roboto Condensed', sans-serif;}  
                                       h2 {font-family: 'Roboto Condensed', sans-serif;} 
                                       h3 {font-family: 'Roboto Condensed', sans-serif;}  
                                       h4 {font-family: 'Roboto Condensed', sans-serif;}  
                                       h5 {font-family: 'Roboto Condensed', sans-serif;}  
                                       h6 {font-family: 'Roboto Condensed', sans-serif;}")    #apply font styles 
  ),
  
  ### Introduction of MRT-SS Calculator on left-hand side panel ###
  sidebarPanel(
    
    ### Purpose ###
    
    h3("Purpose"),
    p("This applet provides a sample size calculator for Micro-randomized Trials with binary outcomes. The sample size formula is developed in",
      a("Sample Size Considerations for Micro-Randomized Trials with Binary Outcome", href="https://sites.google.com/view/tianchen-qian/research"),
      ", which is currently work in progress. Please contact Tianchen Qian (qiantianchen.thu@gmail.com) if you would like a copy of the draft manuscript."),
    
    ### Inputs Required ###
    
    h3("Required Inputs"),
    tags$div(strong("1. Study Setup"),
             tags$ul(
               tags$li("Duration of the study in days"),
               tags$li("Number of decision time points within each day."),
               tags$li("Randomization probability, i.e. the probability of assigning the treatment at a decision time point.
                     You can upload a file to specify the randomization probability for 
                     each decision time or day.")
               
             )
    ),
    tags$div(strong("2. Availability"),
             tags$ul(
               tags$li("Treatment can only be provided when an indivudual is available; see more details on availablity below.",
                       " The expected availability is the probability a person is available to receive the intervention at the decision times."),
               tags$li("You need to select a time-varying pattern for the expected availability. There are three patterns you can 
                     choose from: constant, or linear over decision points.")
             ),
             tags$div(strong("3. Success Probability Null Curve"),
                      tags$ul(
                        tags$li("The Success Probability Null Curve at each decision time point is defined as the probability",
                                " of the proximal outcome equal to 1 for available individuals who are not assigned treatment."),
                        tags$li("You need to provide the trend of success probability null curve.",
                                " This could be either constant or linear over decision points. ")
                      )
             ),
             tags$div(strong("4. Proximal Treatment Effect"),
                      tags$ul(
                        tags$li("The Proximal Treatment Effect at each decision time point is defined as the mean difference",
                                " in the proximal outcome between available people who are assigned a treatment versus available people",
                                " who are not assigned treatment. In this work, we only consider the binary treatment."),
                        tags$li("You need to provide the trend of proximal treatment effects.", 
                                " This could be either constant or time-varying, e.g. linear or quadratic over decision points.")
                      )
             )
             
    ),
    
    h3("Outputs"),
    tags$ul(
      tags$li("We provide both power and sample size calculation. In both cases, you will need to input the desired significance level."),
      tags$li("Pressing the history button will display a list of powers and/or sample sizes calculated during the present session")
      #                tags$li("When the output sample size is small, reconsider the followings: \n (1) whether you are correctly or conservatively guessing the average of expected availability, 
      #                       (2) is the duration of study too long, and (3) is the average of treatment effect too optimistic or the trend is too simple?    ")
    ),
    
    br(),
    br(),
    ### Background ###
    bsCollapse(multiple = FALSE, id = "background",
               bsCollapsePanel(h3("Background"),
                               p(strong("Just-in-time mobile interventions: ")), 
                               p("In just-in-time mobile interventions, treatments, provided via a mobile device,
                                      are intended to help an individual in the moment, such as engaging in an healthy behavior when an opportunity arises
                                      or successfully coping with a stressful event. The treatments are ususally intended to have a proximal, near-term impact on the individual."),
                               
                               p(strong("Micro-randomized Trials:")), 
                               p("In a micro-randomized trial, treatments are sequentially randomized throughout the conduct of the study,
                                       with the result that each participant may be randomized at hundreds or thousands of decision times. At a decision time, treatment options may correspond to
                                       whether or not a treatment is provided; for example, whether or not participant is provided a tailored activity suggestion. Or treatment options may be
                                       alternative types of treatment that can be provided in the same situation; for example, the activity planning treatment might have two options, asking the participant to create an activity plan for
                                       the next day or asking them to pick a plan from a list of previously created plans.  Considerations of treatment burden often imply that the randomization will not be uniform. For example, if treatment randomization probability is 0.4 and there are 5 decision time per day and if an participant is always
                                       available, activity suggestions are delivered on average 2 time points per day. "),
                               
                               p(strong("Availability")), 
                               p("At some decision times, feasibility, ethics or burden considerations mean that the pariticipant is unavailable for treatment, 
and thus treatment should not be delivered. For example, if sensors indicate that the participant is likely driving a car or 
the participantis currently walking, then the activity message should not be sent. Other examples of when participants are 
unavailable for treatment include, in the alcohol recovery setting, a ‘warning’ treatment would only be potentially provided
when sensors indicate that the participant is within 10 feet of a high-risk location or a treatment might only be provided 
if the participant reports a high level of craving.  Individuals may be unavailable for treatmentby choice. For example, 
the application on the phone should permit the user to turn off the activity messages for some amount of time; this option 
is considered critical to maintaining participant buy-in and engagement."),
                               
                               
                               p(strong("Additional Consideration:")), 
                               
                               
                               tags$div("1. In the input, we require the inputs of \"Duration of the Study (Days) \" and \"Number of Decision Time Points per Day\". The use of the word \"Day\" only means the treatment effect are constant within each \"day\".  
                For example, suppose one is interested in conducting a 12-week study with one decision time per day and want to calculate the sample size under the conjecture that the treatment effects pretty much stay in the same level within each week. 
                 Then enter 12 and 7 under \"Duration of the Study (Days) \" and \"Number of Decision Time Points per Day\" respectively will serve the purpose."),
                               br(),
                               tags$div("2. When the output sample size is small, one might reconsider the followings: \n (1) whether you are correctly or conservatively guessing the average of expected availability, 
                                (2) whether the duration of study is too long, and (3) wehther the average of treatment effect is overestimated and the trend of treatment effects is too simplistic."),
                               br(),
                               tags$div("3. In this work, we only consider the case where proximal treatment effect within each day are identical and the trend of effects are constant, linear and quadratic. To calculate the sample size for general treatment effects, see the formula in Liao et.al (2016).  ")
                               
                               
                               
               ))
  ),
  
  ### Main Panel on the right-hand side ###
  mainPanel(
    tags$hr(),
    
    ### Study Setup ###
    
    h3("Study Setup"),
    br(),
    br(),
    fluidRow(
      column(3,
             numericInput("days",label = "Duration of the Study (Days)",value = 30)
      ),
      column(3,
             numericInput("occ_per_day",label = "Number of Decision Time Points per Day",value = 1)
      ),
      column(6,
             textOutput("setting_warning")  ### output warnings when you type in wrong format for
      )                                     ### "Durarion of the study" and "Number of decision time
    ),                                      ### per day"
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
    fluidRow(
      column(5,
             ### Three patterns of expected availability to choose from 
             ### quadratic, constant and linear
             selectizeInput("avail_choices", 
                            label = "Select one of the following patterns for the expected availability", 
                            choices=list("Constant"="constant",
                                         "Linear"="linear",
                                         "Time-Varying: Days"= "tv_days",
                                         "Time-Varying: Decision Points"="tv_dec_pts"),
                            options = list(
                              placeholder = "Please select a pattern",
                              onInitialize = I('function() { this.setValue(0); }')
                            )
             ),
             ### Inputs for constant pattern of expected availability ###
             conditionalPanel(condition="input.avail_choices =='constant' ",
                              sliderInput("avail_constant_mean",
                                          label="Average of Expected Availability", 
                                          min = 0, max = 1,value = 0.7)
             ),
             
             ### Inputs for linear pattern of expected availability ###
             conditionalPanel(condition="input.avail_choices == 'linear' ",
                              sliderInput("avail_linear_initial", 
                                          label = "Initial Value of Expected Availability", 
                                          min = 0, max = 1,value = 0.8),
                              sliderInput("avail_linear_final",
                                          label="Final Value of Expected Availability", 
                                          min = 0, max = 1,value = 0.6)
             ),
             conditionalPanel(condition="input.avail_choices == 'tv_days' ",
                              fileInput('file0',
                                        'Choose a .csv file of time-varying expected availabilitys (Days) to upload',
                                        accept = c('.csv')
                              ),
                              p('If you want a template of .csv file ,',
                                'you can first download the template and then try uploading them.'
                              ),
                              downloadButton("ea_days_template", "Template"),
                              p('In the sample file, the expected availability is contantly 0.7'),
                              p('The number of inputs for this file should be equal to the number of days.'),
                              p('Showing the first 5 rows of the uploaded file. '),
                              dataTableOutput('ea_inter_table_days')
             ),
             
             conditionalPanel(condition="input.avail_choices == 'tv_dec_pts' ",
                              fileInput('file0a',
                                        'Choose a .csv file of time-varying expected availabilitys (Decision Times) to upload',
                                        accept = c('.csv')
                              ),
                              p('If you want a template of .csv file ,',
                                'you can first download the template and then try uploading them.'
                              ),
                              downloadButton("ea_dec_pts_template", "Template"),
                              p('In the sample file, the expected availability is contantly 0.7'),
                              p('The number of inputs for this file should be equal to the number of days.'),
                              p('Showing the first 5 rows of the uploaded file. '),
                              dataTableOutput('ea_inter_table_dec_pts')
             ),
             
             
             ### Comments on constant pattern of expected availability ###
             conditionalPanel(condition="input.avail_choices == 'constant'",
                              p(em("Notes: "),
                                " A simplistic constant availability pattern.")
                              
             ),
             ### Comments on linear pattern of expected availability ###
             conditionalPanel(condition="input.avail_choices == 'linear'",
                              p(em("Notes: "), "A linearly increasing pattern of expected availability might be used if participants
                              will find the intervention useful and thus more likely to turn the intervention on"),
                              p(
                                "A linearly decreasing pattern of expected availability might be used if participants learn more about the intervetion
                              and get bored through the course of the study and thus getting less likely to turn on the invervention."
                              )
             ),
      ),
      column(1),
      column(6,
             conditionalPanel(condition="['constant', 'linear'].includes(input.avail_choices)",
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
      column(5,
             ### Three patterns of randomization probability to choose from
             selectizeInput("rand_prob_choices",
                            label = "Select one of the following patterns for the randomization probability",
                            choices=list("Constant"="constant",
                                         "Time-Varying: Days"="tv_days",
                                         "Time-Varying: Decision Points"="tv_dec_pts"),
                            options = list(
                              placeholder = "Please select a pattern",
                              onInitialize = I('function() { this.setValue(0); }')
                            )
             ),
             ### Inputs for constant pattern of expected availability ###
             conditionalPanel(condition="input.rand_prob_choices =='constant' ",
                              numericInput("rand_prob_const",
                                           label = "Constant Randomization Probability",
                                           value = 0.4 ),
                              textOutput("setting_warning_ranPro") ### Output warnings when you type in wrong format for
             ),

             ### Inputs for linear pattern of expected availability ###
             conditionalPanel(condition="input.rand_prob_choices == 'tv_days' ",
                              fileInput('file1',
                                        'Choose a .csv file of time-varying randomization probability (Days) to upload',
                                        accept = c('.csv')
                              ),
                              p('If you want a template of .csv file ,',
                                'you can first download the template and then try uploading them.'
                              ),
                              downloadButton("days_template", "Template"),
                              p('In the sample file, the randomization probability is contantly 0.4'),
                              p('The number of inputs for this file should be equal to the number of days.'),
                              p('Showing the first 5 rows of the uploaded file. '),
                              dataTableOutput('P_inter_table_days')
             ),

             ### Inputs for linear pattern of expected availability ###
             conditionalPanel(condition="input.rand_prob_choices == 'tv_dec_pts' ",
                              fileInput('file2',
                                        'Choose a .csv file of time-varying randomization probability (Decision Times) to upload',
                                        accept = c('.csv')
                              ),
                              p('If you want a template of .csv file,',
                                'you can first download the template and then try uploading them.'
                              ),
                              downloadButton("dec_pts_template", "Template"),
                              p('In the sample file, the randomization probability is contantly 0.4'),
                              p('The number of inputs for this file should be equal to the number of decision times.'),
                              p('Showing the first 5 rows of the uploaded file. '),
                              dataTableOutput('P_inter_table_dec')
             ),
      )
    ),
    tags$hr(),
    
    
    #### Specifying trend for Success Probability Null Curve ####
    
    fluidRow(
      column(6,
             h3("Success Probability Null Curve"))
      #       column(6,
      #              h3("","Alternative Hypothesis"))
    ),
    br(),
    br(),
    
    fluidRow(
      column(
        5,
             ### Three trends of success probability null curve to choose from
             ### constant, linear, loglinear
        selectizeInput(
          "alpha_choices", 
          label = "Select one of the following trends for the success probability null curve", 
          choices=list("Constant"="constant",
                       "Log-linear"="loglinear",
                       "Log-quadratic"="logquadratic"),
          options = list(placeholder = "Please select a trend",
                         onInitialize = I('function() { this.setValue(0); }'))),
             ### Inputs for constant trend of success probability null curve ###
        conditionalPanel(
          condition="input.alpha_choices =='constant'",
          sliderInput(
            "alpha_constant_mean", 
            label = "Average of Success Probability Null Curve",
            min = 0, max = 1, value = 0.5),
          p(em("Notes"),
            ": The success probability under no treatment stays constant over the study.")),
        
        ### Inputs for loglinear trend of success probability null curve ###
        conditionalPanel(
          condition="input.alpha_choices == 'loglinear' ",
          sliderInput(
            "alpha_loglinear_initial", 
            label = "Initial Value of Success Probability Null Curve", 
            min = 0, max = 1,value = 0.6),
          sliderInput(
            "alpha_loglinear_final",
            label="Final Value of Success Probability Null Curve", 
            min = 0, max = 1,value = 0.4),
          p(em("Notes"),": The loglinearly increasing form of a success probability null curve might be used if participants will get more enthusiastically engage in the apps and thus the success probability under no treatment will increase as the study goes."),
          p("The loglinearly decreasing form of a success probability null 
            curve might be used if participantsare likely to disengage the 
            activity suggestionss and thus the success probability under 
            no treatment will decrease as the study goes.")
        ),
             
             ### Inputs for logquadratic trend of success probability null curve ###
        conditionalPanel(
          condition="input.alpha_choices == 'logquadratic' ",
          sliderInput(
            "alpha_logquad_initial", 
            label = "Initial Value of Success Probability Null Curve", 
            min = 0, max = 1,value = 0.6),
          sliderInput(
            "alpha_logquad_change",
            label="Change point Value of Success Probability Null Curve", 
            min = 0, max = 1,value = 0.4),
          sliderInput(
            "alpha_logquad_final",
            label="Final Value of Success Probability Null Curve", 
            min = 0, max = 1,value = 0.4), 
          p(em("Notes"),
            ": The success probability under no treatment varies 
                 quadratically over the study.")
        )
      ),
      column(1),
      column(6,
             conditionalPanel(
               condition=
                 "['constant', 'loglinear', 'logquadratic'].includes(input.alpha_choices)",
               plotOutput("alpha_graph"))
      )
      
    ),
    
    tags$hr(),
    
    
    #### Specifying trend for Proximal Treatment Effect ####
    
    fluidRow(
      column(6,
             h3("Proximal Treatment Effect (relative risk)"))
      #       column(6,
      #              h3("","Alternative Hypothesis"))
    ),
    br(),
    br(),
    
    fluidRow(
      column(5,
             ### Three trends of proximal treatment effect to choose from
             ### quadratic, constant, linear
             selectizeInput("beta_choices", 
                            label = "Select one of the following trends for the proximal treatment effect", 
                            choices=list("Constant"="constant",
                                         "Log-linear"="loglinear",
                                         "Log-Quadratic"="logquadratic"),
                            options = list(
                              placeholder = "Please select a trend",
                              onInitialize = I('function() { this.setValue(0); }')
                            )
             ),
             ### Inputs for constant trend of proximal treatment effect ###
             conditionalPanel(condition="input.beta_choices =='constant'",
                              sliderInput("beta_constant_mean", 
                                          label = "Average of Proximal Treatment Effect",
                                          min = 0, max = 3.0, value = 1.2, step = 0.01),
                              p(em("Notes"),
                                ": The proximal treatment effect stays constant over the study.")
             ),
             ### Inputs for loglinear trend of proximal treatment effect ###
             conditionalPanel(condition="input.beta_choices == 'loglinear' ",
                              sliderInput("beta_loglinear_initial", 
                                          label = "Initial Value of Proximal Treatment Effect",
                                          min = 0, max = 3.0, value = 1.3, step = 0.01),
                              sliderInput("beta_loglinear_final",
                                          label="Final Value of Proximal Treatment Effect",
                                          min = 0, max = 3.0, value = 1.1, step = 0.01),
                              p(em("Notes"),
                              ": The loglinearly increasing form of a proximal treatment effect might be used if participants will get more enthusiastically engage in the apps and thus the proximal treatment effect will increase as the study goes."),
                              p("The loglinearly decreasing form of a proximal treatment effect might be used if participantsare likely to disengage the activity suggestionss and thus the proximal treatment effect will decrease as the  study goes.")
             )
      ),
      column(1),
      column(6,
             conditionalPanel(condition="['constant', 'loglinear'].includes(input.beta_choices)
                            && ['constant', 'loglinear'].includes(input.alpha_choices)",
                              plotOutput("beta_graph"))
      )
      
    ),
    
    tags$hr(),
    
    
    
    ### Specifying whether you are interested in calculating the sample size or the power ###
    fluidRow(
      column(4,
             
             ### Choices to choose from "sample size" or "power" ###
             radioButtons("radio_choices", label = "Are you interested in finding sample size or power?", 
                          choices=list("Sample Size"="choice_sample_size",
                                       "Power"="choice_power"),
                          selected = "choice_sample_size")
      ),
      column(4,
             
             ### type in the desired power if you want to calculate the sample size ###
             conditionalPanel(condition="input.radio_choices=='choice_sample_size'",
                              numericInput("power", 
                                           label = HTML("Desired Power"), 
                                           value = 0.8)
             ),
             
             ### type in the sample size if you want to calculate the power attained ###
             conditionalPanel(condition="input.radio_choices=='choice_power'",
                              numericInput("sample_size", 
                                           label = HTML("Number of Participants"), 
                                           value = 40)
             ),
             
             ### type in significance level for both cases ###
             numericInput("sig_level", 
                          label = HTML("Significance Level"), 
                          value = 0.05)
             
      ),
      column(4,
             
             ### Output warnings if you type in wrong format for "desired power" ###
             conditionalPanel(condition="input.radio_choices=='choice_sample_size'",
                              textOutput("choice_sample_size_warning")
             ),
             
             ### Output warnings if you type in wrong format for "Number of participants" ###
             conditionalPanel(condition="input.radio_choices=='choice_power'",
                              textOutput("choice_power_warning")
             ),
             
             ### Output warnings if you type in wrong format for "Significance level" ###
             textOutput("significance_warning")
      )
    ),
    
    tags$hr(),
    
    ##### choice to calculate sample size(action buttons) #####
    
    tabsetPanel(
      tabPanel("Current Result",
               conditionalPanel(condition = "input.radio_choices == 'choice_sample_size'",
                                actionButton("button_calculate_sample_size", "Result"),
                                uiOutput("sample_size"),
                                plotOutput("power_vs_n1"),
                                dataTableOutput("power_summary1")
               ),
               
               ##### choice to calculate sample size(action buttons) #####
               
               conditionalPanel(condition = "input.radio_choices == 'choice_power'",
                                actionButton("button_calculate_power", 
                                             "Result"),
                                uiOutput("power"),
                                plotOutput("power_vs_n2"),
                                dataTableOutput("power_summary2")                                      
               ),
               
      ),
      tabPanel("History",
               conditionalPanel(condition = "input.radio_choices == 'choice_sample_size'",
                                dataTableOutput("sample_size_history_table"),
                                uiOutput('download_ss')
               ),
               
               
               conditionalPanel(condition = "input.radio_choices == 'choice_power'",
                                dataTableOutput("power_history_table"),
                                uiOutput('download_pow')
               ))
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
            title = "Input must be greater than or equal to 0, and less than or equal to Average Standardized Effect",
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
  footer = HTML("<p style = 'font-size:12px'> Please direct correspondence to <a href='mailto:sunji@umich.edu'>sunji@umich.edu</a></p>")
  
))
