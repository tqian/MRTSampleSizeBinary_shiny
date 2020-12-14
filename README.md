# Binary Outcome Micro-Randomized Trial Sample Size/Power Calculator

This shiny app provides a sample size calculator for Micro-randomized Trials
with binary outcomes. The sample size formula is developed in ["Sample Size
Considerations for Micro-Randomized Trials with Binary
Outcome"](https://sites.google.com/view/tianchen-qian/research) by E. Cohn, T.
Qian, and S. Murphy. This is currently a work in progress; contact Tianchen Qian
(qiantianchen.thu@gmail.com) if you would like a copy of the draft manuscript.

Before using the calculator the user should have knowledge of the following:

1. **Study Setup**
The number of decision time points/duration of study and the randomization
probability, i.e. the probability of assigning the treatment at each decision
time point.

1. **Availability** 
Treatment can only be provided when an individual is available. The expected
availability is the probability a person is available to receive the
intervention at the decision times. User can select prespecified time-varying
pattern (constant, linear) or upload a .csv file  for the expected availability.

1. **Success Probability Null Curve** 
The success probability null curve is defined as the probability of the proximal
outcome being a success for available individuals who are not assigned
treatment. The user can define the trend of success probability null curve
within three families of options: constant, log-linear, and log-quadratic.

1. **Proximal Treatment Effect**
The proximal treatment Effect at each decision time point is defined as the mean
difference in the proximal outcome between available people who are assigned a
treatment versus available people who are not assigned treatment. In this work,
we only consider the binary treatment. The user can define trend of proximal
treatment effects within three families of options: constant, log-linear, and
log-quadratic.

This shiny app provides power and/or sample size information in the context of
micro-randomized trials with binary outcomes. In either cases, the user will
need to input the desired significance level. In addition to raw numbers, a
graph of power vs sample size is returned. A summary of the history of
computations performed in a session with this calculator are stored and
available to view and download.
