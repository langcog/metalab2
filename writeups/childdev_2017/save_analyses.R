## LOAD DATA AND PACKAGES ####
source("analyses/initial_data.R")


## RUN ANALYSES ####
source("analyses/sample_size.R")
source("analyses/power.R")
source("analyses/method.R")
source("analyses/p_values.R")
source("analyses/funnel.R")
source("analyses/bias.R")

## SAVE ENVIRONMENT FOR USE IN PAPER ####
save.image("educationpaper_environment.RData")
