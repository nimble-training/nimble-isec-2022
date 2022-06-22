## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE,
                      cache = TRUE)
library(nimble)
library(coda)


## ----load_dipper--------------------------------------------------------------
dipper_example_dir <- file.path("..", "..", "content", "examples","dipper")
dipper <- read.csv(file.path(dipper_example_dir,"dipper.csv"))
y <- as.matrix(dipper[ , 1:7])
y <- y + 1 # Code as 1 = not captured, 2 = captured.
first <- apply(y, 1, function(x) min(which(x != 1))) # first capture time
y <- y[ first != 7, ] # remove records with first capture on last occasion
head(y)


## ----dipper_code_dcat---------------------------------------------------------
dipper_code_dcat <- nimbleCode({
  phi ~ dunif(0, 1) # prior survival
  p ~ dunif(0, 1) # prior detection
  # likelihood
  gamma[1,1:2] <- c(phi, 1-phi)      # Pr(alive t -> alive t+1), Pr(alive t -> dead t+1)
  gamma[2,1:2] <- c(0, 1)            # Pr(dead t -> alive t+1), Pr(dead t -> dead t+1)
  delta[1:2] <- c(1, 0)              # Pr(alive t = 1) = 1, Pr(dead t = 1) = 0
  omega[1,1:2] <- c(1 - p, p)        # Pr(alive t -> non-detected t), Pr(alive t -> detected t)
  omega[2,1:2] <- c(1, 0)            # Pr(dead t -> non-detected t), Pr(dead t -> detected t)
  for (i in 1:N){
    z[i,first[i]] ~ dcat(delta[1:2]) # Illustrates initial state probabilities
    for (j in (first[i]+1):T){
      z[i,j] ~ dcat(gamma[z[i,j-1], 1:2])
      y[i,j] ~ dcat(omega[z[i,j], 1:2])
    }
  }
})


## ----setupInputs--------------------------------------------------------------
zinits <- matrix(2, nrow = nrow(y), ncol = ncol(y)) # create inits for unknown states
zdata <- matrix(NA, nrow = nrow(y), ncol = ncol(y)) # create data for known states
for(i in 1:nrow(zinits)) {
  known_alive <- range(which(y[i,] == 2))
  zinits[i, known_alive[1] : known_alive[2] ] <- NA # inits -> for known elements
  zdata[i, known_alive[1] : known_alive[2] ] <- 1   # data -> known values
}
dipper_constants <- list(N = nrow(y), 
                         T = ncol(y), 
                         first = first)
dipper_data <- list(y = y,
                    z = zdata)
dipper_inits <- function() list(phi = runif(1,0,1),
                                p = runif(1,0,1),
                                z = zinits)
head(dipper_data$z)     # data and inits have comlpementary
head(dipper_inits()$z)  # NAs 


## ---- eval=FALSE--------------------------------------------------------------
## nimbleCode({
##   # ...incomplete code snippet...
##   for(i in 1:2) sex_int[i] ~ dnorm(0, sd = 1000)  # prior for sex-specific intercepts
##   for(i in 1:num_farms) farm_effect[i] ~ dnorm(0, sd = farm_sd) # priors for farm random effects
##   farm_sd ~ dunif(0, 100)                         # prior for random effect std. dev.
##   for(i in 1:num_animals) {
##     logit(disease_probability[i]) <-              # logit link
##       sex_int[ sex[i] ] +                         # nested indexing of sex-specific intercepts
##       farm_effect[ farm_ids[i] ]                  # nested indexing of farm effects
##     y[i] ~ dbern(disease_probability[i])          # "likelihood"
##   }
##   # ...
## })


## ----nimbleModel, eval = TRUE, echo = TRUE------------------------------------
dipper_model <- nimbleModel(code = dipper_code_dcat,
                            constants = dipper_constants,
                            data = dipper_data,     # data can be set later.
                            inits = dipper_inits()  # inits can be set later.
                            )                       # dimensions is also a useful argument.


## ----configureMCMC, eval=TRUE-------------------------------------------------
dipper_MCMCconf <- configureMCMC(dipper_model, monitors = c("phi", "p")) # can be skipped if you don't plan to customize
dipper_MCMC <- buildMCMC(dipper_MCMCconf)


## ----compileNimble, eval = TRUE, echo = TRUE, message = TRUE------------------
C_dipper_model <- compileNimble(dipper_model) # These two lines can be done in one line.
C_dipper_MCMC <- compileNimble(dipper_MCMC, project = dipper_model)


## ----runMCMC, eval = TRUE, echo = TRUE, message = TRUE------------------------
samples <- runMCMC(C_dipper_MCMC, niter = 10000, samplesAsCodaMCMC = TRUE)
# Alternative:
# C_dipper_MCMC$run(1000)
# samples <- as.matrix(C_dipper_MCMC$mvSamples)
summary(samples)
plot(samples)


## ----nimbleModel2, eval = TRUE, echo = FALSE----------------------------------
# Rebuild the model here for safe knitr behavior
dipper_model <- nimbleModel(code = dipper_code_dcat,
                            constants = dipper_constants,
                            data = dipper_data,     # data can be set later.
                            inits = dipper_inits()  # inits can be set later.
                            )                       # dimensions is also a useful argument.
C_dipper_model <- compileNimble(dipper_model) # These two lines can be done in one line.


## ----model_demo, eval=TRUE----------------------------------------------------
class(dipper_model)[1]  # This is a reference class (S5) object
dipper_model$gamma           # Look at a model variable,
dipper_model$y[1:2, ]        # or part of one.
dipper_model$isData('gamma') # Query what is data
dipper_model$getNodeNames()[1:10]  # Query what are the nodes (vertices) in the graph,
dipper_model$getDependencies("z[1, 3]") # and what depends on what..
dipper_model$calculate()     # Calculate the entire model. Return sum of log probabilities.
dipper_model$calculate('z[1, 3]') # Calculate one or more nodes in the model.
dipper_model$calculate(dipper_model$getDependencies('z[1, 3]')) # Calculate based on model structure.
dipper_model$simulate("y", includeData = TRUE) # Simulate new data
head(dipper_model$y)
dipper_model$calculate("y")   # Calculate new sum of log probabilities
C_dipper_model$y <- dipper_model$y # The compiled model can be used in the same way
C_dipper_model$calculate()


## ---- eval=FALSE--------------------------------------------------------------
## nimbleCode({
##   tau <- 1E-0.6
##   mu ~ dnorm(0, tau)
## })


## ---- eval=FALSE--------------------------------------------------------------
## nimbleCode({
##   tau <- 1E-0.6
##   some_long_name_created_by_nimble <- 1/sqrt(tau) # a lifted node
##   mu ~ dnorm(0, sd = some_long_name_created_by_nimble)
## })


## ---- eval=FALSE--------------------------------------------------------------
## nimbleCode({
##   for(i in 1:n) y[i] ~ dnorm(a + b*x[i], sd = sigma)
## })


## ---- eval=FALSE--------------------------------------------------------------
## nimbleCode({
##   for(i in 1:n) {
##     some_long_name_generated_by_nimble[i] <- a + b*x[i] # lifted nodes
##     y ~ dnorm(some_long_name_generated_by_nimble[i], sd = sigma)
##   })


## ---- eval=TRUE---------------------------------------------------------------
# These steps would be done before buildMCMC
dipper_MCMCconf$printSamplers("phi")
dipper_MCMCconf$removeSamplers("phi")
dipper_MCMCconf$addSampler("phi", type = "slice")
dipper_MCMCconf$printSamplers("phi")

