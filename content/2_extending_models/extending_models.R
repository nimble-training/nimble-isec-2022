## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE,
                      cache = TRUE)
library(nimble)


## -----------------------------------------------------------------------------
dipper_code_dCJS <- nimbleCode({
  phi ~ dunif(0, 1) # survival prior
  p ~ dunif(0, 1)   # detection prior
  for (i in 1:N){
    y[i, first[i]:T] ~ dCJS(phi, p)
  }
})


## ----load_dipper--------------------------------------------------------------
dipper_example_dir <- file.path("..", "..", "content", "examples","dipper")
dipper <- read.csv(file.path(dipper_example_dir,"dipper.csv"))
y <- as.matrix(dipper[ , 1:7])
y <- y + 1 # Code as 1 = not captured, 2 = captured.
first <- apply(y, 1, function(x) min(which(x != 1))) # first capture time
y <- y[ first != 7, ] # remove records with first capture on last occasion
head(y)


## ----setupInputs--------------------------------------------------------------
dipper_constants <- list(N = nrow(y), 
                         T = ncol(y), 
                         first = first)
dipper_data <- list(y = y)
dipper_inits <- function() list(phi = runif(1,0,1),
                                p = runif(1,0,1))


## -----------------------------------------------------------------------------
dCJS_R <- function (x, probSurvive, probCapture, log = FALSE) {
  probAliveGivenHistory <- 1
  logProbData <- 0
  for (t in 2:length(x)) {
    probAlive <- probAliveGivenHistory * probSurvive
    if (x[t] == 1) {
      probThisObs <- probAlive * probCapture
      probAliveGivenHistory <- 1
    } else {
      probAliveNotSeen <- probAlive * (1 - probCapture)
      probThisObs <- probAliveNotSeen + (1 - probAlive)
      probAliveGivenHistory <- probAliveNotSeen/probThisObs
    }
    logProbData <- logProbData + log(probThisObs)
  }
  if (log) return(logProbData)
  return(exp(logProbData))
}


## -----------------------------------------------------------------------------
y[5,] # A good example capture history
dCJS_R(y[5,], probSurvive = 0.7, probCapture = 0.5, log = TRUE)


## -----------------------------------------------------------------------------
dCJS <- nimbleFunction(
  run = function(x = double(1),            # vector
                 probSurvive = double(0),  # scalar
                 probCapture = double(0),  # scalar
                 log = integer(0, default = 0)) {  # integer scalar
    returnType(double())  # scalar return, can be anywhere
    probAliveGivenHistory <- 1
    logProbData <- 0
    for (t in 2:length(x)) {
      probAlive <- probAliveGivenHistory * probSurvive
      if (x[t] == 1) {
        probThisObs <- probAlive * probCapture
        probAliveGivenHistory <- 1
      } else {
        probAliveNotSeen <- probAlive * (1 - probCapture)
        probThisObs <- probAliveNotSeen + (1 - probAlive)
        probAliveGivenHistory <- probAliveNotSeen/probThisObs
      }
      logProbData <- logProbData + log(probThisObs)
    }
    if (log) return(logProbData)
    return(exp(logProbData))
  }
)


## -----------------------------------------------------------------------------
dCJS(y[5,], probSurvive = 0.7, probCapture = 0.5, log = TRUE)


## ---- eval=FALSE--------------------------------------------------------------
## debugonce(dCJS)
## dCJS(y[5,], probSurvive = 0.7, probCapture = 0.5, log = TRUE)


## -----------------------------------------------------------------------------
dipper_code_dCJS <- nimbleCode({
  phi ~ dunif(0, 1) # survival prior
  p ~ dunif(0, 1)   # detection prior
  for (i in 1:N){
    y[i, first[i]:T] ~ dCJS(phi, p)
  }
})


## -----------------------------------------------------------------------------
dipper_model <- nimbleModel(code = dipper_code_dCJS,
                            constants = dipper_constants,
                            data = dipper_data,     # data can be set later.
                            inits = dipper_inits()  # inits can be set later.
                            )                       # dimensions is also a useful argument.


## -----------------------------------------------------------------------------
dipper_model$calculate()


## ---- eval=FALSE--------------------------------------------------------------
## debugonce(dCJS)
## dipper_model$calculate("y[5,]")


## -----------------------------------------------------------------------------
dipper_MCMC <- buildMCMC(dipper_model)


## ---- eval=FALSE--------------------------------------------------------------
## debug(dCJS)
## dipper_MCMC$run(niter = 5)
## undebug(dCJS)


## -----------------------------------------------------------------------------
C_dCJS <- compileNimble(dCJS)


## -----------------------------------------------------------------------------
C_dCJS(y[5,], 0.7, 0.5, log=TRUE)


## ---- echo=FALSE--------------------------------------------------------------
dipper_model <- nimbleModel(code = dipper_code_dCJS,
                            constants = dipper_constants,
                            data = dipper_data,     # data can be set later.
                            inits = dipper_inits()  # inits can be set later.
                            )                       # dimensions is also a useful argument.


## ---- echo=FALSE--------------------------------------------------------------
dCJS <- nimbleFunction(
  run = function(x = double(1),            # vector
                 probSurvive = double(0),  # scalar
                 probCapture = double(0),  # scalar
                 log = integer(0, default = 0)) {  # integer scalar
    returnType(double())  # scalar return, can be anywhere
    probAliveGivenHistory <- 1
    logProbData <- 0
    for (t in 2:length(x)) {
      probAlive <- probAliveGivenHistory * probSurvive
      if (x[t] == 1) {
        probThisObs <- probAlive * probCapture
        probAliveGivenHistory <- 1
      } else {
        probAliveNotSeen <- probAlive * (1 - probCapture)
        probThisObs <- probAliveNotSeen + (1 - probAlive)
        probAliveGivenHistory <- probAliveNotSeen/probThisObs
      }
      logProbData <- logProbData + log(probThisObs)
    }
    if (log) return(logProbData)
    return(exp(logProbData))
  }
)


## ---- eval=FALSE--------------------------------------------------------------
## C_dipper_model <- compileNimble(dipper_model)


## ---- eval=FALSE--------------------------------------------------------------
## C_dipper_model$calculate()
## C_dipper_model$phi <- 0.7
## C_dipper_model$p <- 0.5
## C_dipper_model$calculate() # Ensure any lifted nodes are calculated
## C_dipper_model$calculate('y[5,]')


## -----------------------------------------------------------------------------
dist_code <- nimbleCode({
  for(i in 1:num_animals) {
    for(j in 1:num_detectors) {
      dist2[i, j] <- (sxy[i,1] - detector_xy[j,1])^2 + (sxy[i,2] - detector_xy[j,2])^2
    } # sxy are individual activity centers. detector_xy and detector locations.
  }
})


## -----------------------------------------------------------------------------
dist_code_vec <- nimbleCode({
  for(i in 1:num_animals) {
    dist2[i, 1:num_detectors] <- (sxy[i,1] - detector_xy[1:num_detectors,1])^2 + (sxy[i,2] - detector_xy[1:num_detectors,2])^2
  }
})


## -----------------------------------------------------------------------------
dist_model <- nimbleModel(dist_code_vec, constants = list(num_animals = 2, num_detectors = 3 ))
dist_model$detector_xy <- matrix(rnorm(6), nrow = 3)
dist_model$sxy <- matrix(rnorm(4), nrow = 2)
dist_model$calculate()
dist_model$dist2


## ---- eval=FALSE--------------------------------------------------------------
## for(i in 1:num_animals) {
##   dist2[i, 1:num_detectors] <- calcDistances(sxy[i, 1:2], detector_xy[1:num_detectors, 1:2]) # You write calcDistances as a nimbleFunction
## }
## 


## -----------------------------------------------------------------------------
add2 <- function(x) {
  message("Hello from add2")
  x + 2 # A very complicated calculation
}


## -----------------------------------------------------------------------------
Radd2 <- nimbleRcall(
  function(x = double(1)){}, # Empty function to give type annotations
  Rfun = 'add2',             # name of R function
  returnType = double(1))    # return type


## -----------------------------------------------------------------------------
demoCode <- nimbleCode({
    for(i in 1:4) {x[i] ~ dnorm(0,1)} 
    z[1:4] <- Radd2(x[1:4])
})
demoModel <- nimbleModel(demoCode, inits = list(x = rnorm(4)))
CdemoModel <- compileNimble(demoModel)
CdemoModel$calculate()


## -----------------------------------------------------------------------------
RW_MH_demo <- nimbleFunction(
  name = 'RW_MH_demo',
  contains = sampler_BASE,
  setup = function(model, mvSaved, target, control) {
    ## Extract proposal scale from control list, defaulting to 1
    scale <- control$scale
    if(is.null(scale)) scale <- 1
    ## Query model structure
    calcNodes <- model$getDependencies(target)
  },
  run = function() {
    currentValue <- model[[target]]
    propValue <- rnorm(1, mean = currentValue,  sd = scale)
    model[[target]] <<- propValue
    logMHR <- model$calculateDiff(calcNodes)
    jump <- decide(logMHR)
    if(jump) {
      nimCopy(from = model, to = mvSaved, row = 1, nodes = calcNodes, logProb = TRUE)
    } else {
      nimCopy(from = mvSaved, to = model, row = 1, nodes = calcNodes, logProb = TRUE)
    }
  },
  methods = list(
    reset = function() {}
  )
)


## -----------------------------------------------------------------------------
toy_model <- nimbleModel(nimbleCode({x ~ dnorm(0,1)}))
toy_MCMCconf <- configureMCMC(toy_model, nodes = NULL)
# This choice of scale won't mix well, for illustration:
toy_MCMCconf$addSampler(target = "x", type = "RW_MH_demo", control = list(scale = 0.5))
toy_MCMC <- buildMCMC(toy_MCMCconf)
compiled <- compileNimble(toy_model, toy_MCMC)
samples <- runMCMC(compiled$toy_MCMC, niter = 1000)
plot(samples[,'x'], type = "l")

