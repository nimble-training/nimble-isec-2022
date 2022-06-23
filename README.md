#  nimble-isec-2022

This repository will contain materials for the virtual NIMBLE workshop
for the 2022 ISEC meeting, 25 June 2022, 13:00-17:00, South Africa
time (04:00-08:00 California time).  The online location is set up via the ISEC meeting.

To prepare for the workshop:

 - Install NIMBLE (see below)
 - Install additional packages (see below)
 - Download these materials.  They should be complete by the morning
   of 24 June 2022.

All materials for the workshop will be in this GitHub repository. If you're familiar with Git/GitHub, you already know how to get all the materials on your computer. If you're not, go [here](https://github.com/nimble-training/nimble-isec-2022), click the (green) "Code" button, and choose the "Download ZIP" option.

This workshop is organized into five modules:

1. Overview of `nimble` using a capture-recapture example
2. Extending models and methods in NIMBLE.
3. Model selection and related topics in NIMBLE, with a worked
   meta-analysis example.
4. Overview of `nimbleEcology` with a worked hidden Markov model (HMM)
example.
5. (If time permits) Introduction to `nimbleSCR` for spatial capture-recapture, with a worked example.

## Background for the workshop

This workshop will focus on the `nimble` software per se, not the statistical models.  The material assumes attendees have basic knowledge of:

- ecological statistical models such as capture-recapture, occupancy, N-mixture, hidden Markov, and spatial capture-recapture.
- writing models in the "BUGS language", of which variants are in WinBUGS, OpenBUGS, JAGS, and NIMBLE.

Without this background, you will still be able to follow the workshop, but we will not have time to fill in basics of these topics.

## Help with NIMBLE

Our user manual is [here](https://r-nimble.org/html_manual/cha-welcome-nimble.html).

We have a 'cheatsheet' [here](https://r-nimble.org/documentation).

For those of you who are not already familiar with writing models in WinBUGS, JAGS, or NIMBLE, you may want to look through the first module (Introduction to NIMBLE) or Section 5.2 of our user manual in advance.

We're happy to answer questions about writing models as we proceed through the workshop, but if you have no experience with it, reviewing in advance will greatly lessen the odds you feel lost right at the beginning.

## Installing NIMBLE

NIMBLE is an R package on CRAN, so in general it will be straightforward to install as with any R package, but you do need a compiler and related tools on your system.

In summary, here are the steps.

1. Install compiler tools on your system. [https://r-nimble.org/download](https://r-nimble.org/download) will point you to more details on how to install *Rtools* on Windows and how to install the command line tools of *Xcode* on a Mac. Note that if you have packages requiring a compiler (e.g., *Rcpp*) on your computer, you should already have the compiler tools installed.

2. Install the *nimble* package from CRAN in the usual fashion for an R package (e.g. `install.packages("nimble")`). More details (including troubleshooting tips) can also be found in Section 4 of the [NIMBLE manual](https://r-nimble.org/html_manual/cha-installing-nimble.html).

3) To test that things are working please run the following code in R:

```
library(nimble)
code <- nimbleCode({
  y ~ dnorm(0,1)
})
model <- nimbleModel(code)
cModel <- compileNimble(model)
```

If that runs without error, you're all set. If not, please see the troubleshooting tips.  The most common problems have to do with the compiler tools.  On Windows, the PATH must be set (see link to Rtools installation details from our download linked above).  On OS X, command line tools must be installed as part of Xcode.  If you remain stuck, please email the [nimble-users group](https://r-nimble.org/more/issues-and-groups) for help.

In general we encourage you to update to the most recent version of NIMBLE, 0.12.2.

## Installing additional packages

Some of the packages we will use (beyond those automatically installed with `nimble`) can be installed as follows:

```
install.packages(c("mcmcplots",  "coda", "nimbleEcology", "nimbleSCR"))
```

`compareMCMCs` is a package that is not yet on CRAN (but will be soon):

```
library(remotes)
install_github("nimble-dev/compareMCMCs", subdir = "compareMCMCs")
```

Windows users will probably need to use this invocation:

```
library(remotes)
install_github("nimble-dev/compareMCMCs", subdir = "compareMCMCs", INSTALL_opts = "--no-multiarch")
```
