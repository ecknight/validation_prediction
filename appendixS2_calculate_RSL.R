#' ---
#' title: "Appendix S2 - Relative Sound Level (RSL) Measurement"
#' author: "Elly C. Knight"
#' date: "February 12, 2021"
#' output: pdf_document
#' ---
#'
#'From: Knight, E.C., Sòlymos, P, Scott, C., and Bayne, E.M. 2020. Validation prediction: a flexible protocol to increase efficiency of automated acoustic processing for wildlife research. Ecological Applications.
#'
#'NOTE: CODE BUG IN PUBLISHED VERSION FIXED. Rescale=FALSE in bandpass filter.
#' # Preamble
#'
#+ echo=FALSE
knitr::opts_chunk$set(eval = FALSE)
#' Load required R packages (install 1st if not yet available)
pkgs <- c(
  "tidyverse", #for data wrangling
  "tuneR", #for reading wav files
  "seewave") #for RSL measurement

inst <- rownames(installed.packages())
needed_pkgs <- setdiff(pkgs, inst)
if (length(needed_pkgs) > 0L)
  install.packages(needed_pkgs,
                   repos = "https://cloud.r-project.org/")
for (i in pkgs)
  library(i, character.only = TRUE)
#'
#' # Get list of clips to measure
#' 
#' Set working directory before you run this! 
files <- list.files(pattern = "*.wav")
#'
#' # Measure RSL
#' 
#' Set minimum and maximum frequency for bandpass filter (optional - comment out in loop below if not using)
minfreq <- 2000
maxfreq <- 10000
#' 
dB <- data.frame()
for(file in files){
  wave <- readWave(filename=file) #read in clip as wav
  wave <- ffilter(wave, f=44100, from=minfreq, to=maxfreq,
                  rescale=FALSE, output="Wave") #apply bandpass filter
  envelope <- env(wave, plot = FALSE) #create envelope function
  envelope.dB <- 20*log(envelope, base = 10) #convert to dB
  dB <- data.frame(RSL=max(envelope.dB), file=file) %>% 
    rbind(dB) #take max of dB and write to dataframe
  print(paste0("Measured file ", file)) #print message when complete
}
#'
#' # Save results
#'
write.csv(dB, "RSLMeasurements.csv", row.names = FALSE)
