#' ---
#' title: "Appendix S3 - Validation Prediction"
#' author: "Elly C. Knight and Peter Sòlymos"
#' date: "March 20, 2020"
#' output: pdf_document
#' ---
#'
#' From: Knight, E.C., Sòlymos, P, Scott, C., and Bayne, E.M. 2020. Validation prediction: a flexible protocol to increase efficiency of automated acoustic processing for wildlife research. Ecological Applications.
#'
#' # Preamble
#'
#+ echo=FALSE
knitr::opts_chunk$set(eval = FALSE)
#'
#' Settings some options
options(scipen = 999,
    "install.packages.compile.from.source" = "never")
#'
#' Load required R packages (install 1st if not yet available)
pkgs <- c(
    "tidyverse", #for data wrangling
    "stringr", #for data wrangling
    "colorspace", #for colour ramps
    "dismo", #for boosted regression trees
    "gbm", #for boosted regression tree prediction
    "lubridate", #for date manipulation
    "data.table", #to concatenate lists of dataframes
    "parallel", #for parallel computing
    "pbapply") #for progress bar

inst <- rownames(installed.packages())
needed_pkgs <- setdiff(pkgs, inst)
if (length(needed_pkgs) > 0L)
    install.packages(needed_pkgs,
        repos = "https://cloud.r-project.org/")
for (i in pkgs)
    library(i, character.only = TRUE)
#'
#' ## Settings
#'
#' Number of bootstrap runs
boot <- 100
#' Training sample sizes
fraction <- seq(1000, 16000, 1000)
#' Tree complexity
tc <- 3
#' Learning rates
lr <- c(0.1, 0.05, 0.01, 0.005)
#' ## Data wrangling
#'
#' Data will be available on Dryad upon manuscript acceptance
coni_read <- read.csv("UP_CONIpeent0mv2_20_20_results_validated.csv")
#' Get proper date formats, select relevant fields,
#' and filter the data set for our needs
coni <- coni_read %>%
    mutate(yday = yday(as.POSIXct(strptime(Date, format = "%Y-%m-%d")))) %>%
    dplyr::select(CONI,
        score,
        level,
        SunsetT,
        yday,
        recording,
        location) %>%
    filter(score >= 60) %>% 
  mutate(id = row_number())
#' Set aside testing data 
coni$CONI <- as.numeric(coni$CONI)
set.seed(999) #set seed to make it reproducible
train <- dplyr::sample_frac(coni, 0.8, replace = FALSE)
test <- anti_join(coni, train, by = 'id')
#'
#'Record start time
ptm <- proc.time()
#' # Step 4 - Set learning rate
#'
#' ## 4a. Train boosted regression trees
#'
#' List of models: combinations of `fraction` and learning rate (`lr`)
loop <- expand.grid(fraction = fraction, lr = lr)
#' Function to fit a BRT and return its summaries (discarding the fitted model),
#' input `z` is a row from `loop`
get_brt1 <- function(i) {
    # set score and fraction values for this run
    lr.i <- loop$lr[i]
    fraction.i <- loop$fraction[i]
    NAME <- paste0("brt.lr.thresh-", lr.i, "-", fraction.i)
    # select training data
    train.i <- dplyr::sample_n(train, fraction.i, replace = FALSE)
    # run BRT & calculate running time
    start_time <- Sys.time()
    for(j in 1:3){
      mod <- try(
        dismo::gbm.step(
          data = train.i,
          gbm.x = 2:5,
          gbm.y = 1,
          family = "bernoulli",
          tree.complexity = tc,
          learning.rate = lr.i,
          verbose = TRUE,
          tolerance.method = "auto"))
      end_time <- Sys.time()
      time <- end_time - start_time
      if (class(mod)=="gbm") break
    }
    if (class(mod)=="gbm") {
      data.frame(
        model = NAME,
        train.auc = mod$self.statistics$discrimination,
        train.dev = mod$self.statistics$mean.resid,
        test.auc = mod$cv.statistics$discrimination.mean,
        test.dev = mod$cv.statistics$deviance.mean,
        trees = mod$n.trees,
        tc = tc,
        lr = lr.i,
        fraction = fraction.i,
        time = time)
    } else {
      data.frame(
        model = NAME,
        train.auc = NA,
        train.dev = NA,
        test.auc = NA,
        test.dev = NA,
        trees = NA,
        tc = tc,
        lr = lr.i,
        fraction = fraction.i,
        time = time)
    }
}
#' Use multiple cores if possible. Hang on, this might take a while
cl <- makeCluster(8)
clusterExport(cl, c("loop", "train", "tc"))

tmp <- pbapply::pblapply(seq_len(nrow(loop)), get_brt1, cl=cl)
#' Stop cluster and collapse results into a single dataframe
stopCluster(cl)

mod.lr.thresh <- do.call(rbind, tmp)
#' Save output
write.csv(mod.lr.thresh, "Step4SetLearningRate.csv", row.names = FALSE)
#'
#' # Step 5 - Sample size
#'
#' ## 5a. Set learning rates
#'
#' Learning rates and sample size thresholds defined below should be determined via visual inspection of results from Step 6
lr.1 <- c(0.1) #Learning rate 1
lr.2 <- c(0.05) #Learning rate 2
lr.3 <- c(0.01) #Learning rate 3
trees.1 <- 10000 #Threshold for 2 to 1
trees.2 <- 3000 #Threshold for 3 to 2
#'
#' ## 5b. Train boosted regression trees
#' 
#' Function to fit a BRT and return its summaries (discarding the fitted model)
get_brt2 <- function(fraction.i) {
    # select training data
    train.i <- dplyr::sample_n(train, fraction.i, replace = FALSE)
    # set learning rate depending on training dataset size
    lr.i <- case_when(
        fraction.i >= trees.1 ~ lr.1,
        fraction.i < trees.1 & fraction.i >= trees.2 ~ lr.2,
        fraction.i < trees.2 ~ lr.3)
    # run BRT & calculate running time
    start_time <- Sys.time()
    mod <- try(
        dismo::gbm.step(
            data = train.i,
            gbm.x = 2:5,
            gbm.y = 1,
            family = "bernoulli",
            tree.complexity = tc,
            learning.rate = lr.i,
            verbose = TRUE,
            tolerance.method = "auto"))
    end_time <- Sys.time()
    time <- end_time - start_time
    attr(mod, "time") <- time
    attr(mod, "lr") <- lr.i
    attr(mod, "tc") <- tc
    attr(mod, "fraction") <- fraction.i
    mod
}
#' Use multiple cores if possible. Hang on, this might take a while
cl <- makeCluster(4)
clusterExport(cl, c("train", "tc", "trees.1", "trees.2", "lr.1", "lr.2", "lr.3"))
clusterEvalQ(cl, library(dplyr))

list.n <- pbapply::pblapply(fraction, get_brt2, cl=cl)
#' Stop cluster
stopCluster(cl)
#' Get summaries of the models
mod.n <- data.frame()

get_summary <- function(mod) {
    OK <- !inherits(mod, "try-error")
    data.frame(
        model = paste0("brt.n-", lr.i, "-", fraction.i),
        train.auc = if (OK) mod$self.statistics$discrimination else NA,
        train.dev = if (OK) mod$self.statistics$mean.resid else NA,
        test.auc = if (OK) mod$cv.statistics$discrimination.mean else NA,
        test.dev = if (OK) mod$cv.statistics$deviance.mean else NA,
        trees = if (OK) mod$n.trees else NA,
        tc = attr(mod, "tc"),
        lr = attr(mod, "lr"),
        fraction = attr(mod, "fraction"),
        time = attr(mod, "time"))
}
mod.n <- do.call(rbind, lapply(mod.n, get_summary))
list.n <- list.n[!sapply(list.n, inherits, "try-error")]
#' Save ouptput
write.csv(mod.n, "Step5SampleSize.csv", row.names = FALSE)
#'
#' ## 5c. Test boosted regression trees for recall
#'
#'Fit 25% of test data to each model and calculate recall for n bootstraps
all.n <- data.frame()

for (i in 1:length(list.n)) {
    cat("Running", i, "of", length(list.n), "models ...")
    flush.console()
    # define fraction from model
    fraction.i <- list.n[[i]]$nTrain
    # set classification threshold
    threshold.i <- 0.5
    for (j in 1:boot) {
        boot.i <- as.numeric(paste0(j))
        test.i <- dplyr::sample_frac(test, 0.25, replace = FALSE)
        # predict on retained test data
        pred <- gbm::predict.gbm(list.n[[i]],
                test.i,
                n.trees = list.n[[i]]$gbm.call$best.trees,
                type = "response")
        # bind predictions to test data frame
        test2 <- cbind(test.i, pred)
        # predict true or false
        test2$pred <- ifelse(test2$pred > threshold.i, 1, 0)
        # extract metrics
        test2$fp <- as.numeric(ifelse(test2$CONI == 0 & test2$pred == 1, 1, 0))
        fp <- mean(test2$fp)
        test2$fn <- as.numeric(ifelse(test2$CONI == 1 & test2$pred == 0, 1, 0))
        fn <- mean(test2$fn)
        test2$tp <- as.numeric(ifelse(test2$CONI == 1 & test2$pred == 1, 1, 0))
        tp <- mean(test2$tp)
        test2$tn <- as.numeric(ifelse(test2$CONI == 0 & test2$pred == 0, 1, 0))
        tn <- mean(test2$tn)
        p <- sum(test2$tp) / (sum(test2$tp) + sum(test2$fp))
        r <- sum(test2$tp) / (sum(test2$tp) + sum(test2$fn))
        test.y <- subset(test2, pred == 1)
        n <- (nrow(test2) - nrow(test.y)) / nrow(test2)
        # bind metrics together
        metrics <- data.frame(boot.i, fraction.i, fp, fn, tp, tn, p, r, n)
        # bind to data frame of results
        all.n <- rbind(all.n, metrics)
    }
    cat(" OK\n")
}
#' Save output
write.csv(all.n, "Step5SampleSizeTesting.csv", row.names = FALSE)
#'
#' ## 5d. Find sample size for asymptote of recall
#'
#' Fit logistic growth curve
all.n$fraction.i <- as.numeric(all.n$fraction.i)

ls.n <- nls(r ~ SSlogis(fraction.i, Asym, xmid, scal), data = all.n)
#'Determine asymptote of recall
Asym <-
    round(environment(ls.n[["m"]][["fitted"]])[["env"]][["Asym"]], 3)
scale <- environment(ls.n[["m"]][["fitted"]])[["env"]][["scal"]]
xmid <- environment(ls.n[["m"]][["fitted"]])[["env"]][["xmid"]]
#' Fit growth curve to new data
new_frac <- seq(min(all.n$fraction.i), max(all.n$fraction.i),
                length.out = 1000)

ls.fit <- data.frame(
  r=predict(newdata = data.frame(fraction.i = new_frac), object = ls.n),
  fraction = new_frac)
#' Find sample size for 99% of asymptote
ls.sum <- ls.fit %>%
    mutate(r = round(r, digits = 3)) %>%
    filter(r >= 0.99 * Asym)

n <- round(min(ls.sum$fraction),-1)

ls.n.summary <- data.frame(cbind(Asym, scale, xmid, n))
#' Save output
write.csv(ls.fit, "Step5SampleSizeNLS.csv", row.names = FALSE)
#'
#' # Step 6 - Final model
#'
#' ## 6a. Train boosted regression tree
#'
#' Select training data
train.i <- dplyr::sample_n(train, 15000, replace = FALSE)
#' Run model
mod <- dismo::gbm.step(
    data = train.i,
    gbm.x = 2:5,
    gbm.y = 1,
    family = "bernoulli",
    tree.complexity = tc,
    learning.rate = 0.1,
    verbose = TRUE,
    tolerance.method = "auto")
#Get model summary
a <- data.frame(
    train.auc = mod$self.statistics$discrimination,
    train.dev = mod$self.statistics$mean.resid,
    test.auc = mod$cv.statistics$discrimination.mean,
    test.dev = mod$cv.statistics$deviance.mean,
    test.se = mod$cv.statistics$deviance.se,
    trees = mod$n.trees,
    tc = tc)
print(a)
#' Save final model
saveRDS(mod, "FinalBRT.rds")
#'
#' ## 6b. Interogate predictors & interactions
#' 
#' Relative contribution of predictors
summary(mod)
#'Magnitude of interaction importance
mod.int <- gbm.interactions(mod)
mod.int$interactions
#'
#' # Step 8 - Calculate performance metrics for temporal scale
#' 
#' Note the following code calculates metrics for all three temporal scales (detection, visit, location). Choose which code to use following selection of temporal scale for your analysis in Step 9.
#'
#' ## 8a. Test final model with withheld data
#'
#' Set the range of classification thresholds to test
thresh <- seq(0.01, 0.99, by = 0.01)
#' Test final model and predict validation for each classification threshold
all.thresh <- list()

for (i in 1:boot) {
    cat("Running", i, "of", boot, "bootstrap runs ...")
    flush.console()
    boot.i <- as.numeric(paste0(i))

    test.i <-
        dplyr::sample_frac(data.frame(test), 0.25, replace = FALSE)
    # predict on retained test data
    prob <- predict.gbm(mod,
            test.i,
            n.trees = mod$gbm.call$best.trees,
            type = "response")
    # bind predictions to test data frame
    test2 <- cbind(test.i, prob)
    for (j in 1:length(thresh)) {
        threshold.i <- as.numeric(paste0(thresh[j]))
        k <- i * j
        all.thresh[[k]] <- data.frame(
            test2,
            pred = ifelse(test2$prob > threshold.i, 1, 0),
            thresh = threshold.i,
            boot = boot.i)
    }
    cat(" OK\n")
}
all.thresh.out <- rbindlist(all.thresh)
#' Save output
write.csv(all.thresh.out, "Step8Testing.csv", row.names = FALSE)
#'
#' ## 8b. Calculate metrics for detection temporal scale
#'
loop <- all.thresh.out %>%
    dplyr::select(thresh, boot) %>%
    unique()

detection <- list()

for (i in 1:nrow(loop)) {
    cat("Running", i, "of", nrow(loop), "loops ...")
    flush.console()
    thresh.i <- as.numeric(paste0(loop$thresh[i]))
    boot.i <- as.numeric(paste0(loop$boot[i]))

    dat <- all.thresh.out %>%
        dplyr::filter(thresh == thresh.i,
            boot == boot.i) %>% 
      mutate(fp=as.numeric(ifelse(CONI==0 & pred==1, 1, 0)),
             fn=as.numeric(ifelse(CONI==1 & pred==0, 1, 0)),
             tp=as.numeric(ifelse(CONI==1 & pred==1, 1, 0)),
             tn=as.numeric(ifelse(CONI==0 & pred==0, 1, 0)))
    
    detection[[i]] <- dat %>% 
      summarize(fp=mean(fp),
                fn=mean(fn),
                tp=mean(tp),
                tn=mean(tn),
                nrow=nrow(dat),
                pred=sum(pred),
                CONI=sum(CONI)) %>% 
      mutate(p=tp/(tp+fp),
             r=tp/(tp+fn),
             n=1-pred/nrow,
             boot=boot.i,
             thresh=thresh.i)
    cat(" OK\n")
}
detection.out <- rbindlist(detection) %>%
    mutate(var = "detection")
#'
#' Save output
write.csv(detection.out, "Step8DetectionResponseVariable.csv", row.names = FALSE)
#' 
#' ## 8c. Aggregate and calculate metrics for visit temporal scale
#'
loop <- all.thresh.out %>%
    dplyr::select(thresh, boot) %>%
    unique()

visit <- list()

for (i in 1:nrow(loop)) {
    cat("Running", i, "of", nrow(loop), "loops ...")
    flush.console()
    thresh.i <- as.numeric(paste0(loop$thresh[i]))
    boot.i <- as.numeric(paste0(loop$boot[i]))

    dat <- all.thresh.out %>%
        dplyr::filter(thresh == thresh.i,
            boot == boot.i)
    
    dat1 <- dat %>% 
      group_by(recording) %>%
      dplyr::summarize(CONI = sum(CONI),
                       pred = sum(pred)) %>% 
      mutate(fp=as.numeric(ifelse(CONI==0 & pred>0, 1, 0)),
             fn=as.numeric(ifelse(CONI>0 & pred==0, 1, 0)),
             tp=as.numeric(ifelse(CONI>0 & pred>0, 1, 0)),
             tn=as.numeric(ifelse(CONI==0 & pred==0, 1, 0))) %>% 
      ungroup()
      
    visit[[i]] <- dat1 %>% 
      summarize(fp=mean(fp),
                fn=mean(fn),
                tp=mean(tp),
                tn=mean(tn),
                nrow=nrow(dat),
                pred=sum(pred),
                CONI=sum(CONI)) %>% 
      mutate(p=tp/(tp+fp),
             r=tp/(tp+fn),
             n=1-sum(dat$pred)/nrow(dat),
             boot=boot.i,
             thresh=thresh.i)
    cat(" OK\n")
}

visit.out <- rbindlist(visit) %>%
    mutate(var = "visit")
#'
#' Save output
write.csv(visit.out, "Step8VisitResponseVariable.csv", row.names = FALSE)
#' ## 8d. Aggregate and calculate metrics for location temporal scale
#'
loop <- all.thresh.out %>%
    dplyr::select(thresh, boot) %>%
    unique()

loc <- list()

for (i in 1:nrow(loop)) {
    cat("Running", i, "of", nrow(loop), "loops ...")
    flush.console()
    thresh.i <- as.numeric(paste0(loop$thresh[i]))
    boot.i <- as.numeric(paste0(loop$boot[i]))

    dat <- all.thresh.out %>%
      dplyr::filter(thresh == thresh.i,
                    boot == boot.i)
    
    dat1 <- dat %>% 
      group_by(location) %>%
      dplyr::summarize(CONI = sum(CONI),
                       pred = sum(pred)) %>% 
      mutate(fp=as.numeric(ifelse(CONI==0 & pred>0, 1, 0)),
             fn=as.numeric(ifelse(CONI>0 & pred==0, 1, 0)),
             tp=as.numeric(ifelse(CONI>0 & pred>0, 1, 0)),
             tn=as.numeric(ifelse(CONI==0 & pred==0, 1, 0))) %>% 
      ungroup()
    
    loc[[i]] <- dat1 %>% 
      summarize(fp=mean(fp),
                fn=mean(fn),
                tp=mean(tp),
                tn=mean(tn),
                nrow=nrow(dat),
                pred=sum(pred),
                CONI=sum(CONI)) %>% 
      mutate(p=tp/(tp+fp),
             r=tp/(tp+fn),
             n=1-sum(dat$pred)/nrow(dat),
             boot=boot.i,
             thresh=thresh.i)
    cat(" OK\n")
}

loc.out <- rbindlist(loc) %>%
    mutate(var = "location")
#' Save output
write.csv(loc.out, "Step8LocationResponseVariable.csv", row.names = FALSE)
#'
#' # Step 9 - Determine classification threshold for priority scenario
#'
#' ## 9a. Retain detections scenario
#'
min <- 0.99 #Set minimum mean recall

detection.mean <- detection.out %>%
    group_by(thresh) %>%
    summarize(r = round(mean(r), 2),
        n = mean(n),
        p = mean(p)) %>%
      filter(r >= min) %>%
    arrange(desc(n))
head(detection.mean)
#'
#' ## 9b. Total error scenario
#'
visit.mean <- visit.out %>%
    mutate(pr = r + p) %>%
    group_by(thresh) %>%
    summarize(
        pr = mean(pr),
        n = mean(n),
        r = mean(r),
        p = mean(p)
    ) %>%
    arrange(desc(pr))
head(visit.mean)
#'
#' ## 9c. No validation scenario
#'
min <- 0.99 #Set minimum mean precision

loc.mean <- loc.out %>%
    group_by(thresh) %>%
    summarize(r = mean(r),
        p = round(mean(p),2),
        n = mean(n)) %>%
#    filter(p >= min) %>%
    arrange(desc(p)) 
head(loc.mean)
#'
#'Substract time from start time to get run time
proc.time() - ptm
