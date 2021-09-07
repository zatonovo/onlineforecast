## Load current package (must be outcommented)
## library("devtools")
## load_all(as.package("../../../onlineforecast"))
##dir.create("savedcheck")    

test_that("run", {

    ## ------------------------------------------------------------------------
    D <- Dbuilding
    D$y <- D$heatload
    D$tday <- make_tday(D$t, kseq=1:36)

    ## ------------------------------------------------------------------------
    D <- subset(D, c("2010-12-15", "2011-02-01"))
    Dtrain <- subset(D, c("2010-12-15", "2011-01-01"))
    Dtrain$scoreperiod <- in_range("2010-12-20", Dtrain$t)

    ## ------------------------------------------------------------------------
    model <- forecastmodel$new()
    model$output = "y"
    model$add_inputs(Ta = "lp(Ta, a1=0.9)", 
                     I = "lp(I, a1=0.7)", 
                     mu_tday = "fs(tday/24, nharmonics=10)",
                     mu = "one()")
    model$add_regprm("rls_prm(lambda=0.9)")

    ## ------------------------------------------------------------------------
    model$add_prmbounds(Ta__a1 = c(0.8, 0.9, 0.9999),
                        I__a1 =  c(0.4, 0.8, 0.9999),
                        lambda = c(0.9, 0.99, 0.9999))

    ## ------------------------------------------------------------------------
    model$kseq <- c(1,18)
    model$prm <- rls_optim(model, Dtrain, control=list(maxit=2), cachedir="", printout=FALSE)$par
    
    model$kseq <- 1:36
    val <- rls_fit(model$prm, model, D, returnanalysis=TRUE, printout=FALSE)

    ## Keep the result for later check
    D$Yhat1 <- val$Yhat

    ## ------------------------------------------------------------------------
    ## Save for later check
    filenm <- "savedcheck/test-rls-heat_Yhat.RDS"
    ##saveRDS(D$Yhat1, filenm)
    ## Load to check that is same as saved
    expect_equal(D$Yhat1, readRDS(filenm))

    ## ------------------------------------------------------------------------
    ## Do it recursively
    ## ------------------------------------------------------------------------
    itrain <- which(in_range("2010-12-15",D$t,"2011-01-01"))
    itest <- which(in_range("2011-01-01",D$t,"2011-01-01 12:00"))
    rls_fit(model$prm, model, subset(D, itrain), printout=FALSE)

    D$Yhat2 <- data.frame(matrix(NA, nrow(D$Yhat1), ncol(D$Yhat1)))
    names(D$Yhat2) <- names(D$Yhat1)
    for(i in itest){
        Dnew <- subset(D, i)
        Dnewtr <- model$transform_data(Dnew)
        rls_update(model, Dnewtr, Dnew[[model$output]])
        D$Yhat2[i, ] <- as.numeric(rls_predict(model, Dnewtr))
    }

    ## ------------------------------------------------------------------------
    ## Check that the recursive and fit forecasts are close to each other
    expect_true(sum(D$Yhat1[itest, ] - D$Yhat2[itest, ]) < 0.00001)
})







