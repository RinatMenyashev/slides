CalcMeans <- function(dta, param, niter = 50, sitename) {
  #browser()
  print(paste("Calculating for ", sitename))
  #niter <- 5
  #dta <- tmp[site == "123.ru" & ispublisher == "new", price, diff_lagged]
  
  if(nrow(dta) > 100) {
    
    ncol <- length(param)

    res <- boot(dta[, (length(boot.cntrl)+1):(length(boot.cntrl) + ncol), with = F]
                , function(x, i) {                
                  colMeans(x[i], na.rm = TRUE)
                }
                , R = niter
                , strata = dta[[boot.cntrl]]
    );

    #browser()
    stat <- sapply((1:length(res$t0))[!is.na(res$t0)], function(x) {
      c(res$t0[x], int = boot.ci(res, type = "basic", index = x)$basic[4:5])
    }) 
    # dim(stat); print(dta);
    
    return(list(param = names(res$t0)[!is.na(res$t0)], mean = stat[1,], low = stat[2,], high = stat[3,]))  
  }
}
