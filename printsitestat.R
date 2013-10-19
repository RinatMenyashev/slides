PrintSiteStat <- function(dta, file) {
  
  #file <- "confirm.s7.ru"
  #dta <- tmp[site == file]
  norders <- nrow(dta)
  print(paste("Analysing ", file, " Obs no = ",norders, sep = ""))
  
  
  
 
  dist.price = paste("figure/", file,".dist.price.png", sep = "")
  cox = paste("figure/", file,".cox.", sep = "")
  eff1 = ""
  eff2 = ""
  
  
  png(filename = dist.price)
  dist <- ggplot( dta[price < 1e4], aes(x=(price), fill = (isfriend))) + 
    geom_density( position="identity", alpha = 0.2) +
    scale_x_log10(limits = c(1e3,5e4));
  print(dist)
  dev.off()
  #ggsave(file=dist.price, plot=dist,width = 2.1, height = 2.5)
  #source('E:/Dropbox/R/slides/calcsitestat.R', echo=TRUE)
  #surv <- CalcSurvTimes(data[diff < 150])
  
  
    
  if (nrow(dta[!is.na(diff_lagged)]) > 100000) {  
    dist.diff = paste("figure/", file,".dist.diff.png", sep = "")
    png(filename = dist.diff)
    
    dist <- ggplot(dta[event == 1], aes(x=(diff_lagged), fill = (ispublisher))) + 
      geom_density( position="identity", alpha = 0.2)+
      scale_x_log10(limits = c());
    print(dist)
    dev.off()
    #ggsave(file=dist.diff, plot=dist)
  }  else {
    dist.diff <- ""
  }
  #source('C:/Dropbox/slides/plot.model.R', echo=F)
  


  dta <- tmp[site == "utkonos.ru"]; 
  if (dta[, list(sum = sum(leadsnum > 0, na.rm = T) )] > 2000) {
    
    print("Calculating effects for price")  
    factors <- vector(); 
    for(x in c("ispublisher", "isfriend", "age_gr", "sex", "city", "sn", "week", "influence_gr") ) {
      if(nrow(dta[, list(nobs =.N), by = x]) > 1){
        factors <- paste(factors, x, sep = "+")
      }
    };
    cont <- ""
    form0 <- as.formula(paste("lprice ~", cont, factors, sep = ""))
    
    model0 <- lm(form0 ,data = dta)
    eff0 <- paste("figure/", file,".eff.aov.png", sep = "")
    plot.model(model0, eff0, c("age_gr", "sex", "city", "sn", "week", "influence_gr", "isfriend", "ispublisher"), ncols = 4)
    
    
    print("Calculating effects for publishing")  
    factors <- vector(); 
    for(x in c("campaign_id","age_gr", "sex", "city", "week") ) {
      if(nrow(dta[, list(nobs =.N), by = x]) > 1){
        factors <- paste(factors, x, sep = "+")
      }
    };
    cont <- "lprice"
    form1 <- as.formula(paste("prn~", cont, factors, sep = ""))
    
    model1 <- glm(form1 ,   family = binomial, data = dta)
    eff1 <- paste("figure/", file,".eff.publ.png", sep = "")
    plot.model(model1, eff1, c("age_gr", "sex", "city", "week"), ncols = 2, conf.int = 0.8)
  
  
    if (dta[, list(sum = sum(frnum > 0, na.rm = T) )] > 50) {
      print("Calculating effects for bringing a friend")  
      
      factors <- vector(); 
      for(x in c("campaign_id", "age_gr", "sex", "city", "sn", "week", "influence_gr") ) {
        if(nrow(dta[, list(nobs =.N), by = x]) > 1){
          factors <- paste(factors, x, sep = "+")
        }
      };
      cont <- "lprice"
      form2 <- as.formula(paste("frn~", cont, factors, sep = "") )   
      model2 <- glm(form2 , family = binomial, data = dta)
      eff2 <- paste("figure/", file,".eff.friend.png", sep = "")
      plot.model(model2, eff2, c("age_gr", "sex", "city", "sn", "week", "influence_gr"), ncols = 3, conf.int = 0.8)
    
  }}

    
  if (norders > 10000) { 
    print("Calculating effects for clv")  
    
    factors <- vector(); 
    for(x in c("age_gr", "city", "sn", "week") ) {
      if(nrow(dta[, list(nobs =.N), by = x]) > 1){
        factors <- paste(factors, x, sep = "+")
      }
    };
    cont <- "lprice + cluster(customer_id)"
    y <- Surv(dta$diff, dta$event)
    
    cox.model <- list()
    sapply( c( "prn", "frn"), function(str) {
      if(nrow(dta[!is.na(diff), list(nobs =.N), by = str]) > 1) {
        
        form1 <- as.formula(paste("y ~ ", cont, factors, " + strat(", str, ")", sep = ""))
        try({
          cox.model[[str]] <- cph(formula = form1 , data = dta, x = T, y = T, surv = T, time.inc = 7)
          cox.fit  <- survfit(cox.model[[str]])
          png(paste(cox, str, ".png", sep = ""))
          cox.plot <- survplot((cox.fit), n.risk = T, levels.only = T, xlim = c(0, min(150, max(dta$diff))), col = c("red", "green", "blue"))
          dev.off()
        })
      }
    })
    
    #z <- allEffects(logit.model)
    cox.pub <- paste("figure/", file,".cox.pub.png", sep = "")
    cox.fr <- paste("figure/", file,".cox.fr.png", sep = "")
    
  } else {
    cox.pub <- ""
    cox.fr <- ""
  }
  gc()
  return(
        as.data.table(list(dist.price = dist.price,
#              dist.diff = dist.diff,
              cox.pub = cox.pub,
              cox.fr  = cox.fr,
              eff1 = eff1,
              eff2 = eff2)
        )
        )
} 