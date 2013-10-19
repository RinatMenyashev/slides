PrintCatStat <- function(dta, file) {
  
#     
#   mean.pricediff <- paste("figure/", file, ".publ1.png",sep="")
# 
#   g<-ggplot(boot.res[[1]], aes_string(color = "ispublisher")) + 
#     geom_point(aes(x = site, y = mean), size = 3, position = position_dodge(width = 0.3)) + 
#     #scale_y_log10() +
#     theme_bw() + scale_x_discrete('site') + 
#     geom_errorbar(aes(x=site, ymin = low, ymax = high), size = 1,   width = 0.5, position = position_dodge(width = 0.3)) + 
#     #coord_flip() size +
#     theme(axis.text.x = element_text(size = 14,angle = 90, hjust = 1)) + 
#     facet_wrap(~ param, scales = "free")
# 
#   
#   param <- c("price")
#   boot.res <- lapply(c("isfriend", "ispublisher") , function(cat)
#     dta[ , CalcMeans(.SD, param, 50), by = c("site", cat), .SDcols = c(boot.cntrl, param)])
#   boot.res[[1]]$low[boot.res[[1]]$low < 0] <- 0
#   boot.res[[2]]$low[boot.res[[2]]$low < 0] <- 0
#   boot.res[[1]]$param[boot.res[[1]]$param == "price"] <- "Average order"
#   boot.res[[2]]$param[boot.res[[2]]$param == "price"] <- "Average order"
#   
#   
  
  if ("splitvar" %in% names(dta)) {
    splitvars = unique(dta$splitvar)
    
    outgraphs <- lapply(splitvars, function(x) {
      
      path <- paste("figure/", file, ".", x, ".png",sep="")
      png(path)
      g <-    ggplot(dta[splitvar == x], aes_string(color = c("subsample"))) + 
      geom_point(aes(x = site, y = mean), size = 3, position = position_dodge(width = 0.3)) + 
      #scale_y_log10() +
      theme_bw() + scale_x_discrete('site') + 
      geom_errorbar(aes(x=site, ymin = low, ymax = high), size = 1,   width = 0.5, position = position_dodge(width = 0.3)) + 
      #coord_flip() size +
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
      facet_wrap(~  param , scales = "free")
    
      print(g)
      dev.off()
      res <- c(path)
      names(res) <- x
      return(res)
    })
  } else {
    mean.kpis <- paste("figure/", file, ".kpis.png", sep="")
    png(mean.kpis)
    g <- ggplot(dta) + 
      geom_point(aes(x = site, y = mean), size = 3, position = position_dodge(width = 0.3)) + 
      scale_y_continuous(labels = percent_format()) +
      theme_bw() + scale_x_discrete('site') + 
      geom_errorbar(aes(x=site, ymin = low, ymax = high), size = 1,   width = 0.5, position = position_dodge(width = 0.3)) + 
      #coord_flip() size +
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
      facet_wrap(~ param, scales = "free")
    print(g)
    dev.off()
    outgraphs <- list(kpi = mean.kpis)
  } 
  
  gc()
   return(as.data.table(outgraphs))
  
}