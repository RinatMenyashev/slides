library(survival)
library(data.table)
library(RODBC)
library(car)
library(boot)
library(ggplot2)
library(rms)
library(slidify)
library(stringr)
library(scales)
library(effects)

Sys.setlocale(category = "LC_ALL", locale = "C")
Sys.setenv(lang = "en")

ch <- odbcConnect("flock64", DBMSencoding = "UTF8")
system.time(data1 <- sqlQuery(ch, "select * from rin_shares limit 1000000"))
write.csv(data1, file = "E:/Dropbox/R/slides/panel_shares.csv")

# query <- "select sites.id, domain, count(orders.id) as siteornum
#           , count(orders.parent_offer_id)  as sitefrnum 
#           from sites 
#           inner join orders on orders.site_id = sites.id  
#           group by site_id"
# sites <- as.data.table(sqlQuery(ch, query));
# head(sites)
#==========================================================


tmp <- read.csv(file = "E:/Dropbox/R/slides/panel_shares.csv")
tmp <- data.table(tmp, key = "campaign_id")
tail(tmp, 10)
head(tmp, 10)


ch2 <- odbcConnect("flock64", DBMSencoding = "UTF8")
campaigns <- sqlQuery(ch2, "select campaigns.id as campaign_id, campaigns.title, site_id, sites.domain as site from campaigns 
                      inner join sites on sites.id = campaigns.site_id 
                      where sites.is_test = 0;")
campaigns <- data.table(campaigns, key = "campaign_id")
head(campaigns)
tmp <- merge(tmp, campaigns[, campaign_id, site], by = "campaign_id")
head(tmp)
write.csv(tmp, file = "E:/Dropbox/R/slides/panel_shares.csv")

#=======================================For AB Tests ============================
cmp.abtests.short <- read.csv(file = "abtests.csv", sep = ";")

cmp.abtests.full <- data.table( merge(as.data.frame(campaigns), cmp.abtests.short, all = FALSE
                                        , by.x = c("title", "site"), by.y = c("Campaign.Name", "site")
                                        , sort = T), key = c("site_id", "campaign_id"))

setkey(cmp.abtests.full, site_id, campaign_id)

#=======================Andrey - here your part starts =============================
tmp <- read.csv(file = "E:/Dropbox/R/slides/panel_shares.csv")

tmp <- as.data.table(tmp)
source('E:/Dropbox/R/slides/refine.R', echo=TRUE)
refine(tmp)

##=====================Bootstrapping means ======================================
boot.cntrl = c("city")
param = c("price", "diff_lagged")

source('E:/Dropbox/R/slides/calcmeans.R', echo=TRUE)

boot.res <- lapply(c("ispublisher") , function(cat)
  tmp[ ispublisher != "new", CalcMeans(.SD, param, 50), by = c("site", cat), .SDcols = c(boot.cntrl, param)])


param <- c("price")
boot.res <- lapply(c("isfriend", "ispublisher") , function(cat)
  tmp[ , CalcMeans(.SD, param, 50), by = c("site", cat), .SDcols = c(boot.cntrl, param)])

boot.res$param <- factor(boot.res$param)



##=====================Plotting graphs ======================================


stats <- tmp[ , list(
  from = min(date)
  , to = max(date)
  , NumberOfOrders = length(customer_id)
  , AOV = round(mean(price, na.rm = T)) 
  , FrequencyOfPurchases = round(30/mean(diff_lagged, na.rm = T), 2)
  , Leads2Offers = round(1000*mean(leads2offers))/10
  , Friends2Offers = round(1000*mean(friends2offers))/10)
             , by = "site"]

stats[, NumberOfOrders.rk := rank(-NumberOfOrders, ties.method = "min")]
stats[, AOV.rk := rank(-AOV, ties.method = "min")]
stats[, FrequencyOfPurchases.rk := rank(-FrequencyOfPurchases, ties.method = "min")]
stats[, Leads2Offers.rk := rank(-Leads2Offers, ties.method = "min")]
stats[, Friends2Offers.rk := rank(-Friends2Offers, ties.method = "min")]

stats[, NumberOfOrders.rkp := percent(1 - NumberOfOrders.rk/max(NumberOfOrders.rk))]
stats[, AOV.rkp := percent(1 - AOV.rk/max(AOV.rk))]
stats[, FrequencyOfPurchases.rkp := percent(1 - FrequencyOfPurchases.rk/max(FrequencyOfPurchases.rk))]
stats[, Leads2Offers.rkp := percent(1 - Leads2Offers.rk/max(Leads2Offers.rk))]
stats[, Friends2Offers.rkp := percent(1 - Friends2Offers.rk/max(Friends2Offers.rk))]


stats <- as.data.frame(stats)

source('C:/Dropbox/slides/printsitestat.R', echo=TRUE)


system.time(
  graphs <- tmp[, PrintSiteStat(.SD[
    .N < 2e5 & (
      age_gr!= "unknown" 
      | prn == "ispublisher" 
      | (age_gr == "unknown" )
    )
    ]
                                , site)
                , by = "site"]
)


graphs <- as.data.frame(graphs)
slidify("report_test.Rmd")

#==================================

source('calcmeans.R', echo=TRUE)
source('multiplot.R', echo=TRUE)


stats <- data.table(stats, key = "site")
ch <- odbcConnect("flock64", DBMSencoding = "UTF8")

categories <- sqlQuery(ch, "select sites.domain as site, sites.company_size, ifnull(category, 'unknown') as category 
                       from sites inner join accounts on accounts.id = sites.account_id where is_test = 0;")
categories <- data.table(categories, key = "site")
stats.cat <- merge(stats, categories)

categories <- stats.cat[, list(NumberOfOrders = sum(NumberOfOrders)) ,by = "category"]



graphs.category <- list()
boot.cntrl = c("city")

param = c("price", "diff_lagged")
boot.res <- rbindlist(lapply(c("ispublisher", "isfriend") , function(cat) {
  data = tmp[, CalcMeans(.SD, param, 100, sitename = site), by = c("site", cat), .SDcols = c(boot.cntrl, param)]
  setnames(data, cat, "subsample")
  data[, splitvar := cat]
}
))
boot.res[low < 0, low :=  0]
boot.res[param == "price", param :=  "Average order"]
boot.res[param == "diff_lagged", param := "DaysBtwOrders"]
graph.data1 <- merge(boot.res, stats.cat[, site, category], by = "site")
source('printcatstat.R', echo=TRUE)
graphs.category[["price"]] <- graph.data1[ , PrintCatStat(.SD, category), by="category"]


param = c("leads2offers", "friends2offers")
boot.res <- tmp[, CalcMeans(.SD, param, 100, sitename = site), by = c("site"), .SDcols = c(boot.cntrl, param)]
graph.data2 <- merge(boot.res, stats.cat[, site, category], by = "site")


graphs.category[["kpis"]] <- graph.data2[ , PrintCatStat(.SD, category), by="category"]
graphs.cat <- merge(as.data.frame(graphs.category[["price"]]), as.data.frame(graphs.category[["kpis"]]), by = "category")
