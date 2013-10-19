library(survival)
library(data.table)
library(RODBC)
library(car)
library(boot)
library(ggplot2)
library(rms)
library(scales)
library(effects)
library(xtable)


Sys.setlocale(category = "LC_ALL", locale = "C")
Sys.setenv(lang = "en")

ch <- odbcConnect("flock64", DBMSencoding = "UTF8")
system.time(data1 <- sqlQuery(ch, "select * from rin_shares limit 1000000"))
write.csv(data1, file = "C:/Dropbox/slides/panel_shares1000000.csv")

# query <- "select sites.id, domain, count(orders.id) as siteornum
#           , count(orders.parent_offer_id)  as sitefrnum 
#           from sites 
#           inner join orders on orders.site_id = sites.id  
#           group by site_id"
# sites <- as.data.table(sqlQuery(ch, query));
# head(sites)
#==========================================================

categories <- sqlQuery(ch, "select sites.domain as site, sites.company_size, 
                       ifnull(category, 'unknown') as category from sites 
                       inner join accounts on accounts.id = sites.account_id 
                       where is_test = 0;")
tmp <- read.csv(file = "C:/Dropbox/slides/panel_shares1000000.csv")
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
write.csv(tmp, file = ("C:/Dropbox/slides/panel_shares1000000.csv"))

#=======================================For AB Tests ============================
cmp.abtests.short <- read.csv(file = "abtests.csv", sep = ";")

cmp.abtests.full <- data.table( merge(as.data.frame(campaigns), cmp.abtests.short, all = FALSE
                                      , by.x = c("title", "site"), by.y = c("Campaign.Name", "site")
                                      , sort = T), key = c("site_id", "campaign_id"))

setkey(cmp.abtests.full, site_id, campaign_id)

#=======================Andrey - here your part starts =============================
tmp <- read.csv(file = "C:/Dropbox/slides/panel_shares1000000.csv")

tmp <- as.data.table(tmp)
source('C:/Dropbox/slides/refine.R', echo=TRUE)
refine(tmp)
setkey(tmp, site, customer_id); 
tmp[,orseq := rank(date), by = c("site", "customer_id")]
setkey(tmp, site, customer_id, orseq)

tmp[,prev:=tmp[J(site, customer_id,orseq - 1), date, mult='last']]
tmp[,date2:=tmp[J(site, customer_id,orseq + 1), date, mult='last']]

tmp[, diff := as.numeric(date2 - date)]
tmp[, diff_lagged := as.numeric(date - prev)]

tmp[, ispublisher := NULL]
tmp[, ispublisher:= tmp[J(site, customer_id,orseq-1), prn, mult='last']]
tmp[, ispublisher.succ := tmp[J(site, customer_id,orseq-1), frnum > 0, mult='last']]
tmp[, ispublisher := ifelse( ispublisher == F, 0, ifelse(ispublisher.succ == F, 1, 2))]
tmp[, ispublisher := ifelse( is.na(ispublisher), -1, ispublisher)]

tmp[, ispublisher := factor(ispublisher, -1:2, labels = c("new", "ordinary", "publisher", "publisher.succ"))]
tmp[, isfriend := factor(isfriend, 0:1, labels =c("ordinary", "friend"))]
table(tmp$ispublisher)
tmp[, length(customer_id), by = "site"]



ggplot( tmp[event == 1 & site == "utkonos.ru"], aes(x=(diff_lagged), fill = as.factor(sex))) + 
  geom_density( position="identity", alpha = 0.2)  +
  scale_x_log10();


ggplot(tmp[price < 5e4,], aes(x=(price), fill = (ispublisher))) + 
  geom_density( position="identity", alpha = 0.2) +
  scale_x_log10(limits = c(1e3,5e4));


ggplot(tmp[price < 5e4 & site == "utkonos.ru",], aes(x=(diff_lagged), fill = (ispublisher))) + 
  geom_density( position="identity", alpha = 0.2) +
  scale_x_log10(limits = c(1e3,5e4));



tmp[, leads2offers := ifelse(is.na(leadsnum), 0, leadsnum)]
tmp[, friends2offers := frnum]
tmp[, leads2shares := leadsnum]
tmp[, friends2shares := ifelse(is.na(leadsnum), NA, frnum)]

##=====================Bootstrapping means ======================================
boot.cntrl = c("city")
param = c("price", "diff_lagged")

source('C:/Dropbox/slides/calcmeans.R', echo=TRUE)

boot.res <- lapply(c("ispublisher") , function(cat)
  tmp[ ispublisher != "new", CalcMeans(.SD, param, 50,site), by = c("site", cat), .SDcols = c(boot.cntrl, param)])


param <- c("price")
boot.res <- lapply(c("isfriend", "ispublisher") , function(cat)
  tmp[ , CalcMeans(.SD, param, 50,site), by = c("site", cat), .SDcols = c(boot.cntrl, param)])

boot.res$param <- factor(boot.res$param)



##=====================Plotting graphs ======================================

ggplot(boot.res[[2]], aes_string(color = "ispublisher")) + 
  geom_point(aes(x = site, y = mean), size = 3, position = position_dodge(width = 0.3)) + 
  #scale_y_log10() +
  theme_bw() + scale_x_discrete('site') + 
  geom_errorbar(aes(x=site, ymin = low, ymax = high), size = 1,   width = 0.5, position = position_dodge(width = 0.3)) + 
  #coord_flip() size +
  theme(axis.text.x = element_text(size = 14,angle = 90, hjust = 1)) + 
  facet_wrap(~ param, scales = "free");


stats <- tmp[ , list(
  from = min(date)
  , to = max(date)
  , NumberOfOrders = length(customer_id)
  , MeanOrderSize = round(mean(price, na.rm = T)) 
  , FrequencyOfPurchases = round(30/mean(diff_lagged, na.rm = T), 2)
  , Leads2Offers = round(1000*mean(leads2offers))/10
  , Friends2Offers = round(1000*mean(friends2offers))/10)
             , by = "site"]

stats[, NumberOfOrders.rk := rank(-NumberOfOrders, ties.method = "min")]
stats[, MeanOrderSize.rk := rank(-MeanOrderSize, ties.method = "min")]
stats[, FrequencyOfPurchases.rk := rank(-FrequencyOfPurchases, ties.method = "min")]
stats[, Leads2Offers.rk := rank(-Leads2Offers, ties.method = "min")]
stats[, Friends2Offers.rk := rank(-Friends2Offers, ties.method = "min")]

stats[, NumberOfOrders.rkp := percent(1 - NumberOfOrders.rk/max(NumberOfOrders.rk))]
stats[, MeanOrderSize.rkp := percent(1 - MeanOrderSize.rk/max(MeanOrderSize.rk))]
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

source('C:/Dropbox/slides/multiplot.R', echo=TRUE)

stats.cat <- as.data.table(merge(stats, categories, by = "site"))

categories <- stats.cat[, list(NumberOfOrders = sum(NumberOfOrders)) ,by = "category"]

stats.table <- as.data.frame(stats.cat[,list(NumberOfOrders = mean(NumberOfOrders), AOV = mean(AOV), FrequencyOfPurchases = mean(FrequencyOfPurchases))
                                       , by= "category" ])

source('C:/Dropbox/slides/calcmeans.R', echo=TRUE)


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
source('C:/Dropbox/slides/printcatstat.R', echo=TRUE)
graphs.category[["price"]] <- graph.data1[ , PrintCatStat(.SD, category), by="category"]


param = c("leads2offers", "friends2offers")
boot.res <- tmp[, CalcMeans(.SD, param, 100, sitename = site), by = c("site"), .SDcols = c(boot.cntrl, param)]
graph.data2 <- merge(boot.res, stats.cat[, site, category], by = "site")


graphs.category[["kpis"]] <- graph.data2[ , PrintCatStat(.SD, category), by="category"]
graphs.cat <- merge(as.data.frame(graphs.category[["price"]]), as.data.frame(graphs.category[["kpis"]]), by = "category")
