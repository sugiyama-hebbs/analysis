# This runs linear regression with several models

# candidate predictors
# ページビュー
# セッション数
# 訪問者数
# 平均滞在時間
# 平均ストア内回遊ページ
# PC/スマホ/アプリビュー
#
# 
# output 
# 注文数　
# 注文点数
# 注文者数
# 購買率


### Preparation ###
source("function/load_basic_lib.R")

### Read in data ###
sales_yahoo_raw <- read.xlsx("data/yahoo_20200912.xlsx", sheet = 1, startRow = 2, colNames = T) # dataframe
  
### Organize data ###
sales_yahoo1 <- sales_yahoo_raw # copy
colnames(sales_yahoo1) <- c("date","sales","num_order","num_product","num_customer_purchase","order_per_unique_customer",
                           "sales_per_customer","pageview","num_session","num_unique_customer","avg_stay_time","avg_page_view_per_customer",
                           "sales_pc","pageview_pc","sales_sp","pageview_sp","sales_app","pageview_app","sales_mobile","pageview_mobile")

sales_yahoo2 <- sales_yahoo1 %>% 
  mutate(date = convertToDate(date)) %>% 
  mutate(weekday = factor(weekdays(date, abbreviate = T), levels = c("Mon","Tue","Wed","Thu","Fri","Sat","Sun"))) %>% 
  mutate(day = as.numeric( substr(date, start = 9, stop = 10)), weekend = ifelse(weekday %in% c("Sat","Sun"), 1, 0)) %>% 
  mutate(five_day = ifelse(day %in% c(5,15,25), 1, 0)) # 5のつく日
  

sales_yahoo <- sales_yahoo2 # copy the final version

### Plot ###

## page view vs number of order
pv_vs_no_pre <- ggplot(sales_yahoo, aes(x = pageview, y = num_order, color = weekday)) + 
  geom_point() +
  geom_smooth(method = "lm", formula = y~x, se = F, aes(group = NA), color = "black") # +
  # stat_cor(method = "spearman",aes(group = NA), color = "black")

pv_vs_no <- format_gg(gplot = pv_vs_no_pre, xlabel = "Page View", ylabel = "Number of Order",
                      ptitle = "Page view vs Number of Order, Linear Fit",
                      xlimit = c(0, 750), ylimit = c(0,12), yticks = seq(0,12,2), 
                      pcol = rainbow(7))


## five day vs number of order
tmp_df <- sales_yahoo %>% 
  mutate(tmp_x = sapply(five_day, function(x){ out <- x+ rnorm(1, mean = 0, sd = .02)})) # add some jittering to avoid overlapping

# group avg
tmp_df2 <- sales_yahoo %>% 
  group_by(five_day) %>% 
  summarise(m = mean(num_order, na.rm = T), se = sd(num_order, na.rm = T)/sqrt(length(num_order)))
# tmp3 <- dplyr::filter(sales_yahoo, num_order == 0) # how many days without sales?

fd_vs_no_pre <- ggplot(tmp_df) + 
  geom_point(aes(x = tmp_x, y = num_order, color = weekday)) +
  geom_point(data = tmp_df2, aes(x = five_day, y = m), size = 2) +
  geom_errorbar(data = tmp_df2, aes(x = five_day, ymin = m-se, ymax = m + se), color = "black", width = .1, size = 1)
  
  
fd_vs_no <- format_gg(gplot = fd_vs_no_pre, xlabel = "Go no Tsuku Hi (1: yes, 0:no)", ylabel = "Number of Order",
                        ptitle = "5-no-tsuku-hi vs Number of Order (Ind + Grp Mean and SE)",
                        xlimit = c(-1, 2), ylimit = c(0,12), xticks = 0:1, yticks = seq(0,12,2), 
                        pcol = rainbow(7))


## five day vs page view

# group avg
tmp_df11 <- sales_yahoo %>% 
  group_by(five_day) %>% 
  summarise(m = mean(pageview, na.rm = T), se = sd(pageview, na.rm = T)/sqrt(length(pageview)))
# tmp3 <- dplyr::filter(sales_yahoo, num_order == 0) # how many days without sales?

fd_vs_pv_pre <- ggplot(sales_yahoo, aes(x = five_day)) + 
  geom_point(aes(y = pageview, color = weekday)) +
  geom_point(data = tmp_df11, aes(x = five_day, y = m), size = 2) +
  geom_errorbar(data = tmp_df11, aes(x = five_day, ymin = m-se, ymax = m + se), color = "black", width = .1, size = 1)


fd_vs_pv <- format_gg(gplot = fd_vs_pv_pre, xlabel = "Go no Tsuku Hi (1: yes, 0:no)", ylabel = "Number of Order",
                      ptitle = "5-no-tsuku-hi vs Page View (Ind + Grp Mean and SE)",
                      xlimit = c(-1, 2), ylimit = c(0,700), xticks = 0:1, yticks = seq(0,700,200), 
                      pcol = rainbow(7))


## order_per_unique_customer vs five_day
tmp_df21 <- sales_yahoo %>% 
  group_by(five_day) %>% 
  summarise(m = mean(order_per_unique_customer, na.rm = T), se = sd(order_per_unique_customer, na.rm = T)/sqrt(length(order_per_unique_customer)))
# tmp3 <- dplyr::filter(sales_yahoo, num_order == 0) # how many days without sales?

fd_vs_pv_opuc_pre <- ggplot(sales_yahoo, aes(x = five_day)) + 
  geom_point(aes(y = order_per_unique_customer, color = weekday)) +
  geom_point(data = tmp_df21, aes(x = five_day, y = m), size = 2) +
  geom_errorbar(data = tmp_df21, aes(x = five_day, ymin = m-se, ymax = m + se), color = "black", width = .1, size = 1)


fd_vs_pv_opuc <- format_gg(gplot = fd_vs_pv_opuc_pre, xlabel = "Go no Tsuku Hi (1: yes, 0:no)", ylabel = "Order by Unique Customer",
                      ptitle = "5-no-tsuku-hi vs Page View (Ind + Grp Mean and SE)",
                      xlimit = c(-1, 2), ylimit = c(0,0.1), xticks = 0:1, yticks = seq(0, 0.2,.05), 
                      pcol = rainbow(7))



## Save plots
# Prep
main_dir <- "linear_reg_yahoo"
sub_dir <- "linear_reg_yahoo"
desc_note <- "Simple linear regressions on sales data"

# Save
save_plots(tgt_plot = pv_vs_no, fname = "pageview_numorder_lin", pdf_only = T)
save_plots(tgt_plot = fd_vs_no, fname = "fiveday_numorder", pdf_only = T)
save_plots(tgt_plot = fd_vs_pv, fname = "fiveday_pageview", pdf_only = T)
save_plots(tgt_plot = fd_vs_pv_opuc, fname = "fiveday_orderbyuniquecustomer", pdf_only = T)


### Stats ###
library(stargazer)

## Model 1
stats_res1 <- lm(formula = num_order ~ pageview, data = sales_yahoo)
summary(stats_res1)

## Model 2
stats_res2 <- lm(formula = num_order ~ pageview + five_day, data = sales_yahoo)
summary(stats_res2)

## Model 3
stats_res3 <- lm(formula = num_order ~ pageview + weekend + five_day, data = sales_yahoo)
summary(stats_res3)

stargazer(stats_res1, stats_res2, stats_res3, type = "html", align = T, out = sprintf("stats/%s_stargazer.html", sub_dir),
          report = ("vcsp*"))




