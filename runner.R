install.packages("tibbletime")
install.packages("imputeTS")
library(tseries)
library(zoo)
library(rio)
library(tibbletime)
library(dplyr)
library(imputeTS)

cur_dir <- getwd()
cur_dir

# Scrape data for features from individual dataset files for required number of years
dataset_creator = function(folder_path, file_name, val_col_title, date_select) {
  df = import_list(paste(folder_path, "/", file_name, sep = ""))$`FRED Graph`
  colnames(df) = c("Date", val_col_title)
  df = df[df$Date >= date_select,]
  return(df)  
}

dataset_folder_path = paste(cur_dir, "/datasets", sep = "")
files = list.files(dataset_folder_path)
sp500_index = get.hist.quote(instrument = "^GSPC", start = "2010-01-01", end = "2020-11-01", quote = "Close", provider = "yahoo", compression = "m")

avg_earnings_manu = dataset_creator(dataset_folder_path, files[[1]], "AWE - Manufacturing (In Dollars)", "2010-01-01")
avg_earnings_private = dataset_creator(dataset_folder_path, files[[2]], "AWE - Private (In Dollars)", "2010-01-01")
avg_weekly_manu = dataset_creator(dataset_folder_path, files[[3]], "AWH - Manufacturing", "2010-01-01")
avg_weekly_private = dataset_creator(dataset_folder_path, files[[4]], "AWH - Private", "2010-01-01")
cpi_urban = dataset_creator(dataset_folder_path, files[[5]], "CPI - Urban", "2010-01-01")
emv_deficit = dataset_creator(dataset_folder_path, files[[6]], "EMV - Deficit", "2010-01-01")
emv_disease = dataset_creator(dataset_folder_path, files[[7]], "EMV - Disease", "2010-01-01")
emv_immigration = dataset_creator(dataset_folder_path, files[[8]], "EMV - Immigration", "2010-01-01")
emv_overall = dataset_creator(dataset_folder_path, files[[9]], "EMV - Overall", "2010-01-01")
emv_politics = dataset_creator(dataset_folder_path, files[[10]], "EMV - Politics", "2010-01-01")
epu_fiscal = dataset_creator(dataset_folder_path, files[[11]], "EPU Index - Fiscal", "2010-01-01")
exports_pi = dataset_creator(dataset_folder_path, files[[12]], "Exports Price Index", "2010-01-01")
fed_fund_rate = dataset_creator(dataset_folder_path, files[[13]], "Effective Fed Funds Rate (%)", "2010-01-01")
imports_pi = dataset_creator(dataset_folder_path, files[[14]], "Imports Price Index", "2010-01-01")
industrial_pi = dataset_creator(dataset_folder_path, files[[15]], "Industrial Production Index", "2010-01-01")
labour_participation_rate = dataset_creator(dataset_folder_path, files[[17]], "Labor Participation Rate (%)", "2010-01-01")
unemployment_rate = dataset_creator(dataset_folder_path, files[[18]], "Unemployment Rate (%)", "2010-01-01")
mv_fed_debt = dataset_creator(dataset_folder_path, files[[19]], "Market Value of Federal Debt (billions)", "2010-01-01")

one_month_libor = dataset_creator(dataset_folder_path, files[[20]], "One Month Libor (%)", "2010-01-01")
one_month_libor = one_month_libor[(xts:::startof(one_month_libor$Date, "months")),]
one_month_libor = one_month_libor[1 : nrow(one_month_libor) - 1,]
one_month_libor[,2] = na_kalman(one_month_libor[,2])

savings_rate = dataset_creator(dataset_folder_path, files[[21]], "Personal Savings Rate (%)", "2010-01-01")
savings_rate = rbind(savings_rate, c(NA, NA))
savings_rate[,2] = na_kalman(savings_rate[,2])

ppi_commodities = dataset_creator(dataset_folder_path, files[[22]], "PPI - Commodities", "2010-01-01")

ppi_investments = dataset_creator(dataset_folder_path, files[[23]], "PPI - Investments", "2010-01-01")
ppi_investments = rbind(ppi_investments, c(NA, NA))
ppi_investments[,2] = na_kalman(ppi_investments[,2]) 

ppi_manufacturing = dataset_creator(dataset_folder_path, files[[24]], "PPI - Manufacturing", "2010-01-01")
real_er = dataset_creator(dataset_folder_path, files[[25]], "Real Effective Exchange Rate (%)", "2010-01-01")

three_month_yields = dataset_creator(dataset_folder_path, files[[26]], "Three Month Yields (%)", "2010-01-01")
three_month_yields = three_month_yields[(xts:::startof(three_month_yields$Date, "months")),]
three_month_yields[,2] = na_kalman(three_month_yields[,2])

gdp = import_list(paste(dataset_folder_path, "/", files[[16]], sep = ""))
gdp_data = tail(gdp$Data, 129)
gdp_data = rbind(gdp_data, c(NA, NA, NA))
gdp_data[, c(2, 3)] = na_kalman(gdp_data[, 2:3])

stock_vs_mefactors = data.frame(avg_earnings_manu$Date, as.numeric(sp500_index$Close), gdp_data[,3], three_month_yields[,2],
                                real_er[,2], ppi_manufacturing[,2], ppi_investments[,2], ppi_commodities[,2], savings_rate[,2],
                                one_month_libor[,2], mv_fed_debt[,2], labour_participation_rate[,2], unemployment_rate[,2], 
                                industrial_pi[,2], imports_pi[,2], fed_fund_rate[,2], exports_pi[,2], epu_fiscal[,2],
                                emv_politics[,2], emv_overall[,2], emv_immigration[,2], emv_disease[,2], emv_deficit[,2], 
                                cpi_urban[,2], avg_weekly_private[,2], avg_weekly_manu[,2], avg_earnings_private[,2], avg_earnings_manu[,2])

colnames(stock_vs_mefactors) = c("Date", "S&P Monthly", "Real GDP Index", "3-Month Yields (%)", "Real Exchange Rate (%)",
                                 "PPI - Manufacturing", "PPI - Investments", "PPI - Commodities", "Personal Savings Rate (%)",
                                 "1-month LIBOR (%)", "Market Value - Federal Debt (billions)", "Labour Participation (%)",
                                 "Unemployment (%)", "Indsutrial PI", "Imports PI", "Federal Fund Rate (%)", "Exports PI",
                                 "EPU Index - Fiscal", "EMV - Politics", "EMV - Overall", "EMV - Migration", "EMV - Disease",
                                 "EMV - Deficits", "CPI - Urban", "AWH - Private", "AWH - Manufacturing", "AWE - Private", "AWE - Manufacturing")
stock_vs_mefactors
# Clear console and environment
rm(list=ls())
cat("\014")  # ctrl+L
