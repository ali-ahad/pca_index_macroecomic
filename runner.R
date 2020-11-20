install.packages("tibbletime")
library(tseries)
library(zoo)
library(rio)
library(tibbletime)
library(dplyr)
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

# Get average earnings manufacturing data and filter it over required years
avg_earnings_manu = dataset_creator(dataset_folder_path, files[[1]], "AWE - Manufacturing (In Dollars)", "2010-01-01")
avg_earnings_manu
nrow(avg_earnings_manu)

# Get average earnings private data and filter it over required years
avg_earnings_private = dataset_creator(dataset_folder_path, files[[2]], "AWE - Private (In Dollars)", "2010-01-01")
avg_earnings_private
nrow(avg_earnings_private)

# Get average weekly hours manufacturing data and filter it over required years
avg_weekly_manu = dataset_creator(dataset_folder_path, files[[3]], "AWH - Manufacturing", "2010-01-01")
avg_weekly_manu
nrow(avg_weekly_manu)

# Get average weekly hours manufacturing data and filter it over required years
avg_weekly_private = dataset_creator(dataset_folder_path, files[[4]], "AWH - Private", "2010-01-01")
avg_weekly_private
nrow(avg_weekly_private)

# Get cpi urban data and filter it over required years
cpi_urban = dataset_creator(dataset_folder_path, files[[5]], "CPI - Urban", "2010-01-01")
cpi_urban
nrow(cpi_urban)

emv_deficit = dataset_creator(dataset_folder_path, files[[6]], "EMV - Deficit", "2010-01-01")
emv_deficit
nrow(emv_deficit)

emv_disease = dataset_creator(dataset_folder_path, files[[7]], "EMV - Disease", "2010-01-01")
emv_disease
nrow(emv_disease)

emv_immigration = dataset_creator(dataset_folder_path, files[[8]], "EMV - Immigration", "2010-01-01")
emv_immigration
nrow(emv_immigration)

emv_overall = dataset_creator(dataset_folder_path, files[[9]], "EMV - Overall", "2010-01-01")
emv_overall
nrow(emv_overall)

emv_politics = dataset_creator(dataset_folder_path, files[[10]], "EMV - Politics", "2010-01-01")
emv_politics
nrow(emv_politics)

epu_fiscal = dataset_creator(dataset_folder_path, files[[11]], "EPU Index - Fiscal", "2010-01-01")
epu_fiscal
nrow(epu_fiscal)

exports_pi = dataset_creator(dataset_folder_path, files[[12]], "Exports Price Index", "2010-01-01")
exports_pi
nrow(exports_pi)

fed_fund_rate = dataset_creator(dataset_folder_path, files[[13]], "Effective Fed Funds Rate (%)", "2010-01-01")
fed_fund_rate
nrow(fed_fund_rate)

imports_pi = dataset_creator(dataset_folder_path, files[[14]], "Imports Price Index", "2010-01-01")
imports_pi
nrow(imports_pi)

industrial_pi = dataset_creator(dataset_folder_path, files[[15]], "Industrial Production Index", "2010-01-01")
industrial_pi
nrow(industrial_pi)

labour_participation_rate = dataset_creator(dataset_folder_path, files[[17]], "Labor Participation Rate (%)", "2010-01-01")
labour_participation_rate
nrow(labour_participation_rate)

unemployment_rate = dataset_creator(dataset_folder_path, files[[18]], "Unemployment Rate (%)", "2010-01-01")
unemployment_rate
nrow(unemployment_rate)

mv_fed_debt = dataset_creator(dataset_folder_path, files[[19]], "Market Value of Federal Debt (billions)", "2010-01-01")
mv_fed_debt
nrow(mv_fed_debt)

one_month_libor = dataset_creator(dataset_folder_path, files[[20]], "One Month Libor (%)", "2010-01-01")
one_month_libor = one_month_libor[(xts:::startof(one_month_libor$Date, "months")),]
one_month_libor = one_month_libor[1 : nrow(one_month_libor) - 1,]
one_month_libor
nrow(one_month_libor)

savings_rate = dataset_creator(dataset_folder_path, files[[21]], "Personal Savings Rate (%)", "2010-01-01")
savings_rate = rbind(savings_rate, c(NA, NA))
savings_rate
nrow(savings_rate)

ppi_commodities = dataset_creator(dataset_folder_path, files[[22]], "PPI - Commodities", "2010-01-01")
ppi_commodities
nrow(ppi_commodities)

ppi_investments = dataset_creator(dataset_folder_path, files[[23]], "PPI - Investments", "2010-01-01")
ppi_investments = rbind(ppi_investments, c(NA, NA))
nrow(ppi_investments)

ppi_manufacturing = dataset_creator(dataset_folder_path, files[[24]], "PPI - Manufacturing", "2010-01-01")
ppi_manufacturing
nrow(ppi_manufacturing)

real_er = dataset_creator(dataset_folder_path, files[[25]], "Real Effective Exchange Rate (%)", "2010-01-01")
real_er
nrow(real_er)

three_month_yields = dataset_creator(dataset_folder_path, files[[26]], "Three Month Yields (%)", "2010-01-01")
three_month_yields = three_month_yields[(xts:::startof(three_month_yields$Date, "months")),]
three_month_yields
nrow(three_month_yields)

gdp = import_list(paste(dataset_folder_path, "/", files[[16]], sep = ""))
gdp_data = tail(gdp$Data, 129)
gdp_data = rbind(gdp_data, c(NA, NA, NA))
gdp_data$
nrow(gdp_data)

stock_vs_mefactors = data.frame(avg_earnings_manu$Date, as.numeric(sp500_index$Close), gdp_data$`Monthly Real GDP Index`, three_month_yields$`Three Month Yields (%)`,
                                real_er$`Real Effective Exchange Rate (%)`, ppi_manufacturing$`PPI - Manufacturing`, 
                                ppi_investments$`PPI - Investments`, ppi_commodities$`PPI - Commodities`, savings_rate$`Personal Savings Rate (%)`,
                                one_month_libor$`One Month Libor (%)`, mv_fed_debt$`Market Value of Federal Debt (billions)`, labour_participation_rate$`Labor Participation Rate (%)`,
                                unemployment_rate$`Unemployment Rate (%)`, industrial_pi$`Industrial Production Index`, imports_pi$`Imports Price Index`,
                                fed_fund_rate$`Effective Fed Funds Rate (%)`, exports_pi$`Exports Price Index`, epu_fiscal$`EPU Index - Fiscal`,
                                emv_politics$`EMV - Politics`, emv_overall$`EMV - Overall`, emv_immigration$`EMV - Immigration`, emv_disease$`EMV - Disease`,
                                emv_deficit$`EMV - Deficit`, cpi_urban$`CPI - Urban`, avg_weekly_private$`AWH - Private`, 
                                avg_weekly_manu$`AWH - Manufacturing`, avg_earnings_private$`AWE - Private (In Dollars)`, avg_earnings_manu$`AWE - Manufacturing (In Dollars)`)

stock_vs_mefactors

# Clear console and environment
rm(list=ls())
cat("\014")  # ctrl+L
