library(tseries)
library(zoo)
library(rio)
library(tibbletime)
library(dplyr)
library(imputeTS)

cur_dir <- getwd()
cur_dir

# Scrape data for features from individual dataset files for required number of years
dataset_creator = function(folder_path, file_name) {
  df = import_list(paste(folder_path, "/", file_name, sep = ""))$`FRED Graph`
  colnames(df) = c("Date")
  df = df[df$Date >= "1992-02-01",]
  return(df)  
}

dataset_folder_path = paste(cur_dir, "/datasets", sep = "")
files = list.files(dataset_folder_path)
files
sp500_index = get.hist.quote(instrument = "^GSPC", start = "1992-02-01", end = "2020-11-01", quote = "Close", provider = "yahoo", compression = "m")

cpi_urban = dataset_creator(dataset_folder_path, files[[1]])
emv_deficit = dataset_creator(dataset_folder_path, files[[2]])
emv_disease = dataset_creator(dataset_folder_path, files[[3]])
emv_immigration = dataset_creator(dataset_folder_path, files[[4]])
emv_overall = dataset_creator(dataset_folder_path, files[[5]])
emv_politics = dataset_creator(dataset_folder_path, files[[6]])
epu_fiscal = dataset_creator(dataset_folder_path, files[[7]])
exports_pi = dataset_creator(dataset_folder_path, files[[8]])
fed_fund_rate = dataset_creator(dataset_folder_path, files[[9]])
imports_pi = dataset_creator(dataset_folder_path, files[[10]])
industrial_pi = dataset_creator(dataset_folder_path, files[[11]])
labour_participation_rate = dataset_creator(dataset_folder_path, files[[13]])
unemployment_rate = dataset_creator(dataset_folder_path, files[[14]])
mv_fed_debt = dataset_creator(dataset_folder_path, files[[15]])

one_month_libor = dataset_creator(dataset_folder_path, files[[16]])
one_month_libor = one_month_libor[(xts:::startof(one_month_libor$Date, "months")),]
one_month_libor = one_month_libor[1 : nrow(one_month_libor) - 1,]
one_month_libor[,2] = na_kalman(one_month_libor[,2])

savings_rate = dataset_creator(dataset_folder_path, files[[17]])
savings_rate = rbind(savings_rate, c(NA, NA))
savings_rate[,2] = na_kalman(savings_rate[,2])

ppi_commodities = dataset_creator(dataset_folder_path, files[[18]])

ppi_investments = dataset_creator(dataset_folder_path, files[[19]])
ppi_investments = rbind(ppi_investments, c(NA, NA))
ppi_investments[,2] = na_kalman(ppi_investments[,2]) 

ppi_manufacturing = dataset_creator(dataset_folder_path, files[[20]])

three_month_yields = dataset_creator(dataset_folder_path, files[[21]])
three_month_yields = three_month_yields[(xts:::startof(three_month_yields$Date, "months")),]
three_month_yields[,2] = na_kalman(three_month_yields[,2])

gdp = import_list(paste(dataset_folder_path, "/", files[[12]], sep = ""))
gdp_data = tail(gdp$Data, 344)
gdp_data = rbind(gdp_data, c(NA, NA, NA))
gdp_data[, c(2, 3)] = na_kalman(gdp_data[, 2:3])

stock_vs_mefactors = data.frame(three_month_yields$Date, as.numeric(sp500_index$Close), gdp_data[,3], three_month_yields[,2],
                                ppi_manufacturing[,2], ppi_investments[,2], ppi_commodities[,2], savings_rate[,2],
                                one_month_libor[,2], mv_fed_debt[,2], labour_participation_rate[,2], unemployment_rate[,2], 
                                industrial_pi[,2], imports_pi[,2], fed_fund_rate[,2], exports_pi[,2], epu_fiscal[,2],
                                emv_politics[,2], emv_overall[,2], emv_immigration[,2], emv_disease[,2], emv_deficit[,2], 
                                cpi_urban[,2])

colnames(stock_vs_mefactors) = c("Date", "S&P Monthly", "Real GDP Index", "3-Month Yields (%)", "PPI - Manufacturing", 
                                 "PPI - Investments", "PPI - Commodities", "Personal Savings Rate (%)", "1-month LIBOR (%)", 
                                 "Market Value - Federal Debt (billions)", "Labour Participation (%)", "Unemployment (%)", 
                                 "Indsutrial PI", "Imports PI", "Federal Fund Rate (%)", "Exports PI", "EPU Index - Fiscal", 
                                 "EMV - Politics", "EMV - Overall", "EMV - Migration", "EMV - Disease", "EMV - Deficits", 
                                 "CPI - Urban")
save(stock_vs_mefactors, file = "scrapped_data.RData")
