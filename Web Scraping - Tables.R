library(rvest)
library(dplyr)

bcra = read_html("https://www.bcra.gob.ar/PublicacionesEstadisticas/Principales_variables.asp")
bcra_table = bcra %>% html_node("table") %>% html_table()
bcra_df = bcra_table %>% filter(bcra_table != "")
bcra_df = bcra_df[-4,]
colnames(bcra_df) = c("Variables","Last Update","Value")
bcra_df

nasdaq100 = read_html("https://www.slickcharts.com/nasdaq100")
nasdaq100_table = nasdaq100 %>% html_node("table") %>% html_table()
nasdaq100_table = nasdaq100_table[,-1]
nasdaq100_table

GDP_growth_rate = read_html("https://tradingeconomics.com/country-list/gdp-annual-growth-rate")
GDP_growth_rate_table = GDP_growth_rate %>% html_node("table") %>% html_table()
GDP_growth_rate_table