############################################################################################################
# Scripts for computing figures and generating graphs in Statistics Explained article
# http://ec.europa.eu/eurostat/statistics-explained/index.php?title=Households%27_income,_consumption_and_wealth,_and_how_they_interact_-_statistics_on_main_results
# Run on R 64-bits 3.3.1


library(dplyr)
library(haven)
library(Cairo)
library(eurostat)
library(reshape2)
library(plot3D)

#################################################################################################################################################
### FIGURE 1
#################################################################################################################################################



#################################################################################################################################################
### FIGURE 2
#################################################################################################################################################

indicators_sr <- get_eurostat("icw_sr_01", time_format = "num")
indicators_agg <- get_eurostat("icw_sr_08", time_format = "num")

indicators_agg <- filter(indicators_agg,
                         age == "TOTAL")
indicators_sr <- filter(indicators_sr,
                        age == "TOTAL")
indicators_agg <- mutate(indicators_agg,
                         sr_agg = 100 - values)
indicators_sr <- mutate(indicators_sr,
                        sr_med = values)
figure2 <- merge(indicators_sr[,c("geo","sr_med")], indicators_agg[,c("geo","sr_agg")])
figure2 <- figure2[order(figure2$sr_med),]

barplot(t(figure2[,2:3]), beside = TRUE, col = c(col1, col2), main = NA,
        border = NA, legend.text = c("Median saving rate", "Aggregate saving rate"),
        names.arg = figure2$geo, cex.names = 0.5, ylim = c(-20,50), cex.axis = 0.7,
        args.legend = list(x = "topleft", bty = "n", border = NA, cex = 0.5))
