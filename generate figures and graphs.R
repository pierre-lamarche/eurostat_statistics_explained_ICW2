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

joint_dist <- get_eurostat("icw_sr_07", time_format = "num")
jointDist <- filter(joint_dist, 
                    geo == "BE")
jointDist <- mutate(jointDist,
                    q_inc = as.numeric(substr(quant_inc,2,3)),
                    q_exp = as.numeric(substr(quant_expn,2,3)))
matJointDist <- acast(data = jointDist, q_exp~q_inc, value.var = "values")/10

hist3D(1:10,1:10, matJointDist, xlab = "Income", ylab = "Consumption", 
       border = "grey", space = 0.2)

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


#################################################################################################################################################
### FIGURE 3
#################################################################################################################################################

indicators_sr <- get_eurostat("icw_sr_08", time_format = "num")
sr_na <- get_eurostat("nasa_10_ki", time_format = "num")
sr_agg <- filter(indicators_sr,
                 age == "TOTAL")
sr_na_2010 <- filter(sr_na,
                     time == 2010 & sector == "S14_S15" & na_item == "SRG_S14_S15")

sr_na_2010 <- mutate(sr_na_2010,
                     sr_na = values)
sr_agg <- mutate(sr_agg,
                 sr_agg = 100 - values)
sr <- merge(sr_agg, sr_na_2010, by = "geo")

plot(sr$sr_agg, sr$sr_na, type = "p", pch = 18, col = col1, bty = "n", xaxt="n",yaxt="n", 
     xlim = c(0,50), ylim = c(-5,20), xlab = NA, ylab = NA)
grid(nx = NA, ny = NULL)
axis(1,pos=0)
axis(2,pos=0, tick = FALSE, las = 1)
text(sr$sr_agg, sr$sr_na, labels = sr$geo, cex = 0.7, pos = 3)

