#################################
#Celina Lundevik, University of Oslo, 2023
#################################

install.packages("ggplot2")
library(ggplot2)


sk <- lapply(1:55, function(x) readRDS(sprintf("./try_results/scenario%d.RData", x)))


# change from nested data structure to flat data table data structure. By county
# including quantiles
sk <- lapply(1:length(sk), function(i) {
      lapply(1:length(sk[[i]]), function(j) {
            
            data.table(year = 1:nrow(sk[[i]][[j]]$tot_vals),
                       scenario = i,
                       county = j,
                       mean = rowMeans(sk[[i]][[j]]$tot_vals), 
                       sd = apply(sk[[i]][[j]]$tot_vals, 1, sd),
                       q_2.5 = apply(sk[[i]][[j]]$tot_vals, 1, quantile, probs=0.025),
                       q_975 = apply(sk[[i]][[j]]$tot_vals, 1, quantile, probs = 0.975))
            
      }) |> rbindlist()
}) |> rbindlist()

sk2 <- sk[, .(mean = sum(mean), sd = sqrt(sd^2), Q_2.5 = sum(q_2.5), Q_975 = sum(q_975)), .(year, scenario)]


#Making legends with bycatch and hunting
akkar <- expand.grid(hunt = seq(0, 0.10, 0.01), bycatch = seq(0,0.1, 0.025))
sk2$fscenario <- factor(sk$scenario, levels = 1:55, 
                        labels = sprintf("hunt = %.02f, bycatch = %.02f", akkar$hunt, akkar$bycatch))


#Read in scenarios w/ a skewed bycatch
sk1 <- lapply(1:55, function(x) readRDS(sprintf("./results/scenario%d.RData", x)))

# change from nested data structure to flat data table data structure. By county
# including quantiles
sk1 <- lapply(1:length(sk1), function(i) {
      lapply(1:length(sk1[[i]]), function(j) {
            
            data.table(year = 1:nrow(sk1[[i]][[j]]$tot_vals),
                       scenario = i,
                       county = j,
                       mean = rowMeans(sk1[[i]][[j]]$tot_vals), 
                       sd = apply(sk1[[i]][[j]]$tot_vals, 1, sd),
                       q_2.5 = apply(sk1[[i]][[j]]$tot_vals, 1, quantile, probs=0.025),
                       q_975 = apply(sk1[[i]][[j]]$tot_vals, 1, quantile, probs = 0.975))
            
      }) |> rbindlist()
}) |> rbindlist()

#Making legends with bycatch and hunting
akkar <- expand.grid(hunt = seq(0, 0.10, 0.01), bycatch = seq(0,0.1, 0.025))
sk1$fscenario <- factor(sk1$scenario, levels = 1:55, 
                        labels = sprintf("hunt = %.02f, bycatch = %.02f", akkar$hunt, akkar$bycatch))


#Making a dataset for the Norwegian population. Adding the countie populations together. With quantiles
sk3 <- sk1[, .(mean = sum(mean), sd = sqrt(sd^2), Q_2.5 = sum(q_2.5), Q_975 = sum(q_975)), .(year, scenario)]


#Legends for sk3
#With hunt and bycatch
akkar <- expand.grid(hunt = seq(0, 0.10, 0.01), bycatch = seq(0,0.1, 0.025))
sk3$fscenario <- factor(sk3$scenario, levels = 1:55, 
                        labels = "skewed")

##Figure 2.5% bycatch w/skewed bycatch##
ggplot()+
      labs(fscenario = "", fill = "", col = "")+
      facet_wrap(~scenario, scales = "free_y", ncol = 3, labeller = as_labeller(c("12" = "A: Hunt= 0.00", 
                                                                                  "13" = "B: Hunt= 0.01", 
                                                                                  "14" = "C: Hunt= 0.02",
                                                                                  "15" = "D: Hunt= 0.03", 
                                                                                  "16" = "E: Hunt= 0.04",
                                                                                  "17" = "F: Hunt= 0.05", 
                                                                                  "18" = "G: Hunt= 0.06",
                                                                                  "19" = "H: Hunt= 0.07", 
                                                                                  "20" = "I: Hunt= 0.08",
                                                                                  "21" = "J: Hunt= 0.09",
                                                                                  "22" = "K: Hunt= 0.10"
                                                                                  
      ))) +
      geom_ribbon(data = sk3[scenario %in% c(12:22)], aes(x = year, ymin = Q_2.5, ymax = Q_975, fill = "Setting B"), alpha = 0.5)+
      geom_ribbon(data = sk2[scenario %in% c(12:22)], aes(x = year, ymin = Q_2.5, ymax = Q_975, fill = "Setting A"), alpha = 0.5)+
      geom_line(data = sk3[scenario %in% c(12:22)], lwd = 1, aes(x = year, y = mean, group = scenario, col = "Setting B"))+
      geom_line(data = sk2[scenario %in% c(12:22)], lwd = 1, aes(x = year, y = mean, group = scenario, col = "Setting A"))+
      ggtitle("                                 (Bycatch = 0.025)
      "
      )+
      xlab("Time (Year)")+
      ylab("Population size (N)")+
      theme_bw()+
      theme(legend.position = c(.85, .1))


##Figure 5% bycatch w/skewed bycatch##

ggplot()+
      labs(fscenario = "", fill = "", col = "")+
      facet_wrap(~scenario, scales = "free_y", ncol = 3, labeller = as_labeller(c("23" = "A: Hunt= 0.00", 
                                                                                  "24" = "B: Hunt= 0.01", 
                                                                                  "25" = "C: Hunt= 0.02",
                                                                                  "26" = "D: Hunt= 0.03", 
                                                                                  "27" = "E: Hunt= 0.04",
                                                                                  "28" = "F: Hunt= 0.05", 
                                                                                  "29" = "G: Hunt= 0.06",
                                                                                  "30" = "H: Hunt= 0.07", 
                                                                                  "31" = "I: Hunt= 0.08",
                                                                                  "32" = "J: Hunt= 0.09",
                                                                                  "33" = "K: Hunt= 0.10"
                                                                                  
      ))) +
      geom_ribbon(data = sk3[scenario %in% c(23:33)], aes(x = year, ymin = Q_2.5, ymax = Q_975, fill = "Setting B"), alpha = 0.5)+
      geom_ribbon(data = sk2[scenario %in% c(23:33)], aes(x = year, ymin = Q_2.5, ymax = Q_975, fill = "Setting A"), alpha = 0.5)+
      geom_line(data = sk3[scenario %in% c(23:33)], lwd = 1, aes(x = year, y = mean, group = scenario, col = "Setting B"))+
      geom_line(data = sk2[scenario %in% c(23:33)], lwd = 1, aes(x = year, y = mean, group = scenario, col = "Setting A"))+
      ggtitle("                                 (Bycatch = 0.05)
      "
      )+
      xlab("Time (Year)")+
      ylab("Population size (N)")+
      theme_bw()+
      theme(legend.position = c(.85, .1))


##Figure 7.5% bycatch w/skewed bycatch##
ggplot()+
      labs(fscenario = "", fill = "", col = "")+
      facet_wrap(~scenario, scales = "free_y", ncol = 3, labeller = as_labeller(c("34" = "A: Hunt= 0.00", 
                                                                                  "35" = "B: Hunt= 0.01", 
                                                                                  "36" = "C: Hunt= 0.02",
                                                                                  "37" = "D: Hunt= 0.03", 
                                                                                  "38" = "E: Hunt= 0.04",
                                                                                  "39" = "F: Hunt= 0.05", 
                                                                                  "40" = "G: Hunt= 0.06",
                                                                                  "41" = "H: Hunt= 0.07", 
                                                                                  "42" = "I: Hunt= 0.08",
                                                                                  "43" = "J: Hunt= 0.09",
                                                                                  "44" = "K: Hunt= 0.10"
                                                                                  
      ))) +
      geom_ribbon(data = sk3[scenario %in% c(34:44)], aes(x = year, ymin = Q_2.5, ymax = Q_975, fill = "Setting B"), alpha = 0.5)+
      geom_ribbon(data = sk2[scenario %in% c(34:44)], aes(x = year, ymin = Q_2.5, ymax = Q_975, fill = "Setting A"), alpha = 0.5)+
      geom_line(data = sk3[scenario %in% c(34:44)], lwd = 1, aes(x = year, y = mean, group = scenario, col = "Setting B"))+
      geom_line(data = sk2[scenario %in% c(34:44)], lwd = 1, aes(x = year, y = mean, group = scenario, col = "Setting A"))+
      ggtitle("                                 (Bycatch = 0.075)
      "
      )+
      xlab("Time (Year)")+
      ylab("Population size (N)")+
      theme_bw()+
      theme(legend.position = c(.85, .1))

##Figure 10% bycatch w/skewed bycatch##
ggplot()+
      labs(fscenario = "", fill = "", col = "")+
      facet_wrap(~scenario, scales = "free_y", ncol = 3, labeller = as_labeller(c("45" = "A: Hunt= 0.00", 
                                                                                  "46" = "B: Hunt= 0.01", 
                                                                                  "47" = "C: Hunt= 0.02",
                                                                                  "48" = "D: Hunt= 0.03", 
                                                                                  "49" = "E: Hunt= 0.04",
                                                                                  "50" = "F: Hunt= 0.05", 
                                                                                  "51" = "G: Hunt= 0.06",
                                                                                  "52" = "H: Hunt= 0.07", 
                                                                                  "53" = "I: Hunt= 0.08",
                                                                                  "54" = "J: Hunt= 0.09",
                                                                                  "55" = "K: Hunt= 0.10"
                                                                                  
      ))) +
      geom_ribbon(data = sk3[scenario %in% c(45:55)], aes(x = year, ymin = Q_2.5, ymax = Q_975, fill = "Setting B"), alpha = 0.5)+
      geom_ribbon(data = sk2[scenario %in% c(45:55)], aes(x = year, ymin = Q_2.5, ymax = Q_975, fill = "Setting A"), alpha = 0.5)+
      geom_line(data = sk3[scenario %in% c(45:55)], lwd = 1, aes(x = year, y = mean, group = scenario, col = "Setting B"))+
      geom_line(data = sk2[scenario %in% c(45:55)], lwd = 1, aes(x = year, y = mean, group = scenario, col = "Setting A"))+
      ggtitle("                                 (Bycatch = 0.10)
      "
      )+
      xlab("Time (Year)")+
      ylab("Population size (N)")+
      theme_bw()+
      theme(legend.position = c(.85, .1))



