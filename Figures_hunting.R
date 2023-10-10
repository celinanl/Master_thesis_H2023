#################################
#Celina Lundevik, University of Oslo, 2023
#################################

install.packages("ggplot2")
library(ggplot2)


sk <- lapply(1:55, function(x) readRDS(sprintf("./results/scenario%d.RData", x)))


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

##Figure bycatch = 0% ##

ggplot(data = sk2[scenario %in% c(1:11)])+
      labs(fscenario = "", fill = "", col = "")+
      facet_wrap(~scenario, scales = "free_y", ncol = 3, labeller = as_labeller(c("1" = "A: Hunt= 0.00", 
                                                                                  "2" = "B: Hunt= 0.01", 
                                                                                  "3" = "C: Hunt= 0.02",
                                                                                  "4" = "D: Hunt= 0.03", 
                                                                                  "5" = "E: Hunt= 0.04",
                                                                                  "6" = "F: Hunt= 0.05", 
                                                                                  "7" = "G: Hunt= 0.06",
                                                                                  "8" = "H: Hunt= 0.07", 
                                                                                  "9" = "I: Hunt= 0.08",
                                                                                  "10" = "J: Hunt= 0.09",
                                                                                  "11" = "K: Hunt= 0.10"
                                                                                  
      ))) +
      geom_ribbon(aes(x = year, ymin = Q_2.5, ymax = Q_975, fill = fscenario), alpha = 0.5)+
      geom_line(aes(x = year, y = mean, group = scenario, col = fscenario))+
      ggtitle("                                       (Bycatch = 0.00)
      "
      )+
      xlab("Time (Year)")+
      ylab("Population size (N)")+
      theme_bw()+
      theme(legend.position = "none")



###crossbar

sk2_scenario12 <- subset(sk2, year == 101 & scenario == c(1:11))
my_labels<- c("0.0", "", "", "", "", "0.05", "", "", "", "", "0.1")

ggplot(data = sk2_scenario12, aes(x=factor(scenario), y=mean)) +
      geom_point() +
      scale_x_discrete(labels = my_labels) +
      ylab("Population size (N)")+
      xlab("Hunting level")+
      geom_crossbar(aes(ymin=Q_2.5, ymax=Q_975))+
      theme(
            panel.background = element_rect(fill = 'white', color = 'lightgray'),
            panel.grid = element_line(color = "lightgray")
      )

##Figure 3.4 (QE)##
sk <- lapply(1:55, function(x) readRDS(sprintf("./try_results/scenario%d.RData", x)))

# change from nested data structure to flat data table data structure. By county
# excluding quantiles
sk_qe <- lapply(1:length(sk), function(i) {
      lapply(1:length(sk[[i]]), function(j) {
            n_reps = ncol(sk[[i]][[j]]$tot_vals)
            n_year = nrow(sk[[i]][[j]]$tot_vals)
            data.table(year = rep(1:n_year, times = n_reps),
                       scenario = i,
                       county = j,
                       replicate = rep(1:n_reps, each = n_year),
                       tot_val = as.vector(sk[[i]][[j]]$tot_vals))
            
      }) |> rbindlist()
}) |> rbindlist()

#Calculating the quasi-extinction probability for the whole population (bycatch setting A)
sk2_qe = sk_qe[, .(tot_val=sum(tot_val, na.rm = TRUE)), .(replicate, year, scenario)]

sk3_qe <- sk2_qe[, .(prob_qe = sum(tot_val<QE*13)/.N), .(year, scenario)]



my_labels<- c("0.0", "0.01", "0.02", "0.03", "0.04", "0.05", "0.06", "0.07", "0.08", "0.09", "0.1")
ggplot(data = sk3_qe[scenario %in% 1:11][year %in% 101]) +
      geom_col(aes(x = factor(scenario), y=prob_qe, col = factor(scenario), fill=factor(scenario)))+
      scale_x_discrete(labels = my_labels) +
      ylab("Quasi-extinction probability after 100 years")+
      xlab("Hunting level")+
      facet_wrap(~year, labeller = as_labeller(c("101" = "Bycatch = 0.00")))+
      theme(
            legend.position = "none",
            panel.background = element_rect(fill = 'white', color = 'lightgray'),
            panel.grid = element_line(color = "lightgray")
      )


# Picking out the exact value of Prob_QE
sk3_qe %>% 
      filter(year == 101, scenario == 11) %>% 
      select(prob_qe)
