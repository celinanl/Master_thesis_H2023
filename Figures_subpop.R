#################################
#Celina Lundevik, University of Oslo, 2023
#################################

install.packages("ggplot2")
library(ggplot2)
install.packages("patchwork")
library(patchwork)

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


#Read in scenarios w/ a skewed bycatch
sk1 <- lapply(1:55, function(x) readRDS(sprintf("./results2/scenario%d.RData", x)))

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
sk$fscenario <- factor(sk$scenario, levels = 1:55, 
                       labels = sprintf("hunt = %.02f", akkar$hunt))

akkar <- expand.grid(hunt = seq(0, 0.10, 0.01), bycatch = seq(0,0.1, 0.025))
sk1$fscenario <- factor(sk1$scenario, levels = 1:55, 
                        labels = sprintf("hunt = %.02f", akkar$hunt))

##Growth curves##

#BC_0.00#

pc_1<- ggplot(data = sk[county %in% c(3, 7, 10) & scenario %in% c(1,4,6,11)]) + 
      labs(fscenario = "", fill = "", col = "")+
      facet_wrap(~county, scales = "free_y", labeller = as_labeller(c("3" = "Nordland",
                                                                      "7" = "Sogn & Fjordane",
                                                                      "10" = "East-Agder"
                                                                      
                                                                      # Add more county names and labels as needed
      ))) +
      geom_ribbon(data = sk[county %in% c(3, 7, 10) & scenario %in% c(1,4,6,11)], aes(x = year, ymin = q_2.5, ymax = q_975, fill = fscenario), alpha = 0.5)+
      geom_line(lwd = 1,aes(x = year, y = mean, group = scenario, col = fscenario))+
      
      xlab("Time (Year)")+
      ylab("Population size (N)")+
      theme(
            panel.background = element_rect(fill = 'white', color = 'lightgray'),
            panel.grid = element_line(color = "lightgray"))+
      theme(legend.position = "none")
      
  

#BC_0.05#
pc_2 <- ggplot(data = sk1[county %in% c(3, 7, 10) & scenario %in% c(23,26,28,33)]) + 
      labs(fscenario = "", fill = "", col = "")+
      facet_wrap(~county, scales = "free_y", labeller = as_labeller(c("3" = "Nordland",
                                                                      "7" = "Sogn & Fjordane",
                                                                      "10" = "East-Agder"
                                                                      
                                                                      # Add more county names and labels as needed
      ))) +
      geom_ribbon(data = sk1[county %in% c(3, 7, 10) & scenario %in% c(23,26,28,33)], aes(x = year, ymin = q_2.5, ymax = q_975, fill = fscenario), alpha = 0.5)+
      geom_line(lwd = 1,aes(x = year, y = mean, group = scenario, col = fscenario))+
      
      xlab("Time (Year)")+
      ylab("Population size (N)")+
      theme(legend.position = "bottom")+
      theme(
            panel.background = element_rect(fill = 'white', color = 'lightgray'),
            panel.grid = element_line(color = "lightgray"))
      


pc_1/pc_2


#appendix

akkar <- expand.grid(hunt = seq(0, 0.10, 0.01), bycatch = seq(0,0.1, 0.025))
sk0$fscenario <- factor(sk0$scenario, levels = 1:55, 
                        labels = sprintf("hunt = %.02f", akkar$hunt))


ggplot(data = sk[scenario %in% 12:22]) + 
      labs(fscenario = "", fill = "", col = "")+
      facet_wrap(~county, scales = "free_y", labeller = as_labeller(c("1" = "Finnmark", 
                                                                      "2" = "Troms", 
                                                                      "3" = "Nordland",
                                                                      "4" = "N-Trøndelag",
                                                                      "5" = "S-Trøndelag",
                                                                      "6" = "Møre & Romsdal",
                                                                      "7" = "Sogn & Fjordane",
                                                                      "8" = "Rogaland",
                                                                      "9" = "West-Agder",
                                                                      "10" = "East-Agder",
                                                                      "11" = "Telemark",
                                                                      "12" = "Vestfold",
                                                                      "13" = "Østfold"
                                                                      # Add more county names and labels as needed
      ))) +
      geom_line(aes(x = year, y = mean, group = scenario, col = fscenario))+
      ggtitle(" 
                                                (Bycatch = 0.025)
              
              "
      )+
      xlab("Time (Year)")+
      ylab("Population size (N)")+
      theme_classic()+
      theme(legend.position = "right")




#BC_0.00#
sk_11 <- subset(sk, year == 101 & scenario %in% c(1,4,6,11) & county %in% c(3,7,10))

ggplot(data = sk_11, aes(x=factor(scenario), y=mean)) +
      geom_point() +
      scale_x_discrete(labels = my_labels) +
      ylab("Population size (N)")+
      xlab("Hunting level (%)")+
      facet_wrap(~county, scales = "free_y", labeller = as_labeller(c("3" = "Nordland",
                                                                      "7" = "Sogn & Fjordane",
                                                                      "10" = "East-Agder"
                                                                      # Add more county names and labels as needed
      ))) +
      geom_crossbar(aes(ymin=q_2.5, ymax=q_975))+
      theme(
            plot.margin = unit(c(1, 1, 8, 1), "lines"),  # Adjust the plot margins
            panel.background = element_rect(fill = 'white', color = 'lightgray'),
            panel.grid = element_line(color = "lightgray")
      )


#BC_0.05#
sk_11 <- subset(sk1, year == 101 & scenario %in% c(23,26,28,33) & county %in% c(3,7,10))

ggplot(data = sk_11, aes(x=factor(scenario), y=mean)) +
      geom_point() +
      scale_x_discrete(labels = my_labels) +
      ylab("Population size (N)")+
      xlab("Hunting level (%)")+
      facet_wrap(~county, scales = "free_y", labeller = as_labeller(c("3" = "Nordland",
                                                                      "7" = "Sogn & Fjordane",
                                                                      "10" = "East-Agder"
                                                                      # Add more county names and labels as needed
      ))) +
      geom_crossbar(aes(ymin=q_2.5, ymax=q_975))+
      theme(
            plot.margin = unit(c(1, 1, 8, 1), "lines"),  # Adjust the plot margins
            panel.background = element_rect(fill = 'white', color = 'lightgray'),
            panel.grid = element_line(color = "lightgray")
      )


##QE_plots##

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



# calculating the quasi-extinction probability by county by making a new column with the Prob_qe
sk0_qe <- sk_qe[, . (prob_qe = (sum(tot_val<QE)/.N)), .(year, scenario, county)]

#Making fitting labels for sk0_qe
my_labels<- c("0.0", "0.03", "0.05", "0.1")

#Making a bar plot for the QE in all the counties
pc_qe1<- ggplot(data =sk0_qe[scenario %in% 1:11][year %in% 101][county %in% c(3,7,10)])+
            geom_col(aes(x = factor(scenario), y=prob_qe, col = factor(scenario), fill=factor(scenario)))+
            scale_x_discrete(labels = my_labels) +
            ylab("QE probability after 100 years")+
            xlab("Hunting level")+
            facet_wrap(~county,labeller = as_labeller(c("3" = "Nordland",
                                                  "7" = "Sogn & Fjordane",
                                                  "10" = "East-Agder"
                                                  # Add more county names and labels as needed
            ))) +
            theme(
                  plot.margin = unit(c(1, 1, 8, 1), "lines"),  # Adjust the plot margins
                  panel.background = element_rect(fill = 'white', color = 'lightgray'),
                  panel.grid = element_line(color = "lightgrey"),
                  legend.position = "none"
            )

pc_qe1<- ggplot(data =sk0_qe[scenario %in% c(1,4,6,11)][year %in% 101][county %in% c(3,7,10)])+
      geom_col(aes(x = factor(scenario), y=prob_qe, col = factor(scenario), fill=factor(scenario)))+
      scale_x_discrete(labels = my_labels) +
      ylab("QE probability after 100 years")+
      xlab("Hunting level")+
      facet_wrap(~county,labeller = as_labeller(c("3" = "Nordland",
                                                  "7" = "Sogn & Fjordane",
                                                  "10" = "East-Agder"
                                                  # Add more county names and labels as needed
      ))) +
      theme(
            panel.background = element_rect(fill = 'white', color = 'lightgray'),
            panel.grid = element_line(color = "lightgrey"),
            legend.position = "none"
      )

pc_qe2<-ggplot(data =sk0_qe[scenario %in% 23:33][year %in% 101][county %in% c(3,7,10)])+
            geom_col(aes(x = factor(scenario), y=prob_qe, col = factor(scenario), fill=factor(scenario)))+
            scale_x_discrete(labels = my_labels) +
            ylab("QE probability after 100 years")+
            xlab("Hunting level")+
            facet_wrap(~county,labeller = as_labeller(c("3" = "Nordland",
                                                  "7" = "Sogn & Fjordane",
                                                  "10" = "East-Agder"
                                                  # Add more county names and labels as needed
            ))) +
            theme(
                  plot.margin = unit(c(1, 1, 8, 1), "lines"),  # Adjust the plot margins
                  panel.background = element_rect(fill = 'white', color = 'lightgray'),
            
                  panel.grid = element_line(color = "lightgray"),
                  legend.position = "none"
            )

pc_qe2<-ggplot(data =sk0_qe[scenario %in% c(23,26,28,33)][year %in% 101][county %in% c(3,7,10)])+
      geom_col(aes(x = factor(scenario), y=prob_qe, col = factor(scenario), fill=factor(scenario)))+
      scale_x_discrete(labels = my_labels) +
      ylab("QE probability after 100 years")+
      xlab("Hunting level")+
      facet_wrap(~county,labeller = as_labeller(c("3" = "Nordland",
                                                  "7" = "Sogn & Fjordane",
                                                  "10" = "East-Agder"
                                                  # Add more county names and labels as needed
      ))) +
      theme(
            panel.background = element_rect(fill = 'white', color = 'lightgray'),
            
            panel.grid = element_line(color = "lightgray"),
            legend.position = "none"
      )

pc_qe1/pc_qe2
ggplot(data =sk0_qe[scenario %in% 23:33][year %in% 101][county %in% c(3,7,10)])+
      geom_col(aes(x = factor(scenario), y=prob_qe, col = factor(scenario), fill=factor(scenario)))+
      scale_x_discrete(labels = my_labels) +
      ylab("Quasi-extinction probability after 100 years")+
      xlab("Hunting level")+
      facet_wrap(~county,labeller = as_labeller(c("3" = "Nordland",
                                                  "7" = "Sogn & Fjordane",
                                                  "10" = "East-Agder"
                                                  # Add more county names and labels as needed
      ))) +
      theme(
            plot.margin = unit(c(1, 1, 8, 1), "lines"),  # Adjust the plot margins
            panel.background = element_rect(fill = 'white', color = 'lightgray'),
            panel.grid = element_line(color = "lightgray"),
            legend.position = "none"
      )
