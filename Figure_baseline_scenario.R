#################################
#Celina Lundevik, University of Oslo, 2023
#################################

install.packages("ggplot2")
library(ggplot2)

sk0 <- lapply(1, function(x) readRDS(sprintf("./results/scenario%d.RData", x)))

# change from nested data structure to flat data table data structure. By county
# including quantiles
sk0 <- lapply(1:length(sk0), function(i) {
      lapply(1:length(sk0[[i]]), function(j) {
            
            data.table(year = 1:nrow(sk0[[i]][[j]]$tot_vals),
                       scenario = i,
                       county = j,
                       mean = rowMeans(sk0[[i]][[j]]$tot_vals), 
                       sd = apply(sk0[[i]][[j]]$tot_vals, 1, sd),
                       q_2.5 = apply(sk0[[i]][[j]]$tot_vals, 1, quantile, probs=0.025),
                       q_975 = apply(sk0[[i]][[j]]$tot_vals, 1, quantile, probs = 0.975))
            
      }) |> rbindlist()
}) |> rbindlist()




#Making a dataset for the Norwegian population. Adding the county populations together. With quantiles
sk4 <- sk0[, .(mean = sum(mean), sd = sqrt(sd^2), Q_2.5 = sum(q_2.5), Q_975 = sum(q_975)), .(year, scenario)]

##Figure 3.1##
#Making legends with bycatch and hunting
akkar <- expand.grid(hunt = seq(0, 0.10, 0.01), bycatch = seq(0,0.1, 0.025))
sk4$fscenario <- factor(sk4$scenario, levels = 1:55, 
                        labels = sprintf("hunt = %.02f, bycatch = %.02f", akkar$hunt, akkar$bycatch))
ggplot(data = sk4[scenario %in% 1])+
      labs(fscenario = "", fill = "", col = "")+
      facet_wrap(~scenario, labeller = as_labeller(c("1" = "H=0.0, E=0.0, P_D=0.0, f=0.0")))+
      geom_hline(yintercept=15467, linetype="dashed", color = "black")+
      geom_ribbon(aes(x = year, ymin = Q_2.5, ymax = Q_975, fill = fscenario), alpha = 0.5)+
      geom_line(aes(x = year, y = mean, group = scenario, col = fscenario))+
      xlab("Time (Year)")+
      ylab("Population size (N)")+
      theme_bw()+
      theme(legend.position = "none")

year_labels <- c(0, 25, 50, 75, 100)

# Filter the data for the year X and extract the mean value
sk4 %>% 
      filter(year == 5) %>% 
      select(c(Q_2.5, Q_975))


sk4 %>% 
      filter(abs(mean - 10000) == min(abs(mean - 10000))) %>% 
      select(year)