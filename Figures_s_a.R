#################################
#Celina Lundevik, University of Oslo, 2023
#################################

install.packages("ggplot2")
library(ggplot2)
install.packages("patchwork")
library(patchwork)


sa <- lapply(1:4, function(x) readRDS(sprintf("./Sens_a1/scenario%d.RData", x)))


# change from nested data structure to flat data table data structure. By county
# including quantiles
sa <- lapply(1:length(sa), function(i) {
      lapply(1:length(sa[[i]]), function(j) {
            
            data.table(year = 1:nrow(sa[[i]][[j]]$tot_vals),
                       scenario = i,
                       county = j,
                       mean = rowMeans(sa[[i]][[j]]$tot_vals), 
                       sd = apply(sa[[i]][[j]]$tot_vals, 1, sd),
                       q_2.5 = apply(sa[[i]][[j]]$tot_vals, 1, quantile, probs=0.025),
                       q_975 = apply(sa[[i]][[j]]$tot_vals, 1, quantile, probs = 0.975))
            
      }) |> rbindlist()
}) |> rbindlist()

SA <- sa[, .(mean = sum(mean), sd = sqrt(sd^2), Q_2.5 = sum(q_2.5), Q_975 = sum(q_975)), .(year, scenario)]

akkar <- expand.grid(hunt = seq(0, 0.10, 0.01), bycatch = seq(0,0.1, 0.025))
SA$fscenario <- factor(sa$scenario)

ggplot()+
      labs(fscenario = "", fill = "", col = "")+
      
      geom_ribbon(data = SA[scenario %in% 1], aes(x = year, ymin = Q_2.5, ymax = Q_975, fill = "blue"), alpha = 0.5)+
      geom_line(data = SA[scenario %in% 1], lwd = 1, aes(x = year, y = mean, group = scenario, col = "blue"))+
      geom_ribbon(data = SA[scenario %in% 2], aes(x = year, ymin = Q_2.5, ymax = Q_975, fill = "green"), alpha = 0.5)+
      geom_line(data = SA[scenario %in% 2], lwd = 1, aes(x = year, y = mean, group = scenario, col = "green"))+
      geom_ribbon(data = SA[scenario %in% 3], aes(x = year, ymin = Q_2.5, ymax = Q_975, fill = "red"), alpha = 0.5)+
      geom_line(data = SA[scenario %in% 3], lwd = 1, aes(x = year, y = mean, group = scenario, col = "red"))+
      geom_ribbon(data = sk2[scenario %in% c(28)], aes(x = year, ymin = Q_2.5, ymax = Q_975, fill = "Setting A"), alpha = 0.5)+
      geom_line(data = sk2[scenario %in% c(28)], lwd = 1, aes(x = year, y = mean, group = scenario, col = "Setting A"))+
      
      xlab("Time (Year)")+
      ylab("Population size (N)")+
      theme(legend.position = "none")

sa2 <- lapply(1:4, function(x) readRDS(sprintf("./Sens_a2/scenario%d.RData", x)))


# change from nested data structure to flat data table data structure. By county
# including quantiles
sa2 <- lapply(1:length(sa2), function(i) {
      lapply(1:length(sa2[[i]]), function(j) {
            
            data.table(year = 1:nrow(sa2[[i]][[j]]$tot_vals),
                       scenario = i,
                       county = j,
                       mean = rowMeans(sa2[[i]][[j]]$tot_vals), 
                       sd = apply(sa2[[i]][[j]]$tot_vals, 1, sd),
                       q_2.5 = apply(sa2[[i]][[j]]$tot_vals, 1, quantile, probs=0.025),
                       q_975 = apply(sa2[[i]][[j]]$tot_vals, 1, quantile, probs = 0.975))
            
      }) |> rbindlist()
}) |> rbindlist()

SA2 <- sa2[, .(mean = sum(mean), sd = sqrt(sd^2), Q_2.5 = sum(q_2.5), Q_975 = sum(q_975)), .(year, scenario)]

akkar <- expand.grid(hunt = seq(0, 0.10, 0.01), bycatch = seq(0,0.1, 0.025))
SA2$fscenario <- factor(sa2$scenario, 
                       labels = sprintf("hunt = %.02f, bycatch = %.02f", akkar$hunt, akkar$bycatch))

ggplot()+
      geom_ribbon(data = SA2[scenario %in% 1], aes(x = year, ymin = Q_2.5, ymax = Q_975, fill = "blue"), alpha = 0.5)+
      geom_line(data = SA2[scenario %in% 1], lwd = 1, aes(x = year, y = mean, group = scenario, col = "blue"))+
      geom_ribbon(data = SA2[scenario %in% 2], aes(x = year, ymin = Q_2.5, ymax = Q_975, fill = "green"), alpha = 0.5)+
      geom_line(data = SA2[scenario %in% 2], lwd = 1, aes(x = year, y = mean, group = scenario, col = "green"))+
      geom_ribbon(data = SA2[scenario %in% 4], aes(x = year, ymin = Q_2.5, ymax = Q_975, fill = "red"), alpha = 0.5)+
      geom_line(data = SA2[scenario %in% 4], lwd = 1, aes(x = year, y = mean, group = scenario, col = "red"))+
      geom_ribbon(data = sk3[scenario %in% c(28)], aes(x = year, ymin = Q_2.5, ymax = Q_975, fill = "Setting B"), alpha = 0.5)+
      geom_line(data = sk3[scenario %in% c(28)], lwd = 1, aes(x = year, y = mean, group = scenario, col = "Setting B"))+
      
      xlab("Time (Year)")+
      ylab("Population size (N)")+
      theme(legend.position = "bottom")

sa_s <- subset(SA, year == 101)
sa_sk <- subset(sk2, year == 101 & scenario == 28)
sa_sk1 <- rbind(sa_sk, sa_s, fill=TRUE)
labels_A<- c("28Aa", "28Ab", "28Ac", "28Ad", "28Ae")

s_a1<- ggplot(data = sa_sk1, aes(x=factor(scenario), y=mean)) +
            facet_wrap(~year, labeller = as_labeller(c("101" = "Setting A"))) +
            geom_point() +
            scale_x_discrete(labels = labels_A) +
            ylim(0,8000)+
            geom_hline(yintercept=7733, linetype="dashed", color = "black")+
            ylab("Population size (N)")+
            xlab("Scenario 28")+
            geom_crossbar(aes(ymin=Q_2.5, ymax=Q_975))+
            theme(
                  panel.background = element_rect(fill = 'white', color = 'lightgray'),
                  panel.grid = element_line(color = "lightgray")
            )

sa2_s <- subset(SA2, year == 101)
sa2_sk <- subset(sk3, year == 101 & scenario == 28)
sa2_sk2 <- rbind(sa2_sk, sa2_s, fill=TRUE)
labels_B<- c("28Ba", "28Bb", "28Bc", "28Bd", "28Be")

s_a2<- ggplot(data = sa2_sk2, aes(x=factor(scenario), y=mean)) +
            facet_wrap(~year, labeller = as_labeller(c("101" = "Setting B"))) +
            geom_point() +
            ylim(0,8000)+
            scale_x_discrete(labels = labels_B) +
            geom_hline(yintercept=7733, linetype="dashed", color = "black")+
            ylab("Population size (N)")+
            xlab("Scenario 28")+
            geom_crossbar(aes(ymin=Q_2.5, ymax=Q_975))+
            theme(
                  panel.background = element_rect(fill = 'white', color = 'lightgray'),
                  panel.grid = element_line(color = "lightgray")
            )
s_a1 + s_a2

sa27_b <- lapply(1:4, function(x) readRDS(sprintf("./Sens_a27B/scenario%d.RData", x)))


# change from nested data structure to flat data table data structure. By county
# including quantiles
sa27_b <- lapply(1:length(sa27_b), function(i) {
      lapply(1:length(sa27_b[[i]]), function(j) {
            
            data.table(year = 1:nrow(sa27_b[[i]][[j]]$tot_vals),
                       scenario = i,
                       county = j,
                       mean = rowMeans(sa27_b[[i]][[j]]$tot_vals), 
                       sd = apply(sa27_b[[i]][[j]]$tot_vals, 1, sd),
                       q_2.5 = apply(sa27_b[[i]][[j]]$tot_vals, 1, quantile, probs=0.025),
                       q_975 = apply(sa27_b[[i]][[j]]$tot_vals, 1, quantile, probs = 0.975))
            
      }) |> rbindlist()
}) |> rbindlist()

SA_27B <- sa27_b[, .(mean = sum(mean), sd = sqrt(sd^2), Q_2.5 = sum(q_2.5), Q_975 = sum(q_975)), .(year, scenario)]

sa27_B <- subset(SA_27B, year == 101)
sa27_sk <- subset(sk3, year == 101 & scenario == 27)
sa27_sk3 <- rbind(sa27_sk, sa27_B, fill=TRUE)
labels_B27<- c("27Ba", "27Bb", "27Bc", "27Bd", "27Be")


s_a27b<- ggplot(data = sa27_sk3, aes(x=factor(scenario), y=mean)) +
      facet_wrap(~year, labeller = as_labeller(c("101" = "Setting B"))) +
      geom_point() +
      scale_x_discrete(labels = labels_B27) +
      ylim(0,8000)+
      geom_hline(yintercept=7733, linetype="dashed", color = "black")+
      ylab("Population size (N)")+
      xlab("Scenario 27")+
      geom_crossbar(aes(ymin=Q_2.5, ymax=Q_975))+
      theme(
            panel.background = element_rect(fill = 'white', color = 'lightgray'),
            panel.grid = element_line(color = "lightgray")
      )
s_a27b

sa27_a <- lapply(1:4, function(x) readRDS(sprintf("./Sens_a27A/scenario%d.RData", x)))


# change from nested data structure to flat data table data structure. By county
# including quantiles
sa27_a <- lapply(1:length(sa27_a), function(i) {
      lapply(1:length(sa27_a[[i]]), function(j) {
            
            data.table(year = 1:nrow(sa27_a[[i]][[j]]$tot_vals),
                       scenario = i,
                       county = j,
                       mean = rowMeans(sa27_a[[i]][[j]]$tot_vals), 
                       sd = apply(sa27_a[[i]][[j]]$tot_vals, 1, sd),
                       q_2.5 = apply(sa27_a[[i]][[j]]$tot_vals, 1, quantile, probs=0.025),
                       q_975 = apply(sa27_a[[i]][[j]]$tot_vals, 1, quantile, probs = 0.975))
            
      }) |> rbindlist()
}) |> rbindlist()

SA_27A <- sa27_a[, .(mean = sum(mean), sd = sqrt(sd^2), Q_2.5 = sum(q_2.5), Q_975 = sum(q_975)), .(year, scenario)]

sa27_A <- subset(SA_27A, year == 101)
sa27_ska <- subset(sk2, year == 101 & scenario == 27)
sa27_sk2 <- rbind(sa27_ska, sa27_A, fill=TRUE)
labels_A27<- c("27Aa", "27Ab", "27Ac", "27Ad" , "27Ae")

s_a27a<- ggplot(data = sa27_sk2, aes(x=factor(scenario), y=mean)) +
      facet_wrap(~year, labeller = as_labeller(c("101" = "Setting A"))) +
      geom_point() +
      scale_x_discrete(labels = labels_A27) +
      ylim(0,8000)+
      geom_hline(yintercept=7733, linetype="dashed", color = "black")+
      ylab("Population size (N)")+
      xlab("Scenario 27")+
      geom_crossbar(aes(ymin=Q_2.5, ymax=Q_975))+
      theme(
            panel.background = element_rect(fill = 'white', color = 'lightgray'),
            panel.grid = element_line(color = "lightgray")
      )
s_a27a

(s_a27a + s_a27b) / (s_a1 + s_a2)  



sa27_B %>% 
      filter( scenario == 2) %>% 
      select(mean)
sa27_A %>% 
      filter( scenario == 2) %>% 
      select(mean)

sa_s %>% 
      filter( scenario == 2) %>% 
      select(mean)

sa2_s %>% 
      filter( scenario == 2) %>% 
      select(mean)

sk3 %>% 
      filter(year == 101, scenario == 27) %>% 
      select(mean)
