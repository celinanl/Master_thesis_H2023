#################################
#Celina Lundevik, University of Oslo, 2023
#################################

install.packages("ggplot2")
library(ggplot2)
install.packages("patchwork")
library(patchwork)

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
sk3_qe$Setting <- "A"


# Picking out the exact value of Prob_QE
sk3_qe %>% 
      filter(year == 101, scenario == 11) %>% 
      select(prob_qe)
##Figure 3.4##
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

#Doing the same for the bycatch settings B
sk1 <- lapply(1:55, function(x) readRDS(sprintf("./results/scenario%d.RData", x)))

sk1_qe <- lapply(1:length(sk1), function(i) {
      lapply(1:length(sk1[[i]]), function(j) {
            n_reps = ncol(sk1[[i]][[j]]$tot_vals)
            n_year = nrow(sk1[[i]][[j]]$tot_vals)
            data.table(year = rep(1:n_year, times = n_reps),
                       scenario = i,
                       county = j,
                       replicate = rep(1:n_reps, each = n_year),
                       tot_val = as.vector(sk1[[i]][[j]]$tot_vals))
            
      }) |> rbindlist()
}) |> rbindlist()


sk4_qe = sk1_qe[, .(tot_val=sum(tot_val, na.rm = TRUE)), .(replicate, year, scenario)]

sk5_qe <- sk4_qe[, .(prob_qe = sum(tot_val<QE*13)/.N), .(year, scenario)]

sk5_qe$Setting <- "B"
ggplot(data =sk5_qe[scenario %in% 12:22][year %in% 101])+
      geom_col(aes(x = scenario, y=prob_qe, col = factor(scenario), fill=factor(scenario)))



#Combining the two bycatch settings in one dataset
sk3.5_qe <- rbind(sk3_qe, sk5_qe)

# Comparing the prob_QE for the two bycatchsettings in a bar plot
my_labels<- c("0.0", "0.01", "0.02", "0.03", "0.04", "0.05", "0.06", "0.07", "0.08", "0.09", "0.1")

##BC_0.025##

p1<- ggplot(data = sk3_qe[scenario %in% 12:22][year %in% 101]) +
      geom_col(aes(x=factor(scenario), y=prob_qe, fill = Setting)) +
      scale_x_discrete(labels = my_labels) +
      ylab("QE probability after 100 years")+
      xlab("Hunting level")+
      facet_wrap(~year, labeller = as_labeller(c("101" = "Bycatch = 0.025")))+
      theme(
            panel.background = element_rect(fill = 'white', color = 'lightgray'),
            panel.grid = element_line(color = "lightgray")
      )
p2<- ggplot(data = sk5_qe[scenario %in% 12:22][year %in% 101]) +
            geom_col( aes(x=factor(scenario), y=prob_qe, fill = Setting)) +
            scale_x_discrete(labels = my_labels) +
            ylab("QE probability after 100 years")+
            xlab("Hunting level")+
            facet_wrap(~year, labeller = as_labeller(c("101" = "Bycatch = 0.025")))+
            theme(
                  panel.background = element_rect(fill = 'white', color = 'lightgray'),
                  panel.grid = element_line(color = "lightgray")
            )+
      scale_fill_manual(values = "#00BFC4")

p1/p2

##BC_0.05##
ggplot(data = sk3.5_qe[scenario %in% 23:33][year %in% 101]) +
      geom_col(aes(x=factor(scenario), y=prob_qe, fill = Setting), position = "dodge") +
      scale_x_discrete(labels = my_labels) +
      ylab("Quasi-extinction probability after 100 years")+
      xlab("Hunting level")+
      facet_wrap(~year, labeller = as_labeller(c("101" = "Bycatch = 0.05")))+
      theme(
            panel.background = element_rect(fill = 'white', color = 'lightgray'),
            panel.grid = element_line(color = "lightgray")
      )

p1<- ggplot(data = sk3_qe[scenario %in% 23:33][year %in% 101]) +
      geom_col(aes(x=factor(scenario), y=prob_qe, fill = Setting)) +
      scale_x_discrete(labels = my_labels) +
      ylab("QE probability after 100 years")+
      xlab("Hunting level")+
      facet_wrap(~year, labeller = as_labeller(c("101" = "Bycatch = 0.05")))+
      theme(
            panel.background = element_rect(fill = 'white', color = 'lightgray'),
            panel.grid = element_line(color = "lightgray")
      )
p2<- ggplot(data = sk5_qe[scenario %in% 23:33][year %in% 101]) +
      geom_col( aes(x=factor(scenario), y=prob_qe, fill = Setting)) +
      scale_x_discrete(labels = my_labels) +
      ylab("QE probability after 100 years")+
      xlab("Hunting level")+
      facet_wrap(~year, labeller = as_labeller(c("101" = "Bycatch = 0.05")))+
      theme(
            panel.background = element_rect(fill = 'white', color = 'lightgray'),
            panel.grid = element_line(color = "lightgray")
      )+
      scale_fill_manual(values = "#00BFC4")

p1/p2

##BC_0.075##
ggplot(data = sk3.5_qe[scenario %in% 34:44][year %in% 101]) +
      geom_col(aes(x=factor(scenario), y=prob_qe, fill = Setting), position = "dodge") +
      scale_x_discrete(labels = my_labels) +
      ylab("Quasi-extinction probability after 100 years")+
      xlab("Hunting level")+
      facet_wrap(~year, labeller = as_labeller(c("101" = "Bycatch = 0.075")))+
      theme(
            panel.background = element_rect(fill = 'white', color = 'lightgray'),
            panel.grid = element_line(color = "lightgray")
      )

p1<- ggplot(data = sk3_qe[scenario %in% 34:44][year %in% 101]) +
      geom_col(aes(x=factor(scenario), y=prob_qe, fill = Setting)) +
      scale_x_discrete(labels = my_labels) +
      ylab("QE probability after 100 years")+
      xlab("Hunting level")+
      facet_wrap(~year, labeller = as_labeller(c("101" = "Bycatch = 0.075")))+
      theme(
            panel.background = element_rect(fill = 'white', color = 'lightgray'),
            panel.grid = element_line(color = "lightgray")
      )
p2<- ggplot(data = sk5_qe[scenario %in% 34:44][year %in% 101]) +
      geom_col( aes(x=factor(scenario), y=prob_qe, fill = Setting)) +
      scale_x_discrete(labels = my_labels) +
      ylab("QE probability after 100 years")+
      xlab("Hunting level")+
      facet_wrap(~year, labeller = as_labeller(c("101" = "Bycatch = 0.075")))+
      theme(
            panel.background = element_rect(fill = 'white', color = 'lightgray'),
            panel.grid = element_line(color = "lightgray")
      )+
      scale_fill_manual(values = "#00BFC4")

p1/p2

##BC_0.10##
ggplot(data = sk3.5_qe[scenario %in% 45:55][year %in% 101]) +
      geom_col(aes(x=factor(scenario), y=prob_qe, fill = Setting), position = "dodge") +
      scale_x_discrete(labels = my_labels) +
      ylab("Quasi-extinction probability after 100 years")+
      xlab("Hunting level")+
      facet_wrap(~year, labeller = as_labeller(c("101" = "Bycatch = 0.10")))+
      theme(
            panel.background = element_rect(fill = 'white', color = 'lightgray'),
            panel.grid = element_line(color = "lightgray")
      )

p1<- ggplot(data = sk3_qe[scenario %in% 45:55][year %in% 101]) +
      geom_col(aes(x=factor(scenario), y=prob_qe, fill = Setting)) +
      scale_x_discrete(labels = my_labels) +
      ylab("QE probability after 100 years")+
      xlab("Hunting level")+
      facet_wrap(~year, labeller = as_labeller(c("101" = "Bycatch = 0.10")))+
      theme(
            panel.background = element_rect(fill = 'white', color = 'lightgray'),
            panel.grid = element_line(color = "lightgray")
      )
p2<- ggplot(data = sk5_qe[scenario %in% 45:55][year %in% 101]) +
      geom_col( aes(x=factor(scenario), y=prob_qe, fill = Setting)) +
      scale_x_discrete(labels = my_labels) +
      ylab("QE probability after 100 years")+
      xlab("Hunting level")+
      facet_wrap(~year, labeller = as_labeller(c("101" = "Bycatch = 0.10")))+
      theme(
            panel.background = element_rect(fill = 'white', color = 'lightgray'),
            panel.grid = element_line(color = "lightgray")
      )+
      scale_fill_manual(values = "#00BFC4")

p1/p2


