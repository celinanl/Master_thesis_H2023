#################################
#Celina Lundevik, University of Oslo, 2023
#################################
#Packages

setwd("/Users/celinanl/Documents/Biovitenskap/Master/Masteroppgave/R-script\ mm")
results_dir <- file.path(getwd(), "results2")


if(!require("ggplot2")){
      install.packages("ggplot2")
} else if(!require("reshape2")){
      install.packages("reshape2")
}
if(!require("progress")){
      install.packages('progress')
}  

library(ggplot2)
library(reshape2)
library(progress)
printf <- function(...) cat(sprintf(...), sep=" ")


source('utilityFunctions.R')  

source('Population_model_SettingA.R')

#################################
#parameters for harbour seals, Silva et al. 2021

## Number of cycles (years):
t = 100 

## Max age:
A = 38

## Fecundity rate:
B = rep(0,A)
B[4] = 0.17
B[5] = 0.33
B[6:27] = 0.47
B[27:(A-1)] = 0.35


## Survival rate:
# age-specific
S = rep(0,A)
S[1] = 0.75
S[2:4] = 0.89
S[5:(A-1)] = 0.95 

# ## Initial population sizes (per age class):
library(data.table)
stein2021<- fread ("steinHist.csv")


library(tidyverse)
stein2021 <- pivot_longer(stein2021, 
                          cols=c(3:length(stein2021)),
                          names_to = 'Fylke',
                          values_to='N')

stein2021$Fylke <- factor(stein2021$Fylke, levels=rev(unique(stein2021$Fylke)))



## Table of latest estimates for each region:
latest <- do.call('rbind', by(stein2021, stein2021$Fylke, function(x) {
      x <- x[which(!is.na(x$N) & x$År>0),]
      n <- x$N[which.max(x$År)]
      år <- max(x$År)
      data.frame(År=år, N=n)
}))

latest

## Non-haulout correction, using correction factor 0.72, taken from Lonergan et al. (2013):
latest$Nc <- round(latest$N/0.72)

P <- do.call('cbind', lapply(latest$N, function(x) round(structure(B=B*S/2, S=S, N=x))))
colnames(P) <- row.names(latest)


df <- as.data.frame(P)
df$Year <- 1:nrow(P)

# Reshape the data into a long format
df_long <- df %>%
      gather(Fylke, Frequency, -Year)


## Carrying capacity. Nominally set to 10 x current population
# K=c(2380,1370,1155,2204,2509,1363,1319,894,1645,905,1062) 
K <- 2*apply(P, 2, sum)


## f = Modification to birthrates, will reduce 
## initial fertility down by f % until 0, 
## f can be 0... numeric of a vector of additive effects 
f = 0.1  

## R = level of random variation per cycle 
## (0.05 = random value within 5% +- calculated value 
## based on beta distribution a = b = 5)... numeric
R = 0.05 

## E = Entanglement (bycatch), currently a single value of seals removed from 
## each population per cycle, will be modified to have optional age class skew 
## or be a function of population size... numeric
E = 0.05

## H = Hunting pressure (as proportion of population)
h = seq(0, 0.1, by=0.01) 

## H_delay = Number of cycles (years) before hunting is implemented
H_delay = 0 



#P_D = Probability of a disease outbreak in each cycle... numeric
#O_D = Outcome of disease, maximum proportion of an age class killed during an outbreak... numeric
#W_D = Proportion of O_D experienced by age class... vector


P_D = 0.07
O_D = 0.65
W_D = c(100,11.55,9.95,9.15,9,10.25,9,8.5,7.25,7,5.6,2.6,1.9,0.2,3,1.7,0.1,0,0.2,1.1,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11)/100 #weighting of disease per age class


# #M = Migration: proportion of a population which is present in each population at the end of each cycle... matrix 
## Manually:
M_Finnmark <- c(99, 1, rep(0, nrow(latest)-2))
M_Troms <- c(1, 98, 1, rep(0, nrow(latest)-3))
M_Nordland <- c(0, 1, 98, 1, rep(0, nrow(latest)-4))
M_N.Trøndelag <- c(rep(0, 2), 1, 98, 1, rep(0, nrow(latest)-5))
M_S.Trøndelag <- c(rep(0, 3), 1, 98, 1, rep(0, nrow(latest)-6))
M_Møre.Romsdal <- c(rep(0, 4), 1, 98, 1, rep(0, nrow(latest)-7))
M_Sogn.Fjordane <- c(rep(0, 5), 1, 98, 1, rep(0, nrow(latest)-8))
M_Rogaland <- c(rep(0, 6), 1, 98, 1, rep(0, 4))
M_Vest.Agder <- c(rep(0, 7), 1, 98, 1, rep(0, 3))
M_Aust.Agder <- c(rep(0, 8), 1, 98, 1, rep(0, 2))
M_Telemark <- c(rep(0, 9), 1, 98, 1, 0)
M_Vestfold <- c(rep(0, 10), 1, 98, 1)
M_Østfold <- c(rep(0, 11), 1, 98)

M <- rbind(M_Finnmark, M_Troms, M_Nordland, M_N.Trøndelag, M_S.Trøndelag, M_Møre.Romsdal,
           M_Sogn.Fjordane, M_Rogaland, M_Vest.Agder, M_Aust.Agder, M_Telemark, M_Vestfold, M_Østfold)
M <- M/100

## Distance-based
migrate <- function(cumdist=fylke.locs$cumdist/1000, cutoff=250, cNames=fylke.locs$Fylke) {
      D <- matrix(nrow=length(cumdist), ncol=length(cumdist), dimnames=list(cNames, cNames))
      
      for(i in 1:length(cumdist)) {
            for(j in 1:length(cumdist)) {
                  D[i,j] <- abs(cumdist[i]-cumdist[j])
            }
      }
      
      M <- round(1/(D/10), 2)
      
      for(i in 1:nrow(M)) {
            which.zero <- which(D[i,]>cutoff)
            M[i,which.zero] <- 0
            M[i,i] <- 1-(sum(M[i,-i]))
      }  
      
      M
}

fylke.locs <- read.csv('coastDist.csv', header=T)
M <- migrate()

## No migration:


M <- diag(rep(1, nrow(latest)))

colnames(M) = colnames(P)
rownames(M) = colnames(P) 


#################################
#run model 'reps' number of times saving the mean curve and quasi extinction risk

reps = 1000 #number of iterations
QE = 100 #quasi extinction value, the simulation considers a population size of 100 individuals to be 
#the minimum viable population size for the specie


# ulike grader av bifangst, fra 0 til 10%
e <- c(0, 0.025, 0.05, 0.075, 0.1) #bifangst

# totalt antall scenarioer = antall forskjellige bifangstscenarioer 
# ganger med antall forskjellige jaktscenarioer

# run the scenarios
for (E_i in 1:length(e)) {
      E <- e[E_i]
      for (H_i in 1:length(h)) {
            H <- h[H_i]
            this_scenario <- (E_i-1)*length(h)+H_i#+6*(H_i-1)
            printf("Running PVA for scenario %g/%g (bycatch=%.03f, takes=%.02f)\n", this_scenario, length(e) * length(h), E, H)
            t_tot_vals = matrix(0, ncol = ncol(P), nrow = (t+1))
            Prob_QE = rep(0, ncol(P))
            
            ptm = proc.time()
            
            all.reps <- list()
            pb <- progress_bar$new(total=reps)
            for(i in 1:reps){
                  pb$tick()
                  tot_vals = c()
                  
                  for(j in 1:ncol(P)){
                        tot_vals[j] = (sum(P[,j]))
                  }
                  
                  tot_vals = matrix(tot_vals, ncol = ncol(P))
                  
                  colnames(tot_vals) = colnames(P)
                  
                  for(j in 1:t){
                        Pt = PopMod(t = j,
                                    A = A,
                                    K = K,
                                    P = P,
                                    B = B,
                                    S = S,
                                    H = H,
                                    f = f,
                                    H_delay =  H_delay,
                                    E = E,
                                    R = R,
                                    P_D = P_D,
                                    O_D = O_D,
                                    W_D = W_D,
                                    M = M)
                        
                        Tot_Pop = c()
                        
                        for(j in 1:ncol(Pt)){
                              Tot_Pop[j] = sum(Pt[,j])
                        }
                        
                        tot_vals = rbind(tot_vals,Tot_Pop)
                        
                  }
                  
                  for(j in 1:ncol(tot_vals)){
                        if(length(which(tot_vals[,j] < QE))>1){
                              Prob_QE[j] = Prob_QE[j]+1
                        } #if the population dips below QE at any point it is counted as extinction... can be replaced with populations state after t: if(length(tot_vals[,j][length(tot_vals[,j])] < QE))>1){Prob_QE[j] = Prob_QE[j]+1}
                  }  
                  
                  for(j in 1:ncol(P)){
                        
                        t_tot_vals[,j] = t_tot_vals[,j] + tot_vals[,j]
                        
                  }
                  
                  Prob_QE = Prob_QE/reps
                  Prob_QE = rbind(Prob_QE)
                  colnames(Prob_QE) = colnames(P)
                  colnames(t_tot_vals) <- colnames(P)
                  m_tot_vals = t_tot_vals/reps
                  colnames(m_tot_vals) = colnames(P)
                  
                  all.reps[[i]] <- list(tot_vals=tot_vals,
                                        m_tot_vals=m_tot_vals,
                                        Prob_QE=Prob_QE)
            }
            
            
            
            
            
            proc.time() - ptm
            
            pop.fate <- list()
            
            for(i in 1:nrow(latest)) {
                  pop.fate[[i]] <- list(tot_vals=do.call('cbind', lapply(all.reps, function(x) x$tot_vals[,i])),
                                        m_tot_vals=do.call('cbind', lapply(all.reps, function(x) x$m_tot_vals[,i])),
                                        Prob_QE=do.call('cbind', lapply(all.reps, function(x) x$Prob_QE[,i]))
                  )
                  pop.fate[[i]]$env <- apply(pop.fate[[i]]$tot_vals, 1, quantile, prob=c(0.025, 0.5, 0.975)) 
            }
            
            names(pop.fate) <- row.names(latest)
            
            scenario_name <- file.path(results_dir, sprintf("scenario%g", this_scenario))
            saveRDS(pop.fate, file = sprintf("%s.RData", scenario_name))
            #save(pop.fate, file=paste('PopFate_Kvote', H, '.RData', sep=''))
            #png(paste('PopTrend_Kvote_', H, '.png', sep=''), height=20, width=20, units='cm', res=500)
            png(sprintf("%s.png", scenario_name), height=20, width=20, units='cm', res=500)
            
            par(mfrow=c(4,4), mar=rep(1, 4), oma=c(0, 1, 0, 0))
            for(i in 1:length(pop.fate)) {
                  matplot(pop.fate[[i]]$tot_vals, pch='.', col=1, xlab='Years', ylab='N', main=names(pop.fate)[i])
                  matlines(t(pop.fate[[i]]$env) , lwd=c(1,2,1), col=2, lty=c(2,1,2))
            }
            dev.off()
      }
}


