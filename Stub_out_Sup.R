#Influence of stubbornness and strategies on outcomes
#Varying majority (y) and minority (x) stubbornness (axes) and link updating strategies (facets)
#to obtain final state 
#Plots are obtained for majority to minority ratio of 80/20, 60/40 and 50/50

#Loading required model functions (incoming model, outgoing model, consensus speeds,boundary checks)

source("Speed.R")
source("Incoming_model.R")
source("Outgoing_model.R")
source("If_boundary.R")

##############################

#INPUT
#set constant global features
N <- 100 #total population size
T <- 10000 #number of time steps

##set parameter values

#inertia
l <- 0.3

#varying stubbornnness and strategies

#resolution for variation
res <- 0.1

eg1 <- expand.grid(
  # varying some:
  BP <- seq(0.05, 0.95, by = res),
  BM <- seq(0.05, 0.95, by = res),
  P1P <- c(0,0,0.5),
  P1M <- c(0,0,0.5),
  # Columns for outcomes:
  im_out <- NA,
  om_out <- NA,
  stringsAsFactors = TRUE,
  KEEP.OUT.ATTRS = TRUE
)

eg2 <- expand.grid(
  # varying some:
  BP <- seq(0.05, 0.95, by = res),
  BM <- seq(0.05, 0.95, by = res),
  P2P <- c(0,0.5,0),
  P2M <- c(0,0.5,0),
  # Columns for outcomes:
  stringsAsFactors = TRUE,
  KEEP.OUT.ATTRS = TRUE
)

eg <- data.frame(eg1,eg2)

##############################

#for different majority/minority proportion
data <- list()
vars_n0 <- c(0.8,0.6,0.5)

for(n0 in vars_n0)
{
  #the following for loop generates the data
  
  for (i in 1:nrow(eg))
  {
    print(paste("Currently considering case", i, "of", nrow(eg)))
    eg$Var5[i] <- IM(eg$Var3[i], eg$Var3.1[i], eg$Var4[i], eg$Var4.1[i], eg$Var1[i], eg$Var2[i], l, n0)$M[T]
    eg$Var6[i] <- OM(eg$Var3[i], eg$Var3.1[i], eg$Var4[i], eg$Var4.1[i], eg$Var1[i], eg$Var2[i], l, n0)$M[T]
  }
  
  ##############################
  #labeling and cleaning the data for visualization
  
  datan0 <- data.frame("BP"=eg$Var1,
                     "BM"=eg$Var2,
                     "Stratp"=NA,
                     "Stratm"=NA,
                     "IM_Mf"=eg$Var5,
                     "OM_Mf"=eg$Var6)
  
  for(j in 1:nrow(datan0))
  {
    datan0$Stratp[j] <- paste(eg$Var3[j], eg$Var3.1[j], sep=",")
    datan0$Stratm[j] <- paste(eg$Var4[j], eg$Var4.1[j], sep=",")
  }
  
  datan0$Stratp <- factor(datan0$Stratp,
                        labels=c("Static", "Disagreement\navoiding", "Agreement\navoiding"))
  
  datan0$Stratp <- factor(datan0$Stratp,
                        levels=c("Agreement\navoiding","Disagreement\navoiding", "Static"))
  
  datan0$Stratm <- factor(datan0$Stratm,
                        labels=c("Static", "Disagreement\navoiding", "Agreement\navoiding"))
  
  data[[paste0("Maj_",n0*100)]] <- datan0
  
  ################################
  
  #visualizing the data
  
  library(ggplot2)
  library(dplyr)
  
  
  #highlight the consensus outcomes
  conim <- datan0[abs(datan0$IM_Mf) > 0.8,]
  conom <- datan0[abs(datan0$OM_Mf) > 0.8,]
  
  #Incoming model
  pim <- datan0 %>%
    ggplot(aes(x = BM, y = BP, fill = IM_Mf)) +
    facet_grid(Stratp ~ Stratm)+
    geom_tile(color = "grey") +
    geom_tile(data = conim, color = "yellow", linewidth = 1, linetype = "dotted")+
    scale_fill_gradient2(high = scales::muted("red"),low = scales::muted("blue"),
                         mid = "grey", guide = "colourbar", limits = c(-1,1))+
    theme_classic(base_size = 15)+
    labs(title = paste0("Effect of stubbornness
  on final state (IM) ; ", "n+(0) = ", n0), x= expression(paste("Minority stubbornness ", (beta['-']) )), 
         y = expression(paste("Majority stubbornness ", (beta['+']))), 
         fill = "Final State", subtitle= "Minority Strategy")+
    theme(plot.title = element_text(hjust = 0.5), 
          plot.subtitle = element_text(hjust = 0.5), 
          text = element_text(size=20),
          axis.text.x = element_text(size = 10),
          axis.text.y = element_text(size = 10),
          legend.position = "bottom", legend.key.width = unit(2,"cm"),
          legend.title = element_text(vjust = 0.9))+
    ggpubr::rremove("grid")+ggpubr::border()
  
  #Outgoing model
  pom <- datan0 %>%
    ggplot(aes(x = BM, y = BP, fill = OM_Mf)) +
    facet_grid(Stratp ~ Stratm)+
    geom_tile(color = "grey") +
    geom_tile(data = conom, color = "yellow", linewidth = 1, linetype = "dotted")+
    scale_fill_gradient2(high = scales::muted("red"),low = scales::muted("blue"),
                         mid = "grey", guide = "colourbar", limits = c(-1,1))+
    theme_classic(base_size = 15)+
    labs(title = paste0("Effect of stubbornness
  on final state (OM) ; ", "n+(0) = ", n0), x= expression(paste("Minority stubbornness ", (beta['-']) )), 
         y = expression(paste("Majority stubbornness ", (beta['+']))), 
         fill = "Final State", subtitle= "Minority Strategy")+
    theme(plot.title = element_text(hjust = 0.5), 
          plot.subtitle = element_text(hjust = 0.5), 
          text = element_text(size=20),
          axis.text.x = element_text(size = 10),
          axis.text.y = element_text(size = 10),
          legend.position = "bottom", legend.key.width = unit(2,"cm"),
          legend.title = element_text(vjust = 0.9))+
    ggpubr::rremove("grid")+ggpubr::border()
  
  pim
  pom
}

