#Influence of disagreement avoiding link updating on consensus outcomes 
#Varying majority and minority disagreement avoiding probabilities while keeping agreement avoiding probabilities constant (0 as default)
#facets vary stubbornness values 
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

#rewiring probabilities
p1p <- 0 #phi1+
p1m <- 0 #phi1-

#inertia
l <- 0.3 

#input for disagreement avoidance probability and stubbornness

res <- 0.1 #set resolution for variation in input

eg <- expand.grid(
  P2P <- seq(0,0.9, by=res),
  P2M <- seq(0,0.9, by=res),
  BP <- c(0.05,0.20,0.5,0.95),
  BM <- c(0.05,0.20,0.5,0.95),
  im_out <- NA,
  om_out <- NA,
  stringsAsFactors = TRUE,
  KEEP.OUT.ATTRS = TRUE
)

################################

#for different majority/minority proportion
data <- list()

for(n0 in c(0.8,0.6,0.5))
{
  #the following for loop generates the data
  
  for (i in 1:nrow(eg))
  {
    print(paste("Currently considering case", i, "of", nrow(eg)))
    eg$Var5[i] <- IM(p1p, eg$Var1[i], p1m, eg$Var2[i], eg$Var3[i], eg$Var4[i],l,n0)$M[T]
    eg$Var6[i] <- OM(p1p, eg$Var1[i], p1m, eg$Var2[i], eg$Var3[i], eg$Var4[i],l,n0)$M[T]
  }
  
  ################################
  #cleaning and labeling data
  datan0 <- eg
  colnames(datan0) <- c("phi2p", "phi2m", "BP", "BM", "IM_Mf","OM_Mf")
  
  ################################
  #data visualization
  library(ggplot2)
  library(dplyr)
  
  datan0$BP <- factor(datan0$BP, levels=c(0.95, 0.5, 0.2, 0.05))
  
  conim <- datan0[abs(datan0$IM_Mf) > 0.8,]
  conom <- datan0[abs(datan0$OM_Mf) > 0.8,]
  
  #Incoming model
  pim <- datan0 %>%
    ggplot(aes(x=phi2m, y=phi2p, fill=IM_Mf))+
    facet_grid(BP ~ BM)+
    geom_tile(color = "grey")+
    geom_tile(data = conim, color = "yellow", size = 0.7, linetype = "dotted")+
    scale_fill_gradient2(high = scales::muted("red"),low = scales::muted("blue"),
                         mid = "grey",guide = "colourbar", limits = c(-1,1))+
    theme_classic(base_size = 15)+
    labs(title = paste0("Effect of disagreement avoidance\non final state (IM) ; ","n+(0) = ", n0),
         x = expression(paste("Minority disagreement avoidance ", (phi['2-']) )), 
         y = expression(paste("Majority disagreement avoidance ", (phi['2+']) )), fill = "Final
State", subtitle = expression(paste("Minority stubbornness ", (beta['-']) )))+
    theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5), text = element_text(size=14))+
    ggpubr::rremove("grid")+ggpubr::border()+
    theme(legend.position = "none")
  
  #Outgoing model
  pom <- datan0 %>%
    ggplot(aes(x=phi2m, y=phi2p, fill=OM_Mf))+
    facet_grid(BP ~ BM)+
    geom_tile(color = "grey")+
    geom_tile(data = conom, color = "yellow", size = 0.7, linetype = "dotted")+
    scale_fill_gradient2(high = scales::muted("red"),low = scales::muted("blue"),
                         mid = "grey",guide = "colourbar", limits = c(-1,1))+
    theme_classic(base_size = 15)+
    labs(title = paste0("Effect of disagreement avoidance\non final state (OM) ; ","n+(0) = ", n0),
         x = expression(paste("Minority disagreement avoidance ", (phi['2-']) )), 
         y = expression(paste("Majority disagreement avoidance ", (phi['2+']) )), fill = "Final
State", subtitle = expression(paste("Minority stubbornness ", (beta['-']) )))+
    theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5), text = element_text(size=14))+
    ggpubr::rremove("grid")+ggpubr::border()+
    theme(legend.position = "none")

  data[[paste0("Maj_",n0*100)]] <- datan0
  
  pim
  pom
}
