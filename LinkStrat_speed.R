#Influence of disagreement/agreement avoiding on majority consensus speeds
#Varying majority (red) and minority (blue) disagreement (A) and agreement (B) avoidance probabilities in x axis 
#to obtain convergence speeds in Y axis

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
n0 <- 0.8 #majority proportion

#stubbornness 
bp <- 0.5 # + stubbornness
bm <- 0 # - stubbornness

#inertia
l <- 0.3

#varying inputs for link updating probabilities
#vary one and keep rest to 0.5

##agreement avoidance variation
###varying majority agreement avoidance
variation <- seq(0,0.8,by=0.05)

EG1p <- expand.grid(
  eg1p <- variation,
  eg2p <- 0.5,
  eg1m <- 0.5,
  eg2m <- 0.5,
  im_speed <- NA,
  om_speed <- NA,
  stringsAsFactors = TRUE,
  KEEP.OUT.ATTRS = TRUE
)

###varying minority agreement avoidance
EG1m <- expand.grid(
  eg1p <- 0.5,
  eg2p <- 0.5,
  eg1m <- variation,
  eg2m <- 0.5,
  im_speed <- NA,
  om_speed <- NA,
  stringsAsFactors = TRUE,
  KEEP.OUT.ATTRS = TRUE
)

##disagreement avoidance variation
###varying majority disagreement avoidance
EG2p <- expand.grid(
  eg1p <- 0.5,
  eg2p <- variation,
  eg1m <- 0.5,
  eg2m <- 0.5,
  im_speed <- NA,
  om_speed <- NA,
  stringsAsFactors = TRUE,
  KEEP.OUT.ATTRS = TRUE
)

###varying minority agreement avoidance
EG2m <- expand.grid(
  eg1p <- 0.5,
  eg2p <- 0.5,
  eg1m <- 0.5,
  eg2m <- variation,
  im_speed <- NA,
  om_speed <- NA,
  stringsAsFactors = TRUE,
  KEEP.OUT.ATTRS = TRUE
)

###############################

#the following for loop generates the data
for (i in 1:nrow(EG1m))
{ 
  print(paste("Currently considering case", i, "of", nrow(EG1m)))
  EG1p$Var5[i] <- CSpeed(IM(EG1p$Var1[i], EG1p$Var2[i], EG1p$Var3[i], EG1p$Var4[i], bp, bm, l, n0)$M)
  EG1p$Var6[i] <- CSpeed(OM(EG1p$Var1[i], EG1p$Var2[i], EG1p$Var3[i], EG1p$Var4[i], bp, bm, l, n0)$M)
  
  EG1m$Var5[i] <- CSpeed(IM(EG1m$Var1[i], EG1m$Var2[i], EG1m$Var3[i], EG1m$Var4[i], bp, bm, l, n0)$M)
  EG1m$Var6[i] <- CSpeed(OM(EG1m$Var1[i], EG1m$Var2[i], EG1m$Var3[i], EG1m$Var4[i], bp, bm, l, n0)$M)
  
  EG2p$Var5[i] <- CSpeed(IM(EG2p$Var1[i], EG2p$Var2[i], EG2p$Var3[i], EG2p$Var4[i], bp, bm, l, n0)$M)
  EG2p$Var6[i] <- CSpeed(OM(EG2p$Var1[i], EG2p$Var2[i], EG2p$Var3[i], EG2p$Var4[i], bp, bm, l, n0)$M)
  
  EG2m$Var5[i] <- CSpeed(IM(EG2m$Var1[i],EG2m$Var2[i],EG2m$Var3[i],EG2m$Var4[i],bp,bm,l,n0)$M)
  EG2m$Var6[i] <- CSpeed(OM(EG2m$Var1[i],EG2m$Var2[i],EG2m$Var3[i],EG2m$Var4[i],bp,bm,l,n0)$M)
} 

###############################

#cleaning and labeling data for visualization
aas <- data.frame("aprob"=rep(variation,4), "sub" = c(rep("Majority",length(variation)), rep("Minority",length(variation)),
                                                      rep("Majority",length(variation)), rep("Minority",length(variation))),
                  "Model" = c(rep("Incoming", 2*length(variation)), rep("Outgoing", 2*length(variation))),
                  "Speed" = c(EG1p$Var5,EG1m$Var5,EG1p$Var6,EG1m$Var6))

das <- data.frame("dprob"=rep(variation,4), "sub" = c(rep("Majority",length(variation)), rep("Minority",length(variation)),
                                                      rep("Majority",length(variation)), rep("Minority",length(variation))),
                  "Model" = c(rep("Incoming", 2*length(variation)), rep("Outgoing", 2*length(variation))),
                  "Speed" = c(EG2p$Var5,EG2m$Var5,EG2p$Var6,EG2m$Var6))

data <- list(das,aas)
names(data) <- c("Disagreement avoidance","Agreement avoidance")

###############################
#visualizing the data

library(ggplot2)
library(dplyr)
library(scales)

#Disagreement avoidance variation (left)

data[[1]] %>%
  ggplot(aes(x=dprob, y=Speed, linetype = Model, colour = sub))+
  scale_y_log10(limits = c(10^-3.25, 10^-1.3),
                breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  geom_line()+
  theme_light(base_size = 13)+
  ggpubr::border()+
  scale_linetype_manual(values = c("solid", "dashed"), name = "Link updating subpopulation")+
  scale_color_manual(values = c("#FF0000","#0000FF"), name = "Model")+
  labs(title = "Effect of disagreement avoidance\non convergence speed", 
       x = expression(paste('Disagreement avoidance probability ',(phi[2]))),
       y = "Speed to majority consensus" ) +
  theme(plot.title = element_text(hjust = 0.5), text = element_text(size = 20),
        legend.text = element_text(size=14), legend.title = element_text(size=15),
        legend.position = "bottom", legend.box = "vertical")

#Agreement avoidance variation (left)

data[[2]] %>%
  ggplot(aes(x=aprob, y=Speed, linetype=Model, colour=sub))+
  scale_y_log10(limits = c(10^-3.25, 10^-1.3),
                breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  geom_line()+
  theme_light(base_size = 13)+
  ggpubr::border()+
  scale_linetype_manual(values = c("solid", "dashed"), name = "Link updating subpopulation")+
  scale_color_manual(values = c("#FF0000","#0000FF"), name = "Model")+
  labs(title = "Effect of Agreement avoidance\non convergence speed", 
       x = expression(paste('Agreement avoidance probability ',(phi[1]))), 
       y = "Speed to majority consensus" ) +
  theme(plot.title = element_text(hjust = 0.5), text = element_text(size = 20),
        legend.text = element_text(size=15), legend.title = element_text(size=15),
        legend.position = "bottom", legend.box = "vertical")


