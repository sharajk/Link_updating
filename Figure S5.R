#Figure S5: Influence of inertia on outcomes
#Varying stubbornness in facets and disagreement avoidance of each subpopulation

#Loading required model functions (incoming model, outgoing model, consensus speeds)

library(devtools)
library(roxygen2)
source_url("https://raw.githubusercontent.com/sharajk/Link_updating/057a5a7a4ce6f72fe06ea31e607518a09fbaba71/Speed.R")
source_url("https://raw.githubusercontent.com/sharajk/Link_updating/main/Incoming%20model.R")
source_url("https://raw.githubusercontent.com/sharajk/Link_updating/main/Outgoing%20model.R")

##############################

#INPUT
#set constant global features
N <- 100 #total population size
T <- 10000 #number of time steps
n0 <- 0.8 #majority proportion

#agreement avoidant rewiring parameters 
p1p <- 0.5 #phi1+
p1m <- 0.5 #phi2+

#varying inertia, stubbornness, and strategy
variation <- seq(0,0.9,by=0.1)

egp <- expand.grid(
  BP <- c(0.05,0.2,0.95),
  BM <- c(0.05,0.2,0.95),
  p2p <- c(0,0.5,0),
  l <- variation,
  im_out <- NA,
  stringsAsFactors = TRUE,
  KEEP.OUT.ATTRS = TRUE
)

egm <- expand.grid(
  BP <- c(0.05,0.2,0.95),
  BM <- c(0.05,0.2,0.95),
  p2m <- c(0,0,0.5),
  l <- variation,
  om_out <- NA,
  stringsAsFactors = TRUE,
  KEEP.OUT.ATTRS = TRUE
)

eg <- data.frame(egp,egm)

for(i in 1:nrow(eg))
{
  eg$Var5[i] <- IM(p1p,eg$Var3[i],p1m,eg$Var3.1[i],eg$Var1[i],eg$Var2[i],eg$Var4[i],n0)$M[T]
  eg$Var5.1[i] <- OM(p1p,eg$Var3[i],p1m,eg$Var3.1[i],eg$Var1[i],eg$Var2[i],eg$Var4[i],n0)$M[T]
}

################################
#cleaning and labeling data

data <- data.frame("Strategy"=NA, "BP"=eg$Var1, "BM" = eg$Var2, 
                   "Lambda" = eg$Var4, "IM_Mf" = eg$Var5, "OM_Mf"= eg$Var5.1)


for(i in 1:nrow(eg))
{
  data$Strategy[i] <- paste(eg$Var3[i],eg$Var3.1[i], sep = ",")
}

data$Strategy <- factor(data$Strategy, labels = 
                        c("Static", "Disagreement avoidant minority",
                          "Disagreement avoidant majority"))

################################
#visualizing data
library(ggplot2)
library(dplyr)

data$BP <- factor(data$BP, levels = c(0.95,0.2,0.05))

data %>%
  ggplot(aes(x=Lambda, y=IM_Mf, colour = Strategy))+
  geom_line()+
  ylim(-1,1)+
  facet_grid(BP~BM)+
  theme_classic(base_size = 15)+
  scale_colour_manual(values = c("#000000", "#0000FF", "#FF0000"))+
  labs(title = "Effect of inertia \non final state (IM)", 
       x= expression(paste( "Inertia", (lambda) )),
       y = "Final State", subtitle = expression(paste("Minority Stubbornness ", (beta["-"] ))))+
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5), 
        text = element_text(size=20))+
  theme(legend.position = "bottom")+
  ggpubr::rremove("grid")+ggpubr::border()

data %>%
  ggplot(aes(x=Lambda, y=OM_Mf, colour = Strategy))+
  geom_line()+
  ylim(-1,1)+
  facet_grid(BP~BM)+
  theme_classic(base_size = 15)+
  scale_colour_manual(values = c("#000000", "#0000FF", "#FF0000"))+
  labs(title = "Effect of inertia \non final state (OM)", 
       x= expression(paste( "Inertia", (lambda) )),
       y = "Final State", subtitle = expression(paste("Minority Stubbornness ", (beta["-"] ))))+
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5), 
        text = element_text(size=20))+
  theme(legend.position = "bottom")+
  ggpubr::rremove("grid")+ggpubr::border()

