#Figure S5: Influence of inertia on outcomes
#Varying stubbornness in facets and disagreement avoidance of each subpopulation

#INPUT
#set constant global features
N <- 100 #total population size
T <- 10000 #number of time steps
n0 <- 0.8 #majority proportion

#agreement avoidant rewiring parameters 
p1p <- 0.5 #phi1+
p2p <- 0.5 #phi2+

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
                        c("Static", "Disagreement avoidant majority", 
                          "Disagreement avoidant minority"))

################################
#visualizing data
library(ggplot2)
library(dplyr)

data %>%
  ggplot(aes(x=Lambda, y=IM_Mf, colour = Strategy))+
  geom_line()+
  ylim(-1,1)+
  facet_grid(BP~BM)+
  theme_classic(base_size = 15)+
  scale_colour_manual(values = c("#000000", "#FF0000", "#0000FF"))+
  labs(title = "Effect of inertia \non final state (IM)", 
       x= expression(paste( "Inertia", (lambda) )),
       y = "Final State", subtitle = expression(paste("Minority Stubbornness ", (beta["-"] ))))+
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5), 
        text = element_text(size=18))+
  theme(legend.position = "bottom")+
  ggpubr::rremove("grid")+ggpubr::border()

data %>%
  ggplot(aes(x=Lambda, y=OM_Mf, colour = Strategy))+
  geom_line()+
  ylim(-1,1)+
  facet_grid(BP~BM)+
  theme_classic(base_size = 15)+
  scale_colour_manual(values = c("#000000", "#FF0000", "#0000FF"))+
  labs(title = "Effect of inertia \non final state (OM)", 
       x= expression(paste( "Inertia", (lambda) )),
       y = "Final State", subtitle = expression(paste("Minority Stubbornness ", (beta["-"] ))))+
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5), 
        text = element_text(size=18))+
  theme(legend.position = "bottom")+
  ggpubr::rremove("grid")+ggpubr::border()

