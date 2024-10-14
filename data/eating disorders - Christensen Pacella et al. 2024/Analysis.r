######################################################################################
#####                                                                            #### 
#####     Code for Christensen Pacella et al. Analysis of ED Measures            #####  
#####                                Assessment                                  #####  
#####              Adapted from Supplementary material for                       #####
#####             Fried 2016, Journal of Affective Disorders                     #####                  
#####                               Main analysis                                #####
#####                                                                            ##### 
######################################################################################

library('qgraph')
library('ggplot2')
library('data.table')
library('reshape2')
library('psych')
library('ade4')
library('viridis')

getwd()
setwd("~/")
getwd()

###you will need to set directory yourself
setwd("/Users/kelseyhagan/Dropbox/Projects/Co-Author/ED Measures/Analyses")

##### Data preparation
data = fread("MatrixB_EDMeasures_5-26-2023.csv")     #Load data for estimating Jaccard index (no difference between specific and compound symptoms)
dataplot = fread("MatrixA_EDMeasures_5-26-2023.csv") #Load data for plot (difference between specific and compound symptoms)

##### Estimation of overlap, using the Jaccard Index

# EDEQ
data1<-data[which(data$QEDD==1|data$EDEQ==1),]
a1<-1-(dist.binary(matrix(c(data$EDEQ, data$EDDS),nrow=2,byrow=T), method = 1)^2); a1
b1<-1-(dist.binary(matrix(c(data$EDEQ, data$EPSI),nrow=2,byrow=T), method = 1)^2)
c1<-1-(dist.binary(matrix(c(data$EDEQ, data$EAT26),nrow=2,byrow=T), method = 1)^2)
d1<-1-(dist.binary(matrix(c(data$EDEQ, data$BULITR),nrow=2,byrow=T), method = 1)^2)
e1<-1-(dist.binary(matrix(c(data$EDEQ,data$QEDD),nrow=2,byrow=T), method = 1)^2)
EDEQ.v<-c(1,a1,b1,c1,d1,e1)

# EDDS
a2<-1-(dist.binary(matrix(c(data$EDDS, data$EDEQ),nrow=2,byrow=T), method = 1)^2)
b2<-1-(dist.binary(matrix(c(data$EDDS, data$EPSI),nrow=2,byrow=T), method = 1)^2)
c2<-1-(dist.binary(matrix(c(data$EDDS, data$EAT26),nrow=2,byrow=T), method = 1)^2)
d2<-1-(dist.binary(matrix(c(data$EDDS, data$BULITR),nrow=2,byrow=T), method = 1)^2)
e2<-1-(dist.binary(matrix(c(data$EDDS, data$QEDD),nrow=2,byrow=T), method = 1)^2)
EDDS.v<-c(a2,1,b2,c2,d2,e2)

# EPSI
a3<-1-(dist.binary(matrix(c(data$EPSI, data$EDEQ),nrow=2,byrow=T), method = 1)^2)
b3<-1-(dist.binary(matrix(c(data$EPSI, data$EDDS),nrow=2,byrow=T), method = 1)^2)
c3<-1-(dist.binary(matrix(c(data$EPSI, data$EAT26),nrow=2,byrow=T), method = 1)^2)
d3<-1-(dist.binary(matrix(c(data$EPSI, data$BULITR),nrow=2,byrow=T), method = 1)^2)
e3<-1-(dist.binary(matrix(c(data$EPSI, data$QEDD),nrow=2,byrow=T), method = 1)^2)
EPSI.v<-c(a3,b3,1,c3,d3,e3)

# EAT26
a4<-1-(dist.binary(matrix(c(data$EAT26, data$EDEQ),nrow=2,byrow=T), method = 1)^2)
b4<-1-(dist.binary(matrix(c(data$EAT26, data$EDDS),nrow=2,byrow=T), method = 1)^2)
c4<-1-(dist.binary(matrix(c(data$EAT26, data$EPSI),nrow=2,byrow=T), method = 1)^2)
d4<-1-(dist.binary(matrix(c(data$EAT26, data$BULITR),nrow=2,byrow=T), method = 1)^2)
e4<-1-(dist.binary(matrix(c(data$EAT26, data$QEDD),nrow=2,byrow=T), method = 1)^2)
EAT26.v<-c(a4,b4,c4,1,d4,e4)

# BULIT-R
a5<-1-(dist.binary(matrix(c(data$BULITR, data$EDEQ),nrow=2,byrow=T), method = 1)^2)
b5<-1-(dist.binary(matrix(c(data$BULITR, data$EDDS),nrow=2,byrow=T), method = 1)^2)
c5<-1-(dist.binary(matrix(c(data$BULITR, data$EPSI),nrow=2,byrow=T), method = 1)^2)
d5<-1-(dist.binary(matrix(c(data$BULITR, data$EAT26),nrow=2,byrow=T), method = 1)^2)
e5<-1-(dist.binary(matrix(c(data$BULITR, data$QEDD),nrow=2,byrow=T), method = 1)^2)
BULITR.v<-c(a5,b5,c5, d5,1, e5)

# QEDD
a6<-1-(dist.binary(matrix(c(data$QEDD, data$EDEQ),nrow=2,byrow=T), method = 1)^2)
b6<-1-(dist.binary(matrix(c(data$QEDD, data$EDDS),nrow=2,byrow=T), method = 1)^2)
c6<-1-(dist.binary(matrix(c(data$QEDD, data$EPSI),nrow=2,byrow=T), method = 1)^2)
d6<-1-(dist.binary(matrix(c(data$QEDD, data$EAT26),nrow=2,byrow=T), method = 1)^2)
e6<-1-(dist.binary(matrix(c(data$QEDD, data$BULITR),nrow=2,byrow=T), method = 1)^2)
QEDD.v<-c(a6,b6,c6, d6,e6,1)

# Create table
M = matrix(nrow=6, ncol=6) 
colnames(M) <- c("EDEQ",	"EDDS", "EPSI",	"EAT26"	,"BULITR"	,"QEDD")
rownames(M) <- c("EDEQ",	"EDDS",	"EPSI",	"EAT26"	,"BULITR"	,"QEDD")
M[1,]<-EDEQ.v
M[2,]<-EDDS.v
M[3,]<-EPSI.v
M[4,]<-EAT26.v
M[5,]<-BULITR.v
M[6,]<-QEDD.v
isSymmetric(M)

M
M[M == 1] <- 0 # replace diagonal with 0
M
colMeans(M) # note: per Eiko's erratum, these original computations were incorrect, so the code below is what is correct


#       EDEQ       EDDS       EPSI     EAT26     BULITR      QEDD
#EDEQ   0.0000000 0.26829268 0.12000000 0.1666667 0.24000000 0.3095238
#EDDS   0.2682927 0.00000000 0.09090909 0.2000000 0.15789474 0.3823529
#EPSI   0.1200000 0.09090909 0.00000000 0.1304348 0.08333333 0.1590909
#EAT26  0.1666667 0.20000000 0.13043478 0.0000000 0.13043478 0.2439024
#BULITR 0.2400000 0.15789474 0.08333333 0.1304348 0.00000000 0.2380952
#QEDD   0.3095238 0.38235294 0.15909091 0.2439024 0.23809524 0.0000000

mean(colMeans(M)) #incorrect

# ---------- beginning of new code part 1, August 28 2018
x <- colMeans(M)
x[1] <- sum(M[,1])/5
x[2] <- sum(M[,2])/5 
x[3] <- sum(M[,3])/5
x[4] <- sum(M[,4])/5
x[5] <- sum(M[,5])/5
x[6] <- sum(M[,6])/5
x

#EDEQ      EDDS      EPSI     EAT26    BULITR      QEDD 
#0.2208966 0.2198899 0.1167536 0.1742877 0.1699516 0.2665931    # without the diagonal, which should be correct

mean(x) #0.1947288 without the diag, which is correct

# ---------- end of new code part 1, August 28 2018

# kh adjusted 5-26-2023 (I'm not sure this is right)
length1<-c(28,20,26,24,25,25) # length of original (adjusted) questionnaires; e.g. EDEQ 32 items originally, condensed down to 30
length2<-c(30,22,40,34,36,29) # items in analysis per scale; EDEQ captures 21 items

cor(length1, x) #0.2938447
cor(length2, x) #0.5720158

##### Figure 1 https://osf.io/xa3sh 

install.packages("ggrepel")
library(ggrepel)
library(dplyr)

set.seed(223)
#d <- dataplot
#d[, S := factor(paste0("S",1:nrow(d)))] #Create symptom variable
#d = melt(d, id.vars="S", variable.name="Scales", value.name="Type") #Transform to long format
#d = d[(d$Type>=1),] #Keep the scales in which the symptoms are 1 (present) or 2 (included)
d <- read.csv("plot_labels_revision.csv", header = TRUE)
d<-as.data.table(d) # next steps will only work if data table, so change
#d[, Type := factor(Type, labels=c("Contains compound symptom", "Contains specific symptom"))]
d$Type <-as.factor(d$Type)
#d[, count := .N, by=S]

# Symptom order
#sympt.order = d[, .N, by=S][order(N)][, S] 
#d[, S := factor(S, levels = sympt.order)]

# ... (your existing code)

# Order levels of S numerically
d[, S := factor(S, levels = unique(d[order(as.numeric(gsub("S", "", S))), S]))]

# ... (continue with your existing code)

# Scale order by frequency
scale.order = d[, .N, by=Scales][order(N)][, Scales]
d[, Scales := factor(Scales, levels = scale.order)]
d[, Scales2 := as.numeric(Scales)]

# Plot
pal1 <- c("#56B4E9", "#009E73", "#F0E442", "#D55E00", "#CC79A7", "purple")

ED_plot<- ggplot(d, aes(x= S, y=Scales2, group=S, color=as.factor(Scales), shape=Type, rev=F)) +
  geom_line() + #keep this here, otherwise there is an error 
  xlab("") +
  ylab("") +
  # Generate the grid lines
  #1:6 for number of scales, 1:87 for number of symptoms RV
  geom_hline(yintercept = 1:6, colour = "grey80", size = .2) +
  geom_vline(xintercept = 1:87, colour = "grey80", size = .2) +
  # Points and lines
  geom_line(colour="grey60") +
  geom_point(size=3, fill="white") +
  # Fill the middle space with a white blank rectangle
  geom_rect(xmin=-Inf,xmax=Inf,ymin=-Inf,ymax=.6,fill="white", color=NA) +
  # Polar coordinates (THIS IS THE PART THAT MAKES IT INTO A CIRCLE-SHAPE)
  #scale shape manual changes shapes of the plot points
  coord_polar() +
  scale_shape_manual(values=c(21,19)) +
  theme (
    axis.text.x = element_text(size = 12, angle = 360, hjust = 0.5, vjust = -0.5),
    panel.border = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    panel.background = element_blank(),
    legend.position="bottom", 
    legend.text = element_text(size = 16), 
    legend.title = element_blank(), 
    plot.margin = unit(rep(.5,4), "lines")) +
  labs(fill="") + # remove legend title
  scale_y_continuous(limits=c(-4,8), expand=c(0,0), breaks=1:6, labels= NULL) +
  scale_color_manual(values=pal1) 
  # geom_text(aes (label = S), size = 0, position = position_stack(vjust = 0.5))
 # geom_text_repel(aes(label = S), size = 3, segment.alpha = 0.2)

ggsave(plot=ED_plot,filename="ED_overlap_plot_1-2-2024.pdf", width=12, height=12, useDingbats=FALSE)


##### Figure 2
adjacency<-read.csv("Matrix_B_ED_2-15-2023.csv", head=F)
adjacency<-adjacency[-1,]
adjacency<-as.data.frame(lapply(adjacency,as.numeric))

adjacency$V1[adjacency$V1== 1] <- 0
adjacency$V1[adjacency$V1== 2] <- 1
adjacency$V2[adjacency$V2== 1] <- 0
adjacency$V2[adjacency$V2== 2] <- 1
adjacency$V3[adjacency$V3== 1] <- 0
adjacency$V3[adjacency$V3== 2] <- 1
adjacency$V4[adjacency$V4== 1] <- 0
adjacency$V4[adjacency$V4== 2] <- 1
adjacency$V5[adjacency$V5== 1] <- 0
adjacency$V5[adjacency$V5== 2] <- 1
adjacency$V6[adjacency$V6== 1] <- 0
adjacency$V6[adjacency$V6== 2] <- 1

adjacency<-as.matrix(adjacency)
adj <-  1*(adjacency%*%t(adjacency)) 
diag(adj) = 0

pdf("Figure2.pdf", useDingbats=FALSE)
Fig2<-qgraph(adj, layout="spring", vsize=3, cut=7, border.width=1.5, 
             border.color="black", repulsion=.8) 
dev.off()
