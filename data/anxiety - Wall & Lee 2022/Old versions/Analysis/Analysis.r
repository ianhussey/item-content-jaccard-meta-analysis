######################################################################################
#####                                                                            ##### 
#####                          Supplementary Material                            ##### 
#####            Used R version 3.6.3 and RStudio version 1.2.5042               #####
#####                                                                            #####
#####         Code adapted from Fried, (2016) supplementary materials            #####
#####                https://doi.org/10.1016/j.jad.2016.10.019                   #####
#####                            Thank you Eiko!                                 #####
#####                                                                            ##### 
######################################################################################

library('qgraph')
library('ggplot2')
library('data.table')
library('reshape2')
library('psych')
library('ade4')
library('viridis')

##### Data preparation
data = fread("MatrixB.csv")       #Load data for estimating Jaccard index (no difference between specific and compound symptoms)
dataplot = fread("MatrixA2.csv")  #Load data for plot (difference between specific and compound symptoms). 
                                  #MatrixA is raw data from spreadsheet
                                  #MatrixA2 was manually ordered. Rows with most symptoms to rows with fewest symptoms, top to bottom

##### Estimation of overlap, using the Jaccard Index

# MASQ
data1<-data[which(data$MASQ==1|data$MASQ==1),]
a1<-1-(dist.binary(matrix(c(data$MASQ ,data$"STAI-T"),nrow=2,byrow=T), method = 1)^2); a1
b1<-1-(dist.binary(matrix(c(data$MASQ ,data$"STAI-S"),nrow=2,byrow=T), method = 1)^2)
c1<-1-(dist.binary(matrix(c(data$MASQ ,data$"TMAS"),nrow=2,byrow=T), method = 1)^2)
d1<-1-(dist.binary(matrix(c(data$MASQ ,data$"STICSA"),nrow=2,byrow=T), method = 1)^2)
e1<-1-(dist.binary(matrix(c(data$MASQ ,data$"SCL-90-R"),nrow=2,byrow=T), method = 1)^2)
f1<-1-(dist.binary(matrix(c(data$MASQ ,data$"ZungSAS"),nrow=2,byrow=T), method = 1)^2)
g1<-1-(dist.binary(matrix(c(data$MASQ ,data$"CC-DAS"),nrow=2,byrow=T), method = 1)^2)
h1<-1-(dist.binary(matrix(c(data$MASQ ,data$"4DSQ"),nrow=2,byrow=T), method = 1)^2)
i1<-1-(dist.binary(matrix(c(data$MASQ ,data$"HADS"),nrow=2,byrow=T), method = 1)^2)
j1<-1-(dist.binary(matrix(c(data$MASQ ,data$"GAD-7"),nrow=2,byrow=T), method = 1)^2)
k1<-1-(dist.binary(matrix(c(data$MASQ ,data$"DASS"),nrow=2,byrow=T), method = 1)^2)
l1<-1-(dist.binary(matrix(c(data$MASQ ,data$"BAI"),nrow=2,byrow=T), method = 1)^2)
m1<-1-(dist.binary(matrix(c(data$MASQ ,data$"CAS"),nrow=2,byrow=T), method = 1)^2)
MASQ.v<-c(1,a1,b1,c1,d1,e1,f1,g1,h1,i1,j1,k1,l1,m1)

# STAI-T
a2<-1-(dist.binary(matrix(c(data$"STAI-T", data$"MASQ"),nrow=2,byrow=T), method = 1)^2)
b2<-1-(dist.binary(matrix(c(data$"STAI-T" ,data$"STAI-S"),nrow=2,byrow=T), method = 1)^2)
c2<-1-(dist.binary(matrix(c(data$"STAI-T" ,data$"TMAS"),nrow=2,byrow=T), method = 1)^2)
d2<-1-(dist.binary(matrix(c(data$"STAI-T" ,data$"STICSA"),nrow=2,byrow=T), method = 1)^2)
e2<-1-(dist.binary(matrix(c(data$"STAI-T" ,data$"SCL-90-R"),nrow=2,byrow=T), method = 1)^2)
f2<-1-(dist.binary(matrix(c(data$"STAI-T" ,data$"ZungSAS"),nrow=2,byrow=T), method = 1)^2)
g2<-1-(dist.binary(matrix(c(data$"STAI-T" ,data$"CC-DAS"),nrow=2,byrow=T), method = 1)^2)
h2<-1-(dist.binary(matrix(c(data$"STAI-T" ,data$"4DSQ"),nrow=2,byrow=T), method = 1)^2)
i2<-1-(dist.binary(matrix(c(data$"STAI-T" ,data$"HADS"),nrow=2,byrow=T), method = 1)^2)
j2<-1-(dist.binary(matrix(c(data$"STAI-T" ,data$"GAD-7"),nrow=2,byrow=T), method = 1)^2)
k2<-1-(dist.binary(matrix(c(data$"STAI-T" ,data$"DASS"),nrow=2,byrow=T), method = 1)^2)
l2<-1-(dist.binary(matrix(c(data$"STAI-T" ,data$"BAI"),nrow=2,byrow=T), method = 1)^2)
m2<-1-(dist.binary(matrix(c(data$"STAI-T" ,data$"CAS"),nrow=2,byrow=T), method = 1)^2)
"STAI-T.v"<-c(a2,1,b2,c2,d2,e2,f2,g2,h2,i2,j2,k2,l2,m2)

# STAI-S
a3<-1-(dist.binary(matrix(c(data$"STAI-S", data$"MASQ"),nrow=2,byrow=T), method = 1)^2)
b3<-1-(dist.binary(matrix(c(data$"STAI-S" ,data$"STAI-T"),nrow=2,byrow=T), method = 1)^2)
c3<-1-(dist.binary(matrix(c(data$"STAI-S" ,data$"TMAS"),nrow=2,byrow=T), method = 1)^2)
d3<-1-(dist.binary(matrix(c(data$"STAI-S" ,data$"STICSA"),nrow=2,byrow=T), method = 1)^2)
e3<-1-(dist.binary(matrix(c(data$"STAI-S" ,data$"SCL-90-R"),nrow=2,byrow=T), method = 1)^2)
f3<-1-(dist.binary(matrix(c(data$"STAI-S" ,data$"ZungSAS"),nrow=2,byrow=T), method = 1)^2)
g3<-1-(dist.binary(matrix(c(data$"STAI-S" ,data$"CC-DAS"),nrow=2,byrow=T), method = 1)^2)
h3<-1-(dist.binary(matrix(c(data$"STAI-S" ,data$"4DSQ"),nrow=2,byrow=T), method = 1)^2)
i3<-1-(dist.binary(matrix(c(data$"STAI-S" ,data$"HADS"),nrow=2,byrow=T), method = 1)^2)
j3<-1-(dist.binary(matrix(c(data$"STAI-S" ,data$"GAD-7"),nrow=2,byrow=T), method = 1)^2)
k3<-1-(dist.binary(matrix(c(data$"STAI-S" ,data$"DASS"),nrow=2,byrow=T), method = 1)^2)
l3<-1-(dist.binary(matrix(c(data$"STAI-S" ,data$"BAI"),nrow=2,byrow=T), method = 1)^2)
m3<-1-(dist.binary(matrix(c(data$"STAI-S" ,data$"CAS"),nrow=2,byrow=T), method = 1)^2)
"STAI-S.v"<-c(a3,b3,1,c3,d3,e3,f3,g3,h3,i3,j3,k3,l3,m3)

# TMAS
a4<-1-(dist.binary(matrix(c(data$"TMAS", data$"MASQ"),nrow=2,byrow=T), method = 1)^2)
b4<-1-(dist.binary(matrix(c(data$"TMAS" ,data$"STAI-T"),nrow=2,byrow=T), method = 1)^2)
c4<-1-(dist.binary(matrix(c(data$"TMAS" ,data$"STAI-S"),nrow=2,byrow=T), method = 1)^2)
d4<-1-(dist.binary(matrix(c(data$"TMAS" ,data$"STICSA"),nrow=2,byrow=T), method = 1)^2)
e4<-1-(dist.binary(matrix(c(data$"TMAS" ,data$"SCL-90-R"),nrow=2,byrow=T), method = 1)^2)
f4<-1-(dist.binary(matrix(c(data$"TMAS" ,data$"ZungSAS"),nrow=2,byrow=T), method = 1)^2)
g4<-1-(dist.binary(matrix(c(data$"TMAS" ,data$"CC-DAS"),nrow=2,byrow=T), method = 1)^2)
h4<-1-(dist.binary(matrix(c(data$"TMAS" ,data$"4DSQ"),nrow=2,byrow=T), method = 1)^2)
i4<-1-(dist.binary(matrix(c(data$"TMAS" ,data$"HADS"),nrow=2,byrow=T), method = 1)^2)
j4<-1-(dist.binary(matrix(c(data$"TMAS" ,data$"GAD-7"),nrow=2,byrow=T), method = 1)^2)
k4<-1-(dist.binary(matrix(c(data$"TMAS" ,data$"DASS"),nrow=2,byrow=T), method = 1)^2)
l4<-1-(dist.binary(matrix(c(data$"TMAS" ,data$"BAI"),nrow=2,byrow=T), method = 1)^2)
m4<-1-(dist.binary(matrix(c(data$"TMAS" ,data$"CAS"),nrow=2,byrow=T), method = 1)^2)
"TMAS.v"<-c(a4,b4,c4,1,d4,e4,f4,g4,h4,i4,j4,k4,l4,m4)

# STICSA
a5<-1-(dist.binary(matrix(c(data$"STICSA", data$"MASQ"),nrow=2,byrow=T), method = 1)^2)
b5<-1-(dist.binary(matrix(c(data$"STICSA" ,data$"STAI-T"),nrow=2,byrow=T), method = 1)^2)
c5<-1-(dist.binary(matrix(c(data$"STICSA" ,data$"STAI-S"),nrow=2,byrow=T), method = 1)^2)
d5<-1-(dist.binary(matrix(c(data$"STICSA" ,data$"TMAS"),nrow=2,byrow=T), method = 1)^2)
e5<-1-(dist.binary(matrix(c(data$"STICSA" ,data$"SCL-90-R"),nrow=2,byrow=T), method = 1)^2)
f5<-1-(dist.binary(matrix(c(data$"STICSA" ,data$"ZungSAS"),nrow=2,byrow=T), method = 1)^2)
g5<-1-(dist.binary(matrix(c(data$"STICSA" ,data$"CC-DAS"),nrow=2,byrow=T), method = 1)^2)
h5<-1-(dist.binary(matrix(c(data$"STICSA" ,data$"4DSQ"),nrow=2,byrow=T), method = 1)^2)
i5<-1-(dist.binary(matrix(c(data$"STICSA" ,data$"HADS"),nrow=2,byrow=T), method = 1)^2)
j5<-1-(dist.binary(matrix(c(data$"STICSA" ,data$"GAD-7"),nrow=2,byrow=T), method = 1)^2)
k5<-1-(dist.binary(matrix(c(data$"STICSA" ,data$"DASS"),nrow=2,byrow=T), method = 1)^2)
l5<-1-(dist.binary(matrix(c(data$"STICSA" ,data$"BAI"),nrow=2,byrow=T), method = 1)^2)
m5<-1-(dist.binary(matrix(c(data$"STICSA" ,data$"CAS"),nrow=2,byrow=T), method = 1)^2)
"STICSA.v"<-c(a5,b5,c5,d5,1,e5,f5,g5,h5,i5,j5,k5,l5,m5)

# SCL-90-R
a6<-1-(dist.binary(matrix(c(data$"SCL-90-R", data$"MASQ"),nrow=2,byrow=T), method = 1)^2)
b6<-1-(dist.binary(matrix(c(data$"SCL-90-R" ,data$"STAI-T"),nrow=2,byrow=T), method = 1)^2)
c6<-1-(dist.binary(matrix(c(data$"SCL-90-R" ,data$"STAI-S"),nrow=2,byrow=T), method = 1)^2)
d6<-1-(dist.binary(matrix(c(data$"SCL-90-R" ,data$"TMAS"),nrow=2,byrow=T), method = 1)^2)
e6<-1-(dist.binary(matrix(c(data$"SCL-90-R" ,data$"STICSA"),nrow=2,byrow=T), method = 1)^2)
f6<-1-(dist.binary(matrix(c(data$"SCL-90-R" ,data$"ZungSAS"),nrow=2,byrow=T), method = 1)^2)
g6<-1-(dist.binary(matrix(c(data$"SCL-90-R" ,data$"CC-DAS"),nrow=2,byrow=T), method = 1)^2)
h6<-1-(dist.binary(matrix(c(data$"SCL-90-R" ,data$"4DSQ"),nrow=2,byrow=T), method = 1)^2)
i6<-1-(dist.binary(matrix(c(data$"SCL-90-R" ,data$"HADS"),nrow=2,byrow=T), method = 1)^2)
j6<-1-(dist.binary(matrix(c(data$"SCL-90-R" ,data$"GAD-7"),nrow=2,byrow=T), method = 1)^2)
k6<-1-(dist.binary(matrix(c(data$"SCL-90-R" ,data$"DASS"),nrow=2,byrow=T), method = 1)^2)
l6<-1-(dist.binary(matrix(c(data$"SCL-90-R" ,data$"BAI"),nrow=2,byrow=T), method = 1)^2)
m6<-1-(dist.binary(matrix(c(data$"SCL-90-R" ,data$"CAS"),nrow=2,byrow=T), method = 1)^2)
"SCL-90-R.v"<-c(a6,b6,c6,d6,e6,1,f6,g6,h6,i6,j6,k6,l6,m6)

# ZungSAS
a7<-1-(dist.binary(matrix(c(data$"ZungSAS", data$"MASQ"),nrow=2,byrow=T), method = 1)^2)
b7<-1-(dist.binary(matrix(c(data$"ZungSAS" ,data$"STAI-T"),nrow=2,byrow=T), method = 1)^2)
c7<-1-(dist.binary(matrix(c(data$"ZungSAS" ,data$"STAI-S"),nrow=2,byrow=T), method = 1)^2)
d7<-1-(dist.binary(matrix(c(data$"ZungSAS" ,data$"TMAS"),nrow=2,byrow=T), method = 1)^2)
e7<-1-(dist.binary(matrix(c(data$"ZungSAS" ,data$"STICSA"),nrow=2,byrow=T), method = 1)^2)
f7<-1-(dist.binary(matrix(c(data$"ZungSAS" ,data$"SCL-90-R"),nrow=2,byrow=T), method = 1)^2)
g7<-1-(dist.binary(matrix(c(data$"ZungSAS" ,data$"CC-DAS"),nrow=2,byrow=T), method = 1)^2)
h7<-1-(dist.binary(matrix(c(data$"ZungSAS" ,data$"4DSQ"),nrow=2,byrow=T), method = 1)^2)
i7<-1-(dist.binary(matrix(c(data$"ZungSAS" ,data$"HADS"),nrow=2,byrow=T), method = 1)^2)
j7<-1-(dist.binary(matrix(c(data$"ZungSAS" ,data$"GAD-7"),nrow=2,byrow=T), method = 1)^2)
k7<-1-(dist.binary(matrix(c(data$"ZungSAS" ,data$"DASS"),nrow=2,byrow=T), method = 1)^2)
l7<-1-(dist.binary(matrix(c(data$"ZungSAS" ,data$"BAI"),nrow=2,byrow=T), method = 1)^2)
m7<-1-(dist.binary(matrix(c(data$"ZungSAS" ,data$"CAS"),nrow=2,byrow=T), method = 1)^2)
"ZungSAS.v"<-c(a7,b7,c7,d7,e7,f7,1,g7,h7,i7,j7,k7,l7,m7)

# CC-DAS
a8<-1-(dist.binary(matrix(c(data$"CC-DAS", data$"MASQ"),nrow=2,byrow=T), method = 1)^2)
b8<-1-(dist.binary(matrix(c(data$"CC-DAS" ,data$"STAI-T"),nrow=2,byrow=T), method = 1)^2)
c8<-1-(dist.binary(matrix(c(data$"CC-DAS" ,data$"STAI-S"),nrow=2,byrow=T), method = 1)^2)
d8<-1-(dist.binary(matrix(c(data$"CC-DAS" ,data$"TMAS"),nrow=2,byrow=T), method = 1)^2)
e8<-1-(dist.binary(matrix(c(data$"CC-DAS" ,data$"STICSA"),nrow=2,byrow=T), method = 1)^2)
f8<-1-(dist.binary(matrix(c(data$"CC-DAS" ,data$"SCL-90-R"),nrow=2,byrow=T), method = 1)^2)
g8<-1-(dist.binary(matrix(c(data$"CC-DAS" ,data$"ZungSAS"),nrow=2,byrow=T), method = 1)^2)
h8<-1-(dist.binary(matrix(c(data$"CC-DAS" ,data$"4DSQ"),nrow=2,byrow=T), method = 1)^2)
i8<-1-(dist.binary(matrix(c(data$"CC-DAS" ,data$"HADS"),nrow=2,byrow=T), method = 1)^2)
j8<-1-(dist.binary(matrix(c(data$"CC-DAS" ,data$"GAD-7"),nrow=2,byrow=T), method = 1)^2)
k8<-1-(dist.binary(matrix(c(data$"CC-DAS" ,data$"DASS"),nrow=2,byrow=T), method = 1)^2)
l8<-1-(dist.binary(matrix(c(data$"CC-DAS" ,data$"BAI"),nrow=2,byrow=T), method = 1)^2)
m8<-1-(dist.binary(matrix(c(data$"CC-DAS" ,data$"CAS"),nrow=2,byrow=T), method = 1)^2)
"CC-DAS.v"<-c(a8,b8,c8,d8,e8,f8,g8,1,h8,i8,j8,k8,l8,m8)

# 4DSQ
a9<-1-(dist.binary(matrix(c(data$"4DSQ", data$"MASQ"),nrow=2,byrow=T), method = 1)^2)
b9<-1-(dist.binary(matrix(c(data$"4DSQ" ,data$"STAI-T"),nrow=2,byrow=T), method = 1)^2)
c9<-1-(dist.binary(matrix(c(data$"4DSQ" ,data$"STAI-S"),nrow=2,byrow=T), method = 1)^2)
d9<-1-(dist.binary(matrix(c(data$"4DSQ" ,data$"TMAS"),nrow=2,byrow=T), method = 1)^2)
e9<-1-(dist.binary(matrix(c(data$"4DSQ" ,data$"STICSA"),nrow=2,byrow=T), method = 1)^2)
f9<-1-(dist.binary(matrix(c(data$"4DSQ" ,data$"SCL-90-R"),nrow=2,byrow=T), method = 1)^2)
g9<-1-(dist.binary(matrix(c(data$"4DSQ" ,data$"ZungSAS"),nrow=2,byrow=T), method = 1)^2)
h9<-1-(dist.binary(matrix(c(data$"4DSQ" ,data$"CC-DAS"),nrow=2,byrow=T), method = 1)^2)
i9<-1-(dist.binary(matrix(c(data$"4DSQ" ,data$"HADS"),nrow=2,byrow=T), method = 1)^2)
j9<-1-(dist.binary(matrix(c(data$"4DSQ" ,data$"GAD-7"),nrow=2,byrow=T), method = 1)^2)
k9<-1-(dist.binary(matrix(c(data$"4DSQ" ,data$"DASS"),nrow=2,byrow=T), method = 1)^2)
l9<-1-(dist.binary(matrix(c(data$"4DSQ" ,data$"BAI"),nrow=2,byrow=T), method = 1)^2)
m9<-1-(dist.binary(matrix(c(data$"4DSQ" ,data$"CAS"),nrow=2,byrow=T), method = 1)^2)
"4DSQ.v"<-c(a9,b9,c9,d9,e9,f9,g9,h9,1,i9,j9,k9,l9,m9)

# HADS
a10<-1-(dist.binary(matrix(c(data$"HADS", data$"MASQ"),nrow=2,byrow=T), method = 1)^2)
b10<-1-(dist.binary(matrix(c(data$"HADS" ,data$"STAI-T"),nrow=2,byrow=T), method = 1)^2)
c10<-1-(dist.binary(matrix(c(data$"HADS" ,data$"STAI-S"),nrow=2,byrow=T), method = 1)^2)
d10<-1-(dist.binary(matrix(c(data$"HADS" ,data$"TMAS"),nrow=2,byrow=T), method = 1)^2)
e10<-1-(dist.binary(matrix(c(data$"HADS" ,data$"STICSA"),nrow=2,byrow=T), method = 1)^2)
f10<-1-(dist.binary(matrix(c(data$"HADS" ,data$"SCL-90-R"),nrow=2,byrow=T), method = 1)^2)
g10<-1-(dist.binary(matrix(c(data$"HADS" ,data$"ZungSAS"),nrow=2,byrow=T), method = 1)^2)
h10<-1-(dist.binary(matrix(c(data$"HADS" ,data$"CC-DAS"),nrow=2,byrow=T), method = 1)^2)
i10<-1-(dist.binary(matrix(c(data$"HADS" ,data$"4DSQ"),nrow=2,byrow=T), method = 1)^2)
j10<-1-(dist.binary(matrix(c(data$"HADS" ,data$"GAD-7"),nrow=2,byrow=T), method = 1)^2)
k10<-1-(dist.binary(matrix(c(data$"HADS" ,data$"DASS"),nrow=2,byrow=T), method = 1)^2)
l10<-1-(dist.binary(matrix(c(data$"HADS" ,data$"BAI"),nrow=2,byrow=T), method = 1)^2)
m10<-1-(dist.binary(matrix(c(data$"HADS" ,data$"CAS"),nrow=2,byrow=T), method = 1)^2)
"HADS.v"<-c(a10,b10,c10,d10,e10,f10,g10,h10,i10,1,j10,k10,l10,m10)

# GAD-7
a11<-1-(dist.binary(matrix(c(data$"GAD-7", data$"MASQ"),nrow=2,byrow=T), method = 1)^2)
b11<-1-(dist.binary(matrix(c(data$"GAD-7" ,data$"STAI-T"),nrow=2,byrow=T), method = 1)^2)
c11<-1-(dist.binary(matrix(c(data$"GAD-7" ,data$"STAI-S"),nrow=2,byrow=T), method = 1)^2)
d11<-1-(dist.binary(matrix(c(data$"GAD-7" ,data$"TMAS"),nrow=2,byrow=T), method = 1)^2)
e11<-1-(dist.binary(matrix(c(data$"GAD-7" ,data$"STICSA"),nrow=2,byrow=T), method = 1)^2)
f11<-1-(dist.binary(matrix(c(data$"GAD-7" ,data$"SCL-90-R"),nrow=2,byrow=T), method = 1)^2)
g11<-1-(dist.binary(matrix(c(data$"GAD-7" ,data$"ZungSAS"),nrow=2,byrow=T), method = 1)^2)
h11<-1-(dist.binary(matrix(c(data$"GAD-7" ,data$"CC-DAS"),nrow=2,byrow=T), method = 1)^2)
i11<-1-(dist.binary(matrix(c(data$"GAD-7" ,data$"4DSQ"),nrow=2,byrow=T), method = 1)^2)
j11<-1-(dist.binary(matrix(c(data$"GAD-7" ,data$"HADS"),nrow=2,byrow=T), method = 1)^2)
k11<-1-(dist.binary(matrix(c(data$"GAD-7" ,data$"DASS"),nrow=2,byrow=T), method = 1)^2)
l11<-1-(dist.binary(matrix(c(data$"GAD-7" ,data$"BAI"),nrow=2,byrow=T), method = 1)^2)
m11<-1-(dist.binary(matrix(c(data$"GAD-7" ,data$"CAS"),nrow=2,byrow=T), method = 1)^2)
"GAD-7.v"<-c(a11,b11,c11,d11,e11,f11,g11,h11,i11,j11,1,k11,l11,m11)

# DASS
a12<-1-(dist.binary(matrix(c(data$"DASS", data$"MASQ"),nrow=2,byrow=T), method = 1)^2)
b12<-1-(dist.binary(matrix(c(data$"DASS" ,data$"STAI-T"),nrow=2,byrow=T), method = 1)^2)
c12<-1-(dist.binary(matrix(c(data$"DASS" ,data$"STAI-S"),nrow=2,byrow=T), method = 1)^2)
d12<-1-(dist.binary(matrix(c(data$"DASS" ,data$"TMAS"),nrow=2,byrow=T), method = 1)^2)
e12<-1-(dist.binary(matrix(c(data$"DASS" ,data$"STICSA"),nrow=2,byrow=T), method = 1)^2)
f12<-1-(dist.binary(matrix(c(data$"DASS" ,data$"SCL-90-R"),nrow=2,byrow=T), method = 1)^2)
g12<-1-(dist.binary(matrix(c(data$"DASS" ,data$"ZungSAS"),nrow=2,byrow=T), method = 1)^2)
h12<-1-(dist.binary(matrix(c(data$"DASS" ,data$"CC-DAS"),nrow=2,byrow=T), method = 1)^2)
i12<-1-(dist.binary(matrix(c(data$"DASS" ,data$"4DSQ"),nrow=2,byrow=T), method = 1)^2)
j12<-1-(dist.binary(matrix(c(data$"DASS" ,data$"HADS"),nrow=2,byrow=T), method = 1)^2)
k12<-1-(dist.binary(matrix(c(data$"DASS" ,data$"GAD-7"),nrow=2,byrow=T), method = 1)^2)
l12<-1-(dist.binary(matrix(c(data$"DASS" ,data$"BAI"),nrow=2,byrow=T), method = 1)^2)
m12<-1-(dist.binary(matrix(c(data$"DASS" ,data$"CAS"),nrow=2,byrow=T), method = 1)^2)
"DASS.v"<-c(a12,b12,c12,d12,e12,f12,g12,h12,i12,j12,k12,1,l12,m12)

# BAI
a13<-1-(dist.binary(matrix(c(data$"BAI", data$"MASQ"),nrow=2,byrow=T), method = 1)^2)
b13<-1-(dist.binary(matrix(c(data$"BAI" ,data$"STAI-T"),nrow=2,byrow=T), method = 1)^2)
c13<-1-(dist.binary(matrix(c(data$"BAI" ,data$"STAI-S"),nrow=2,byrow=T), method = 1)^2)
d13<-1-(dist.binary(matrix(c(data$"BAI" ,data$"TMAS"),nrow=2,byrow=T), method = 1)^2)
e13<-1-(dist.binary(matrix(c(data$"BAI" ,data$"STICSA"),nrow=2,byrow=T), method = 1)^2)
f13<-1-(dist.binary(matrix(c(data$"BAI" ,data$"SCL-90-R"),nrow=2,byrow=T), method = 1)^2)
g13<-1-(dist.binary(matrix(c(data$"BAI" ,data$"ZungSAS"),nrow=2,byrow=T), method = 1)^2)
h13<-1-(dist.binary(matrix(c(data$"BAI" ,data$"CC-DAS"),nrow=2,byrow=T), method = 1)^2)
i13<-1-(dist.binary(matrix(c(data$"BAI" ,data$"4DSQ"),nrow=2,byrow=T), method = 1)^2)
j13<-1-(dist.binary(matrix(c(data$"BAI" ,data$"HADS"),nrow=2,byrow=T), method = 1)^2)
k13<-1-(dist.binary(matrix(c(data$"BAI" ,data$"GAD-7"),nrow=2,byrow=T), method = 1)^2)
l13<-1-(dist.binary(matrix(c(data$"BAI" ,data$"DASS"),nrow=2,byrow=T), method = 1)^2)
m13<-1-(dist.binary(matrix(c(data$"BAI" ,data$"CAS"),nrow=2,byrow=T), method = 1)^2)
"BAI.v"<-c(a13,b13,c13,d13,e13,f13,g13,h13,i13,j13,k13,l13,1,m13)

# CAS
a14<-1-(dist.binary(matrix(c(data$"CAS", data$"MASQ"),nrow=2,byrow=T), method = 1)^2)
b14<-1-(dist.binary(matrix(c(data$"CAS" ,data$"STAI-T"),nrow=2,byrow=T), method = 1)^2)
c14<-1-(dist.binary(matrix(c(data$"CAS" ,data$"STAI-S"),nrow=2,byrow=T), method = 1)^2)
d14<-1-(dist.binary(matrix(c(data$"CAS" ,data$"TMAS"),nrow=2,byrow=T), method = 1)^2)
e14<-1-(dist.binary(matrix(c(data$"CAS" ,data$"STICSA"),nrow=2,byrow=T), method = 1)^2)
f14<-1-(dist.binary(matrix(c(data$"CAS" ,data$"SCL-90-R"),nrow=2,byrow=T), method = 1)^2)
g14<-1-(dist.binary(matrix(c(data$"CAS" ,data$"ZungSAS"),nrow=2,byrow=T), method = 1)^2)
h14<-1-(dist.binary(matrix(c(data$"CAS" ,data$"CC-DAS"),nrow=2,byrow=T), method = 1)^2)
i14<-1-(dist.binary(matrix(c(data$"CAS" ,data$"4DSQ"),nrow=2,byrow=T), method = 1)^2)
j14<-1-(dist.binary(matrix(c(data$"CAS" ,data$"HADS"),nrow=2,byrow=T), method = 1)^2)
k14<-1-(dist.binary(matrix(c(data$"CAS" ,data$"GAD-7"),nrow=2,byrow=T), method = 1)^2)
l14<-1-(dist.binary(matrix(c(data$"CAS" ,data$"DASS"),nrow=2,byrow=T), method = 1)^2)
m14<-1-(dist.binary(matrix(c(data$"CAS" ,data$"BAI"),nrow=2,byrow=T), method = 1)^2)
"CAS.v"<-c(a14,b14,c14,d14,e14,f14,g14,h14,i14,j14,k14,l14,m14,1)

# Create table
M = matrix(nrow=14, ncol=14) 
colnames(M) <- c("MASQ",	"STAI-T",	"STAI-S",	"TMAS",	"STICSA", "SCL-90-R", "ZungSAS", 
                 "CC-DAS", "4DSQ", "HADS", "GAD-7", "DASS", "BAI", "CAS")
rownames(M) <- c("MASQ",	"STAI-T",	"STAI-S",	"TMAS",	"STICSA", "SCL-90-R", "ZungSAS", 
                 "CC-DAS", "4DSQ", "HADS", "GAD-7", "DASS", "BAI", "CAS")
M[1,]<-MASQ.v
M[2,]<-`STAI-T.v`
M[3,]<-`STAI-S.v`
M[4,]<-TMAS.v
M[5,]<-STICSA.v
M[6,]<-`SCL-90-R.v`
M[7,]<-ZungSAS.v
M[8,]<-`CC-DAS.v`
M[9,]<-`4DSQ.v`
M[10,]<-HADS.v
M[11,]<-`GAD-7.v`
M[12,]<-DASS.v
M[13,]<-BAI.v
M[14,]<-CAS.v
isSymmetric(M)

M
M[M == 1] <- 0 # replace diagonal with 0
colMeans(M)
mean(colMeans(M)) #0.231

length1<-c(28,20,20,50,42,29,20,9,28,7,7,14,21,25) # length of original questionnaires
length2<-c(25,14,14,31,20,25,25,8,20,8,7,19,16,16) # items in analysis per scale; MASQ captures 25 items

cor(length1, colMeans(M)) #0.150
cor(length2, colMeans(M)) #0.593

#write table to csv
#write.csv(M, file = "cor_table.csv",row.names=T)


##### Figure 1 
set.seed(223)
d <- dataplot
d[, S := factor(paste0("S",1:nrow(d)))] #Create symptom variable
d = melt(d, id.vars="S", variable.name="Scales", value.name="Type") #Transform to long format
d = d[Type>=1] #Keep the scales in which the symptoms are 1 (present) or 2 (included)
d[, Type := factor(Type, labels=c("Scale contains compound symptom", "Scale contains specific symptom"))]
d[, count := .N, by=S]

# Symptom order
sympt.order = d[, .N, by=S][order(N)][, S] #Replace by order
d[, S := factor(S, levels = sympt.order)]

# Scale order by frequency
scale.order = d[, .N, by=Scales][order(N)][, Scales]
d[, Scales := factor(Scales, levels = scale.order)]
d[, Scales2 := as.numeric(Scales)]


# Plot
# Pick color for each scale
pal1 <- c("#000000", "#999999", "#AA00AA", "#FF55FF", "#5555FF", "#0000AA", "#00AAAA", 
          "#00E6E6", "#55FF55", "#00AA00", "#FDEB28", "#FFAA00", "#FF5555", "#AA0000")

a<- ggplot(d, aes(x=S, y=Scales2, group=S, color=as.factor(Scales), shape=Type, rev=F)) +
  geom_line() + #keep this here, otherwise there is an error 
  xlab("") +
  ylab("") +
  theme(text = element_text(size=15)) +  #change axis label font size
  # Generate the grid lines
  geom_hline(yintercept = 1:14, colour = "grey80", size = .2) +
  geom_vline(xintercept = 1:60, colour = "grey80", size = .2) +
  # Lines
  geom_line(colour="grey60") +
  # Fill the middle space with a white blank circle
  geom_rect(xmin=-Inf,xmax=Inf,ymin=-Inf,ymax=.97,fill="white", color=NA) +
  # Points
  geom_point(size=3, fill="white", stroke=1) +
  # Polar coordinates
  coord_polar() +
  scale_shape_manual(values=c(21,19)) +
  # The angle for the symptoms and remove the default grid lines
  theme(axis.text.x = element_text(angle = 360/(2*pi)*rev( pi/2 + seq( pi/60, 2*pi-pi/60, len=60)) + 
                                     c(rep(0, 30), rep(180,30))),
        panel.border = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        panel.background = element_blank(),
        legend.title = element_blank(),
        legend.position = "top") +
  scale_y_continuous(limits=c(-4,14), expand=c(0,0), breaks=1:14, labels=d[, levels(Scales)]) +
  scale_color_manual(values=pal1); a

ggsave(plot=a,filename="Figure1.pdf", width=12, height=12, useDingbats=FALSE)
# Figure was edited using other software to move legends, reorder symptoms, and add subscale tags. I'm sure this can be done in R, but it was beyond my ability.