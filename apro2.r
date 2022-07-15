setwd("C:/OneDrive - UC San Diego/data/ASTRAL-PRO-ASTER")
require(reshape2); require(ggplot2); require(scales); require(plyr); require(patchwork);


mr = list("A-Pro2*" ="aster", "A-Pro2" ="aster-new", "MulRF" = "mulrf", "DupTree" = "duptree", "iGTP-Losses"="losses","PF" = "method1", "A-Pro" ="method2","ASTRAL-multi"="multi")
gt = list("Est. (100bp)" = "100", "Est. (500bp)" = "500",  "true" ="true" )
lrates = list('0'="0", '0.1'="01",  '0.5'="05", '1'="1")
drates = list('0'="0",  '0.2'="02", '1'="1", '2'="2", '5'="5")

experiment="E3"
d= read.csv("stat3.stat",sep=" ",header = F,colClasses = c("factor","factor","factor","factor","factor","numeric","numeric","numeric"))
levels(d$V5) = mr
levels(d$V4)=gt
levels(d$V2)=drates
levels(d$V3)=lrates
head(d)

names(d)[2] = "Dup"
names(d)[3] = "Loss/Dup"
names(d)[4] = "Input"

ggplot(aes(x=`Loss/Dup` ,y=V8,color=V5,group=V5),data=d[!d$V5 %in% c("MulRF","PF","iGTP-Losses","ASTRAL-multi"),])+
  stat_summary(geom="line")+
  #geom_boxplot()+
  stat_summary(geom="errorbar",width=0.22)+
  stat_summary(geom="point",size=1)+
  facet_grid(`Input`~`Dup`,scales="free",space="free_x",labeller = label_both)+#coord_cartesian(ylim=c(0,0.15))+
  scale_color_brewer(palette = "Set2",name="")+
  xlab("Loss/Dup rate")+ylab("Species tree error (NRF)")+
  theme_classic()+theme(legend.position = "bottom",panel.border  = element_rect(fill=NA,size = 1))+
  scale_y_continuous(labels=percent)
ggsave("E3.pdf",width = 5,height = 5.4)

d2= read.csv("stat_exp4.stat",sep=" ",header = F,colClasses = c("factor","factor","factor","factor","factor","numeric","numeric","numeric"))
head(d2)
levels(d2$V5) = mr
levels(d2$V4)=gt
levels(d2$V2)=drates
names(d2)[2] = "Dup"
names(d2)[3] = "ILS"
names(d2)[4] = "Input"
head(d2)


levels(d2$Dup) = list("No duploss"="0", "Low duploss"="1", "High duploss"="5")
levels(d2$ILS) = list("ILS: 0% RF"="0", "ILS: 20% RF"="20", "ILS: 50% RF"="50", "ILS: 70% RF"="70")
levels(d2$Input) = list("True" ="true", "Est. (500bp)" = "Est. (500bp)", "Est. (100bp)" = "Est. (100bp)")
ggplot(aes(x=Input,y=V8,color=V5,group=V5),data=d2[d2$V5 %in% c("MulRF","DupTree","A-Pro","ASTRAL-multi"),])+
  stat_summary(geom="line")+
  #geom_boxplot()+
  stat_summary(geom="errorbar",width=0.22)+
  stat_summary(geom="point",size=1)+
  facet_grid(`ILS`~`Dup`)+coord_cartesian(ylim=c(0,0.2))+
  scale_color_brewer(palette = "Set2",name="")+
  xlab("")+ylab("Species tree error (NRF)")+
  theme_classic()+theme(legend.position = "none",panel.border  = element_rect(fill=NA,size = 1))+
  scale_y_continuous(labels=percent) + theme(axis.text.x = element_text(angle = 15, vjust = 1.2, hjust=1))
ggsave("Apro-E4.png",width = 5,height = 6, dpi=2000)

d2$alpha = 0.25
d2[d2$V5 %in% c("DupTree","A-Pro"),]$alpha = 1
ggplot(aes(x=Input,y=V8,color=V5,group=V5, alpha=alpha),data=d2[d2$V5 %in% c("MulRF","DupTree","A-Pro","ASTRAL-multi"),])+
  stat_summary(geom="line")+
  #geom_boxplot()+
  stat_summary(geom="errorbar",width=0.22)+
  stat_summary(geom="point",size=1)+
  facet_grid(`ILS`~`Dup`)+coord_cartesian(ylim=c(0,0.2))+
  scale_color_brewer(palette = "Set2",name="")+
  xlab("")+ylab("Species tree error (NRF)")+
  theme_classic()+theme(legend.position = "none",panel.border  = element_rect(fill=NA,size = 1))+
  scale_y_continuous(labels=percent) + theme(axis.text.x = element_text(angle = 15, vjust = 1.2, hjust=1))
ggsave("Apro-DupTree.png",width = 5,height = 6, dpi=2000)

d2$alpha = 0.25
d2[d2$V5 %in% c("ASTRAL-multi","A-Pro"),]$alpha = 1
ggplot(aes(x=Input,y=V8,color=V5,group=V5, alpha=alpha),data=d2[d2$V5 %in% c("MulRF","DupTree","A-Pro","ASTRAL-multi"),])+
  stat_summary(geom="line")+
  #geom_boxplot()+
  stat_summary(geom="errorbar",width=0.22)+
  stat_summary(geom="point",size=1)+
  facet_grid(`ILS`~`Dup`)+coord_cartesian(ylim=c(0,0.2))+
  scale_color_brewer(palette = "Set2",name="")+
  xlab("")+ylab("Species tree error (NRF)")+
  theme_classic()+theme(legend.position = "none",panel.border  = element_rect(fill=NA,size = 1))+
  scale_y_continuous(labels=percent) + theme(axis.text.x = element_text(angle = 15, vjust = 1.2, hjust=1))
ggsave("Apro-ASTRAL-multi.png",width = 5,height = 6, dpi=2000)

d2$alpha = 0.25
d2[d2$V5 %in% c("MulRF","A-Pro"),]$alpha = 1
ggplot(aes(x=Input,y=V8,color=V5,group=V5, alpha=alpha),data=d2[d2$V5 %in% c("MulRF","DupTree","A-Pro","ASTRAL-multi"),])+
  stat_summary(geom="line")+
  #geom_boxplot()+
  stat_summary(geom="errorbar",width=0.22)+
  stat_summary(geom="point",size=1)+
  facet_grid(`ILS`~`Dup`)+coord_cartesian(ylim=c(0,0.2))+
  scale_color_brewer(palette = "Set2",name="")+
  xlab("")+ylab("Species tree error (NRF)")+
  theme_classic()+theme(legend.position = "none",panel.border  = element_rect(fill=NA,size = 1))+
  scale_y_continuous(labels=percent) + theme(axis.text.x = element_text(angle = 15, vjust = 1.2, hjust=1))
ggsave("Apro-MulRF.png",width = 5,height = 6, dpi=2000)

ggplot(aes(x=ILS,y=V8,color=V5,group=V5),data=d2[!d2$V5 %in% c("MulRF","PF","iGTP-Losses","ASTRAL-multi"),])+
  stat_summary(geom="line")+
  #geom_boxplot()+
  stat_summary(geom="errorbar",width=0.22)+
  stat_summary(geom="point",size=1)+
  facet_grid(`Input`~`Dup`,scales="free_y",labeller = label_both)+#coord_cartesian(ylim=c(0,0.15))+
  scale_color_brewer(palette = "Set2",name="")+
  xlab("ILS level (RF%)")+ylab("Species tree error (NRF)")+
  theme_classic()+theme(legend.position = "bottom",panel.border  = element_rect(fill=NA,size = 1))+
  scale_y_continuous(labels=percent)
ggsave("E4.pdf",width = 4,height = 5.4)

e2t= read.csv("stat_exp2_time.stat",sep=" ",header = F ,colClasses = c("factor","numeric","numeric","factor","factor","numeric"))
e2a =  read.csv("stat_exp2.stat",sep=" ",header = F , colClasses = c("factor","numeric","numeric","factor","factor","numeric","numeric","numeric"))

levels(e2a$V5) = mr
levels(e2t$V5) = mr
e2 = cbind(e2a,e2t[,6])
names(e2)[9] = "runTime"

e1t= read.csv("stat_exp1_time.stat",sep=" ",header = F,colClasses = c("factor","numeric","numeric","factor","factor","numeric"))
e1a =  read.csv("stat_exp1.stat",sep=" ",header = F,colClasses = c("factor","numeric","numeric","factor","factor","numeric","numeric","numeric"))

levels(e1a$V5) = mr
levels(e1t$V5) = mr
e1 = cbind(e1a,e1t[,6])
names(e1)[9] = "runTime"

( e1[e1$V5 %in% c("A-Pro","A-Pro2"),] |>
    ddply(.(V2,V3,V4,V5), summarize, RF = mean(V8), seRF = sd(V8)/sqrt(length(V8)-1), time = mean(runTime)/60, seTime=sd(runTime)/sqrt(length(V8)-1)/60) |>
    ggplot( aes(x=time, xmin=time-seTime, xmax=time+seTime, y=RF, ymin=RF-seRF,ymax=RF+seRF, color=as.factor(V4), linetype=V5))+
    geom_point(aes(shape=(as.factor(V2))), size = 2)+
    geom_line( )+
    geom_errorbar(width=0.08, orientation="x")+#geom_errorbar(width=0.03, orientation="y")+
    scale_y_log10(labels=percent,breaks=c(0.01,0.02,0.05,0.1,0.25))+
    xlab("Running time (minutes)")+ylab("Species tree error (NRF)")+
    scale_shape(name="#species")+
    scale_linetype(name="")+
    scale_color_brewer(palette = "Dark2",name="Gene trees",labels=function(x) sub("true","true gt",sub("00","00 bp",x)))+
    scale_x_log10(breaks=c(0.01,0.1,1,10,100), labels=c(0.01,0.1,1,10,100))+
    theme_bw() + 
    theme(plot.tag.position = c(0.01, 0.98),plot.tag = element_text(size=14,face = "bold"),
                       legend.margin = margin(0,0,0,0,"pt"),legend.box.margin = margin(0,0,0,0,"pt"))+labs(tag="a)") ) +
( e2[e2$V5 %in% c("A-Pro","A-Pro2"),] |>
ddply(.(V2,V3,V4,V5), summarize, RF = mean(V8), seRF = sd(V8)/sqrt(length(V8)-1), time = mean(runTime)/60, seTime=sd(runTime)/sqrt(length(V8)-1)/60) |>
ggplot( aes(x=time, xmin=time-seTime, xmax=time+seTime, y=RF, ymin=RF-seRF,ymax=RF+seRF, color=as.factor(V4), linetype=V5))+
  geom_point(aes(shape=(as.factor(V3))), size = 2)+
  geom_line( )+
  geom_errorbar(width=0.05, orientation="x")+#geom_errorbar(width=0.03, orientation="y")+
  scale_y_log10(labels=percent,breaks=c(0.01,0.02,0.05,0.1,0.25))+
  xlab("Running time (minutes)")+ylab("Species tree error (NRF)")+
  scale_shape(name="#genes")+
  scale_linetype(name="")+
  scale_color_brewer(palette = "Dark2",name="Gene trees",labels=function(x) sub("true","true gt",sub("00","00 bp",x)))+
  scale_x_log10(breaks=c(0.01,0.1,1), labels=c(0.01,0.1,1))+
  theme_bw() + 
  theme(plot.tag.position = c(0.01, 0.98),plot.tag = element_text(size=14,face = "bold"),
        legend.margin = margin(0,0,0,0,"pt"),legend.box.margin = margin(0,0,0,0,"pt"))+labs(tag="b)") ); ggsave("E1-2.pdf",width = 12,height = 5)

summary(aov(V8~V5*(V2+V4),data=e1[e1$V5 %in% c("A-Pro","A-Pro2"),]))
summary(aov(V8~V5*(V3+V4),data=e2[e2$V5 %in% c("A-Pro","A-Pro2"),]))

mean(e1[e1$V5=="A-Pro" & e1$V2==500 & e1$V3==1000, ]$runTime)/mean(e1[e1$V5=="A-Pro2" & e1$V2==500 & e1$V3==1000, ]$runTime)
mean(e2[e2$V5=="A-Pro" & e2$V2==25 & e2$V3==10000, ]$runTime)/mean(e2[e2$V5=="A-Pro2" & e2$V2==25 & e2$V3==10000, ]$runTime)

d = read.csv("S100.csv",sep=",",colClasses = c("factor","factor","factor","factor","numeric","factor","numeric"))
d=d[d$MTHD %in% c("aster-new","apro-v2","mulrf","duptree") & d$DLRT != 0, ]
levels(d$PSIZ) = list("1E7" = "10000000", "5E7" = "50000000")
levels(d$MTHD) = list("A-Pro2" = "aster-new", "A-Pro" = "apro-v2", "MulRF" = "mulrf", "DupTree" = "duptree")
levels(d$SQLN) = list("Seq Len: 25" = "25", "Seq Len: 50" = "50", "Seq Len: 100" = "100", "Seq Len: 250" = "250", "True gene tree" = "0")
ggplot(aes(x=NGEN,y=SERF,color=MTHD),data=d)+
  stat_summary(geom="line")+
  #geom_boxplot()+
  stat_summary(geom="errorbar",width=0.22)+
  stat_summary(geom="point",size=1)+
  facet_grid(SQLN~interaction(PSIZ,DLRT, sep = " / "), scales="free_y")+#coord_cartesian(ylim=c(0,0.2))+
  scale_color_brewer(palette = "Set2",name="")+
  xlab("Number of genes")+ylab("Species tree error (NRF)")+
  theme_classic()+#theme(legend.position = "none",panel.border  = element_rect(fill=NA,size = 1))+
  scale_y_continuous(labels=percent) + scale_x_continuous(trans='log10', breaks = c(25,50,100,500))
ggsave("S100.pdf",width = 9,height = 7)
summary(aov(SERF~MTHD*(PSIZ+DLRT+SQLN+NGEN)+Error(REPL),data=d[d$MTHD %in% c("A-Pro","A-Pro2"),]))
