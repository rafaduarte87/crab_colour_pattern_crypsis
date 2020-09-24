library(tidyverse)
library(readxl)
library(lme4)
library(lmerTest)
library(emmeans)
library(ggpubr)
library(scales)
library(car)
library(insight)
library(arm)
library(jpeg)
library(grid)
library(RCurl)

colour_data_crab<-
  read_excel("data_R_Duarte_2020.xlsx",
             sheet=1,range="A1:P115")
colour_data_background<-
  read_excel("data_R_Duarte_2020.xlsx",
             sheet=3,range="A1:O25")

# subseting backgrounds
alga_background<-colour_data_background%>%
                 filter(type=="alga")
pebble_background<-colour_data_background%>%
                   filter(type=="pebble")
mud_rock_background<-colour_data_background%>%
                     filter(type=="mud_rock")

### calculating the distances between crab and algal colour
distance_total_alga<-rep(NA,114)
for(i in 1:114)
{  
   distance_crab_alga<-rep(NA,8)
   for(j in 1:8)
     {  
     distance_crab_alga[j]<-sqrt(((colour_data_crab$x[i]-alga_background$x[j])^2)+
                      ((colour_data_crab$y[i]-alga_background$y[j])^2))
     }
     distance_total_alga[i]<-mean(distance_crab_alga) 
}

### calculating the distances between crab and pebble colour
distance_total_pebble<-rep(NA,114)
for(i in 1:114)
{  
  distance_crab_pebble<-rep(NA,8)
  for(j in 1:8)
  {  
    distance_crab_pebble[j]<-sqrt(((colour_data_crab$x[i]-pebble_background$x[j])^2)+
                                  ((colour_data_crab$y[i]-pebble_background$y[j])^2))
  }
  distance_total_pebble[i]<-mean(distance_crab_pebble) 
}

### calculating the distances between crab and mud rock colour
distance_total_mud_rock<-rep(NA,114)
for(i in 1:114)
{  
  distance_crab_mud_rock<-rep(NA,8)
  for(j in 1:8)
  {  
    distance_crab_mud_rock[j]<-sqrt(((colour_data_crab$x[i]-mud_rock_background$x[j])^2)+
                                    ((colour_data_crab$y[i]-mud_rock_background$y[j])^2))
  }
  distance_total_mud_rock[i]<-mean(distance_crab_mud_rock) 
}

### creating a dataset for analysis
distance_c_all<-
  tibble(distance=c(distance_total_alga,distance_total_pebble,
         distance_total_mud_rock),background=c(rep("alga",114),rep("pebble",114),
         rep("mud_rock",114)),species=rep(c(rep("Pt",53),rep("Pa",61)),3),
         crab_id=rep(colour_data_crab$id,3),
         size=rep(colour_data_crab$size,3))

distance_c_all<-
  distance_c_all%>%
  mutate(background=factor(background,levels=c("alga","pebble","mud_rock")),
  species=factor(species,levels=c("Pt","Pa")))

distance_small_Pa<-
  distance_c_all%>%
  filter(species=="Pa",size<=14)%>%
  add_column(size_category=rep("s",96))

distance_large_Pa<-
  distance_c_all%>%
  filter(species=="Pa",size>14)%>%
  add_column(size_category=rep("l",87))

distance_small_Pt<-
  distance_c_all%>%
  filter(species=="Pt",size<=13)%>%
  add_column(size_category=rep("s",99))

distance_large_Pt<-
  distance_c_all%>%
  filter(species=="Pt",size>13)%>%
  add_column(size_category=rep("l",60))

distance_tibble<-
  bind_rows(distance_small_Pt,distance_small_Pa,
            distance_large_Pt,distance_large_Pa)%>%
  mutate(size_category=factor(size_category,levels=c("s","l")))

distance_tibble_Pa<-
  distance_tibble%>%
  filter(species=="Pa")

distance_tibble_Pt<-
  distance_tibble%>%
  filter(species=="Pt")

# running the LMER model  
model_c_distance<-
  lmer(distance~species*size_category*background
       +(1|crab_id), data=distance_tibble)

summary(model_c_distance)
anova(model_c_distance)
ranova(model_c_distance)

# checking the assumptions
plot(residuals(model_c_distance))
hist(residuals(model_c_distance))
qqnorm(residuals(model_c_distance))
qqline(residuals(model_c_distance))

# testing the homogeneity of variances
bartlett.test(distance~interaction(species,size_category,background),
              data=distance_tibble)

# performing a posteriori Tukey test 
contrast(emmeans(model_c_distance,~background*size_category|species),"pairwise")

############# CREATING THE CAMOUFLAGE FIGURE ###################################

# creating new names to the facets
size_labs<-c("Small","Large")
names(size_labs)<-c("s","l")

# creating the graph for Pa
box_1_Pa<-
  ggplot(distance_tibble_Pa, 
         aes(x=background,y=distance))+
  facet_grid(~size_category,
             labeller=labeller(size_category=size_labs))+
  geom_boxplot(colour="black",width=0.5,linetype=1,lwd=0.5,
               outlier.shape=NA,fill="tan",alpha=0.8)+
  geom_jitter(fill="tan",colour="black",shape=21,size=2.5,
              width=0.1,show.legend=FALSE)+
  xlab("Background")+
  ylab("Colour distance")

box_2_Pa<-
  box_1_Pa+ 
  scale_x_discrete(name="Background",
                   labels=c("Alga","Pebble","Mud rock"))+
  scale_y_continuous(name="Colour distance", 
                     breaks=c(0,0.025,0.05,0.075,0.1,0.125,0.15), 
                     labels=number_format(accuracy=0.001), 
                     limits=c(0,0.175))
  
box_3_Pa<-
  box_2_Pa+
  theme_bw()+
  theme(plot.margin=margin(0.7,1,0.5,1,"cm"),
        axis.text.x=element_text(colour="black",size=10,hjust=0.5),
        axis.text.y=element_text(colour="black",size=10),
        axis.title.x=element_text(face="bold", colour="black", 
                                  size=12,margin=margin(
                                  t=10,r=20,b=10,l=20)),
        axis.title.y=element_text(face="bold",colour="black", 
                                  size=12,margin=margin(
                                  t=10,r=10,b=10,l=10)),
        legend.position="none",
        panel.background=element_rect(fill="white"),
        panel.border=element_rect(size=1,linetype="solid",
                                  colour="black"),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        strip.text.x=element_text(size=12,face="bold"),
        strip.background=element_rect(colour="black",size=1),
        panel.spacing=unit(0.5,"cm"))
  
text_Pa_1<-
  data.frame(label=c("a","a"),
             size_category=as.factor(c("s","l")),
             x=c(1,1),y=c(0.135,0.1))
text_Pa_2<-
  data.frame(label=c("b","b"),
             size_category=as.factor(c("s","l")),
             x=c(2,2),y=c(0.135,0.08))
text_Pa_3<-
  data.frame(label=c("b","b"),
             size_category=as.factor(c("s","l")),
             x=c(3,3),y=c(0.135,0.08))

box_4_Pa<-
  box_3_Pa+
  geom_text(data=text_Pa_1,
            aes(x=x,y=y,label=label),
            fontface="bold",size=4)+
  geom_text(data=text_Pa_2,
            aes(x=x,y=y,label=label),
            fontface="bold",size=4)+
  geom_text(data=text_Pa_3,
            aes(x=x,y=y,label=label),
            fontface="bold",size=4) 

image_juvenile_panopeus<-readJPEG("juvenile_Pa.jpg")
image_adult_panopeus<-readJPEG("adult_Pa.jpg")

annotation_custom2<- 
  function(grob,xmin=-Inf,xmax=Inf,ymin=-Inf,ymax=Inf,data){ 
    layer(data=data,stat=StatIdentity,position=PositionIdentity,
          geom=ggplot2:::GeomCustomAnn,
          inherit.aes=TRUE,params=list(grob=grob, 
                                       xmin=xmin,xmax=xmax, 
                                       ymin=ymin,ymax=ymax))}                                                                             

panopeus_juvenile<-
  annotation_custom2(
    rasterGrob(image_juvenile_panopeus,interpolate=TRUE),
               xmin=2.6,xmax=3.35,ymin=0.14,ymax=0.18,
               data=distance_tibble_Pa[1,])
panopeus_adult<-
  annotation_custom2(
    rasterGrob(image_adult_panopeus,interpolate=TRUE),
               xmin=2.6,xmax=3.35,ymin=0.14,ymax=0.18,
               data=distance_tibble_Pa[97,])

box_5_Pa<-
  box_4_Pa+
  panopeus_juvenile+
  panopeus_adult

#############################################################################################
# creating the graph for Pt
box_1_Pt<-
  ggplot(distance_tibble_Pt, 
         aes(x=background,y=distance))+
  facet_grid(~size_category,
             labeller=labeller(size_category=size_labs))+
  geom_boxplot(fill="olivedrab",colour="black",width=0.5,linetype=1,lwd=0.5,
               outlier.shape=NA,alpha=0.8)+
  geom_jitter(fill="olivedrab",colour="black",shape=21,size=2.5,
              width=0.1,show.legend=FALSE)+
  xlab("Background")+
  ylab("Colour distance")

box_2_Pt<-
  box_1_Pt+ 
  scale_x_discrete(name="Background",
                   labels=c("Alga","Pebble","Mud rock"))+
  scale_y_continuous(name="Colour distance", 
                     breaks=c(0,0.025,0.05,0.075,0.1,0.125,0.15), 
                     labels=number_format(accuracy=0.001), 
                     limits=c(0,0.175))
box_3_Pt<-
  box_2_Pt+
  theme_bw()+
  theme(plot.margin=margin(0,1,-0.35,1,"cm"),
        axis.text.x=element_text(colour="black",size=10,hjust=0.5),
        axis.text.y=element_text(colour="black",size=10),
        axis.title.x=element_text(face="bold", colour="black", 
                                  size=12,margin=margin(
                                  t=10,r=20,b=10,l=20)),
        axis.title.y=element_text(face="bold",colour="black", 
                                  size=12,margin=margin(
                                  t=10,r=10,b=10,l=10)),
        legend.position="none",
        panel.background=element_rect(fill="white"),
        panel.border=element_rect(size=1,linetype="solid",
                                  colour="black"),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        strip.text=element_blank(),
        rect=element_blank(),
        panel.spacing=unit(0.5,"cm"))

text_Pt_1<-
  data.frame(label=c("a",""),
             size_category=as.factor(c("s","l")),
             x=c(1,1),y=c(0.13,0.12))
text_Pt_2<-
  data.frame(label=c("b","ns"),
             size_category=as.factor(c("s","l")),
             x=c(2,2),y=c(0.13,0.145))
text_Pt_3<-
  data.frame(label=c("b",""),
             size_category=as.factor(c("s","l")),
             x=c(3,3),y=c(0.13,0.14))

box_4_Pt<-
  box_3_Pt+
  geom_text(data=text_Pt_1,
            aes(x=x,y=y,label=label),
            fontface="bold",size=4)+
  geom_text(data=text_Pt_2,
            aes(x=x,y=y,label=label),
            fontface="bold",size=4)+
  geom_text(data=text_Pt_3,
            aes(x=x,y=y,label=label),
            fontface="bold",size=4)+
  geom_segment(data=data.frame(x_i=0.75,y_i=0.135,x_e=3.25,y_e=0.135,
               size_category=as.factor("l")),
               aes(x=x_i,y=y_i,xend=x_e,yend=y_e),
               linetype="solid",size=0.5)

image_juvenile_pachygrapsus<-readJPEG("juvenile_Pt.jpg")  
image_adult_pachygrapsus<-readJPEG("adult_Pt.jpg")  

pachygrapsus_juvenile<-
  annotation_custom2(
    rasterGrob(image_juvenile_pachygrapsus,interpolate=TRUE),
    xmin=2.6,xmax=3.35,ymin=0.14,ymax=0.18,
    data=distance_tibble_Pt[1,])

pachygrapsus_adult<-
  annotation_custom2(
    rasterGrob(image_adult_pachygrapsus,interpolate=TRUE),
    xmin=2.6,xmax=3.35,ymin=0.14,ymax=0.18,
    data=distance_tibble_Pt[100,])

box_5_Pt<-
  box_4_Pt+
  pachygrapsus_juvenile+
  pachygrapsus_adult

box_arranged<-
  ggarrange(box_5_Pa+rremove("x.text")+rremove("x.title"),
            box_5_Pt,heights=c(1,1),
            labels=c("A","B"),
            label.x=c(0.05,0.05),label.y=c(0.93,1.05),
            ncol=1,nrow=2,align="v")

print(box_arranged)
