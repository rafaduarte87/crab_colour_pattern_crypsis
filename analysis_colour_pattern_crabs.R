library(tidyverse)
library(readxl)
library(ggpubr)
library(scales)
library(car)
library(emmeans)
library(jpeg)
library(grid)
library(RCurl)

## importing data
colour_data<-
  read_excel("data_R_Duarte_2020.xlsx",
             sheet=1,range="A1:P115")
pattern_data<-
  read_excel("data_R_Duarte_2020.xlsx",
             sheet=2,range="A1:I115")

## transforming species in a factor with two levels
colour_data<-
  colour_data%>%
  mutate(species=factor(species,levels=c("Pa","Pt")))
pattern_data<-
  pattern_data%>%
  mutate(species=factor(species,levels=c("Pa","Pt")))

## adding a new variable (size category) to both datasets
## colour data
colour_data_Pa_s<-
  colour_data%>%
  filter(species=="Pa",size<14)%>%
  add_column(size_category=rep("s",32))

colour_data_Pa_l<-
  colour_data%>%
  filter(species=="Pa",size>=14)%>%
  add_column(size_category=rep("l",29))

colour_data_Pt_s<-
  colour_data%>%
  filter(species=="Pt",size<13)%>%
  add_column(size_category=rep("s",33))

colour_data_Pt_l<-
  colour_data%>%
  filter(species=="Pt",size>=13)%>%
  add_column(size_category=rep("l",20))

colour_tibble<-
  bind_rows(colour_data_Pt_s,colour_data_Pt_l,
            colour_data_Pa_s,colour_data_Pa_l)%>%
  mutate(size_category=factor(size_category,levels=c("s","l")))

## pattern data
pattern_data_Pa_s<-
  pattern_data%>%
  filter(species=="Pa",size<14)%>%
  add_column(size_category=rep("s",32))

pattern_data_Pa_l<-
  pattern_data%>%
  filter(species=="Pa",size>=14)%>%
  add_column(size_category=rep("l",29))

pattern_data_Pt_s<-
  pattern_data%>%
  filter(species=="Pt",size<13)%>%
  add_column(size_category=rep("s",33))

pattern_data_Pt_l<-
  pattern_data%>%
  filter(species=="Pt",size>=13)%>%
  add_column(size_category=rep("l",20))

pattern_tibble<-
  bind_rows(pattern_data_Pt_s,pattern_data_Pt_l,
            pattern_data_Pa_s,pattern_data_Pa_l)%>%
  mutate(size_category=factor(size_category,levels=c("s","l")))

###########################################################################
# Brightness 
###########################################################################

# performing a two-way ANOVA to compare carapace brightness between crab
# species and size categories

model_brightness<-
  aov(log(brightness)~species*size_category,data=colour_tibble)
anova(model_brightness)

# checking the assumptions
plot(residuals(model_brightness))
hist(residuals(model_brightness))
qqnorm(residuals(model_brightness))
qqline(residuals(model_brightness))

# testing the homogeneity of the variances
bartlett.test(log(brightness)~interaction(species,size_category),
              data=colour_tibble)

# performing a posteriori Tukey test
pairs(emmeans(model_brightness,"size_category",by="species"))

###########################################################################
# Saturation 
###########################################################################

# performing a two-way ANOVA to compare carapace saturation between crab
# species and size categories
model_saturation<-
  aov(saturation~species*size_category,data=colour_tibble)
anova(model_saturation)

# checking the assumptions
plot(residuals(model_saturation))
hist(residuals(model_saturation))
qqnorm(residuals(model_saturation))
qqline(residuals(model_saturation))

# testing the homogeneity of the variances
bartlett.test(saturation~interaction(species,size_category),
              data=colour_tibble)

# performing a posteriori Tukey test
pairs(emmeans(model_saturation,"species"))

###########################################################################
# Hue
###########################################################################

# performing a two-way ANOVA to compare carapace hue between crab
# species and size categories
model_hue<-
  aov(hue_2~species*size_category,data=colour_tibble)
anova(model_hue)

# checking the assumptions
plot(residuals(model_hue))
hist(residuals(model_hue))
qqnorm(residuals(model_hue))
qqline(residuals(model_hue))

# testing the homogeneity of the variances
bartlett.test(hue_2~interaction(species,size_category),
              data=colour_tibble)

# performing a posteriori Tukey test
pairs(emmeans(model_hue,"size_category",by="species"))


###################### CREATING THE COLOUR FIGURE ##########################################
############################################################################################
## Figure 2 A: boxplot comparing brightness values between juveniles and adults within each species
# creating new names to the facets
species_labs<-c("Mud crab","Mottled crab")
names(species_labs)<-c("Pa","Pt")

b_b_1<-
  ggplot(colour_tibble,aes(x=size_category,y=brightness,fill=species))+
  facet_grid(~species,
             labeller=labeller(species=species_labs))+
  xlab("Size class")+ylab("Brightness (%)")+
  geom_boxplot(colour="black",width=0.5,linetype=1,lwd=0.5,
               outlier.shape=NA,alpha=0.8)+
  geom_jitter(aes(fill=species),colour="black",shape=21,size=2.5,
              width=0.1,show.legend=FALSE)

b_b_2<-
  b_b_1+
  scale_x_discrete(name="Size class",
                   labels=c("small","large"))+
  scale_y_continuous(name="Brightness (%)", 
                     breaks=c(0,5,10,15,20,25), 
                     labels=c(0,5,10,15,20,25), 
                     limits=c(0,25))+
  scale_fill_manual(values=c("tan","olivedrab"),
                    labels=c("Pa","Pt"))

b_b_3<-
  b_b_2+
  theme_bw()+
  theme(plot.margin=margin(0.5,1,0.3,1,"cm"),
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

text_brightness<-
  data.frame(label=c("***","ns"),
             species=c("Pa","Pt"),
             x=c(1.5,1.5),y=c(24,10))                
b_b_4<-
  b_b_3+
  geom_text(data=text_brightness,
            mapping=aes(x=x,y=y,label=label),
            fontface="bold",size=4)

imagem_panopeus<-readJPEG("juvenile_Pa.jpg")
imagem_pachygrapsus<-readJPEG("adult_Pt.jpg")

annotation_custom2<- 
  function(grob,xmin=-Inf,xmax=Inf,ymin=-Inf,ymax=Inf,data){ 
    layer(data=data,stat=StatIdentity,position=PositionIdentity,
          geom=ggplot2:::GeomCustomAnn,
          inherit.aes=TRUE,params=list(grob=grob, 
                                       xmin=xmin,xmax=xmax, 
                                       ymin=ymin,ymax=ymax))}                                                                             

panopeus_colour<-annotation_custom2(rasterGrob(imagem_panopeus,interpolate=TRUE),
    xmin=1.75,xmax=2.25,ymin=18.5,ymax=26,data=colour_tibble[54,])
pachygrapsus_colour<-annotation_custom2(rasterGrob(imagem_pachygrapsus,interpolate=TRUE),
    xmin=1.75,xmax=2.25,ymin=18.5,ymax=26,data=colour_tibble[1,])

b_b_5<-b_b_4+panopeus_colour+pachygrapsus_colour

## Figure 2 B: boxplot comparing saturation values between juveniles and adults within each species
b_s_1<-
  ggplot(colour_tibble,aes(x=size_category,y=saturation,fill=species))+
  facet_grid(~species,
             labeller=labeller(species=species_labs))+
  xlab("Size class")+ylab("Saturation")+
  geom_boxplot(colour="black",width=0.5,linetype=1,lwd=0.5,
               outlier.shape=NA,alpha=0.8)+
  geom_jitter(aes(fill=species),colour="black",shape=21,size=2.5,
              width=0.1,show.legend=FALSE)

b_s_2<-
  b_s_1+
  scale_x_discrete(name="Size class",
                   labels=c("small","large"))+
  scale_y_continuous(name="Saturation", 
                     breaks=c(0,0.05,0.1,0.15,0.2,0.25), 
                     labels=number_format(accuracy = 0.01), 
                     limits=c(0,0.25))+
  scale_fill_manual(values=c("tan","olivedrab"),
                    labels=c("Pa","Pt"))

b_s_3<-
  b_s_2+
  theme_bw()+
  theme(plot.margin=margin(0,1,0.8,1,"cm"),
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

text_saturation<-
  data.frame(label=c("a","b"),
             species=c("Pa","Pt"),
             x=c(1.5,1.5),y=c(0.245,0.245))
b_s_4<-
  b_s_3+
  geom_segment(aes(x=0.75,xend=2.25,y=0.23,yend=0.23),
             size=0.5,colour="black")+
  geom_text(data=text_saturation,
            mapping=aes(x=x,y=y,label=label),
            fontface="bold",size=4)

## Figure 2 C: boxplot comparing hue values between juveniles and adults within each species
b_h_1<-
  ggplot(colour_tibble,aes(x=size_category,y=hue_2,fill=species))+
  facet_grid(~species,
             labeller=labeller(species=species_labs))+
  xlab("Size class")+ylab("Hue")+
  geom_boxplot(colour="black",width=0.5,linetype=1,lwd=0.5,
               outlier.shape=NA,alpha=0.8)+
  geom_jitter(aes(fill=species),colour="black",shape=21,size=2.5,
              width=0.1,show.legend=FALSE)
b_h_2<-
  b_h_1+
  scale_x_discrete(name="Size class",
                   labels=c("small","large"))+
  scale_y_continuous(name="Hue", 
                     breaks=c(0.45,0.5,0.55,0.6,0.65,0.7), 
                     labels=number_format(accuracy=0.01), 
                     limits=c(0.45,0.7))+
  scale_fill_manual(values=c("tan","olivedrab"),
                    labels=c("Pa","Pt"))
  
b_h_3<-
  b_h_2+
  theme_bw()+
  theme(plot.margin=margin(-0.5,1,-0.1,1,"cm"),
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

text_hue<-
  data.frame(label=c("ns","***"),
             species=c("Pa","Pt"),
             x=c(1.5,1.5),y=c(0.56,0.62)) 

b_h_4<-
  b_h_3+
  geom_text(data=text_hue,
            mapping=aes(x=x,y=y,label=label),
            fontface="bold",size=4)

# arranging the graphs of brightness and hue in a single figure
colour_graph<-
  ggarrange(b_b_5+rremove("x.text")+rremove("x.title"),
            b_s_4+rremove("x.text")+rremove("x.title"),
            b_h_4,heights=c(1,1,1),labels=c("A","B","C"),
            label.x=c(0.055,0.055,0.055),label.y=c(0.94,1.01,1.07),
            ncol=1,nrow=3,align="v")

print(colour_graph)

##############################################################################
##################### PATTERN ANALYSIS ########################################
###############################################################################

###########################################################################
# Marking size
###########################################################################

# performing a two-way ANOVA to compare carapace marking size between crab
# species and size categories
model_marking_size<-
  aov(log(max_frequency_corrected)~species*size_category,data=pattern_tibble)
anova(model_marking_size)

# checking the assumptions
plot(residuals(model_marking_size))
hist(residuals(model_marking_size))
qqnorm(residuals(model_marking_size))
qqline(residuals(model_marking_size))

# testing the homogeneity of variances
bartlett.test(log(max_frequency_corrected)~interaction(species,size_category),
              data=pattern_tibble)

# performing a posteriori Tukey test 
pairs(emmeans(model_marking_size,"species"))
pairs(emmeans(model_marking_size,"size_category")) 

###########################################################################
# Summed energy
###########################################################################

# performing a two-way ANOVA to compare carapace pattern contrast (i.e. summed
# energy) between crab species and size categories
model_summed_energy<-
  aov(log(sum_energy)~species*size_category,data=pattern_tibble)
anova(model_summed_energy)

# checking the assumptions
plot(residuals(model_summed_energy))
hist(residuals(model_summed_energy))
qqnorm(residuals(model_summed_energy))
qqline(residuals(model_summed_energy))

# testing the homogeneity of variances
bartlett.test(log(sum_energy)~interaction(species,size_category),
              data=pattern_tibble)

# performing a posteriori Tukey test 
pairs(emmeans(model_summed_energy,"size_category",by="species"))

###########################################################################
# Proportion energy
###########################################################################

# performing a two-way ANOVA to compare carapace pattern diversity (i.e. 
# proportion energy) between crab species and size categories
model_prop_energy<-
  aov(log(prop_energy)~species*size_category,data=pattern_tibble)
anova(model_prop_energy)

# checking assumptions
plot(residuals(model_prop_energy))
hist(residuals(model_prop_energy))
qqnorm(residuals(model_prop_energy))
qqline(residuals(model_prop_energy))

# testing the homogeneity of variances
bartlett.test(log(prop_energy)~interaction(species,size_category),
              data=pattern_tibble)

# performing a posteriori Tukey test 
pairs(emmeans(model_prop_energy,"size_category",by="species")) 

###################### CREATING THE PATTERN FIGURE ##########################################
## Figure 3 A: boxplot comparing marking size between life stages within each species ##
b_ms_1<-
  ggplot(pattern_tibble,aes(x=size_category,y=max_frequency_corrected,fill=species))+
  facet_grid(~species,
             labeller=labeller(species=species_labs))+
  xlab("Size class") + ylab("Marking size (mm)")+
  geom_boxplot(colour="black",width=0.5,linetype=1,lwd=0.5,
               outlier.shape=NA,alpha=0.8)+
  geom_jitter(aes(fill=species),colour="black",shape=21,size=2.5,
              width=0.1,show.legend=FALSE)
  
b_ms_2<-
  b_ms_1+
  scale_x_discrete(name="Size class",
                   labels=c("small","large"))+
  scale_y_continuous(name="Marking size (mm)", 
                     breaks=c(0,1.5,3,4.5,6,7.5), 
                     labels=number_format(accuracy=0.1), 
                     limits=c(0,8.9))+
  scale_fill_manual(values=c("tan","olivedrab"),
                    labels=c("Pa","Pt"))
  
b_ms_3<-
  b_ms_2+
  theme_bw()+
  theme(plot.margin=margin(0.5,1,0.3,1,"cm"),
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

text_marking_size_1<-
  data.frame(label=c("***","***"),
             species=c("Pa","Pt"),
             x=c(1.5,1.5),y=c(6,1.7))                
text_marking_size_2<-
  data.frame(label=c("a","b"),
             species=c("Pa","Pt"),
             x=c(1.5,1.5),y=c(7.3,3))                
line_marking_size<-
  data.frame(species=c("Pa","Pt"),
             x=c(0.75,0.75),xend=c(2.25,2.25),
             y=c(6.8,2.5),yend=c(6.8,2.5))

b_ms_4<-
  b_ms_3+
  geom_text(data=text_marking_size_1,
            mapping=aes(x=x,y=y,label=label),
            fontface="bold",size=4)+
  geom_text(data=text_marking_size_2,
            mapping=aes(x=x,y=y,label=label),
            fontface="bold",size=4)+
  geom_segment(data=line_marking_size,
               mapping=aes(x=x,xend=xend,y=y,yend=yend),
               size=0.5,colour="black")

panopeus_pattern<-annotation_custom2(rasterGrob(imagem_panopeus,interpolate=TRUE),
                  xmin=1.75,xmax=2.25,ymin=6.9,ymax=9.6,data=pattern_tibble[54,])
pachygrapsus_pattern<-annotation_custom2(rasterGrob(imagem_pachygrapsus,interpolate=TRUE),
                      xmin=1.75,xmax=2.25,ymin=6.9,ymax=9.6,data=pattern_tibble[1,])
b_ms_5<-
  b_ms_4+
  panopeus_pattern+pachygrapsus_pattern

################################################################################################
## Figure 3 B: boxplot comparing summed energy between life stages within each species
b_se_1<-
  ggplot(pattern_tibble,aes(x=size_category,y=sum_energy,fill=species))+
  facet_grid(~species,
             labeller=labeller(species=species_labs))+
  xlab("Size class")+ylab("Pattern contrast")+
  geom_boxplot(colour="black",width=0.5,linetype=1,lwd=0.5,
               outlier.shape=NA,alpha=0.8)+
  geom_jitter(aes(fill=species),colour="black",shape=21,size=2.5,
              width=0.1,show.legend=FALSE)
b_se_2<-
  b_se_1+
  scale_x_discrete(name="Size class",
                   labels=c("small","large"))+
  scale_y_continuous(name="Pattern contrast", 
                     breaks=c(2,6,10,14,18,22), 
                     limits=c(2,22))+
  scale_fill_manual(values=c("tan","olivedrab"),
                    labels=c("Pa","Pt"))
  
b_se_3<-
  b_se_2+
  theme_bw()+
  theme(plot.margin=margin(0,1,0.8,1,"cm"),
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
  
text_sum_energy<-
  data.frame(label=c("***","ns"),
             species=c("Pa","Pt"),
             x=c(1.5,1.5),y=c(16,10))                
b_se_4<-
  b_se_3+
  geom_text(data=text_sum_energy,
            mapping=aes(x=x,y=y,label=label),
            fontface="bold",size=4)
################################################################################################
## Figure 3 C: boxplot comparing proportion energy between life stages within each species
b_pe_1<-
  ggplot(pattern_tibble,aes(x=size_category,y=prop_energy,fill=species))+
  facet_grid(~species,
             labeller=labeller(species=species_labs))+
  xlab("Size class")+ylab("Pattern diversity")+
  geom_boxplot(colour="black",width=0.5,linetype=1,lwd=0.5,
               outlier.shape=NA,alpha=0.8)+
  geom_jitter(aes(fill=species),colour="black",shape=21,size=2.5,
              width=0.1,show.legend=FALSE)
b_pe_2<-
  b_pe_1+
  scale_x_discrete(name="Size class",
                   labels=c("small","large"))+
  scale_y_continuous(name="Pattern diversity", 
                     breaks=c(0.06,0.075,0.09,0.105,0.12,0.135), 
                     limits=c(0.06,0.135),
                     labels=number_format(accuracy = 0.001))+
  scale_fill_manual(values=c("tan","olivedrab"),
                    labels=c("Pa","Pt"))
  
b_pe_3<-
  b_pe_2+ 
  theme_bw()+
  theme(plot.margin=margin(-0.5,1,-0.1,1,"cm"),
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

text_prop_energy<-
  data.frame(label=c("***","ns"),
             species=c("Pa","Pt"),
             x=c(1.5,1.5),y=c(0.115,0.11))                
b_pe_4<-
  b_pe_3+
  geom_text(data=text_prop_energy,
            mapping=aes(x=x,y=y,label=label),
            fontface="bold",size=4)

### creating a single graph with all comparisons between life stages within 
### each crab species regarding the pattern metrics

pattern_graph<-
  ggarrange(b_ms_5+rremove("x.text")+rremove("x.title"),
            b_se_4+rremove("x.text")+rremove("x.title"),
            b_pe_4,heights=c(1,1,1),labels = c("A","B","C"),
            label.x=c(0.055,0.055,0.055),label.y=c(0.94,1.01,1.07),
            ncol=1,nrow=3,align="v")

print(pattern_graph)