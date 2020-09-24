library(tidyverse)
library(readxl)
library(MASS)

data_Pa<-
  read_excel("data_R_Duarte_2020.xlsx",
              sheet=4,range="A1:K62")
data_Pa<-
  data_Pa%>%
  arrange(desc(size_class))

# calculating the min, max, mean size and sd for all Panopeus
data_Pa%>%
  group_by(species)%>%
  summarise(min_Pa=min(size),
            max_Pa=max(size),
            mean_Pa=mean(size),
            sd_Pa=sd(size))

data_Pa%>%
  filter(size_class=="s") # 32 small crabs

data_Pa%>%
  filter(size_class=="l") # 29 large crabs

set.seed(123)
lda_Pa<-lda(size_class~brightness+saturation+hue_2+
            max_freq+sum_energy+prop_energy,data=data_Pa)

#classifying data

lda_Pa_predict<-predict(lda_Pa)
predict_Pa_overall<-lda_Pa_predict$class
predict_Pa_small<-factor(predict_Pa_overall[1:32],levels=c("s","l"))
predict_Pa_large<-factor(predict_Pa_overall[33:61],levels=c("s","l")) 

# leave-one-out cross-validation
## calculating the percentage of correct classification based on colour
## and pattern metrics

## OVERALL
table_Pa_overall<-table(data_Pa$size_class,predict_Pa_overall)
sum(table_Pa_overall[row(table_Pa_overall)==
                     col(table_Pa_overall)])/sum(table_Pa_overall)
#81.97% of correct classification

## SMALL CRABS
table_Pa_small<-table(data_Pa$size_class[1:32],predict_Pa_small)
sum(table_Pa_small[row(table_Pa_small)==
                       col(table_Pa_small)])/sum(table_Pa_small)
#75% of correct classification

## LARGE CRABS
table_Pa_large<-table(data_Pa$size_class[33:61],predict_Pa_large)
sum(table_Pa_large[row(table_Pa_large)!=
                   col(table_Pa_large)])/sum(table_Pa_large)
#89.65% of correct classification

##############################################################################################
## Performing the same analysis to mottled crabs 
data_Pt<-
  read_excel("data_R_Duarte_2020.xlsx",
             sheet=5,range="A1:K54")
data_Pt<-
  data_Pt%>%
  arrange(desc(size_class))

# calculating the min, max, mean size and sd for all Pachygrapsus
data_Pt%>%
  group_by(species)%>%
  summarise(min_Pt=min(size),
            max_Pt=max(size),
            mean_Pt=mean(size),
            sd_Pt=sd(size))%>%
  as.data.frame()

data_Pt%>%
  filter(size_class=="s") # 33 small crabs

data_Pt%>%
  filter(size_class=="l") # 20 adults

set.seed(123)
lda_Pt<-lda(size_class~brightness+saturation+hue_2+
            max_freq+sum_energy+prop_energy,data=data_Pt)

#classifying data

lda_Pt_predict<-predict(lda_Pt)
predict_Pt_overall<-lda_Pt_predict$class
predict_Pt_small<-factor(predict_Pt_overall[1:33],levels=c("s","l"))
predict_Pt_large<-factor(predict_Pt_overall[34:53],levels=c("s","l")) 

# leave-one-out cross-validation
## calculating the percentage of correct classification based on colour
## and pattern metrics
## OVERALL
table_Pt_overall<-table(data_Pt$size_class,predict_Pt_overall)
sum(table_Pt_overall[row(table_Pt_overall)==
                        col(table_Pt_overall)])/sum(table_Pt_overall)
#75.47% of correct classification

## SMALL CRABS
table_Pt_small<-table(data_Pt$size_class[1:33],predict_Pt_small)
sum(table_Pt_small[row(table_Pt_small)==
                   col(table_Pt_small)])/sum(table_Pt_small)
#90.90% of correct classification

## LARGE CRABS
table_Pt_large<-table(data_Pt$size_class[34:53],predict_Pt_large)
sum(table_Pt_large[row(table_Pt_large)!=
                   col(table_Pt_large)])/sum(table_Pt_large)
#50% of correct classification