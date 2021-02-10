# IST 719 Final Project
# OSHA Injuries
# Kathryn Egan
#################################################################################


## Text Analysis
library(tm)
library(wordcloud)
library(RColorBrewer)
library(stringr)
library(maps)
library(plotrix)
library(ggmap)
library(raster)
library(broom)
library(rgeos)
library(rgdal)
library(dplyr)
library(ggplot2)
library(viridis)
library(ggpubr)


OSHA<-read.csv("C:/Users/katva/Desktop/severeinjury.csv"
                    , header = TRUE
                    , stringsAsFactors =FALSE)

View(OSHA)

###Clean the data
 # The below gsub() codes for fixing company names is from Kaggle, I only changed out df names. 
OSHA$Employer =gsub(".*usps|us postal|united states postal|u.s. postal|u.s postal|u. s postal|u. s. postal.*","US_Postal_Service", 
                  ignore.case = TRUE, OSHA$Employer)
OSHA$Employer =gsub(".*US_Postal_Service.*","USPS", ignore.case = TRUE, OSHA$Employer)
OSHA$Employer =gsub(".*united parcel|ups |ups,.*","United_Parcel_Service", ignore.case = TRUE, OSHA$Employer)
OSHA$Employer =gsub(".*United_Parcel_Service.*","UPS", ignore.case = TRUE, OSHA$Employer)
OSHA$Employer =gsub(".*american airl.*","American Airlines", ignore.case = TRUE, OSHA$Employer)
OSHA$Employer =gsub(".*AT &|AT&.*","AT_T", ignore.case = TRUE, OSHA$Employer)
OSHA$Employer =gsub(".*AT_T.*","AT&T Inc", ignore.case = TRUE, OSHA$Employer)
OSHA$Employer =gsub(".*walmart|wallmart|wal-mart.*","wal_mart", ignore.case = TRUE, OSHA$Employer)
OSHA$Employer =gsub(".*wal_mart.*","Walmart", ignore.case = TRUE, OSHA$Employer)
OSHA$Employer =gsub(".*Publix.*","Publix_", ignore.case = TRUE, OSHA$Employer)
OSHA$Employer =gsub(".*Publix_.*","Publix", ignore.case = TRUE, OSHA$Employer)
OSHA$Employer =gsub(".*Asplundh.*","Asplundh_", ignore.case = TRUE, OSHA$Employer)
OSHA$Employer =gsub(".*Asplundh_.*","Asplundh", ignore.case = TRUE, OSHA$Employer)
OSHA$Employer =gsub(".*sodexo.*","sodexo_", ignore.case = TRUE, OSHA$Employer)
OSHA$Employer =gsub(".*sodexo_.*","Sodexo", ignore.case = TRUE, OSHA$Employer)
OSHA$Employer =gsub(".*Waste Management.*","Waste_Management", ignore.case = TRUE, OSHA$Employer)
OSHA$Employer =gsub(".*Waste_Management.*","Waste Management", ignore.case = TRUE, OSHA$Employer)
OSHA$Employer =gsub(".*Tyson Foods.*","Tyson_Foods", ignore.case = TRUE, OSHA$Employer)
OSHA$Employer =gsub(".*Tyson_Foods.*","Tyson Foods", ignore.case = TRUE, OSHA$Employer)

## My additions to the above code
OSHA$Employer =gsub(".*84 LUMBER COMPANY, A LIMITED PARTNERSHIP.*", "84_Lumber", ignore.case = T, OSHA$Employer)
OSHA$Employer =gsub(".*84_Lumber.*", "84 Lumber", ignore.case = T, OSHA$Employer)
OSHA$Employer =gsub(".*ABC Supply Co. Inc.| ABC Supply Co, Inc.| ABC Supply Co., Inc.*", "ABC_Supply_Co.",ignore.case = T, OSHA$Employer)
OSHA$Employer =gsub(".*ABC_Supply_Co..*", "ABC Supply Co.", ignore.case = T, OSHA$Employer)

OSHA$State<-tolower(OSHA$State)

OSHA$BodyParts<-OSHA$Part.of.Body.Title

summary(OSHA)
### General Visualizations

# setting my color palette 

greens_col<- c("#d3f2a3","#97e196","#6cc08b","#4c9b82","#217a79","#105965","#074050")
my_greens<- colorRampPalette(more_col)

bl_cols <- brewer.pal(5, "Blues")
blues_ramp<-colorRampPalette(c("midnightblue","#64899B","#C0B8B6"))

safety_ramp <- colorRampPalette(c("#DD3739","#201E11")) #red to black

col.num<- 10
pie(rep(1,col.num), col = blues_ramp(10))        

#Color Lovers Palette Grey ="#C0B8B6",blue = "#64899B",Yellow ="#F3CB14", Black = "#201E11", Red = "#DD3739"


## This data set is so massive, that I will create a new data frame that only contains
## data about cases where amputation was involved and focus on that subset

ampsDF<- data.frame(OSHA[OSHA$Amputation=="1", ])
str(ampsDF)


## Amputations by company
x<-table(ampsDF$Employer)
dfAmp<- as.data.frame(x)
names(dfAmp)<-c("Company", "Num")
dfAmp$rank <- rank(-dfAmp$Num,ties.method="min")
dfAmp <- dfAmp[order(dfAmp$rank,decreasing = F),]
dfAmp<-dfAmp[1:10,] ## Only looking at the top 10

dfAmp$percent<- dfAmp$Num / sum(dfAmp$Num) *100
dfAmp <- transform(dfAmp ,Company = reorder(Company, order(percent, decreasing = TRUE)))

# Basic pie chart 
ggplot(dfAmp, aes(x="", y= percent, fill= Company)) +
  geom_bar(stat="identity", width=1, color="grey") +
  coord_polar("y", start=0) +
  theme_void() +
  ggtitle("Top 10 Companies with Amputations Reported to OSHA \n 2015-2017")+
  scale_fill_manual(values = blues_ramp(10))
    
#############################################################################3
 
### By source Only
sourceTab<- table(ampsDF$SourceTitle)
dfSource<- as.data.frame(sourceTab)
names(dfSource)<-c("Source", "Num")
dfSource$rank <- rank(-dfSource$Num,ties.method="min")
dfSource <- dfSource[order(dfSource$rank,decreasing = F),]
dfSource<- dfSource[1:10,]
str(dfSource)

dfSource <- transform(dfSource,Source = reorder(Source, order(Num, decreasing = TRUE)))

x<-ggplot(dfSource, aes(x=Source, y=Num))+
  geom_bar(stat = "identity", fill = safety_ramp(10))  +
  coord_flip() + ggtitle("Top 10 Sources of Amputation Reported to OSHA \n 2015-2017")+
  theme(
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "transparent",colour = NA),
    plot.background = element_rect(fill = "transparent",colour = NA)
  )
x

######################################################################################

# by body part

Body.tab<-table(ampsDF$BodyParts)
dfBody <- data.frame(Body.tab)
names(dfBody) <- c("BodyPart", "Freq")
View(dfBody)
dfBody$rank <- rank(-dfBody$Freq,ties.method="min")
dfBody <- dfBody[order(dfBody$Freq),]

dfBody$prop<-dfBody$Freq/sum(dfBody$Freq)*100
plot(dfBody$BodyPart,dfBody$prop)

library(treemapify)

# treemap 

ggplot(dfBody, 
       aes(fill = BodyPart, 
           area = prop)) +
  geom_treemap() + 
  labs(title = "Frequency of Amputation by Body Part")
### There are just too many variables for aesthetic pleasure, so I placed each item in a category and added the
### frequencies together for each category

dfB<- data.frame(part = c("Finger/FingerNail","Lower Extremity","Upper Extremity","Toe/ToeNail","Nonclassifiable", "MultipleParts",  "Hip", "Ear", "Nose", "Jaw/Chin")
                , count = c(scale(5330, 184, 160, 112, 52, 12, 2, 2, 1, 1)))

dfB$prop<- dfB$count / sum(dfB$count)*100

str(dfB)

ggplot(dfB, 
       aes(fill = part, 
           area = count)) +
  geom_treemap() + 
  labs(title = "Frequency of Amputation by Body Part")+
  scale_fill_manual(values = my_ramp(10))

### Something about the treemap doesn't work visually 
## so I am going to visualize again but in 
### Bubble Chart


ggplot(dfB, aes(part,prop, size = prop))+
  geom_point(color = rev(my_ramp(10)))+
  scale_size(range = c(1, 15))+
  labs(title = "Frequency of Amputation by Body Part")
  







##########################################################################
### Heat map of amputations by latitude & longitude

View(ampsDF)


map("state")
my.cols<-rep(rgb(.86, .22, .22), length(ampsDF$State))
points(ampsDF$Longitude, ampsDF$Latitude, col = my.cols
       , pch = 16
       , cex =.5)
#### Love this one, but its only single dimensional
library(plotrix)
agg.data$index <- round(rescale(x=agg.data$Amputations, c(1,col.num)), 0)
agg.data
# now need to add the colors to the data 
agg.data$col<-my.col.vec[agg.data$index]


m<-map("state")
m$names

state.order<- match.map(database = "state"
                        , regions = agg.data$State
                        , exact = F
                        , warn = T)
# now combine columns from "state" and "agg.data" so they line up
cbind(m$names, agg.data$state[state.order])

map("state", col = agg.data$col[state.order]
    , fill = TRUE , resolution = 0
    , lty = 1 , projection = "polyconic"
    , border = "black")


#########################################################################

########## Word cloud from written reports of injuries


text<- data.frame(ampsDF$Final.Narrative)

text <- text$Final.Narrative[which(text$Final.Narrative != "")] # remove all blank lines in the text
str(text) # look at it to make sure it is there
text[1:3]
## Step 1.2: Create a term matrix of text
words.vec<- VectorSource(text)
words.corpus<- Corpus(words.vec)
words.corpus <- tm_map(words.corpus, content_transformer(tolower))
words.corpus <- tm_map(words.corpus, removePunctuation)
words.corpus<- tm_map(words.corpus, removeNumbers)
words.corpus <- tm_map(words.corpus, removeWords, stopwords("english"))
tdm <- TermDocumentMatrix(words.corpus)
tdm
textMatrix <- as.matrix(tdm)
textMatrix[1:10,]
wordCounts <- rowSums(textMatrix)
wordCounts[1:10]
totalWords <- sum(wordCounts)
totalWords
words <- names(wordCounts)
head(words)
wordCounts <- sort(wordCounts, decreasing=TRUE)
head(wordCounts)
## Step 1.3: Create a Cloud Frame
cloudFrame<-data.frame(word=names(wordCounts),freq=wordCounts)
cloudFrame[1:10,]


myPal<- colorRampPalette(rev(c("hotpink4","midnightblue", "green4")))

par(mar=c(0,0,0,0))

set.seed(1234) # for reproducibility 
wordcloud(words = cloudFrame$word, freq = cloudFrame$freq, min.freq = 1, 
          max.words=150, random.order=FALSE, rot.per=0.5,            
          colors=myPal(length(cloudFrame$freq)))
##############################################################################


#### CODE THAT MAY NOT BE USED ######################
eventTab<- table(ampsDF$EventTitle)
dfEventTab<- as.data.frame(eventTab)
names(dfEventTab)<-c("Event", "Num")
dfEventTab$rank <- rank(-dfEventTab$Num,ties.method="min")
dfEventTab <- dfEventTab[order(dfEventTab$rank,decreasing = F),]
dfEventTab<- dfEventTab[1:10,]
str(dfEventTab)
dfEventTab <- transform(dfEventTab ,Source = reorder(Event, order(Num, decreasing = TRUE)))


x<-ggplot(dfEventTab, aes(x=Event, y=Num))+
  geom_bar(stat = "identity", fill = safety_ramp(10))  +
  coord_flip() + ggtitle("Top 10 Events Leading to Amputation Reported to OSHA \n 2015-2017")+
  theme(
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "transparent",colour = NA),
    plot.background = element_rect(fill = "transparent",colour = NA)
  )
x



merge(dfEventTab, dfSource, by.x="surname", by.y="name")



event<- aggregate(ampsDF$Amputation, list(ampsDF$EventTitle, ampsDF$SourceTitle), sum)
colnames(event)
names(event)<-c("Event", "Source", "Freq")
event$rank <- rank(-event$Freq,ties.method="min")
event <- event[order(event$rank), decreasing = FALSE]
event<- event[1:5,]
event <- transform(event ,Source = reorder(Source, order(freq, decreasing = TRUE)))
ggballoonplot(event, fill = "Freq"
              ,font.label = list(size = 1, color = "black")
              ,size.range = c(5, 10),
              shape = 21)+
              scale_fill_gradient(low = "grey", high = "red")


##### By Intended Violence

selfInflicted<- c( 112, 1120, 1121, 1122, 1123, 1124, 1125, 1126, 1129)

y<-data.frame(filter(ampsDF, Event %in% selfInflicted))

View(y) ## There aren't any intentional self-inflicted injuries in the data

Violence<-c(1, 10, 11, 110, 111, 1110, 1111, 1112, 1113, 1114, 1115, 1116, 1117, 1118, 1119)

IntDF<-data.frame(filter(ampsDF, Event %in% Violence))

View(IntDF) ## there are 5 instances of amputation due to violence

## Look at Source Title and Body Part amputated & company name

IntTab<- table(IntDF$Employer,IntDF$SourceTitle,IntDF$BodyParts)
IntTabDF<- data.frame(IntTab)
names(IntTabDF)<- c("Employer", "Source", "BodyPart", "Freq")
View(IntTabDF)

IntTabDF[IntTabDF$Freq > 0] <- IntTabDF

ggplot(data = IntTabDF
       , aes(x=Employer, y=BodyPart, color = Source)) + 
  geom_point()

#####################################################################









