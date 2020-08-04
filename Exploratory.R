library(igraph)
library(bibliometrix)  
library(tidyverse)
library(here)
# # 
# You searched for: TOPIC: (("Evidence synthesis"  OR "Evidence-synthesis")) 
# Refined by: WEB OF SCIENCE CATEGORIES: ( BIODIVERSITY CONSERVATION OR ECOLOGY ) 
# Timespan: All years. Indexes: SCI-EXPANDED, SSCI, A&HCI, ESCI. 

#files <- c(paste0(here(),"/Data/ConsEcolSearch.bib") , paste0(here(),"/Data/ConsEcolSearch2.bib"), paste0(here(),"/Data/ConsEcolSearch3.bib"), paste0(here(),"/Data/ConsEcolSearch4.bib"))

file<-c(paste0(here(),"/Data/EBC.bib"))
M <- convert2df(file = file, dbsource = "wos", format = "bibtex")
results <- biblioAnalysis(M, sep = ";")
S <- summary(object = results, k = 10, pause = FALSE)
options(width=130)
histResults <- histNetwork(M, min.citations = 1, sep = ";")
head(histResults)
net <- histPlot(histResults, n=67, size = 5, labelsize=5)
# Create a co-citation network

NetMatrix <- biblioNetwork(M, analysis = "co-citation", network = "references", sep = ";")

# Plot the network
net=networkPlot(NetMatrix, n = 50, Title = "Co-Citation Network", type = "fruchterman", size=T, remove.multiple=FALSE, labelsize=0.7,edgesize = 5)
# Create keyword co-occurrences network

NetMatrix <- biblioNetwork(M, analysis = "co-occurrences", network = "keywords", sep = ";")

# Plot the network
net=networkPlot(NetMatrix, normalize="association", weighted=T, n = 30, Title = "Keyword Co-occurrences", type = "fruchterman", size=T,edgesize = 5,labelsize=0.7)



A <- cocMatrix(M, Field = "CR", sep = ";")
A

NetMatrix <- biblioNetwork(M, analysis = "coupling", network = "references", sep = ";")
net=networkPlot(NetMatrix, n=100)




histNetwork(M, min.citations = 0, sep = ";", network = TRUE, verbose = TRUE)

net <- histPlot(histResults, n=50, size = 10, labelsize=5)

bibliometrix::localCitations(M)

######################################################################3
library(bib2df)

df <- bib2df(file)
df$CITED.REFERENCES
df$NUMBER.OF.CITED.REFERENCES





####################################################################

ES <- read_delim("Data/ES.csv", ";", escape_double = FALSE, 
                      trim_ws = TRUE)
ES_2<-ES %>% 
  select(AU,CR) %>% 
  mutate(CR=strsplit(CR,";")) %>% 
  unnest(CR) %>% 
  group_by(AU) %>% 
  mutate(row_num=row_number()) %>% 
  spread(row_num,CR)
positions<-c(2:30)
unique_strings <- ES_2 %>% select(2:30,) %>%  flatten_chr() %>% unique()

unique_strings
