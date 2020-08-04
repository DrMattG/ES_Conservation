# Results: 947
# (from Web of Science Core Collection)
# You searched for: TITLE: ("meta-analysis" or "meta analysis") 
# Refined by: WEB OF SCIENCE CATEGORIES: ( BIODIVERSITY CONSERVATION OR ECOLOGY ) 
# Timespan: All years. Indexes: SCI-EXPANDED, SSCI, A&HCI, ESCI. 
library(visNetwork)
library(bibliometrix)
library(igraph)
library(here)
library(tidytext)
library(textmineR)
library(tidyverse)
library(reshape2)
library(wordcloud)

file1 <-paste0(here(),"/Data/meta_analysisCons1.bib") 
file2 <-paste0(here(),"/Data/meta_analysisCons2.bib") 

M <- convert2df(file = c(file1,file2), dbsource = "isi", format = "bibtex")
M
results <- biblioAnalysis(M, sep = ";")
options(width=100)
S <- summary(object = results, k = 10, pause = FALSE)
CR <- citations(M, field = "article", sep = ";")
#cbind(CR$Cited[1:10])
A <- cocMatrix(M, Field = "CR", sep = ";")

Cites<-igraph::graph_from_incidence_matrix(A)

V(Cites)$Year=sub("^.*([0-9]{4}).*", "\\1", V(Cites)$name)
V(Cites)$Journal=sub(".*([0-9]{4})", "", V(Cites)$name)

deg <- igraph::degree(Cites, mode="all")
sort(deg)
#plot(Cites, vertex.label=NA, vertex.size=deg)

Cites = igraph::delete.vertices(Cites,igraph::degree(Cites)<5)
Cites<-simplify(Cites)
Cites<-delete.vertices(simplify(Cites), degree(Cites)==0)

vn <- toVisNetworkData(Cites)
vn$nodes$title<-vn$nodes$label
unique(vn$nodes$Year)
#Repair some year values Early view
vn$nodes$Year[762]="2020"
vn$nodes$Year[801]="2020"
vn$nodes$Year[811]="2020"
vn$nodes$Year[812]="2020"
vn$nodes$Year[817]="2020"
vn$nodes$Year[818]="2020"
vn$nodes$Year[836]="2020"
vn$nodes$Year[842]="2020"
# vn$nodes$Year[9]="2012"
# vn$nodes$Year[10]="2020"
#vn$nodes$Year[11]="2020"
# vn$nodes$Year[12]="2020"
# vn$nodes$Year[19]="2020"
# vn$nodes$Year[20]="2020"
# vn$nodes$Year[41]="2020"
# vn$nodes$Year[659]="2013"
# vn$nodes$Year[699]="2000"
# vn$nodes$Year[13]="2020"
# vn$nodes$Year[14]="2020"
# vn$nodes$Year[16]="2020"
# vn$nodes$Year[17]="2020"
# vn$nodes$Year[52]="2020"
# vn$nodes$Year[53]="2020"

# vn$nodes$Journal<-gsub(", ", "",vn$nodes$Journal)
# vn$nodes$Journal<-trimws(vn$nodes$Journal, which = "both", whitespace = "[ \t\r\n]")
# unique(vn$nodes$Journal)
# 
# vn$nodes$Journal[2]<-"OIKOS"
# vn$nodes$Journal[3]<-"AMBIO"
# vn$nodes$Journal[7]<-"URBAN ECOSYST"
# vn$nodes$Journal[9]<-"CONSERV BIOL"
# vn$nodes$Journal[10]<-"CONSERV BIOL"
# vn$nodes$Journal[11]<-"CONSERV BIOL"
# vn$nodes$Journal[12]<-"CONSERV BIOL"
# vn$nodes$Journal[13]<-"CONSERV BIOL"
# vn$nodes$Journal[14]<-"AMBIO"
# vn$nodes$Journal[16]<-"PRIMATOL"
# vn$nodes$Journal[17]<-"RESTOR ECOL"
# vn$nodes$Journal[53]<-"SOC NAT RESOUR"
# vn$nodes$Journal[52]<-"URBAN AFF"
# 
# 
# # Nodes are sized by degree (the number of links to other packages)
degree_value <- degree(Cites, mode = "all")
vn$nodes$value <- degree_value[match(vn$nodes$id, names(degree_value))]
# vn$nodes$x<-as.numeric(vn$nodes$Year)
# 
# unique(vn$nodes$x)
# 
# 
# vn$nodes$color<-ifelse(vn$nodes$x>=2015, "red",
#                        ifelse(vn$nodes$x>=2010, "green",
#                               ifelse(vn$nodes$x>=2005, "brown",
#                                      ifelse(vn$nodes$x>=2000,"green",
#                                             ifelse(vn$nodes$x>=1995, "yellow", "lightblue")))))
# 

#unique(vn$nodes$Year)
#which(vn$nodes$Year=="1968")

#vn$nodes$color="blue"
#vn$nodes$color[637]<-"red"
vn$edges$arrows<-"from"
visNetwork(nodes = vn$nodes, edges = vn$edges,main="Meta_analysis_conservation",height = "500px", width = "100%")%>%
  #visOptions(highlightNearest = TRUE)%>%
  visOptions(highlightNearest = list(enabled = TRUE, degree = 1)) %>% 
  visNodes() %>%
  visSave(file =paste0(here(),"/plots/meta_cons.html"), selfcontained = T)

arrange(vn$nodes, -vn$nodes$value)
#Find subgraphs
c1 = cluster_fast_greedy(Cites)

# modularity measure
modularity(c1)
coords = layout_with_fr(Cites)
plot(c1, Cites, layout=coords, vertex.label=NA)
sort(membership(c1))
sizes(c1)



vn$nodes$group=membership(c1)
cols<-viridis::magma(11)

vn$nodes$color<-ifelse(vn$nodes$group==1, plotrix::color.id(cols[1]),
                       ifelse(vn$nodes$group==2, plotrix::color.id(cols[2]),
                              ifelse(vn$nodes$group==3, plotrix::color.id(cols[3]),
                                     ifelse(vn$nodes$group==4,plotrix::color.id(cols[4]),
                                            ifelse(vn$nodes$group==5,plotrix::color.id(cols[5]),
                                                   ifelse(vn$nodes$group==6,plotrix::color.id(cols[6]),
                                                          ifelse(vn$nodes$group==7,plotrix::color.id(cols[7]),
                                                                 ifelse(vn$nodes$group==8,plotrix::color.id(cols[8]),
                                                                        ifelse(vn$nodes$group==9,plotrix::color.id(cols[9]),
                                                                               ifelse(vn$nodes$group==10, plotrix::color.id(cols[10]), plotrix::color.id(cols[11]))))))))))) 




vn$nodes$level=round(rank(as.numeric(vn$nodes$Year)))

visNetwork(nodes = vn$nodes, edges = vn$edges,main="Meta_analysis_conservation",height = "500px", width = "100%")%>%
  visOptions(highlightNearest = list(enabled = TRUE, degree = 1),selectedBy = "Year") %>% 
  visNodes() %>%
  visSave(file =paste0(here(),"/plots/meta_cons_grp.html"), selfcontained = T)
# 


min(vn$nodes$Year)
which(vn$nodes$Year=="1871")

vn$nodes[1315,]
vn$nodes[961,]
