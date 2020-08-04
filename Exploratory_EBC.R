#Results: 557
#(from Web of Science Core Collection)
#You searched for: TOPIC: ("evidence-based" "conservation") 
#Refined by: WEB OF SCIENCE CATEGORIES: ( ECOLOGY OR ENVIRONMENTAL SCIENCES OR BIODIVERSITY CONSERVATION OR ENVIRONMENTAL STUDIES OR ZOOLOGY ) 
#Timespan: All years. Indexes: SCI-EXPANDED, SSCI, A&HCI, ESCI. 

library(visNetwork)
library(bibliometrix)
library(igraph)
library(here)
library(tidytext)
library(textmineR)
library(tidyverse)
library(reshape2)
library(wordcloud)

file1 <-paste0(here(),"/Data/EBC/EBC.bib") 
file2 <-paste0(here(),"/Data/EBC/EBC2.bib") 

M <- convert2df(file = c(file1,file2), dbsource = "isi", format = "bibtex")
M
results <- biblioAnalysis(M, sep = ";")
options(width=100)
S <- summary(object = results, k = 10, pause = FALSE)
CR <- citations(M, field = "article", sep = ";")
#cbind(CR$Cited[1:10])
A <- cocMatrix(M, Field = "CR", sep = ";")


#NetMatrix <- biblioNetwork(M, analysis = "co-citation", network = "references", sep = ";")

# Plot the network
#net=networkPlot(NetMatrix, n = 30, Title = "Co-Citation Network", type = "fruchterman", size=T, remove.multiple=FALSE, labelsize=0.7,edgesize = 5)
# Create a historical citation network
#options(width=130)
#histResults <- histNetwork(M, min.citations = 1, sep = ";")
#net <- histPlot(histResults, n=30, size = 10, labelsize=5)

Cites<-igraph::graph_from_incidence_matrix(A)
V(Cites)$name

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
# #Repair some year values Early view
#vn$nodes$Year[1]="2020"
#vn$nodes$Year[2]="2020"
#vn$nodes$Year[3]="2020"
#vn$nodes$Year[4]="2020"
vn$nodes$Year[5]="2020"
vn$nodes$Year[6]="2020"
vn$nodes$Year[7]="2020"
# vn$nodes$Year[9]="2012"
# vn$nodes$Year[10]="2020"
 vn$nodes$Year[11]="2020"
 vn$nodes$Year[12]="2020"
 vn$nodes$Year[19]="2020"
 vn$nodes$Year[20]="2020"
 vn$nodes$Year[41]="2020"
 vn$nodes$Year[659]="2013"
 vn$nodes$Year[699]="2000"
 # vn$nodes$Year[13]="2020"
# vn$nodes$Year[14]="2020"
# vn$nodes$Year[16]="2020"
# vn$nodes$Year[17]="2020"
# vn$nodes$Year[52]="2020"
# vn$nodes$Year[53]="2020"

 vn$nodes$Journal<-gsub(", ", "",vn$nodes$Journal)
 vn$nodes$Journal<-trimws(vn$nodes$Journal, which = "both", whitespace = "[ \t\r\n]")
 unique(vn$nodes$Journal)
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
which(vn$nodes$Year=="1968")

vn$nodes$color="blue"
vn$nodes$color[637]<-"red"
vn$edges$arrows<-"to"
visNetwork(nodes = vn$nodes, edges = vn$edges,main="EBC",height = "500px", width = "100%")%>%
  #visOptions(highlightNearest = TRUE)%>%
  visOptions(highlightNearest = list(enabled = TRUE, degree = 1)) %>% 
  visNodes() %>%
  visSave(file =paste0(here(),"/plots/EBC.html"), selfcontained = T)


#Find subgraphs
c1 = cluster_fast_greedy(Cites)

# modularity measure
modularity(c1)
coords = layout_with_fr(Cites)
plot(c1, Cites, layout=coords, )
membership(c1)
sizes(c1)



vn$nodes$group=membership(c1)

cols<-viridis::magma(11)
plotrix::color.id(cols[3])
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




 visNetwork(nodes = vn$nodes, edges = vn$edges,main="EBC",height = "500px", width = "100%")%>%
 visOptions(highlightNearest = TRUE)%>%
   visOptions(highlightNearest = list(enabled = TRUE, degree = 1)) %>% 
   visNodes() %>%
   visSave(file =paste0(here(),"/plots/EBC_col.html"), selfcontained = T)
# 
# 
# unique(vn$nodes$Journal)
# 
# vn$nodes$color<-ifelse(vn$nodes$Journal=="CONSERV BIOL", "red","blue")
# 
# visNetwork(nodes = vn$nodes, edges = vn$edges,main="Coexist",height = "500px", width = "100%")%>%
#   #visOptions(highlightNearest = TRUE)%>%
#   visOptions(highlightNearest = list(enabled = TRUE, degree = 2)) %>% 
#   visNodes() %>%
#   visSave(file =paste0(here(),"/plots/Coexist_conb.html"), selfcontained = T)


###############################################################################################################


# 
# 
# # extract the abstracts of main papers
# 
# data=data.frame("Title"=as.character(M$TI),"Abstract"= as.character(M$AB), stringsAsFactors = FALSE)
# text_df <- mutate(data, text = data$Abstract)
# text_df<-text_df %>% 
#   rowid_to_column()
# text_cleaning_tokens<-text_df %>% 
#   unnest_tokens(word, text)
# text_cleaning_tokens$word <- gsub('[[:digit:]]+', '', text_cleaning_tokens$word)
# text_cleaning_tokens$word <- gsub('[[:punct:]]+', '', text_cleaning_tokens$word)
# text_cleaning_tokens <- text_cleaning_tokens %>% filter(!(nchar(word) == 1))%>% 
#   anti_join(stop_words)
# tokens <- text_cleaning_tokens %>% filter(!(word==""))
# tokens <- tokens %>% mutate(ind = row_number())
# tokens <- tokens %>% group_by(Title) %>% mutate(ind = row_number()) %>%
#   tidyr::spread(key = ind, value = word)
# tokens [is.na(tokens)] <- ""
# tokens <- tidyr::unite(tokens, text,-Title,sep =" " )
# tokens$text <- trimws(tokens$text)
# 
# 
# #create DTM
# dtm <- CreateDtm(tokens$text, 
#                  doc_names = tokens$Title, 
#                  ngram_window = c(1, 2))
# #explore the basic frequency
# tf <- TermDocFreq(dtm = dtm)
# original_tf <- tf %>% select(term, term_freq,doc_freq)
# rownames(original_tf) <- 1:nrow(original_tf)
# # Eliminate words appearing less than 2 times or in more than half of the
# # documents
# vocabulary <- tf$term[ tf$term_freq > 1 & tf$doc_freq < nrow(dtm) / 2 ]
# dtm = dtm
# 
# k_list <- seq(1, 40, by = 1)
# model_dir <- paste0("models_", digest::digest(vocabulary, algo = "sha1"))
# if (!dir.exists(model_dir)) dir.create(model_dir)
# model_list <- TmParallelApply(X = k_list, FUN = function(k){
#   filename = file.path(model_dir, paste0(k, "_topics.rda"))
#   
#   if (!file.exists(filename)) {
#     m <- FitLdaModel(dtm = dtm, k = k, iterations = 500)
#     m$k <- k
#     m$coherence <- CalcProbCoherence(phi = m$phi, dtm = dtm, M = 5)
#     save(m, file = filename)
#   } else {
#     load(filename)
#   }
#   
#   m
# }, export=c("dtm", "model_dir")) # export only needed for Windows machines
# #model tuning
# #choosing the best model
# coherence_mat <- data.frame(k = sapply(model_list, function(x) nrow(x$phi)), 
#                             coherence = sapply(model_list, function(x) mean(x$coherence)), 
#                             stringsAsFactors = FALSE)
# ggplot(coherence_mat, aes(x = k, y = coherence)) +
#   geom_point() +
#   geom_line(group = 1)+
#   ggtitle("Best Topic by Coherence Score") + theme_minimal() +
#   scale_x_continuous(breaks = seq(1,40,1)) + ylab("Coherence")
# 
# model <- model_list[which.max(coherence_mat$coherence)][[ 1 ]]
# model$top_terms <- GetTopTerms(phi = model$phi, M = 40)
# top20_wide <- as.data.frame(model$top_terms)
# 
# allterms <-data.frame(t(model$phi))
# 
# allterms$word <- rownames(allterms)
# 
# rownames(allterms) <- 1:nrow(allterms)
# 
# allterms <- melt(allterms,idvars = "word") 
# 
# allterms <- allterms %>% rename(topic = variable)
# 
# FINAL_allterms <- allterms %>% group_by(topic) %>% arrange(desc(value))
# 
# 
# 
# model$topic_linguistic_dist <- CalcHellingerDist(model$phi)
# model$hclust <- hclust(as.dist(model$topic_linguistic_dist), "ward.D")
# model$hclust$labels <- paste(model$hclust$labels, model$labels[ , 1])
# plot(model$hclust)
# 
# final_summary_words <- data.frame(top_terms = t(model$top_terms))
# final_summary_words$topic <- rownames(final_summary_words)
# rownames(final_summary_words) <- 1:nrow(final_summary_words)
# final_summary_words <- final_summary_words %>% melt(id.vars = c("topic"))
# final_summary_words <- final_summary_words %>% rename(word = value) %>% select(-variable)
# final_summary_words <- left_join(final_summary_words,allterms)
# final_summary_words <- final_summary_words %>% group_by(topic,word) %>%
#   arrange(desc(value))
# final_summary_words <- final_summary_words %>% group_by(topic, word) %>% filter(row_number() == 1) %>% 
#   ungroup() %>% tidyr::separate(topic, into =c("t","topic")) %>% select(-t)
# word_topic_freq <- left_join(final_summary_words, original_tf, by = c("word" = "term"))
# 
# for(i in 1:length(unique(final_summary_words$topic)))
# {  wordcloud(words = subset(final_summary_words ,topic == i)$word, freq = subset(final_summary_words ,topic == i)$value, min.freq = 1,
#              max.words=200, random.order=FALSE, rot.per=0.35, 
#              colors=brewer.pal(8, "Dark2"))}
# 
# dev.off()
# 
# d<-dtm
# p <- as.data.frame(predict(object = model, newdata = d, method = "dot"))
# names(p)
# 
# p[, "max"] <- apply(p, 1, max)

file <-paste0(here(),"/Data/EBC/Cite100.bib") 
M <- convert2df(file = file, dbsource = "isi", format = "bibtex")
M
results <- biblioAnalysis(M, sep = ";")
options(width=100)
S <- summary(object = results, k = 10, pause = FALSE)
CR <- citations(M, field = "article", sep = ";")
#cbind(CR$Cited[1:10])
A <- cocMatrix(M, Field = "CR", sep = ";")
Cites<-igraph::graph_from_incidence_matrix(A)
V(Cites)$name

V(Cites)$Year=sub("^.*([0-9]{4}).*", "\\1", V(Cites)$name)
V(Cites)$Journal=sub(".*([0-9]{4})", "", V(Cites)$name)

deg <- igraph::degree(Cites, mode="all")
sort(deg)
#plot(Cites, vertex.label=NA, vertex.size=deg)
vn <- toVisNetworkData(Cites)
vn$nodes$title<-vn$nodes$label
# unique(vn$nodes$Year)
# # #Repair some year values Early view
# #vn$nodes$Year[1]="2020"
# #vn$nodes$Year[2]="2020"
# #vn$nodes$Year[3]="2020"
# #vn$nodes$Year[4]="2020"
# vn$nodes$Year[5]="2020"
# vn$nodes$Year[6]="2020"
# vn$nodes$Year[7]="2020"
# # vn$nodes$Year[9]="2012"
# # vn$nodes$Year[10]="2020"
# vn$nodes$Year[11]="2020"
# vn$nodes$Year[12]="2020"
# vn$nodes$Year[19]="2020"
# vn$nodes$Year[20]="2020"
# vn$nodes$Year[41]="2020"
# vn$nodes$Year[659]="2013"
# vn$nodes$Year[699]="2000"
# # vn$nodes$Year[13]="2020"
# # vn$nodes$Year[14]="2020"
# # vn$nodes$Year[16]="2020"
# # vn$nodes$Year[17]="2020"
# # vn$nodes$Year[52]="2020"
# # vn$nodes$Year[53]="2020"
# 
# vn$nodes$Journal<-gsub(", ", "",vn$nodes$Journal)
# vn$nodes$Journal<-trimws(vn$nodes$Journal, which = "both", whitespace = "[ \t\r\n]")
# unique(vn$nodes$Journal)
# # 
# # vn$nodes$Journal[2]<-"OIKOS"
# # vn$nodes$Journal[3]<-"AMBIO"
# # vn$nodes$Journal[7]<-"URBAN ECOSYST"
# # vn$nodes$Journal[9]<-"CONSERV BIOL"
# # vn$nodes$Journal[10]<-"CONSERV BIOL"
# # vn$nodes$Journal[11]<-"CONSERV BIOL"
# # vn$nodes$Journal[12]<-"CONSERV BIOL"
# # vn$nodes$Journal[13]<-"CONSERV BIOL"
# # vn$nodes$Journal[14]<-"AMBIO"
# # vn$nodes$Journal[16]<-"PRIMATOL"
# # vn$nodes$Journal[17]<-"RESTOR ECOL"
# # vn$nodes$Journal[53]<-"SOC NAT RESOUR"
# # vn$nodes$Journal[52]<-"URBAN AFF"
# # 
# # 
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
vn$edges$arrows<-"to"
visNetwork(nodes = vn$nodes, edges = vn$edges,main="EBC cited 100+",height = "500px", width = "100%")%>%
  #visOptions(highlightNearest = TRUE)%>%
  visOptions(highlightNearest = list(enabled = TRUE, degree = 1)) %>% 
  visNodes() %>%
  visSave(file =paste0(here(),"/plots/EBC_cited100.html"), selfcontained = T)





