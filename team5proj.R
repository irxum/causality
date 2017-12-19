#
# team5proj.R
#
#
#

if ("stringer" %in% installed.packages()[,"Package"] == FALSE) {
  install.packages("stringer")
}

if ("lubridate" %in% installed.packages()[,"Package"] == FALSE) {
  install.packages("lubridate")
}

if ("tm" %in% installed.packages()[,"Package"] == FALSE) {
  install.packages("tm")
}

if ("slam" %in% installed.packages()[,"Package"] == FALSE) {
  install.packages("slam")
}

if ("stm" %in% installed.packages()[,"Package"] == FALSE) {
  install.packages("stm")
}

if ("lmtest" %in% installed.packages()[,"Package"] == FALSE) {
  install.packages("lmtest")
}

if ("ggplot2" %in% installed.packages()[,"Package"] == FALSE) {
  install.packages("ggplot2")
}

if ("gridExtra" %in% installed.packages()[,"Package"] == FALSE) {
  install.packages("gridExtra")
}

if ("cowplot" %in% installed.packages()[,"Package"] == FALSE) {
  install.packages("cowplot")
}


library(stringr)
library(lubridate)
library(tm)
library(slam)
library(stm)
library(lmtest)
library(ggplot2)
library(gridExtra)
library(cowplot)

# MUST UPDATE wk_dir to working directory
wk_dir <- "C:/Users/irxumtenk/Google Drive/Education/MCS-DS_UIUC/CS 410 Text Information Systems/project/Team5Project"


#  ------------------------------------------------------------------------
# Extracting hotel reviews 
# This section will extract hotel reviews in acceptable
# format for this work
#  ------------------------------------------------------------------------



prac_docs_dir <- paste(wk_dir, "/chicago", sep="")
prac_docs_list <- list.files(prac_docs_dir)
ndocs <- length(prac_docs_list)

docs_list <- list()

for (pr in prac_docs_list) {
  list_doc <- paste(wk_dir, "/chicago/", pr, sep="")
  con <- file(list_doc, "r", blocking = FALSE)
  contents <- readLines(con, n=-1)
  n_docs <- length(contents)
  close(con)
  print(contents)
  print(n_docs)
  docs_list <- append(docs_list, contents)
}

docs_list <- unlist(docs_list)

#  ------------------------------------------------------------------------




#  ------------------------------------------------------------------------
#  This section extracts the date and text of each hotel review entry
#  ------------------------------------------------------------------------


docs_tabpos <- str_locate(docs_list, "\t")
docs_len <- nchar(docs_list)

docs_df <- data.frame(docs_list, docs_tabpos, docs_len)

get_doc_date <- function(entry, tabpos)
{
  return(mdy(substr(entry, 1, tabpos-1)))
}

docs_df$entryDate <- mapply(get_doc_date, docs_df[,1], docs_df[,2])


get_doc_text <- function(entry, tabpos, entry_len)
{
  text_entry <- substr(entry, tabpos, entry_len)
  return(gsub("\t", " ", text_entry))
}

docs_df$entry <- mapply(get_doc_text, docs_df$docs_list, docs_df$start, docs_df$docs_len)

keeps <- c("entryDate", "entry")
docs_df <- docs_df[keeps]
#  ------------------------------------------------------------------------




#  ------------------------------------------------------------------------
#  Apply stm


processed <- textProcessor(docs_df$entry, metadata = docs_df)
out <- prepDocuments(processed$documents, processed$vocab, processed$meta)
docs <- out$documents
vocab <- out$vocab
meta <- out$meta


# Already ran the lines below
# Commenting them out to make code run quicker for 
# searchK_results <- searchK(out$documents, out$vocab, K = c(5,10,20,25), init.type = "Spectral",
#                            N = floor(0.1 * length(out$documents)), proportion = 0.5, 
#                            heldout.seed = NULL, M = 10, cores = 1)
#
#
# > searchK_results
# $results
# K   exclus    semcoh   heldout  residual     bound    lbound em.its
# 1  5 9.873199 -60.65232 -6.931442 17.449101 -10292439 -10292435     15
# 2 10 9.443512 -70.51265 -6.845861 13.807400 -10178506 -10178491    463
# 3 20 9.703956 -79.11089 -6.777204 10.818801 -10078923 -10078880    168
# 4 25 9.749566 -83.96344 -6.757834  9.756144 -10044042 -10043984     48
# 
# $call
# searchK(documents = out$documents, vocab = out$vocab, K = c(5, 
#                                                             10, 20, 25), init.type = "Spectral", N = floor(0.1 * length(out$documents)), 
#         proportion = 0.5, heldout.seed = NULL, M = 10, cores = 1)
# 
# attr(,"class")
# [1] "searchK"
# 
# Result above suggests using K = 25

k_val = 25


modelFit <- stm(documents = docs, vocab = vocab,
                       K = k_val, prevalence = NULL,
                       max.em.its = 75, 
                       init.type = "Spectral")



docs_df_keep <- as.numeric(names(docs)) 
docs_df_ek <- docs_df$entry[docs_df_keep]
docs_df_dt <- docs_df$entryDate[docs_df_keep]


docThoughts <- findThoughts(modelFit, texts = docs_df_ek, n=1000)
docTopicsSL <- sageLabels(modelFit, n = 7)

topic1docs_idx <- docThoughts$index[1]

topic1dates <- docs_df_dt[topic1docs_idx$`Topic 1`]
topic1dates <- na.omit(topic1dates)

# these are the dates of the documents with Topic1
print(as.Date(topic1dates, origin="1970-01-01"))

topic1year <- year(as.Date(topic1dates, origin = "1970-01-01"))

topic1peryear <- as.data.frame(table(topic1year))

docsyear <- year(as.Date(docs_df$entryDate, origin = "1970-01-01"))

docsperyear <- as.data.frame(table(docsyear))


topic1docsyear <- merge(topic1peryear, docsperyear, by.x = "topic1year", by.y ="docsyear", all = FALSE)

topic1docsyear$ratio <- topic1docsyear$Freq.x * 100.0 / topic1docsyear$Freq.y

#  ------------------------------------------------------------------------
#  work with GDP per capita data

chi_gdppc <- read.csv(paste(wk_dir, "/chicago_gdppercapita.csv", sep=""))



#  ------------------------------------------------------------------------
#  perform granger test


chi_gdppc_x <- chi_gdppc[chi_gdppc$year <= 2009 & chi_gdppc$year >= 2003,]

topic1docsyear$gdppc <- chi_gdppc_x$value

grangertest(gdppc~ratio, order = 1, data=topic1docsyear)



#  ------------------------------------------------------------------------
#  plot the data


p1 <- ggplot(topic1docsyear, aes(topic1year, ratio)) + geom_point(color="orange") +
  ggtitle("Percentage of documents that are Topic1") + xlab("Year") + ylab("percent [ % ]") +
  theme(axis.text=element_text(size=10),axis.title=element_text(size=11,face="bold")) +
  theme_gray()

p2 <- ggplot(topic1docsyear, aes(topic1year, gdppc)) + geom_point(color="blue") +
  ggtitle("Chicago Area GDP per capita") + xlab("Year") + ylab("GDP [ $ ]") +
  theme(axis.text=element_text(size=10),axis.title=element_text(size=11,face="bold")) +
  theme_gray()

plot_grid(p1, p2, nrow = 2, align = "v", axis = "tb", label_size = 8, scale = 1)
