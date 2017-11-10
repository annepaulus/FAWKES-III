############################################################################
### Purpose of this skript module 05 is to:
### 
### 05.1. Text mining Natura2000 plans
### 05.2. ...
###
### General comments:
### Eventually it would be nice to have some way to automatically extract data from
### the PDF versions of the management plans. This can be done using the xPDF suite of
### packages, but is more straightforward in Windows (which I successfully made work) 
### than Mac (which I did not). Much of this comes from an excellent guide to PDF text 
### extraction by Clay Ford at Virginia:
### http://data.library.virginia.edu/reading-pdf-files-into-r-for-text-mining/
###
### Authors: CH ...
############################################################################

############################################################################
### 05.1. Text mining Natura2000 plans
### Currently focused on English language plans - perhaps we can expand to
### other countries later.
############################################################################

# Weblinks to PDFs can be found from the MANAGEMENT CSV file in the main dataset

# set wd to path2temp where files can be downloaded
setwd(path2temp %+% "/") 

# Load MANAGEMENT CSV into workspace
management<-read.csv("MANAGEMENT.csv")

# Create character vectors for site ID and PDF link
management$SITECODE<-as.character(management$ï..SITECODE)
management$MANAG_PLAN_URL<-as.character(management$MANAG_PLAN_URL)

CompletePlans<-subset(management, management$MANAG_PLAN_URL!="NULL")
UniquePlans<-unique(CompletePlans[,c(1,6)])
PDFPlans<-UniquePlans[grep(".pdf",UniquePlans[,2]),]
PDFPlans$Country<-substr(PDFPlans[,1],1,2)

UKManPlans<-subset(PDFPlans,PDFPlans$Country=="UK")

# Download PDFs from web into wd (currently just the first three)
for(x in 1:116){download.file(UKManPlans$MANAG_PLAN_URL[x], paste(UKManPlans$SITECODE[x],".pdf",sep=""), mode="wb")}

# Retrieve a list of PDFs from that folder
files <- list.files(pattern = "pdf$")

# These functions extract text from the PDFs (which can be provided as a list) into a
# "corpus" object. Corpus objects require special functions to read them, but a few
# of these can be seen below. Current code is restricted to three PDFs as an example
# but can be extended to all 127
Rpdf <- readPDF(control = list(text = "-layout"))
natura <- Corpus(URISource(files[c(1:116)]), 
                   readerControl = list(reader = Rpdf))

# The corpus object can then be "crunched" to find the frequency of words, after some
# pre-processing of the text. In this example, punctuation, stop words, case, word
# endings (leaving just "stems", hence "stemming") and numbers are removed. The result
# is a list of terms with their frequency in the text, which is known as a "term document
# matrix" (TDM).
natura.tdm <- TermDocumentMatrix(natura, control = list(removePunctuation = TRUE,
                                                            stopwords = TRUE,
                                                            tolower = TRUE,
                                                            stemming = TRUE,
                                                            removeNumbers = TRUE))

# The TDM can be inspected to view subsets of the table. Note that there are some terms
# which should be excluded, as they are garbled OCR coding (e.g. â€“).
inspect(natura.tdm[1:10,]) 

# It can also be searched to find the most frequent terms, by specifying a range of
# frequencies by which to subset the TDM
findFreqTerms(natura.tdm, lowfreq = 100, highfreq = Inf)

# You can then find the frequency of those terms by using the frequent terms object
# "ft" to subset the TDM
ft <- findFreqTerms(natura.tdm, lowfreq = 750, highfreq = Inf)
inspect(natura.tdm[ft,]) 

# It is then possible to find the total frequency across a range of documents:
ft.tdm <- inspect(natura.tdm[ft,])
apply(ft.tdm, 1, sum)

# Making use of another great tutorial online: 
# https://www.r-bloggers.com/text-mining-the-complete-works-of-william-shakespeare/
# We can do some more interesting analysis such  as removing the least common terms
TDM.common = removeSparseTerms(natura.tdm, 0.8)
inspect(TDM.common)

# We can also find associations among words in the text
findAssocs(natura.tdm, "water", 0.8) # note "Himalayan" and "balsam" are both common

# Visualise the output using a matrix plot to compare texts
TDM.dense <- as.matrix(natura.tdm[ft,])
par(mar=c(5, 4, 8, 2) + 0.1)
image(t(log(TDM.dense)),axes=FALSE)
axis(2,seq(0,1,by=1/(length(rownames(TDM.dense))-1)),lab=rownames(TDM.dense),las=1)
axis(3,seq(0,1,by=1/(length(files)-1)),lab=files,las=2)
