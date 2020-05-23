
library(assertthat)
library(sqldf)
library(magrittr)
library(dplyr)
library(reshape2)
library(ggplot2)
library(oz)
library(scatterpie)
library(maptools)
library(svglite)

###### Usage  ######
### create and activate conda mapplot_r and run the script as follows
#conda env create -f mapplot_r.yml
#conda activate mapplot_r
#Rscript script.R ./input_file/test_data.txt ./countries.csv 4 out_file


args = commandArgs(trailingOnly=TRUE)
if (length(args)==0) {
  stop("At least one argument must be supplied ", call.=FALSE)
} else if (length(args)==1) {
  # default output file
  args[1] = "input.txt" #input file (tab separated)
  args[2] = "countries.csv" #country_code file
  args[3] = 6 #number of unique feature or allele 
  args[4] = "out" # output file label
}

#args[1]="./Data/BPSL1661.allele.distribution.more.than.5.strain.txt"
#args[2]="./Data/countries.csv"
#args[3]=6
#args[4]="out"

########## Functions #############
data_prep_allele <- function(csv_name) {
  
  import <- read.table(paste0(csv_name), sep = '\t',header = TRUE)
  names(import) <- tolower(names(import))
  
  import <- import[, c("allele",
                       "country")]
  
  assert_that(length(names(import)) == 2)
  
  return(import)
  
}

groupby_allele <- function(data,allele, country) {
  
  final_data <- data %>%
    group_by(allele, country) %>%
    count(allele) %>%
    ungroup %>%
    as.data.frame()
  
  return(final_data)
}


pivot_by_country <- function(data) {
  
  s1 = melt(data, id = c("country", "allele"), measure.vars = "n")
  s2 = dcast(s1, country ~ allele, sum)
  
  s2$Total = rowSums(s2[,2:NCOL(s2)])
  return(s2)
}

convert_value <- function(x) {

  if(x<=10){return(0.3)}
  else if((10<x) & (x<=100)) {return(0.6)}
  else if(x>100) {return(0.9)}
}

##############################################
##############################################
##############################################

#input file
allele_data <- data_prep_allele(args[1]) 


grouped_data<- groupby_allele(allele_data,"allele", "country")
pivotted_data <- pivot_by_country(grouped_data)
#print(pivotted_data)


# Getting the coordinates of each country
country_lookup <- read.csv(paste0(args[2]), stringsAsFactors = F)
names(country_lookup)[1] <- "country_code"

# Combining data
final_data <- merge(x = pivotted_data, y = country_lookup, by.x = "country", by.y = "name", all.x = T)

# Data cleaning for plotting
final_data <- unique(final_data)
#multiplier <- log10(final_data$Total) / log10(max(final_data$Total))
#multiplier <- (final_data$Total) / (final_data$Total)

#1-100,100-1000,>1000
multiplier <- sapply(final_data$Total, convert_value)

final_data <- cbind(final_data, multiplier)
#print(final_data)

# Using map_data()
worldmap <- map_data ("world")
n_feature=as.integer(args[3])
# The palette with grey:
#cbPalette <- c("#009E73", "#F0E442", "#CC79A7", "#E69F00", "#56B4E9", "#0072B2")

mapplot1 <- ggplot(worldmap) + 
  geom_map(data = worldmap, map = worldmap, aes(x=long, y=lat, map_id=region),col = "gray80",size = 0.1, fill = "gray60") +
  geom_scatterpie(aes(x=longitude, y=latitude, group = country, r = multiplier*5), 
                  data = final_data, cols = colnames(final_data[,c(2:(n_feature+1))]) , color=NA, alpha=0.8) +
  scale_fill_brewer(palette = "Set3") +
  # To use for fills, add
  #scale_fill_manual(values=cbPalette) +
  geom_text(aes(x=longitude, y=latitude, group = country, label = country), data = final_data, stat = "identity",
            position = position_dodge(width = 0.75), hjust = 0, vjust = 0, size = 2, angle = 0, color= "gray20",
            check_overlap = FALSE, na.rm = FALSE, show.legend = FALSE, 
            inherit.aes = TRUE) +
  labs(title = "Allele frequency of BPSL1661", x = "Longitude", y = "Latitude") +
  theme(legend.position = "top")

mapplot1

zoomin=F
if(zoomin == T ){
#zoom in to specific region
mapplot2 <- ggplot(worldmap) + 
  geom_map(data = worldmap, map = worldmap, aes(x=long, y=lat, map_id=region), fill = "gray60") +
  geom_scatterpie(aes(x=longitude, y=latitude, group = country, r = multiplier*5), 
                  data = final_data, cols = colnames(final_data[,c(2:(n_feature+1))]) , color=NA, alpha=1.0) +
  #scale_fill_brewer(palette = "Set3") +
  # To use for fills, add
  scale_fill_manual(values=cbPalette) +
  xlim(-200,60) + ylim(10, 75) +
  geom_text(aes(x=longitude, y=latitude, group = country, label = country), data = final_data, stat = "identity",
            position = position_dodge(width = 0.75), hjust = 0, vjust = 0, size = 2, angle = 0, color= "gray20",
            check_overlap = FALSE, na.rm = FALSE, show.legend = FALSE, 
            inherit.aes = TRUE) +
  labs(title = "Allele frequency of BPSL1661", x = "Longitude", y = "Latitude") +
  theme(legend.position = "top")

mapplot2
}

#write output
write.table(final_data,file=paste0(args[4],'.csv'), sep = ',', row.names=FALSE)

# Save plot
ggsave(filename = paste0(args[4],'.svg'), mapplot1,
       width = 15,
       height = 8.5,
       units = "in",
       device = "svg"
)
