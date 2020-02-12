library(dplyr)

setwd("D:\\Github\\complex_networks_research")
getwd()

country.data = read.csv("country-unique.csv", header=TRUE)
full.data = read.csv("full.csv", header=TRUE)
full.data[,"Period.Desc."] = unlist(strsplit(as.character(full.data$Period.Desc.)," "))[seq(2,length(full.data$Period.Desc.)*2,2)]
years = unique(full.data$Period.Desc.)

full.data = full.data %>%
  group_by(Period.Desc., Reporter, Reporter.Code, Partner, Partner.Code, Trade.Flow, Trade.Flow.Code) %>%
  summarise_each(funs(sum),Trade.Value..US..)

write.csv(full.data, "full-annual.csv",row.names = FALSE)

for (year in years) {
  monthly.links = full.data[full.data$Period.Desc. == year, 
                            c("Reporter.Code", 
                              "Partner.Code", 
                              "Trade.Flow.Code", 
                              "Trade.Value..US..")]
  names(monthly.links) = c("from", "to", "flow", "weight")
  monthly.links = monthly.links[complete.cases(monthly.links),]
  if (nrow(monthly.links) == 0) next
  
  #monthly.links = monthly.links[which(monthly.links$from != monthly.links$to),]
  
  for (i in seq(1,length(monthly.links[,1]))) {
    if (as.integer(monthly.links[i,"flow"]) %% 2 == 1) {
      temp = monthly.links[i,"from"]
      monthly.links[i,"from"] = monthly.links[i,"to"]
      monthly.links[i,"to"] = temp
    }
  }
  
  #print(head(monthly.links))
  #monthly.links[,"weight"] = monthly.links$weight/sd(monthly.links$weight)
  monthly.links[,"weight"] = (monthly.links$weight-min(monthly.links$weight))/(max(monthly.links$weight)-min(monthly.links$weight))
  #monthly.links[,"weight"] = monthly.links$weight/max(monthly.links$weight+1)
  #monthly.links = monthly.links[, c("from", "to", "weight")]
  net <- graph_from_data_frame(d=monthly.links, vertices=country.data, directed=T) 
  #deg <- degree(net, mode="all")
  
  V(net)$size <- numeric(length(country.data$id))
  for (j in 1:length(country.data$id)) {
    #size = sum(monthly.links[as.character(monthly.links$from.name) == country.data$text[j] | as.character(monthly.links$to.name) == country.data$text[j],]$weight, na.rm = TRUE)
    size = sum(monthly.links[monthly.links$from == country.data$id[j] | monthly.links$to == country.data$id[j],]$weight, na.rm = TRUE)
    if (is.na(size)) size = 0
    V(net)$size[j] = size
  }
  V(net)$size = V(net)$size
  
  V(net)$label <- country.data$text
  #E(net)$width <- E(net)$weight*10^7
  #E(net)$arrow.size <- .2
  
  net = delete_edges(net, which(E(net)$width == Inf))
  net = delete_vertices(net, which(V(net)$size == 0))
  
  simplify(net, remove.multiple = TRUE, remove.loops = TRUE,
           edge.attr.comb = "mean")
  
  write_graph(net, paste0("annual-graphml\\",paste0(year, ".graphml")), format = c("graphml"))   
}
