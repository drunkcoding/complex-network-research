
setwd("D:\\Github\\complex_networks_research")
getwd()

#oil.data = read.csv("CrudeOil-Monthly.csv", header=TRUE)
#gas.data = read.csv("NaturalGas-Monthly.csv", header=TRUE)
country.data = read.csv("country-unique.csv", header=TRUE)

filenames = list.files("un-comtrade", pattern="*.csv", full.names=TRUE)
ldf = lapply(filenames, read.csv)
df = do.call("rbind", ldf)
#names(gas.data)

#full.data = rbind(oil.data, gas.data)
#full.data = rbind(full.data, df)
full.data = df

kickout.names = c("Br. Virgin Isds", 
                  "Nepal", 
                  "Tuvalu", 
                  "Solomon Isds", 
                  "EU-28", 
                  "Timor-Leste", 
                  "Trinidad and Tobago", 
                  "Zimbabwe",
                  "Cura??ao",
                  "R??union",
                  "Free Zones",
                  "Special Categories")

full.data = full.data[((full.data$Reporter.Code %in% country.data$id)
                      | (full.data$Partner.Code %in% country.data$id))
                      & !is.na(full.data$Trade.Value..US..)
                      ,]
full.data = full.data[,c("Period.Desc.", 
                         "Trade.Flow.Code", 
                         "Trade.Flow", 
                         "Reporter.Code", 
                         "Reporter", 
                         "Partner.Code", 
                         "Partner", 
                         "Commodity.Code",
                         "Commodity",
                         "Trade.Value..US..")]

write.csv(full.data, "full.csv")

library('igraph')

#nodes = data.frame(id=c(4,8,12,16,20,24,660,10,28,32,51,533,36,40,31,44,48,50,52,112,56,84,204,60,64,68,535,70,72,74,76,86,96,100,854,108,116,120,124,132,136,140,148,152,156,162,166,170,174,178,180,184,188,191,192,531,196,203,384,208,262,212,214,218,818,222,226,232,233,231,238,234,242,246,250,254,258,260,266,270,268,276,288,292,300,304,308,312,316,320,831,324,624,328,332,334,336,340,344,348,352,356,360,364,368,372,833,376,380,388,392,832,400,398,404,296,408,410,414,417,418,428,422,426,430,434,438,440,442,446,807,450,454,458,462,466,470,584,474,478,480,175,484,583,498,492,496,499,500,504,508,104,516,520,524,528,540,554,558,562,566,570,574,580,578,512,586,585,275,591,598,600,604,608,612,616,620,630,634,642,643,646,638,652,654,659,662,663,666,670,882,674,678,682,686,688,690,694,702,534,703,705,90,706,710,239,728,724,144,729,740,744,748,752,756,760,158,762,834,764,626,768,772,776,780,788,792,795,796,798,800,804,784,826,840,581,858,860,548,862,704,92,850,876,732,887,894,716))
#nodes = data.frame(id=seq(1,899))

for (month in unique(full.data$Period.Desc.)) {
#month = "March 2015"
  monthly.links = full.data[full.data$Period.Desc. == month, 
                            c("Reporter.Code", 
                              "Partner.Code", 
                              "Trade.Flow.Code", 
                              "Trade.Value..US..")]
  #monthly.links = monthly.data[, ]
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
  monthly.links[,"weight"] = monthly.links$weight/10^9
  #monthly.links[,"weight"] = monthly.links$weight/max(monthly.links$weight+1)
  #monthly.links = monthly.links[, c("from", "to", "weight")]
  net <- graph_from_data_frame(d=monthly.links, vertices=country.data, directed=T) 
  deg <- degree(net, mode="all")
  
  V(net)$size <- numeric(length(country.data$id))
  for (j in 1:length(country.data$id)) {
    #size = sum(monthly.links[as.character(monthly.links$from.name) == country.data$text[j] | as.character(monthly.links$to.name) == country.data$text[j],]$weight, na.rm = TRUE)
    size = sum(monthly.links[monthly.links$from == country.data$id[j] | monthly.links$to == country.data$id[j],]$weight, na.rm = TRUE)
    if (is.na(size)) size = 0
    V(net)$size[j] = size
  }
  #V(net)$size = V(net)$size*10^8
  
  V(net)$label <- country.data$text
  #E(net)$width <- E(net)$weight*10^7
  #E(net)$arrow.size <- .2
  
  net = delete_edges(net, which(E(net)$width == Inf))
  net = delete_vertices(net, which(V(net)$size == 0))
  
  simplify(net, remove.multiple = TRUE, remove.loops = TRUE,
           edge.attr.comb = "mean")
  
  write_graph(net, paste0("graphml\\",paste0(month, ".graphml")), format = c("graphml"))   
}

#plot(net)

