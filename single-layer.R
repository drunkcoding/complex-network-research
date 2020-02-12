library(igraph)

setwd("D:\\Github\\complex_networks_research")
getwd()


swap = function(fname) {
  data = read.csv(fname, header=TRUE)
  # for (i in 1:nrow(data)) {
  #   if (as.integer(data[i,"Trade.Flow.Code"]) %% 2 == 1) {
  #     temp = data[i,"Reporter.Code"]
  #     data[i,"Reporter.Code"] = data[i,"Partner.Code"]
  #     data[i,"Partner.Code"] = temp
  #   }
  # }
  temp.data.in = data[data$Trade.Flow.Code %% 2 == 1,]
  temp.data.out = data[data$Trade.Flow.Code %% 2 == 0,]
  temp.vec = temp.data.in[,"Reporter.Code"]
  temp.data.in[,"Reporter.Code"] = temp.data.in$Partner.Code
  temp.data.in[,"Partner.Code"] = temp.vec
  data = rbind(temp.data.in,temp.data.out)

  data = data[grepl("12|13|14|15|16|17", data$Period.Desc.),]
  #data = data[grepl("15|16|17", data$Period.Desc.),]
  data = data[!data$Reporter %in% c("EU-28", "World") & !data$Partner %in% c("EU-28", "World"),]
  data[, "Period.Factor"] = as.integer(factor(data$Period.Desc., levels = unique(as.character(data$Period.Desc.))))
  data = data[, c("Period.Factor", "Reporter.Code", "Partner.Code")]
  data[, "capacity"] = rep(1, nrow(data))
  colnames(data) = c("time", "from", "to", "capacity")
  data
}

write.csv(swap("full-annual.csv"), "full-swap.csv", row.names = F)
#write.csv(swap("full-oil.csv"), "full-oil-swap.csv", row.names = F)
#write.csv(swap("full-gas.csv"), "full-gas-swap.csv", row.names = F)




country.data = read.csv("country-unique.csv", header=TRUE)

# full = read.csv("full-annual.csv", header=TRUE)
# total.trade = numeric(length(country.data$id))
# names(total.trade) = country.data$text
# for (i in 1:nrow(full)) {
#   total.trade[names(total.trade)==full[i,"Reporter"]] = total.trade[names(total.trade)==full[i,"Reporter"]] + full[i,"Trade.Value..US.."]
#   total.trade[names(total.trade)==full[i,"Partner"]] = total.trade[names(total.trade)==full[i,"Partner"]] + full[i,"Trade.Value..US.."]
# }
# total.trade = sort(total.trade, TRUE)
# major.names = names(total.trade[total.trade>10^8])


major.names = unique(c("Russian Federation", "Japan", "Saudi Arabia", "China",
                      "India", "Canada", "Germany", "Netherlands", "United Kingdom",
                      "Kazakhstan", "Norway", "USA", "Italy", "Rep. of Korea", "Nigeria", "Algeria",
                      "Spain", "Qatar", "France", "Belgium", "Mexico", "Brazil", "Indonesia", "Australia",
                      "Malaysia"))
major.code = as.integer(country.data[country.data$text %in% major.names,]$text)
  
read.csv.format = function(fname) {
  data = read.csv(fname, header=TRUE)
  split(data[,c("from", "to", "capacity")], data$time)
}

add.timestamp = function(df, lag) {
  df[, "from"] = paste(as.character(df$from), as.character(df$time),sep="-")
  df[, "to"] = paste(as.character(df$to), as.character(df$time+lag),sep="-")
  df
}

#full.data = read.csv.format("full-swap.csv")[as.character(1:12)]
full.data = read.csv("full-swap.csv", header=TRUE)
full.data = add.timestamp(full.data,1)

num.month = length(unique(full.data$time))
num.ctrl = length(major.code)

ctrl.data = data.frame(
                time=rep(1:num.month, each=num.ctrl),
                from=rep("u", times=num.month*num.ctrl),
                to=rep(major.code, times=num.month),
                capacity=rep(1, times=num.month*num.ctrl)
                )
ctrl.data[, "from"] = paste(as.character(ctrl.data$from), as.character(ctrl.data$to),sep="-")
ctrl.data[, "from"] = paste(as.character(ctrl.data$from), as.character(ctrl.data$time),sep="-")
ctrl.data[, "to"] = paste(as.character(ctrl.data$to), as.character(ctrl.data$time+1),sep="-")

start.data = data.frame(
                from=rep("start", nrow(ctrl.data)+length(major.code)),
                to=c(ctrl.data$from, paste(as.character(major.code), as.character(rep(1,length(major.code))),sep="-")),
                capacity=rep(1, nrow(ctrl.data)+length(major.code))
                )

end.data = data.frame(
  from=unique(full.data$to[grepl("-6",full.data$to)])
)

end.data[,"to"] = rep("end", length(end.data$from))
end.data[,"capacity"] = rep(1, length(end.data$from))

graph.data = rbind(full.data, ctrl.data)[,c("from","to", "capacity")]
graph.data = rbind(graph.data, start.data)
graph.data = rbind(graph.data, end.data)
graph.data[, "weight"] = graph.data$capacity
g = graph_from_data_frame(graph.data)

print(length(end.data$from))
print(max_flow(g,"start","end")$value)
#d = distances(g, mode="out")
#d = d[grepl("u",rownames(d)), !grepl("u",colnames(d))]
#all_simple_paths(g, "u-2", "12-2")
#full.data.oil = read.csv.format("full-oil.csv")
#full.data.gas = read.csv.format("full-gas.csv")
#colnames(full.data)




