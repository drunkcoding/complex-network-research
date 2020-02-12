setwd("D:\\Github\\complex_networks_research")
getwd()

full.data = read.csv("full.csv", header=TRUE)
full.data.imports = full.data[full.data$Trade.Flow == "Imports",]
full.data.imports.oil = full.data[full.data$Trade.Flow == "Imports"& 
                                    full.data$Commodity.Code %in% c(2709,270900),]
full.data.imports.gas = full.data[full.data$Trade.Flow == "Imports"& 
                                    !full.data$Commodity.Code %in% c(2709,270900),]
full.data.exports = full.data[full.data$Trade.Flow == "Exports",]
full.data.exports.oil = full.data[full.data$Trade.Flow == "Exports" & 
                                    full.data$Commodity.Code %in% c(2709,270900),]
full.data.exports.gas = full.data[full.data$Trade.Flow == "Exports" & 
                                    !full.data$Commodity.Code %in% c(2709,270900),]
country.data = read.csv("country-unique.csv", header=TRUE)

total.sum = function(data) {
  total.trade = numeric(length(country.data$id))
  names(total.trade) = country.data$text
  for (i in 1:nrow(data)) {
    total.trade[names(total.trade)==data[i,"Reporter"]] = total.trade[names(total.trade)==data[i,"Reporter"]] + data[i,"Trade.Value..US.."]
    total.trade[names(total.trade)==data[i,"Partner"]] = total.trade[names(total.trade)==data[i,"Partner"]] + data[i,"Trade.Value..US.."]
  }
  sort(total.trade, TRUE)
}

names(total.sum(full.data)[1:12])
names(total.sum(full.data.imports)[1:12])
names(total.sum(full.data.imports.oil)[1:12])
names(total.sum(full.data.imports.gas)[1:12])
names(total.sum(full.data.exports)[1:12])
names(total.sum(full.data.exports.oil)[1:12])
names(total.sum(full.data.exports.gas)[1:12])

