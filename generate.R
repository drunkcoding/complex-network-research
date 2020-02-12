library('igraph')
library('ggplot2')
library('reshape2')
library(seriation)
library(stringr)
library(Matrix)
library(moments)

setwd("D:\\Github\\complex_networks_research")
getwd()

filenames = list.files("annual-graphml", pattern="*.graphml", full.names=TRUE)
#list.graph = lapply(filenames, read_graph, format=c("graphml"))
country.data = read.csv("country-unique.csv", header=TRUE)

full.data = read.csv("full-annual.csv", header=TRUE)
country.data = read.csv("country-unique.csv", header=TRUE)

total.trade = numeric(length(country.data$id))

names(total.trade) = country.data$text

for (i in 1:nrow(full.data)) {
  total.trade[names(total.trade)==full.data[i,"Reporter"]] = total.trade[names(total.trade)==full.data[i,"Reporter"]] + full.data[i,"Trade.Value..US.."]
  total.trade[names(total.trade)==full.data[i,"Partner"]] = total.trade[names(total.trade)==full.data[i,"Partner"]] + full.data[i,"Trade.Value..US.."]
}

total.trade = sort(total.trade, TRUE)
keep.names = names(total.trade[total.trade>10^8])
keep.names.10 = names(sort(total.trade)[1:12])
keep.names.20 = names(sort(total.trade)[1:22])
keep.names.30 = names(sort(total.trade)[1:32])
keep.names.40 = names(sort(total.trade)[1:42])
keep.names.50 = names(sort(total.trade)[1:52])
keep.names = c(keep.names, "USA")
#keep.names = keep.names[!keep.names %in% c("Nigeria")]
print(keep.names)

major.names = keep.names[1:52]
major.names = major.names[major.names != "Nigeria"]
major.names = c(major.names, "USA")

major.names = unique(c("World", "EU-28", "Russian Federation", "Japan", "Saudi Arabia", "China", 
                "India", "Canada", "Germany", "Netherlands", "United Kingdom",
                "Kazakhstan", "Norway", "USA", "Italy", "Rep. of Korea", "Nigeria", "Algeria", 
                "Spain", "Qatar", "France", "Belgium", "Mexico", "Brazil", "Indonesia", "Australia",
                "Malaysia"))

Phi = function(fname, t) {
  sample.graph = read_graph(fname, format=c("graphml"))
  Ls = laplacian_matrix(sample.graph,sparse = FALSE,norm=TRUE)
  Ls[is.na(Ls)] = 0
  total.deg = degree(sample.graph,mode=c("total"))
  len = length(total.deg)
  m = strength(sample.graph, mode=c("out")) - strength(sample.graph, mode=c("in"))
  #m = strength(sample.graph, mode=c("all"))
  m = m/sd(m)*10
  #print(nearPD(diag(len) +(m %*% t(m)))$mat-diag(len) +(m %*% t(m)))
  scale = nearPD(diag(len) +(m %*% t(m)))$mat #/ length(country.data$id)
  e = expm(-Ls*t)
  result = e %*% scale %*% t(e)
  #result = e %*% t(e)
  result
}

phi.format = function(phi) {
  #print("phi.format")
  names = as.integer(colnames(phi))
  new.names = character(length(names))
  for (i in seq(1,length(names))) {
    new.names[i] = as.character(country.data$text[which(country.data$id == names[i])])
  }
  colnames(phi) = new.names
  rownames(phi) = new.names
  phi
}

phi.embed = function(phi) {
  #print("phi.embed")
  r = eigen(phi)
  V = r$vectors
  lam = abs(r$values)
  t(t(V)*sqrt(lam))
}

fname.format = function(fname, t, type) {
  #print("fname.format")
  fname = str_replace(fname, " ", "-")
  fname = strsplit(fname, "/")[[1]][2]
  fname = strsplit(fname, ".", fix=TRUE)[[1]][1]
  paste0(paste(paste(fname,type,sep="-"),as.character(t),sep="-"),".pdf")
}

HM = function(fname, t) {
  phi = phi.format(as.matrix(Phi(fname,t)))
  labels = rownames(phi)
  #print(length(labels))
  #phi = phi[labels %in% keep.names, labels %in% keep.names]
  phi.i = phi.embed(phi)
  
  #phi.sd = (phi-mean(phi))/sd(phi)
  phi.sd = phi/sd(phi)
  phi.sd.major = phi.sd[labels %in% major.names, labels %in% major.names]
  
  phi.vec = sort(as.vector(phi.sd), T)
  #print(length(phi.vec))
  phi.vec = phi.vec[!(phi.vec %in% diag(phi.sd))]
  #print(length(phi.vec))
  pdf(paste0("annual-density/",fname.format(fname, t, "power")))
  plot(density(phi.vec[1:length(phi.vec)%/%10*2]),
       xlab = "phi", ylab = "density", main = "Value Density of Phi")
  dev.off()
  
  print(fname)
  print(mean(as.vector(phi.sd.major) %in% phi.vec[1:length(phi.vec)%/%10*2]))
  #involve.names.row = character()
  #involve.names.col = character()
  #cnt = 0
  #for (x in phi.vec[1:length(phi.vec)%/%10*2]) {
  #  m = which(phi.sd == x, arr.ind = TRUE)
  #  involve.names.row = c(involve.names.row, labels[m[1,1]])
  #  involve.names.col = c(involve.names.col, labels[m[1,2]])
  #}
  #print(mean(involve.names.row %in% major.names | involve.names.col %in% major.names))
  #print(cnt/(length(involve.names.row)))
  #print(unique(data.frame(row=involve.names.row,col=involve.names.col)))
  
  print(skewness(phi.vec[phi.vec>0.5]))
  print(kurtosis(phi.vec[phi.vec>0.5]))
  
  #labels = rownames(phi)
  pdf(paste0("annual-density/",fname.format(fname, t, "phi")))
  plot(density(phi.vec),
       xlab = "phi", ylab = "density", main = "Value Density of Phi")
  dev.off()
  
  print(skewness(phi.vec))
  print(kurtosis(phi.vec))
  
  phi1 = phi.i[,1] / sd(phi.i[,1])
  phi2 = phi.i[,2] / sd(phi.i[,2])
  
  val <- phi1 + phi2
  valcol <- (val + abs(min(val)))/max(val + abs(min(val)))
  
  #seleted.phi1 = phi1[labels %in% major.names]
  #seleted.phi2 = phi2[labels %in% major.names]
  
  for (i in 1:length(labels)) {
    if (!labels[i] %in% major.names) labels[i] = ""
  }
  
  pdf(paste0("annual-plot/",fname.format(fname, t, "eigen")))
  plot(phi1, phi2, 
       pch = 1, cex=.2, col=gray(valcol, 0.5))
  text(phi1, phi2, labels=labels, cex=.5, adj=1)
  dev.off()
  
  longData = data.frame(Major=phi1,Minor=phi2)
  ggplot(longData, aes(x = Major, y = Minor)) + 
    geom_point(colour="grey50") + 
    geom_point(data=longData[labels != "", ], colour="red") + theme_light()
  ggsave(paste0("annual-plot-dotonly/",fname.format(fname, t, "eigen")))
  
  #set.seed(2)
  #longData<-melt(phi)
  #o <- seriate(phi, method="BEA_TSP")
  
  #with the same longData then earlier
  #longData$Var1 <- factor(longData$Var1, levels=names(unlist(o[[1]][]))) 
  #longData$Var2 <- factor(longData$Var2, levels=names(unlist(o[[2]][])))
  #levels must be names
  
  
  #ggplot(longData, aes(x = Var2, y = Var1)) + 
  #  geom_raster(aes(fill=value)) + 
  #  scale_fill_gradient(low="blue", high="red") +
  #  labs(x="letters", y="LETTERS", title="Matrix") +
  #  theme_bw() + 
  #  theme(text = element_text(size=2),axis.text.x = element_text(angle=90, hjust=1))
  #ggsave(paste0("annual-plot/",fname.format(fname, t, "sim")))
}

process.img = function(fname) {
  #for (i in c(0.01,0.025,0.05,0.075,0.1,0.25,0.5,0.75,1,2,4,8,16,32,64,128,256,512)) {
    HM(fname, 8)
  #}
}

lapply(filenames, process.img)

