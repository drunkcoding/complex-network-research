library('igraph')
library('ggplot2')
library('reshape2')
library(seriation)
library(stringr)
library(Matrix)
library(gridExtra)

setwd("D:\\Github\\complex_networks_research")
getwd()

filenames = list.files("annual-graphml", pattern="*.graphml", full.names=TRUE)
#list.graph = lapply(filenames, read_graph, format=c("graphml"))
country.data = read.csv("country-unique.csv", header=TRUE)

full.data = read.csv("full-annual.csv", header=TRUE)
country.data = read.csv("country-unique.csv", header=TRUE)

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
  m = strength(sample.graph, mode=c("all"))
  m = m/sd(m)*10
  scale = diag(len) +(m %*% t(m)) #/ length(country.data$id)
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
  #phi = phi[labels %in% keep.names, labels %in% keep.names]
  phi.i = phi.embed(phi)
  
  #labels = rownames(phi)
  
  phi1 = phi.i[,1] / sd(phi.i[,1])
  phi2 = phi.i[,2] / sd(phi.i[,2])
  
  val <- phi1 + phi2
  valcol <- (val + abs(min(val)))/max(val + abs(min(val)))
  
  for (i in 1:length(labels)) {
    if (!labels[i] %in% major.names) labels[i] = ""
  }
  
  pdf(paste0("annual-plot-inset/names/",fname.format(fname, t, "eigen")))
  plot(phi1, phi2, 
       pch = 1, cex=.2, col=gray(valcol, 0.5))
  text(phi1, phi2, labels=labels, cex=.5, adj=1)
  dev.off()
  
  longData = data.frame(Major=phi1,Minor=phi2)
  
  ggplot(longData, aes(x = Major, y = Minor)) + 
    geom_point(colour="grey50") + 
    geom_point(data=longData[labels != "", ], colour="red") + theme_light() +
    geom_text(label=labels,angle=45) + 
    coord_cartesian(xlim=c(-2.2,-1.6), ylim=c(0,.2))
  ggsave(paste0("annual-plot-inset/shrink/",fname.format(fname, t, "eigen")))
  
  
  p1 = ggplot(longData, aes(x = Major, y = Minor)) + 
    geom_point(colour="grey50") + 
    geom_point(data=longData[labels != "", ], colour="red") + 
    theme_light() +
    coord_cartesian(ylim=c(-0.75,1.5))
    #annotation_custom(g,xmin=-2.5,xmax=0,ymin=-10,ymax=-3.75)
  
  p2 = ggplot(longData, aes(x = Major, y = Minor)) + 
    geom_point(colour="grey50") + 
    geom_point(data=longData[labels != "", ], colour="red") + theme_light() +
    coord_cartesian(xlim=c(-2.2,-1.6), ylim=c(0,.2))
  
  pdf(paste0("annual-plot-inset/",fname.format(fname, t, "eigen")))
  grid.arrange(p1, p2, nrow = 2, ncol=1)
  dev.off()
  #ggsave(paste0("annual-plot-inset/",fname.format(fname, t, "eigen")))
}

HM("annual-graphml/2017.graphml", 8)

