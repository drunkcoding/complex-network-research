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
  #print(nearPD(diag(len) +(m %*% t(m)))$mat-diag(len) +(m %*% t(m)))
  scale = nearPD(diag(len) +(m %*% t(m)))$mat #/ length(country.data$id)
  e = expm(-Ls*t)
  result = e %*% scale %*% t(e)
  #result = e %*% t(e)
  result
}

phi.format = function(phi) {
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
  r = eigen(phi)
  V = r$vectors
  lam = abs(r$values)
  t(t(V)*sqrt(lam))
}

fname.format = function(fname, t, type) {
  fname = str_replace(fname, " ", "-")
  fname = strsplit(fname, "/")[[1]][2]
  fname = strsplit(fname, ".", fix=TRUE)[[1]][1]
  paste0(paste(paste(fname,type,sep="-"),as.character(t),sep="-"),".pdf")
}

HM = function(fname, t) {
  phi = phi.format(as.matrix(Phi(fname,t)))
  labels = rownames(phi)
  
  phi.sd = phi/sd(phi)
  phi.sd = phi.sd[labels %in% major.names, labels %in% major.names]
  
  phi.vec = sort(as.vector(phi.sd), T)
  phi.vec = phi.vec[!(phi.vec %in% diag(phi.sd))]
  
  phi.vec.top = phi.vec[1:length(phi.vec)%/%10*2]

  pdf(paste0("annual-density/",fname.format(fname, t, "major")))
  plot(density(phi.vec),
       xlab = "phi", ylab = "density", main = "Value Density of Phi")
  dev.off()
  
  pdf(paste0("annual-density/",fname.format(fname, t, "majortop")))
  plot(density(phi.vec.top),
       xlab = "phi", ylab = "density", main = "Value Density of Phi")
  dev.off()
  
  print(fname)
  print(skewness(phi.vec))
  print(kurtosis(phi.vec))
  print(skewness(phi.vec.top))
  print(kurtosis(phi.vec.top))
  
}

process.img = function(fname) {
  HM(fname, 8)
}

lapply(filenames, process.img)

