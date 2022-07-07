spectra <- read.table("spectra.txt", header = T)
first <- as.vector(as.matrix(spectra[1,]))
library(fda)
data <- t(spectra)
abscissa <- 1:80

m <- 4           # spline order 
degree <- m-1    # spline degree 

nbasis <- 4:75
gcv <- numeric(length(nbasis))
for (i in 1:length(nbasis)){
  basis <- create.bspline.basis(c(1,80), nbasis[i], m)
  gcv[i] <- smooth.basis(abscissa, first, basis)$gcv
}

x11()
par(mfrow=c(1,1))
plot(nbasis,gcv)

optim.n <- nbasis[which.min(gcv)]

basis <- create.bspline.basis(c(1,80), optim.n, m)
smooth.first <- smooth.basis(abscissa, first, basis)

smooth.first$fd$coefs[1:3]

data.fd <- Data2fd(y = data, argvals = abscissa,basisobj = basis)
x11()
layout(cbind(1,2))
matplot(data, type = "l", main = "Emprical", xlab = "frequencies")
plot.fd(data.fd, xlab = "frequencies", main = "smoothed")

x <- seq(1, 80, length = 500)
y <- eval.fd(x, data.fd)
library(fdakma)
set.seed(1)
fdakma <- ?kma(
  x=x, y0=t(y), n.clust = 3, 
  warping.method = 'affine', 
  similarity.method = 'd0.pearson',   # similarity computed as the cosine
  # between the original curves 
  # (correlation)
  center.method = 'k-means'
  #,seeds = c(1,11,21) # you can give a little help to the algorithm...
)

kma.show.results(fdakma)
#by looking at the warping functions one could say that between the clusters there is not much variability in terms of amplutude (little to no pendency in the w.f.)
#the only difference is a translation withouth particolar patterns
#indeed one can show
kma.comp <- kma.compare (
  x=x, y0=t(y), n.clust = 2:3, 
  warping.method = c('affine','shift'), 
  similarity.method = 'd0.pearson',
  center.method = 'k-means',
  plot.graph=1)
#the performance of applying an affine transformation and just a shift is the same
