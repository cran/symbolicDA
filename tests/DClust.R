require(symbolicDA)
data("cars",package="symbolicDA")
sdt<-cars
dist<-dist_SDA(sdt, type="U_3")
clust<-DClust(dist, cl=5, iter=100)
print(clust)
