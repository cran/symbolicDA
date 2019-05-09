plot3dInterval<-function(data,colors){
  d1<-as.matrix(data[,,1])
  d2<-as.matrix(data[,,2])
  open3d(mouseMode="trackball")
  ax<-max(data[,1,])-min(data[,1,])
  ay<-max(data[,2,])-min(data[,2,])
  az<-max(data[,3,])-min(data[,3,])
  if(ax!=0 && ay!=0 && az!=0){
    aspect3d(1/ax,1/ay,1/az)
  }
  for(i in 1:dim(data)[1]){
    t<-cube3d()
    wire3d(translate3d(scale3d(t,abs(d2[i,1]-d1[i,1])/2,abs(d2[i,2]-d1[i,2])/2,abs(d2[i,3]-d1[i,3])/2),(d2[i,1]+d1[i,1])/2,(d2[i,2]+d1[i,2])/2,(d2[i,3]+d1[i,3])/2),col=colors[i])
  }
  axes3d(edges = "bbox", labels = TRUE, tick = TRUE, nticks = 5)
  TRUE
}
