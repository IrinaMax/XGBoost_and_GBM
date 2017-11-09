#https://stackoverflow.com/questions/13570792/scatterplot3d-for-response-surface-in-r

newdat <- expand.grid(fbc=seq(0,150,by=10),
                      cycling=seq(0,3000,by=100))
newdat$pp <- predict(nst_lmod_un,ewdata=newdat)
#Plot points and add surface:
  
  library(rgl)
with(df,plot3d(cycling,fbc,tem))
with(newdat,surface3d(unique(fbc),unique(cycling),pp,
                      alpha=0.3,front="line"))
rgl.snapshot("3d.png")