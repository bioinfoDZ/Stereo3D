StereoPLot3D <- function(data, rot2_data,colors_vec, data_Color,connection_df)
{
    #library(rgl)

    distance=0

    open3d()

    par3d(windowRect = c(100, 100, 612, 612))
    Sys.sleep(0.1) # Allow sluggish window managers to catch up
    parent <- currentSubscene3d()


    layout3d(matrix(c(1,2,3,3), ncol=2, nrow=2, byrow=TRUE), heights=c(4, 1),sharedMouse = TRUE)

    #mfrow3d(2,2, sharedMouse = TRUE)
    plot3d(x=data$X, y=data$Y,z=data$Z, col = data_Color, xlab = 'X', ylab = 'Y', zlab='Z', margin=c(3,3,0,1+distance))
    if(!missing(connection_df))
    {
      for(i in 1:nrow(connection_df))
      {
        f=connection_df$first[i]
        s=connection_df$second[i]
        segments3d(data$X[c(f,s)],data$Y[c(f,s)],data$Z[c(f,s)],col='black',lwd=1)
      }
    }
    next3d()


    plot3d(x=rot2_data$X, y=rot2_data$Y,z=rot2_data$Z, col = data_Color, xlab = 'X', ylab = 'Y', zlab='Z', margin=c(3,1+distance,0,3))
    if(!missing(connection_df))
    {
      for(i in 1:nrow(connection_df))
      {
        f=connection_df$first[i]
        s=connection_df$second[i]
        segments3d(rot2_data$X[c(f,s)],rot2_data$Y[c(f,s)],rot2_data$Z[c(f,s)],col='black',lwd=1)
      }

    }



    #library(plotrix)
    colors=sapply(colors_vec, function(x) color.id(x)[1])

    next3d()
    #legend3d("center", c("2D Points", "3D Points"), pch = c(1, 16))

    legend3d("top", legend=levels(factor(rot2_data$Color)), col=colors,horiz = TRUE, cex=1, inset=c(0.02), bty = "n", pch=16,  xpd = NA, x.intersp=.5, y.intersp=0)
    useSubscene3d(parent)



}
