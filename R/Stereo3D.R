#' @title Create Stereo3D image of given data
#'
#' @description Stereo3D: Using stereo images to enrich 3D visualization
#'
#' @usage Stereo3D(data_file, stereo_angle, distance, connection_file )
#' @param data_file A tab seperated file with \emph{.tsv} extension and having five columns \emph{(index, X, Y, Z and Color)} of the data. Where, \emph{X, Y and Z} represent cordinates of a datapoint, \emph{Color} is the label of the given data point and \emph{index} clolumn have the index information of the datapoints.
#' @param stereo_angle angle by which 3D data to be rotated along Y-axis. \emph{Default:} 5 degree.
#' @param distance Gap between stereo image and original image.
#' @param connection_file A tab seperated file (optional) . Where, \emph{first} and \emph{second} column has indices of start and end point of a connection respectively.
#' @keywords Stereoscopic 3D plot, stereo 3D plot
#' @return Two Stereo3D images
#' \enumerate{
#'  \item  Create Stereoscopic 3D plot with input data filename prefix and \strong{_Stereo.pdf} extention.
#'  \item  Interactive 3D plot of the above image, which can be zoomed and rotated by draging the mouse.
#'  }
#' @export
#' @examples
#' connection_fileName=system.file("extdata", "connection_file.tsv", package = "Stereo3D", mustWork = TRUE)
#' sample_data_file=system.file("extdata", "sample_3D_data.tsv", package = "Stereo3D", mustWork = TRUE)
#' Stereo3D(data_file=sample_data_file, stereo_angle=5, distance=0, connection_file=connection_fileName)


Stereo3D <- function(data_file, stereo_angle=5, distance=0, connection_file=connection_fileName){

  if(data_file != 'empty' & file_ext(data_file)=='tsv' & file.exists(data_file)){
    data=read.table(file=data_file, header = TRUE)
  } else if (data_file != 'empty' & file_ext(data_file)!='tsv'){
    stop("File don't have '.tsv' extension" , call. = TRUE, domain = NULL)
    geterrmessage()
  }else if (data_file != 'empty' & !file.exists(data_file)){
    stop("tab seperated data file path does not exist", call. = TRUE, domain = NULL)
    geterrmessage()
  } else if(data_file=='empty') {
    stop(" tab seperated data file path is not given", call. = TRUE, domain = NULL)
    geterrmessage()
  }
  #data=read.table(file='Mesp1_WT_SubCells.tsv', header = TRUE)

  data_name=sub('\\.tsv$', '',  basename(data_file))
  mat_data=as.matrix(cbind(data[,c(1,2,3)], const=rep(1, nrow(data))))

  angle=stereo_angle
  gam=(pi/180)*angle  # for 15 degree
  # rotation along y (counter-clock wise direction)
  rot_mat=matrix(data =c(cos(gam),0, sin(gam),0,0,1,0,0, -sin(gam),0, cos(gam),0,0,0,0,1) , nrow = 4, ncol=4)
  rot_data=t( rot_mat %*% t(mat_data ))

  colnames(rot_data)=c('X','Y','Z','const')
  rot_data=data.frame(rot_data)
  rot_data$Color=data$Color
  rot2_data=rot_data[,-4]


  data$Color=factor(data$Color)
  colors_vec=rainbow(length(levels(data$Color)))

  data_Color=mapvalues(x=data$Color, from = levels(data$Color), to = colors_vec)

  pdf(paste0(data_name,'_Stereo.pdf'))

    p <-layout(matrix(c(1,2,3,3), ncol=2, nrow=2, byrow=TRUE), heights=c(4, 1))

    if(missing(connection_file))
    {
      scatterplot3d(x=data$X, y=data$Z,z=data$Y, pch=16,color = data_Color, xlab='X', ylab='', zlab='Y', box=FALSE, grid=FALSE, mar=c(3,3,0,1+distance)) #(B-,L,T-,R)
      scatterplot3d(x=rot2_data$X, y=rot2_data$Z,z=rot2_data$Y,pch=16,color = data_Color, xlab='X', ylab='', zlab='Y', box=FALSE, grid=FALSE,  mar=c(3,1+distance,0,3))
    }
    else {
      connection_df=read.table(file = connection_file, header = T)
      sd3_1=scatterplot3d(x=data$X, y=data$Z,z=data$Y, pch=16,color = data_Color, xlab='X', ylab='', zlab='Y', box=FALSE, grid=FALSE, mar=c(3,3,0,1+distance)) #(B-,L,T-,R)
      for(i in 1:nrow(connection_df))
      {
        f=connection_df$first[i]
        s=connection_df$second[i]
        p1 <- sd3_1$xyz.convert(data$X[f],data$Z[f],data$Y[f])
        p2 <- sd3_1$xyz.convert(data$X[s],data$Z[s],data$Y[s])
        segments(p1$x,p1$y,p2$x,p2$y,lwd=1,col='black')
      }

      sd3_2=scatterplot3d(x=rot2_data$X, y=rot2_data$Z,z=rot2_data$Y,pch=16,color = data_Color, xlab='X', ylab='', zlab='Y', box=FALSE, grid=FALSE,  mar=c(3,1+distance,0,3))

      for(i in 1:nrow(connection_df))
      {
        f=connection_df$first[i]
        s=connection_df$second[i]
        p1 <- sd3_2$xyz.convert(rot2_data$X[f],rot2_data$Z[f],rot2_data$Y[f])
        p2 <- sd3_2$xyz.convert(rot2_data$X[s],rot2_data$Z[s],rot2_data$Y[s])
        segments(p1$x,p1$y,p2$x,p2$y,lwd=1,col='black')
      }
    }

    par(mai=c(0.6,0,0,0))
    plot.new()
    legend(x="center", ncol=length(levels(factor(data$Color))), col= colors_vec, bg="white", legend = levels(factor(data$Color)), x.intersp=.5, y.intersp=0, cex = 1, pch = 16, xpd = NA,  bty="n")
    print(p)
    dev.off()

#### interactive plot ##
    if(missing(connection_file))
    {
      StereoPLot3D(data, rot2_data,colors_vec,data_Color)
    }else
    {
      StereoPLot3D(data, rot2_data,colors_vec,data_Color,connection_df)
    }

}
