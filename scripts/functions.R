# functions used to generate raster files around NEON plots


#### GENERATE FLIGHT PATH ID FROM SHAPEFILE ATTRIBUTE TABLE ####
# function to set plot and identify path
find_flight_path <- function(spatial_file,plot_string,plot_column,path_column){
  
  # convert to dataframe
  df <- spatial_file@data
  
  # finds the row in the df that matches the plot_string, pulls out the path id in the path_column
  path <- df[which(df[,plot_column]==plot_string),path_column]
  
  return(path)
}

# test function
# find_flight_path(p_full,"OSBS_014",2,11)
# find_flight_path(p_full,c("OSBS_007","OSBS_014"),2,11)

#### GENERATE STRING OF H5 FILE PATH FOR THE NEON PLOT OF INTEREST ####
# function to create file path name
create_file_path <- function(file_folder,path){
  
  # simple regex statement to return the full file path that matches the uniqe part of the identified path
  h5_file <- list.files(file_folder,full.names=T)[grep(path,list.files(file_folder))]
  
  return(h5_file)
}

#### GENERATE X Y POINT LOCATION OF NEON PLOT CENTER ####
# function to extract the x y coordinates of the plot
# output depends on projection of spatial data, assume it's in UTM meters
find_point_coordinates <- function(spatial_file,plot_string,plot_column){
  
  # convert to dataframe
  df <- spatial_file@data
  
  # find row of plot_string
  r <- which(df[,plot_column]==plot_string)
  
  plot_coords <- spatial_file@coords[r,]
  
  return(unname(plot_coords))
}

# test function
# point_coordinates <- find_point_coordinates(p_full,"OSBS_007",2)
# find_point_coordinates(p_full,c("OSBS_007","OSBS_014"),2)

#### CREATE EXTENT OBJECT THAT DEFINES BUFFER AROUND CENTER OF NEON PLOT ####
# function to create extent or bounding box around point location
create_extent_around_point <- function(point_coordinates,buffer_size){
  
  xmin <- round(point_coordinates[1]-buffer_size)
  xmax <- round(point_coordinates[1]+buffer_size)
  
  ymin <- round(point_coordinates[2]-buffer_size)
  ymax <- round(point_coordinates[2]+buffer_size)
  
  # create extent object
  point_extent <- extent(xmin,xmax,ymin,ymax)
  
  return(point_extent)
}

# test function - have only tested with 1 point
# create_extent_around_point(point_coordinates,40)

#### SAVE IMAGE OBJECT TO FILE ####
save_raster_image <- function(multi_band_raster,save_folder,file_name,flatten=TRUE,img_res=1){
  
  # save tif raster
  writeRaster(multi_band_raster,
              filename=paste(save_folder,file_name,".tif",sep=""),
              format="GTiff",overwrite=T,options="TFW=YES")
  
  if(flatten == TRUE){
    
    # calculate the number of pixels wide or tall - assume image is square
    num_pix <- (multi_band_raster@extent@xmax - multi_band_raster@extent@xmin)/img_res
    
    
    # write tiff file
    # need to use this and not writeRaster because image need to be flattened to RGB image
    tiff(paste(save_folder,file_name,"_flat",".tif",sep=""),width=num_pix,height=num_pix)
    plotRGB(multi_band_raster,stretch="lin")
    dev.off()
    
    
    # copy header file
    file.copy(from=paste(save_folder,file_name,".tfw",sep=""),
              to=paste(save_folder,file_name,"_flat",".tfw",sep=""))
    
}

} ## END FUNCTION

#### CREATE CLIPPED IMAGE OF RGB CAMERA IMAGES FOR PLOT ####
create_RGB_clip <- function(plot_coords,source_folder){
  
  # use function in plyr package
  plot_coords_round <- round_any(plot_coords,1000)
  
  # use these values to find file to import
  f <- list.files(source_folder,
                  pattern = paste0("*",plot_coords_round[1],"_",plot_coords_round[2],"_image.tif$"),
                  full.names = T)
  
  # load raster
  r <- stack(f)
  
  r_clip <- crop(r,plot_extent)
  
  return(r_clip)
  
}

