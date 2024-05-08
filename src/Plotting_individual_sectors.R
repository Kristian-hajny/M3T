#build functions to plot up most sectors as they finish running.  

#take log and remove 0 or negative values
prep_plot_data <- function(input){
  output <- input
  output <- log10(output)
  output[is.infinite(output)] <- NA
  return(output)
}


#plot for log scale
log_plot <- function(input,title,zlim_min=NULL,zlim_max=NULL,
                     filename){
  
  #set filename to the proper path and use input data as filename if none was
  #provided
  if(missing(filename)){
    #save to a separate folder if the input is a summed_sector
    if(grepl(pattern="Summed",x=substitute(input))){
      outputname <- paste0(plot_directory,"Summed_Sectors/",substitute(input))
    }else{
      outputname <- paste0(plot_directory,substitute(input))
    }
  }else{
    outputname <- paste0(plot_directory,filename)
  }
  
  input <- prep_plot_data(input)
  
  png(paste0(outputname,".png"),width = 480*2,height=480*2)
  plot(input,mar=c(3.1, 3.1, 2.1, 7.1)+c(0,0,0,1),
       # col=timPalette(),
       colNA="black",
       main=title,
       plg=list(cex=2,title="log10(nmol/m2/s)",title.cex=2),
       pax=list(cex.axis=2),
       xlab="Longitude",ylab="Latitude",
       cex.main=2,cex.axis=2,cex.lab=2,
       zlim=c(zlim_min,zlim_max))
  plot(County_Tigerlines,add=T,border="dimgrey",col=NA)
  plot(State_Tigerlines,add=T,border="white",lwd=2,col=NA)
  if(exists("focus_city_tigerlines")){
    plot(focus_city_tigerlines,add=T,border="darkgrey",col=NA)
  }
  dev.off()
  
}


#plot for linear scale - mostly identical
not_log_plot <- function(input,title,zlim_min=NULL,zlim_max=NULL,
                         filename){
  if(missing(filename)){
    if(grepl(pattern="Summed",x=substitute(input))){
      outputname <- paste0(plot_directory,"Summed_Sectors/",substitute(input))
    }else{
      outputname <- paste0(plot_directory,substitute(input))
    }
  }else{
    outputname <- paste0(plot_directory,filename)
  }
  
  #Here just set 0 values to NA so that colNA applies
  input[values(input)==0] <- NA
  
  png(paste0(outputname,".png"),width = 480*2,height=480*2)
  par(mar=c(5, 4, 4, 2) + 0.1 + c(0,1,2,1))
  plot(input,mar=c(3.1, 3.1, 2.1, 7.1)+c(0,0,0,1),
       # col=timPalette(),
       colNA="black",
       main=title,
       plg=list(cex=2,title="nmol/m2/s",title.cex=2),
       pax=list(cex.axis=2),
       xlab="Longitude",ylab="Latitude",
       cex.main=2,cex.axis=2,cex.lab=2,
       zlim=c(zlim_min,zlim_max))
  plot(County_Tigerlines,add=T,border="dimgrey",col=NA)
  plot(State_Tigerlines,add=T,border="white",lwd=2,col=NA)
  if(exists("focus_city_tigerlines")){
    plot(focus_city_tigerlines,add=T,border="darkgrey",col=NA)
  }
  dev.off()
}

