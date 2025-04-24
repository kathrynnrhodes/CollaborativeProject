library(magick)
library(tiff)
group_loaded_images <- function(image_objects, processor_function, ...) {
  # Validate inputs
  if (length(image_objects) == 0) {
    stop("No image objects provided")
  }
  
  if (!is.function(processor_function)) {
    stop("processor_function must be a function")
  }
  
  # Call the processor function with the grouped images and any additional arguments
  result <- processor_function(image_objects, ...)
  
  return(result)
}

# Example usage with pre-loaded images:
process_loaded_images <- function(image_list, output_format = "tif", resize = NULL) {
  cat(sprintf("Processing %d pre-loaded images with output format: %s\n", 
              length(image_list), output_format))
  
  return(list(
    status = "success",
    count = length(image_list),
    images = image_list
  ))
}

# Example call
image_1 <- readTIFF("/cloud/project/F_FXR_3_cerebellum_40x_1.tif") #assign your images
image_2 <- readTIFF("/cloud/project/F_FXR_3_cerebellum_40x_2.tif")
image_3 <- readTIFF("/cloud/project/F_FXR_3_cerebellum_40x_3.tif")
my_images <- list(image_1, image_2, image_3) #group the images
result <- group_loaded_images(my_images, process_loaded_images, output_format = "tif") #run the function
summary(result) #confirm grouping of desired, pre-loaded images