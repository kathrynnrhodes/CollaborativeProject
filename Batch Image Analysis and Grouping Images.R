#script for batch analysis of images
# Enhanced function to analyze a single image using magick
analyze_image <- function(image, threshold = "60%") {
  split_image <- image_separate(image)
  split_image <- image_threshold(split_image, type = "white", threshold = threshold)
  split_image <- image_threshold(split_image, type = "black", threshold = threshold)
  final_image <- image_combine(split_image)
  blank <- image_threshold(image, type = "white", threshold = "0%")
  fill <- image_threshold(image, type = "black", threshold = "1000%")
  comparison <- image_compare_dist(final_image, blank, metric = "AE")
  info <- image_info(image)
  width <- info$width
  height <- info$height
  percent <- (comparison$distortion / (width * height)) * 100
  return(list(
    percentage = percent,
    formatted = paste0(round(percent, 2), "%"),
    processed_image = final_image,
    blank_image = blank,
    filled_image = fill,
    comparison_result = comparison,
    width = width,
    height = height
  ))
}

# Function to batch analyze multiple images
batch_analyze_images <- function(image_result, threshold = "60%") {
  if (!is.list(image_result) || is.null(image_result$images)) {
    stop("Expected input from group_loaded_images function with 'images' field")
  }
  images <- image_result$images
  results <- lapply(1:length(images), function(i) {
    if (!inherits(images[[i]], "magick-image")) {
      temp_file <- tempfile(fileext = ".tif")
      writeTIFF(images[[i]], temp_file)
      img <- image_read(temp_file)
      file.remove(temp_file)
    } else {
      img <- images[[i]]
    }
    analysis <- analyze_image(img, threshold)
    analysis$image_index <- i
    analysis$image_name <- ifelse(exists(paste0("image_", i)), paste0("image_", i), paste0("Image ", i))
    
    return(analysis)
  })
  avg_percent <- mean(sapply(results, function(r) r$percentage))
  cat("Individual Image Analysis Results:\n")
  cat("--------------------------------\n")
  for (r in results) {
    cat(sprintf("Image %d (%s):\n", r$image_index, r$image_name))
    cat(sprintf("  Dimensions: %d x %d pixels\n", r$width, r$height))
    cat(sprintf("  Distortion value: %.2f\n", r$comparison_result$distortion))
    cat(sprintf("  Percent difference from blank: %s\n\n", r$formatted))
  }
  return(list(
    individual_results = results,
    average_percentage = avg_percent,
    formatted_average = paste0(round(avg_percent, 2), "%"),
    count = length(images),
    summary = sprintf("Average percent difference across %d images: %s", 
                      length(images), paste0(round(avg_percent, 2), "%"))
  ))
}
#make the group images function
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

# Example usage
library(tiff)
library(magick)
image_1 <- readTIFF("/cloud/project/F_FXR_3_cerebellum_40x_1.tif") 
image_2 <- readTIFF("/cloud/project/F_FXR_3_cerebellum_40x_2.tif")
image_3 <- readTIFF("/cloud/project/F_FXR_3_cerebellum_40x_3.tif")
my_images <- list(image_1, image_2, image_3)
result <- group_loaded_images(my_images, process_loaded_images, output_format = "tif")
summary(result)
analysis_results <- batch_analyze_images(result, threshold = "60%")
print(analysis_results$summary)

#IMPORTANT! DO NOT LABEL IMAGES AS "image1" ETC! This will result in a "file not found" error. Instead, stick to the nomenclature used in the example provided. 
#Happy analysis!