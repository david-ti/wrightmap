classic_wright_map <- function(thresholds, y_axis_range = NULL, 
                                    x_axis_label = "Items", 
                                    y_axis_logit_label = "Logits", 
                                    show_logit_axis = TRUE, 
                                    logit_axis_position = "right", 
                                    outer_margins = c(0, 0, 0, 3), 
                                    cut_points = NULL, 
                                    item_labels = NULL, 
                                    plot_title = "", ...) {
  
  # Define a helper function to generate histogram breaks
  get_histogram_breaks <- function(thresholds, axis_range) {
    min_threshold <- min(thresholds, na.rm = TRUE)
    max_threshold <- max(thresholds, na.rm = TRUE)
    breaks <- seq(from = axis_range[1], to = axis_range[2], length.out = 25)
    
    if (min_threshold < min(breaks)) breaks <- c(min_threshold, breaks)
    if (max_threshold > max(breaks)) breaks <- c(breaks, max_threshold)
    
    return(breaks)
  }

  # Helper function to bin items by level
  bin_items_by_level <- function(level, label_matrix, cut_matrix) {
    paste(sort(label_matrix[cut_matrix == level]), collapse = " | ")
  }
  
  # Convert thresholds to matrix format
  thresholds <- as.matrix(thresholds)
  n_items <- dim(thresholds)[1]  # Number of items
  n_levels <- dim(thresholds)[2]  # Number of levels

  # Generate or use provided item labels
  if (is.null(item_labels)) {
    item_labels <- formatC(1:n_items, digits = 1, format = "d", flag = "0")
  }
  
  # Pad all item labels to the same length with underscores
  max_label_length <- max(nchar(item_labels))
  item_labels <- sprintf(paste0("%-", max_label_length, "s"), item_labels)
  item_labels <- gsub(" ", "_", item_labels)
  
  # Handle multiple levels of items
  item_label_matrix <- matrix(rep(item_labels, n_levels), ncol = n_levels)
  if (n_levels > 1) {
    item_label_matrix <- t(apply(item_label_matrix, 1, paste, c(1:n_levels), sep = "."))
  }

  # Determine y-axis range if not provided
  if (is.null(y_axis_range)) {
    y_axis_range <- range(thresholds, na.rm = TRUE)
    y_padding <- (y_axis_range[2] - y_axis_range[1]) * 0.1
    y_axis_range <- y_axis_range + c(-y_padding, y_padding)
  }
  
  # Set up global graphical parameters once
  par(oma = outer_margins, mgp = c(3, 1, 0))
  
  # Create the plot with customizable title
  plot(seq(1:n_items), rep(0, n_items), type = "n", axes = FALSE, 
       xlab = x_axis_label, ylab = "", ylim = y_axis_range,
       xlim = c(0.5, n_items + 0.5), main = plot_title, 
       cex.lab = 0.8, font.lab = 3, ...)
  
  # Add a box around the plot
  box(bty = "o")
  
  # Handle the display of the logit axis (right or left)
  if (show_logit_axis) {
    if (logit_axis_position == "right") {
      axis(4, las = 1, cex.axis = 0.7, font.axis = 2)
      mtext(y_axis_logit_label, side = 4, line = 1.5, cex = 0.8, font = 3)
    } else if (logit_axis_position == "left") {
      axis(2, las = 1, cex.axis = 0.7, font.axis = 2)
      mtext(y_axis_logit_label, side = 2, line = 1.5, cex = 0.8, font = 3)
    }
  }

  # Add cut points if provided
  if (!is.null(cut_points)) {
    cutLines(cut_points, ...)
  }
  
  # Create histogram without plotting it
  item_histogram <- hist(thresholds, plot = FALSE, breaks = get_histogram_breaks(thresholds, y_axis_range))
  item_bin_locations <- item_histogram$mids
  bin_size <- abs(item_histogram$breaks[1] - item_histogram$breaks[2])
  
  # Prepare histogram data for plotting
  item_histogram <- data.frame(xleft = item_histogram$mids - (bin_size / 2), 
                               ybottom = item_histogram$mids * 0,
                               xright = item_histogram$mids + (bin_size / 2), 
                               ytop = item_histogram$counts)
  
  # Bin items according to thresholds
  binned_items <- matrix(cut(thresholds, breaks = get_histogram_breaks(thresholds, y_axis_range), 
                             labels = c(1:length(item_histogram[, 1] + 1))), ncol = n_levels)
  
  # Generate binned item labels
  binned_list <- unlist(lapply(1:length(item_bin_locations), bin_items_by_level, 
                               item_label_matrix, binned_items))
  
  # Plot the binned item labels with a monospace font
  text(cbind(0, item_bin_locations), labels = binned_list, pos = 4, 
       offset = 1 * 15 / n_items, cex = 0.65, family = "mono")
}