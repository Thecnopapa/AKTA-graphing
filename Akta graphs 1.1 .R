
## Libraries ##
library(ggplot2)
library(tidyverse)
library(readxl)
library(purrr)


# Place this file on the folder with the .xlsx files or above them

## Data Customisation ## 
file_extension <- ".xlsx" # Shouldn't be changed, although it could be changed to another Excel extension (not tested)

recursive <- TRUE # TRUE to include any sub folders in the file directory, FALSE does not include files in subfolders
subfolder <- "" # e.g. "/raw_data", process files only in the specified sub folder
                # useful if other .xlsx files present in the directory
                # or if you only want to process a specific set of files


## Graphs Customisation ## 
show_legend = TRUE # TRUE or FALSE, self explanatory
legend_inside = TRUE # TRUE for inside the graph, FALSE for outside

legend_x <- 0.95 # Only used when inside the graph
legend_y <- 0.5 # Only used when inside legends
legend_justification <- c("right", "bottom") # Only used when inside legends

legend_position <- "right" # For outside legends

main_colour <- "darkblue" # Colour of Left axis + UV curve
conductivity_colour <- "orange" # Colour of Right axis + Conductivity curve
B_colour <- "green" # Colour of Right axis + %B curve

font_size <- 10 # Axis font size
legend_font_size <- font_size/2 # Legend font size, can be set to a discrete value too
start_at_elution <- TRUE # TRUE to omit all data before elution starts, FALSE to also show equilibration and washes
calibrate_UV <- TRUE # TRUE to calibrate UV values to the first value after elution starts, useful if you forgot to calibrate in the akta

use_fraction_lines <- TRUE # TRUE to display fraction vertical lines
fraction_names_font_size <- 1 # Size of fraction names, will need adjusting when changing plot size
fraction_line_width <- 0.1 # Fraction line width, will need adjusting when changing plot size
fraction_line_type <- "longdash" # Fraction line style, I don't know alternatives rn but could be found in ggplot2 info

theme <- theme_classic() + theme( # Theme can be changed, theme_classic() has no axis lines
  # Insert here any ggplot2 theme settings, will override any settings in the code
)

# Set up custom label titles
left_axis_lab <- "Absorbance at 280 (mAU)"
right_axis_lab_B <- "%B"
right_axis_lab_C <- "Conductivity (mS/cm)"
x_axis_lab <- "Elution (ml)"

title_just <- 0.5 # Title justification, 0 = left, 0.5 = centre, 1 = right

n_right_axis_breaks <- 3 # Number of breaks on the right axis, equally distributed

## Export Customisation ##
save_separate <- FALSE # TRUE to save on the specified folder on the next line, FALSE to save next to the data (.xslx) file
                       # Useful to save different rounds of plots, eg. /large_plots + /small_plots with different image sizes 
save_folder <- "/Plots"

# Output figure size
height <- 920
width <- 1280
units <- "px" # Can be set to "px", "cm", "in", or "mm"





## Set up ## 
# Get the list of files (all .xlsx file, so make sure there are no other .xslx files) in the specified working directory and sub folders
wd <-paste( dirname(rstudioapi::getSourceEditorContext()$path), subfolder,sep="")

files <- list.files( wd, recursive = recursive, pattern = file_extension)
ext_len <- str_length(file_extension)
# Set a list for the columns, not used until further down
names <- c("UVml", "UV", "Condml", "Cond", "ConcBml", "ConcB", "Logml", "Log", "Fractionml", "Fraction")

# p stands for Which plot is it working on right now, starting at 1 ofc
p = 1

## Main program, repeats for each file identified ## 
for (file in files) {
  # Import the data #
  file <- paste(wd,"/", file, sep = "")
  filedir <- dirname(file) # full file directory including name for further reference
  filename <- str_sub(basename(file), 0,-ext_len-1) # only the name of the file without .xlsx or the path
  aktadata <- read_excel(file) # Import the data as a data frame
  
  # Clean up the data #
  # This assumes that the first 6 columns are UV / Conductivity / %B
  mlrow <- grep ("ml", aktadata[[1]])[1] # Find the last row with titles (to be removed afterwards)
  fractioncol <- grep( "Fraction", aktadata[mlrow,])[1] # Find the column with fraction data
  logcol <- grep( "Logbook", aktadata[mlrow,])[1] # Find the column with the log data
  # Remove all titles and unwanted data (remove title rows, select columns 1-6 + log columns + fraction columns)
  aktadata <- aktadata[-c(1:mlrow),c(1:6, (logcol-1):(logcol), (fractioncol-1):fractioncol)]
  aktadata[,c(1:7,9)] <- as.data.frame(lapply(aktadata[,c(1:7,9)], as.numeric, except = is.na(aktadata))) # Transform numbers into actual numbers
  colnames(aktadata) <- names # Set column names to the names defined earlier
  
  # Process the data # 
  if (start_at_elution){
    elutionStart <- aktadata$Logml[grep( "Elution", aktadata[[8]])[1]] # Identify the ml at which the akta starts eluting
    firstvalueUV <- detect_index(aktadata$UVml, \(x) x >elutionStart) # Identify the first UV value after elution starts (index value)
    firstvalueCond <- detect_index(aktadata$Condml, \(x) x >elutionStart) # Identify the first Conductivity value after elution starts
  }else{
    elutionStart <- 0
    firstvalueUV <- 1 
    firstvalueCond <- 1
  }
  calibrationml <- aktadata$UVml[firstvalueUV] # The ml value of teh first UV data after elution starts
  calibrationuv <- aktadata$UV[firstvalueUV] # the UV data of that same first value
  if (calibrate_UV){
    aktadata$UV <- aktadata$UV - calibrationuv # Calibrate all values according to the first elution value
  }
  maxUV<- max(aktadata$UV[-c(1:firstvalueUV-1)]) # Find teh maximum UV after elution starts
  aktadata$UV[1:firstvalueUV] <- NA # Remove all value before the elution starts
  
  # Plot the graph #
  graph <- ggplot() + # Set up the ggplot
    geom_line(data = aktadata, aes(x= UVml, y = UV, color = "UV")) + # Plot UV data
    lims(x = c(elutionStart,NA), y = c(0,maxUV) )+ # Set up the graph limits
    xlab(x_axis_lab) + # X title
    ylab(left_axis_lab) + # Y title
    ggtitle(filename) + # Set the title as the data file name
    theme + # My choice of theme, if changed to one with lines might get confusing
    theme(plot.title = element_text(hjust = title_just), # Centre the title
          axis.line.y.left = element_line(color = main_colour) # Change Y axis line colour to match the data 
          ) # Add here other theme related stuff such as font style or axis label settings
  
  breaks <- seq(0,1, length= n_right_axis_breaks)
  
  # Add %B data #
  if (max(aktadata$ConcB, na.rm = TRUE) > 0){ # If highest %B value is larger than 0 then plot %B data
    # Because UV is the main graph all other values need to be scaled so that they are not a flat line at the bottom
    scaleB <- maxUV/100 # 100 stands for the maximum value of %B
    aktadata$relativeB <- aktadata$ConcB*scaleB # Make a new data column with the %B values scaled to the UV data
    graph <- graph + geom_line(data = aktadata, aes(x = ConcBml, y = relativeB, color = "%B"))+ # Plot the scaled data
      scale_y_continuous(sec.axis = sec_axis(trans = ~.*1, # Draw an axis on the left
                                             name = right_axis_lab_B, # Name the new axis
                                             breaks = breaks*100*scaleB, # Select at which values (in %) a label should be shown, more can be added
                                             labels = breaks*100))+ # because of scale, the labels must be changed "manually", must match the previous step
      theme(axis.line.y.right = element_line(color = B_colour)) # Self explanatory
    
    # If no %B detected, plot conductivity instead, for desalting or sec columns
  }else{
    # Scaling needs to be done similarity to %B
    maxCond<- max(aktadata$Cond[-c(1:firstvalueCond-1)], na.rm = TRUE)# Find the max conductivity, basically we want it to match the max UV
    scaleCond <- maxUV/maxCond # Find the ratio between the data
    aktadata$relativeCond <- aktadata$Cond*scaleCond # New data column with scaled conductivity
    graph <- graph + geom_line(data = aktadata, aes(x = Condml, y = relativeCond, color = "Conductivity"))+ # Plot the scaled data
      scale_y_continuous(sec.axis = sec_axis(trans = ~.*1, # Make new axis
                                             name = right_axis_lab_C, # Name new axis
                                             breaks = (breaks*maxCond*scaleCond), # Axis breaks, as fractions of the axis length, more can be set
                                             labels = round(breaks*maxCond,0)))+ # Scaling back the axis values to show original values, should match previous line
      theme(axis.line.y.right = element_line(color = conductivity_colour))# Self explanatory
  }
  # Simple loop that adds lines at each fraction change, Can be removed to add lines matching total elution mls
  if (use_fraction_lines){
    f = 1 # Fraction number
    while (f <= length(na.omit(aktadata$Fraction))){ # Get the number of fractions
      xint <- aktadata$Fractionml[f] # Get the X position for the line and label
      frac <- aktadata$Fraction[f]
      graph <- graph + geom_vline(xintercept = xint,linewidth = fraction_line_width, linetype = fraction_line_type) # Add a line at each fraction
      if (!grepl( "T", frac, ignore.case = TRUE)){
        lab = str_sub(gsub(".", "", frac, fixed = TRUE),2)
        graph <- graph + annotate("text", x= xint+1, y = maxUV*1.1 , label = lab, size = fraction_names_font_size)
      }
      f = f+1
    }
  }
  
  if (show_legend){
    graph <- graph + theme(
      axis.line.y.left = element_line(color = "black"),
      axis.line.y.right = element_line(color = "black"),
      legend.title = element_blank(),
      legend.text = element_text(size = legend_font_size)
      )
      
      if (legend_inside){
        graph <- graph + theme(legend.position = c(legend_x,legend_y),
                               #legend.box.just = "right",
                               #legend.margin = margin(6,6,6,6),
                               legend.justification = legend_justification,
                               legend.box.background = element_rect(color = "black", size =  fraction_line_width)
                               )
      }else {
        graph <- graph + theme(legend.position = legend_position)
      }
      
  }

  graph <- graph +   scale_colour_manual(name = "colour",values = c("UV" = main_colour,
                                                                    "%B" = B_colour,
                                                                    "Conductivity" = conductivity_colour))
  graph # Used while debugging to see the graph plotted
  
  
  if (save_separate){
    savedir <- paste(sep="",wd,save_folder,"/", filename,".png") # Sets file saving location to one particular folder
    
  }else{
    savedir <- paste(filedir, "/",filename,".png", sep = "") ## Turn on to save on original folder, and remove next line ofc
  }
  
  ggsave(savedir, width = width, height = height, units = units) # save the plot at the previously defined folder
  print(paste("saved as:", savedir)) # print where has it been saved
  print(paste("Plots made: ", p,"/",length(files), sep="")) # Print the overall progress of the program
  p = p+1
}

