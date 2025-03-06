# ********************************
# Load Packages and Set Parameters
# *****************************************************************************

# ::::::::::::::::::::::::::::::::::
# Colorblind Palette for Charts ----

# Base colorblind-friendly palette
base_cb_palette <- c(
  "#D55E00", "#E69F00", "#009E73", "#56B4E9", "#0072B2", 
  "#F0E442", "#CC79A7", "#999999", "#000000", "#0099FF", 
  "#66CC99", "#6600CC"
)

# Function to generate a palette for N series
generate_palette <- function(N) {
  if (N <= length(base_cb_palette)) {
    # If N is less than or equal to the base palette size, use the base palette
    return(base_cb_palette[1:N])
  } else {
    # If N is larger, interpolate to generate additional colors
    return(colorRampPalette(base_cb_palette)(N))
  }
}


# ::::::::::::::::::::::::::::::::::
# Hardcoded Trust Names ----


Trust_Names <- c(
  "conpersonal" = "Interpersonal",
  "confinan"    = "Banks",
  "conbus"      = "Business",
  "conclerg"    = "Org. Religion", 
  "coneduc"     = "Education",
  "confed"      = "Executive Branch",
  "conlabor"    = "Org. Labor",
  "conpress"    = "The Press",
  "conmedic"    = "Medicine",
  "contv"       = "The Media", 
  "conjudge"    = "Judicial Branch",
  "consci"      = "Science",
  "conlegis"    = "Legislative Branch",
  "conarmy"     = "The Military"
)

### EOF ###