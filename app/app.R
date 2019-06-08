library(shiny)
library(ggplot2)
library(mritc)
library(imager)

# Read in NIfTI data
T1 <- readMRI(system.file("extdata/t1.rawb.gz", package="mritc"),
              c(91, 109, 91), format="rawb.gz")
# Read in the mask for non-brain matter
mask <- readMRI(system.file("extdata/mask.rawb.gz", package="mritc"),
                c(91, 109, 91), format="rawb.gz")

# INITIALIZATIONS
y <- T1[mask==1]
initial <- initOtsu(y, 2)
prop <- initial$prop
mu <- initial$mu
sigma <- initial$sigma


tc.icm <- mritc(T1, mask, method = "ICM")
t1 <- T1
t1[mask == 1] <- 0
icm.class <- max.col(tc.icm$prob)
tc.icm$mask[tc.icm$mask == 1] <- icm.class

# PLOT FUNCTION
# ggplot theme to be used
plotTheme <- function() {
  theme(
    panel.background = element_rect(
      size = 1,
      colour = "black",
      fill = "white"),
    axis.ticks = element_line(
      size = 2),
    panel.grid.major = element_line(
      colour = "gray80",
      linetype = "dotted"),
    panel.grid.minor = element_line(
      colour = "gray90",
      linetype = "dashed"),
    axis.title.x = element_text(
      size = rel(1.2)),
    axis.title.y = element_text(
      size = rel(1.2)),
    plot.title = element_text(
      size = 20,
      vjust = 1.5)
  )
}

plt_brain <- function(plane = "saggital", k, WM=T, GM=T, CSF=T) {
  # mask is a three dimensional array -- from mritc object
  
  if (plane == "saggital") {
    slice <- T1[k,,] # original image
    wm <- as.cimg(tc.icm$mask[k,,]) %>% as.data.frame()
    gm <- as.cimg(tc.icm$mask[k,,]) %>% as.data.frame()
    csf <- as.cimg(tc.icm$mask[k,,]) %>% as.data.frame()
  }
  else if (plane == "coronal") {
    slice <- T1[,k,] # original image
    wm <- as.cimg(tc.icm$mask[,k,]) %>% as.data.frame()
    gm <- as.cimg(tc.icm$mask[,k,]) %>% as.data.frame()
    csf <- as.cimg(tc.icm$mask[,k,]) %>% as.data.frame()
  }
  else {
    slice <- T1[,,k] # original image
    wm <- as.cimg(tc.icm$mask[,,k]) %>% as.data.frame()
    gm <- as.cimg(tc.icm$mask[,,k]) %>% as.data.frame()
    csf <- as.cimg(tc.icm$mask[,,k]) %>% as.data.frame()
  }
  
  slice_df <- as.cimg(slice) %>% as.data.frame() # df for ggplot2
  
  # create segmented overlays
  # WM
  wm$value[wm$value != 3] <- NA # isolate WM
  wm$value[!is.na(wm$value)] <- "#f6bb13" # assign a colour val to tissue type
  # GM
  gm$value[gm$value != 2] <- NA
  gm$value[!is.na(gm$value)] <- "#a83e27"
  # CSF
  csf$value[csf$value != 1] <- NA
  csf$value[!is.na(csf$value)] <- "#91ae34"
  
  plt <- ggplot(slice_df,aes(x,y)) + geom_raster(aes(fill=value)) +
    scale_x_continuous(expand=c(0,0)) +
    scale_y_continuous(expand=c(0,0)) +
    scale_fill_continuous(low="black",high="white", na.value = NA) +
    coord_fixed() +
    plotTheme() +
    theme(legend.position = "none")
  
  # conditional overlays
  if (WM == T) {
    plt <- plt + annotate(geom = 'raster', x = wm$x, y = wm$y,
                          fill = wm$value, na.rm = TRUE, alpha=0.8)
  }
  if (GM == T) {
    plt <- plt + annotate(geom = 'raster', x = gm$x, y = gm$y,
                          fill = gm$value, na.rm = TRUE, alpha=0.8)
  }
  if (CSF == T) {
    plt <- plt + annotate(geom = 'raster', x = csf$x, y = csf$y,
                          fill = csf$value, na.rm = TRUE, alpha=0.8)
  }
  
  plt
}

# SHINY UI AND SERVER

ui <- fluidPage(
  
  # Title ----
  titlePanel("Brain MRI Segmentation"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Slider for the number of bins ----
      sliderInput(inputId = "k",
                  label = "Depth:",
                  min = 12,
                  max = 77,
                  value = 50),
      
      checkboxInput("CSF", "Cerebrospinal Fluid",
                    value = FALSE),
      checkboxInput("GM", "Gray Matter",
                    value = FALSE),
      checkboxInput("WM", "White Matter",
                    value = FALSE)
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Transverse Plane",
                 plotOutput("transverse")),
        tabPanel("Saggital Plane",
                 plotOutput("saggital")),
        tabPanel("Coronal Plane",
                 plotOutput("coronal"))
      )
    )
  )
)


server <- function(input, output) {
  
  output$transverse <- renderPlot({
    
    plt_brain(plane = "transverse", input$k, WM=input$WM, GM=input$GM, CSF=input$CSF)
    
  })
  
  output$saggital <- renderPlot({
    
    plt_brain(plane = "saggital", input$k, WM=input$WM, GM=input$GM, CSF=input$CSF)
    
  })
  
  output$coronal <- renderPlot({
    
    plt_brain(plane = "coronal", input$k, WM=input$WM, GM=input$GM, CSF=input$CSF)
    
  })
  
}

# RUN
shinyApp(ui = ui, server = server)