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

plt_brain <- function(plane = "saggital", k, mask) {
  # mask is a three dimensional array -- from mritc object
  
  if (plane == "saggital") mask.img <- as.cimg(tc.icm$mask[k,,])
  else if (plane == "coronal") mask.img <- as.cimg(tc.icm$mask[,k,])
  else mask.img <- as.cimg(tc.icm$mask[,,k])
  
  mask_df <- as.data.frame(mask.img)
  
  ggplot(mask_df,aes(x,y))+geom_raster(aes(fill=value)) +
    labs(title = "Axial/Transverse View") +
    scale_x_continuous(expand=c(0,0)) +
    scale_y_continuous(expand=c(0,0)) +
    scale_fill_gradient(low="black",high="white") +
    coord_fixed() +
    plotTheme() +
    theme(legend.position = "none")
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
                  value = 50)
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
    
    plt_brain(plane = "transverse", input$k, mask=tc.icm$mask)
    
  })
  
  output$saggital <- renderPlot({
    
    plt_brain(plane = "saggital", input$k, mask=tc.icm$mask)
    
  })
  
  output$coronal <- renderPlot({
    
    plt_brain(plane = "coronal", input$k, mask=tc.icm$mask)
    
  })
  
}

# RUN
shinyApp(ui = ui, server = server)