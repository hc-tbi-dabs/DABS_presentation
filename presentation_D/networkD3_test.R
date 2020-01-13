library(timevis)
data <- data.frame(
  id      = 1:7,
  content = c("Chieu, Malek(STU), Dragana, Peter"  , 
              "Malek(STU), Dragana, Peter",
              "Dragana, Peter", 
              "Omar, Dragana, Peter", 
              "Manolo, Omar, Dragana", 
              "Grace, Manolo, Omar, Dragana",
              "Jonathan, Grace, Manolo, Omar, Dragana"),
  start   = c("2019-01-04", "2019-06-28", "2019-07-24", "2019-09-24", "2019-10-27", "2020-01-06", "2020-02-12"),
  end     = c("2019-06-28", "2019-07-24", "2019-09-24", "2019-10-27", "2020-01-06", "2020-02-11","2020-03-31")
)

timevis(data)



library(timevis)
data <- data.frame(
  id      = 1:8,
  content = c("Chieu, Malek(ST), Dragana, Peter(acting lead)"  , 
              "Chieu leaves (3)",
              "Malek(ST) leaves (2)", 
              "Omar joins (3)", 
              "Manolo joins (3)", 
              "Grace(ST) joins (4)",
              "Jesse(ST) joins (5)",
              "Jonathan joins (6)"),
  start   = c("2019-01-04", "2019-06-28", "2019-07-24", "2019-09-24", "2019-10-27", "2020-01-06", "2020-01-13", "2020-02-12"),
  end     = c("2019-06-28", NA, NA, NA, NA, NA, NA, NA)
)

timevis(data)

# Load package
library(networkD3)
library(dplyr) # to make the joins easier

src <- c("A", "A", "A", "A",
         "B", "B", "C", "C", "D")
target <- c("B", "C", "D", "J",
            "E", "F", "G", "H", "I")
networkData <- data.frame(src, target, stringsAsFactors = FALSE)

nodes <- data.frame(name = unique(c(src, target)), stringsAsFactors = FALSE)
nodes$id <- 0:(nrow(nodes) - 1)


# create a data frame of the edges that uses id 0:9 instead of their names
edges <- networkData %>%
  left_join(nodes, by = c("src" = "name")) %>%
  select(-src) %>%
  rename(source = id) %>%
  left_join(nodes, by = c("target" = "name")) %>%
  select(-target) %>%
  rename(target = id)

edges$width <- 1

# make a grouping variable that will match to colours
nodes$group <- ifelse(nodes$name %in% src, "lions", "tigers")

# simple with default colours
forceNetwork(Links = edges, Nodes = nodes, 
             Source = "source",
             Target = "target",
             NodeID ="name",
             Group = "group",
             Value = "width",
             opacity = 0.9,
             zoom = TRUE)

# control colours with a JS ordinal scale
# edited 20 May 2017 with updated code from Renal Chesak's answer:
ColourScale <- 'd3.scaleOrdinal()
            .domain(["lions", "tigers"])
           .range(["#FF6900", "#694489"]);'

forceNetwork(Links = edges, Nodes = nodes, 
             Source = "source",
             Target = "target",
             NodeID ="name",
             Group = "group",
             Value = "width",
             opacity = 0.9,
             zoom = TRUE,
             colourScale = JS(ColourScale))





#########################




name <- c("DABS", 
          "People and Culture", "CoP", "Business", "Power BI", "Shiny Dashboards", "Technical", "R", "Python",
          "Business Value and Application", "Use Cases", 
            "EHP Performance Measurement Framework", "Understand Business", "Build Application",
            "Border Center Program", "Initial Meeting", "Map dashboard",
            "DAS", "Innitial meeting", "Automate reports", "Dashboard for analysis",
            "Web Scraping: Tobacco, Vaping",
            "Support Remic Rapid Innovation",
            "Cipher", "Develop ML model", "Develop web application for prediction",
            "Cyclops", "Provided advice", "Github repo for project tracking",
          "Governance", 
            "HC Data Strategy",
            "Garner IT Score",
          "Environment and Digital Infrastructure",
            "Acquisition of HPC for image processing and DL",
            "Data Science Software at HC",
            "SAS 9.4 migration",
          "Communications",
            "Hiring",
              "Data Scientist with experience in DL",
              "2 students",
            "DASR updates",
            "DABS outlook updates",
            "Conferences",
              "Open House Data Literacy",
              "Forward 50",
              "Data and Big Day",
            "HR-Implementation of new SOMC for DS"
          )

group <- c(0, rep(1, 8), rep(2, 20), rep(3,3), rep(4, 4), rep(5,11))
group <-as.character(group)
id <- c(0:46)

nodes <- data.frame(name, group,id)
# write.csv(Nodes, "C:/Users/MMALAVER/Documents/r_code/presentation_D/nodes.csv")


source<- c(rep(0, 5), 1, rep(2,2), 3, 3, 6,6,#pc
           rep(9, 3), rep(10, 3), rep(11, 2), rep(14, 2), rep(17, 3), rep(22, 2),rep(23, 2),rep(26, 2),
           rep(29, 2),
           rep(32, 2),34,
           rep(36, 4), rep(37, 2),rep(42, 4))
target<- c(1, #Pc 
             9, #BV A
             29, #G 
             32, #EDI
             36, #C
           2,3,6,4,5,7,8, #pc
           10,21,22,11,14,17,12,13,15,16,18,19,20,23,26,24,25,27,28,
           30,31,
           33,34,35,
           37, 40,41,42, 38,39, c(43:46)
           )

links <- data.frame(source, target)


ColourScale <- 'd3.scaleOrdinal()
            .domain(["0","1","2","3","4","5"])
           .range(["#000000", "#2FA4C7", "#BFC2C0" , "#694489", "#FF6900",  "#1FC054"]);'

forceNetwork(Links = links, Nodes = nodes, 
             Source = "source",
             Target = "target",
             NodeID ="name",
             Group = "group",
             # Value = "width",
             opacity = 0.9,
             zoom = TRUE,
             colourScale = JS(ColourScale),
             fontSize = 14)


############################################################


# with sans-serif 
simpleNetwork(networkData, fontFamily = "sans-serif")

# with another font 
simpleNetwork(networkData, fontFamily = "fantasy")

# forceNetwork 
data(MisLinks)
data(MisNodes)


# Create graph
forceNetwork(Links = MisLinks, Nodes = MisNodes, Source = "source",
             Target = "target", Value = "value", NodeID = "name",
             Group = "group", Nodesize = "size", opacity = 1, zoom = F, bounded = T)

# with a simple click action - make the circles bigger when clicked
MyClickScript <- 
  '      d3.select(this).select("circle").transition()
.duration(750)
.attr("r", 30)'

forceNetwork(Links = MisLinks, Nodes = MisNodes, Source = "source",
             Target = "target", Value = "value", NodeID = "name",
             Group = "group", opacity = 1, zoom = F, bounded = T,
             clickAction = MyClickScript)

# showing how you can re-use the name of the clicked-on node (which is 'd')
# You are unlikely to want to do this pop-up alert, but you might want 
# instead to use Shiny.onInputChange() to allocate d.XXX to an element
# input$XXX for user in a Shiny app.
MyClickScript <- 'alert("You clicked " + d.name + " which is in row " + (d.index + 1) +
" of your original R data frame");'
forceNetwork(Links = MisLinks, Nodes = MisNodes, Source = "source",
             Target = "target", Value = "value", NodeID = "name",
             Group = "group", opacity = 1, zoom = F, bounded = T,
             clickAction = MyClickScript)

forceNetwork(Links = MisLinks, Nodes = MisNodes, Source = "source",
             Target = "target", Value = "value", NodeID = "name",
             Group = "group", opacity = 1, zoom = F, bounded = T,
             clickAction = "alert('Ouch!')")

# With a different font, and dimensions chosen to illustrate bounded box
forceNetwork(Links = MisLinks, Nodes = MisNodes, Source = "source",
             Target = "target", Value = "value", NodeID = "name",
             Group = "group", opacity = 1, zoom = F, bounded = T,
             fontFamily = "cursive",
             width = 1500, height = 300)

########USE ME
# With a different font, and node text faintly visible when not hovered over
forceNetwork(Links = MisLinks, Nodes = MisNodes, Source = "source",
             Target = "target", Value = "value", NodeID = "name",
             Group = "group", opacity = 1, zoom = F, bounded = T,
             fontFamily = "cursive", opacityNoHover = 0.3)

# Create graph with legend and varying radius
forceNetwork(Links = MisLinks, Nodes = MisNodes, Source = "source",
             Target = "target", Value = "value", NodeID = "name",
             Nodesize = 'size', radiusCalculation = "d.nodesize",
             Group = "group", opacity = 1, legend = T, bounded = F) 

# Create graph with legend and varying radius and a bounded box
forceNetwork(Links = MisLinks, Nodes = MisNodes, Source = "source",
             Target = "target", Value = "value", NodeID = "name",
             Nodesize = 'size', radiusCalculation = " Math.sqrt(d.nodesize)+6",
             Group = "group", opacity = 1, legend = T, bounded = T) 


# sankeyNetwork
URL <- "https://cdn.rawgit.com/christophergandrud/networkD3/master/JSONdata/energy.json"
Energy <- jsonlite::fromJSON(URL)

# Plot
sankeyNetwork(Links = Energy$links, Nodes = Energy$nodes, Source = "source",
              Target = "target", Value = "value", NodeID = "name",
              fontSize = 12, nodeWidth = 30)

# And with a different font
sankeyNetwork(Links = Energy$links, Nodes = Energy$nodes, Source = "source",
              Target = "target", Value = "value", NodeID = "name",
              fontSize = 12, nodeWidth = 30, fontFamily = "monospace")

# as of 0.2.6 sankeyNetwork supports cycles
# simple network with cycle 5 -> 0
net_cycles <- list(
  links = data.frame(
    source = c(0,0,0,1,1,5),
    target = c(1,2,3,4,5,0),
    value = 10
  ),
  nodes = data.frame(
    name = letters[1:6]
  )
)

# notice how few arguments we need now
# some output but not the nice output I expect
sankeyNetwork(
  net_cycles$links,
  net_cycles$nodes,
  Value = "value"
)


# radialNetwork
Flare <- jsonlite::fromJSON(
  "https://gist.githubusercontent.com/mbostock/4063550/raw/a05a94858375bd0ae023f6950a2b13fac5127637/flare.json",
  simplifyDataFrame = FALSE
)

hc <- hclust(dist(USArrests), "ave")

radialNetwork(List = Flare, fontSize = 10, opacity = 0.9, margin=0)
radialNetwork(as.radialNetwork(hc))

# and with a different font
radialNetwork(List = Flare, fontSize = 10, opacity = 0.9, margin=0, fontFamily = "sans-serif")

diagonalNetwork(List = Flare, fontSize = 10, opacity = 0.9, margin=0)
diagonalNetwork(as.radialNetwork(hc), height = 700, margin = 50)


# dendroNetwork
hc <- hclust(dist(USArrests), "ave")

dendroNetwork(hc, height = 600)
dendroNetwork(hc, treeOrientation = "vertical")

dendroNetwork(hc, height = 600, linkType = "diagonal")
dendroNetwork(hc, treeOrientation = "vertical", linkType = "diagonal")

dendroNetwork(hc, textColour = c("red", "green", "orange")[cutree(hc, 3)],
              height = 600)
dendroNetwork(hc, textColour = c("red", "green", "orange")[cutree(hc, 3)],
              treeOrientation = "vertical")

# chordDiagram
hairColourData <- matrix(c(11975,  1951,  8010, 1013,
                           5871, 10048, 16145,  990,
                           8916,  2060,  8090,  940,
                           2868,  6171,  8045, 6907), nrow = 4)

chordNetwork(data = hairColourData, 
             width = 500, 
             height = 500,
             colourScale = c("#000000", "#FFDD89", "#957244", "#F26223"))

## Adding an image

# Deliverables for next fiscal year
# ![](./DASR.jpg)

## Deliverables for next fiscal year
# ```{r pressure, echo=FALSE, fig.cap="A caption", out.width = '100%', out.height = '20%'}
# knitr::include_graphics("./DASR.jpg")
# ```

