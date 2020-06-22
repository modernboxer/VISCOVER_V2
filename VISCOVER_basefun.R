GetCDLcountysmy <- function(stateID, countyID, year, county_fips, counties, states)
{

  baseurl <- "https://nassgeodata.gmu.edu/axis2/services/CDLService/GetCDLFile?"
  crs.cropscape <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
  County <- county_fips %>% filter(state== stateID, county %in% countyID) 
  countyinfor <- data.frame()
  for(i in 1:dim(County))
  {
    if(as.numeric(County$fips[i]) < 100)
      countyinfor <- rbind(countyinfor, County[i,] %>% left_join(., states))
    if(as.numeric(County$fips[i]) > 100)
      countyinfor <- rbind(countyinfor, County[i, ] %>% left_join(., counties))
  }

  if(dim(countyinfor)[1] == 0) 
    return(NULL)
  
  cty_summary <- data.frame()
  for(i in 1:dim(countyinfor)[1])
  {
    poly_county <- countyinfor$geom[i] %>% as_Spatial()
    bb.poly <- methods::as(raster::extent(poly_county), "SpatialPolygons")  
    sp::proj4string(bb.poly) <- sp::CRS("+init=epsg:4326")
    bb.poly.proj <- sp::spTransform(bb.poly, crs.cropscape)
    b2 <- as.vector(sp::bbox(bb.poly.proj))
    for(j in year)
    {
      url <- paste0(baseurl, "year=", j, "&bbox=", glue::glue_collapse(b2, sep = ","))
      html <- RCurl::getURL(url, .opts = list(ssl.verifypeer = FALSE), crlf = TRUE)
      doc <- XML::xmlTreeParse(html)
      top <- XML::xmlRoot(doc)
      tifurl <- XML::xmlValue(top[[1]][["text"]])
      dir.create("temp", showWarnings = FALSE)
      destfile <- paste("temp", "tmp.tif", sep = "/")
      if (Sys.info()["sysname"] == "Windows") {
        downloader::download(tifurl, destfile, mode = "wb",
                             quiet = TRUE)
      }
      if (Sys.info()["sysname"] != "Windows") {
        utils::download.file(tifurl, destfile = destfile, mode = "wb",
                             method = "curl", extra = "-k")
      }
      cdl_raster <- raster::raster(destfile)
      county_raster <- mask(cdl_raster, bb.poly.proj)
      tbl_freq <- county_raster %>% raster::getValues() %>% table %>% as.data.frame %>% 
        "names<-"(c("code", "Freq"))
      tbl_freq$code <- as.numeric(tbl_freq$code)
      cty_summary <- rbind(cty_summary, cbind(year = j, state = County$state[i], 
                                              county = County$county[i], 
                                              fips = County$fips[i],
                                              tbl_freq))
    }
  }
  cty_summary %>% nest(-year, -state, -county, -fips) %>% return()
}




datatable2 <- function(x, vars = NULL, opts = NULL, ...) {
  
  names_x <- names(x)
  if (is.null(vars)) stop("'vars' must be specified!")
  pos <- match(vars, names_x)
  if (any(map_chr(x[, pos], typeof) == "list"))
    stop("list columns are not supported in datatable2()")
  
  pos <- pos[pos <= ncol(x)] + 1
  rownames(x) <- NULL
  if (nrow(x) > 0) x <- cbind(' ' = '&oplus;', x)
  
  # options
  opts <- c(
    opts, 
    list(
      columnDefs = list(
        list(visible = FALSE, targets = c(0, pos)),
        list(orderable = FALSE, className = 'details-control', targets = 1),
        list(className = 'dt-left', targets = 1:3),
        list(className = 'dt-right', targets = 4:ncol(x))
      )
    ))
  
  datatable(
    x, 
    ...,
    escape = -2,
    options = opts,
    callback = JS(.callback2(x = x, pos = c(0, pos)))
  )
}

.callback2 <- function(x, pos = NULL) {
  
  part1 <- "table.column(1).nodes().to$().css({cursor: 'pointer'});"
  
  part2 <- .child_row_table2(x, pos = pos)
  
  part3 <- 
    "
   table.on('click', 'td.details-control', function() {
    var td = $(this), row = table.row(td.closest('tr'));
    if (row.child.isShown()) {
      row.child.hide();
      td.html('&oplus;');
    } else {
      row.child(format(row.data())).show();
      td.html('&ominus;');
    }
  });"
  
  paste(part1, part2, part3)
} 

.child_row_table2 <- function(x, pos = NULL) {
  
  names_x <- paste0(names(x), ":")
  text <- "
  var format = function(d) {
    text = '<div><table >' + 
  "
  
  for (i in seq_along(pos)) {
    text <- paste(text, glue::glue(
      "'<tr>' +
          '<td>' + '{names_x[pos[i]]}' + '</td>' +
          '<td>' + d[{pos[i]}] + '</td>' +
        '</tr>' + " ))
  }
  
  paste0(text,
         "'</table></div>'
      return text;};"
  )
}







# VISCOVER BASE CODE
GetCDLFile <- function(year, b)
{
  bb.poly <- methods::as(raster::extent(b), "SpatialPolygons")   
  sp::proj4string(bb.poly) <- sp::CRS("+init=epsg:4326")
  baseurl <- "https://nassgeodata.gmu.edu/axis2/services/CDLService/GetCDLFile?"
  crs.cropscape <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
  bb.poly.proj <- sp::spTransform(bb.poly, crs.cropscape)
  b2 <- as.vector(sp::bbox(bb.poly.proj))
  url <- paste0(baseurl, "year=", year, "&bbox=", glue::glue_collapse(b2, sep = ","))
  html <- RCurl::getURL(url, .opts = list(ssl.verifypeer = FALSE), crlf = TRUE)
  doc <- XML::xmlTreeParse(html)
  top <- XML::xmlRoot(doc)
  tifurl <- XML::xmlValue(top[[1]][["text"]])
  dir.create("temp", showWarnings = FALSE)
  destfile <- paste("temp", "tmp.tif", sep = "/")
  if (Sys.info()["sysname"] == "Windows") {
    downloader::download(tifurl, destfile, mode = "wb",
                         quiet = TRUE)
  }
  if (Sys.info()["sysname"] != "Windows") {
    utils::download.file(tifurl, destfile = destfile, mode = "wb",
                         method = "curl", extra = "-k")
  }
  cdl_raster <- raster::raster(destfile)
  return(cdl_raster)
}


GetCDLValue <- function(year, lon, lat)
{
  if (length(year) != 1 | length(lon) != 1 | length(lat) !=
      1) {
    stop("One year and one location at a time!")
  }
  pt <- cbind(lon = lon, lat = lat) %>% sp::SpatialPoints(proj4string = sp::CRS("+init=epsg:4326"))
  baseurl <- "https://nassgeodata.gmu.edu/axis2/services/CDLService/GetCDLValue?"
  crs.cropscape <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
  pt.proj <- sp::spTransform(pt, crs.cropscape)
  pt2 <- sp::coordinates(pt.proj)
  url <- paste0(baseurl, "year=", year, "&x=",
                pt2[[1]], "&y=", pt2[[2]])
  html <- RCurl::getURL(url, .opts = list(ssl.verifypeer = FALSE),
                        crlf = TRUE)   #XML
  doc <- XML::xmlTreeParse(html)
  top <- XML::xmlRoot(doc)
  text <- top[[1]][["text"]] %>% XML::xmlValue()        
  #eg: "{x: 246757.749483317, y: 2114807.52236869, value: 5, category: \"Soybeans\", color: \"#267300\"}"
  text.split <- strsplit(text, ": |, ") %>% unlist
  value <- text.split[6]                                     #the value 
  category <- gsub("\"", "", text.split[8])
  color <- gsub("\"|}", "", text.split[10])    
  return(list(value = value, category = category, color = color))
}

GetSDLValue <- function(lon, lat)
{
  if (length(lon) != 1 | length(lat) != 1) {
    stop("One location at a time!")
  }
  pt <- matrix(c(lon, lat), nc = 2, byrow = T)
  circ <- dismo::circles(pt, d = 0.1, lonlat = TRUE)
  p <- rgeos::writeWKT(circ@polygons)
  q <- paste0("SELECT areasymbol, musym, mukey, muname, muacres\n          
              FROM mapunit mu\n          
              INNER JOIN legend on legend.lkey = mu.lkey    \n          
              WHERE mukey IN (\n          \
              SELECT * from SDA_Get_Mukey_from_intersection_with_WktWgs84('",
              p, "')\n          )")
  qres <- soilDB::SDA_query(q)
  if (nrow(qres) > 1) {
    warning(sprintf("%.0f soil mapunits are found, choose the first one",
                    nrow(qres)))
    qres <- qres[1, ]
  }
  qres
}

TileinPoly <- function (tile, poly)
{
  sp::proj4string(poly) <- sp::CRS("+init=epsg:4326")
  poly2 <- sp::spTransform(poly, sp::proj4string(tile))
  #sp::proj4string(tile)
  #"+proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs"
  tilesub <- raster::crop(tile, sp::bbox(poly2))     #first cut the area which includes poly2
  tilepoint <- raster::rasterToPoints(tilesub, spatial = T)
  joint <- tilepoint[sp::geometry(poly2), ]   # part of the area
  count <- joint@data %>% table %>% raster::as.data.frame()
  return(count)
}