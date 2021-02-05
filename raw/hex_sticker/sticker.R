
# packages,settings
    sapply(c('rangeMapper', 'magrittr', 'sf', 'sfheaders', 'data.table',
            'ggplot2', 'ggimage', 'cowplot',  'viridis', 'scales', 
             'hexSticker', 'showtext', 
             'magick'), 
    require, character.only = TRUE, quietly = TRUE)

    con = rmap_connect()

    font_add_google("Chewy", 'Chewy')
    showtext_auto()

# data
    rmap_add_ranges(con, x = wrens, ID = 'sci_name')
    rmap_prepare(con, 'hex', cellsize=650, chunksize = 1)
    rmap_add_bio(con, wrens, 'sci_name')
    rmap_save_map(con)
    x = rmap_to_sf(con, 'species_richness')


    # https://en.wikipedia.org/wiki/Musician_wren   (public domain image)                 21.15
    cypara = image_read('./raw/hex_sticker/Cyphorhinus_aradus.png')     %>% 
          image_trim() %>% image_convert(colorspace = 'gray')  %>% 
          image_flop  %>% 
          image_colorize(70, '#00204DFF')

    # https://en.wikipedia.org/wiki/Grey-mantled_wren (public domain image)               17.25
    odobra = image_read('./raw/hex_sticker/Odontorchilus_branickii.png') %>% 
          image_trim() %>% image_convert(colorspace = 'gray')  %>% 
          image_flop  %>% 
          image_colorize(70, '#00204DFF')

    # https://en.wikipedia.org/wiki/Sepia-brown_wren (public domain image)               23
    cinoli = image_read('./raw/hex_sticker/Cinnycerthia_olivascens.png') %>% 
      image_trim() %>% image_convert(colorspace = 'gray')  %>% 
      image_colorize(70, '#00204DFF')


# inset hex
    L = 0.75      

    pg = 
    ggplot() + geom_hexagon(fill = "#E4CF5BFF" , color = "#FFEA46FF", size = 0.1) +
    

    annotation_raster(cypara, xmin= 1 - L/2,   xmax= 1 + L/2 ,   ymin= 1.4 - L/2, ymax= 1.4 + L/2 )  + 
    annotation_raster(odobra, xmin= 0.6 - L/2, xmax= 0.6 + L/2 , ymin= 0.65 - L/2, ymax= 0.65 + L/2 )  + 
    annotation_raster(cinoli, xmin= 1.4 - L/2, xmax= 1.4 + L/2 , ymin= 0.6 - L/2, ymax= 0.6 + L/2 )  + 
    theme_sticker()
   


    mg = 
    ggplot(x ) + 
      geom_sf(aes(fill = species_richness),  size= 0.05, col ='#FFEA46FF', show.legend = FALSE) + 
      scale_fill_gradientn(colours = viridis(10, option = 'E') ) +
      theme(plot.margin = unit(c(0,0,0,0), "cm") ) +
      theme_void() +
      theme_transparent() 


    gg = ggdraw(mg) + draw_plot(pg, x=0.50, y=0.2, scale = 0.5)
        



    sticker(
      filename   = "man/figures/logo.png",
      subplot    = gg ,
      package    = " rangeMapper",
      
      s_x        = 0.7,
      s_y        = 0.8,
      s_width    = 1.5,
      s_height   = 1.5,
      
      p_x        = 1,
      p_y        = 1.65,
      p_size     = 14,
      p_color    = "#00204DFF",
      p_family   = "Chewy",

      h_color    = "#00204DFF",
      h_fill     = "#c3c4c7"  ,
      h_size     = 0.4
      )
      
