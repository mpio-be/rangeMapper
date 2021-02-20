

#' Thinning of polygonal grids
#'
#' Nearest neighbours spatial thinning of polygonal grids
#'
#' @param  x  an sf data.frame
#' @param lag lag order. 
#' @export
#' 
#' @note
#' This function is still under developement. 
#'
#' @references 
#' Based on SO answer: https://stackoverflow.com/questions/65907022/
#' 
#' @return what type of object does it return. 
#' 
#' @examples
#' require(rangeMapper)
#' require(data.table)
#' con = rmap_connect()
#' data(wrens)
#' rmap_add_ranges(con, x = wrens, ID = 'sci_name')
#' rmap_prepare(con, 'hex', cellsize=250)
#' rmap_save_map(con) 
#' x = rmap_to_sf(con)[, 'cell_id']
#' 
#' x = x[x$cell_id != 356, ]
#' ggplot(x) + geom_sf() + geom_sf_label( aes(label = cell_id))
#' 
#' plot( st_thin(x,5) %>% st_geometry )
#' 

st_thin <- function(x, order) UseMethod("st_thin")

thin_contiguous <- function(x, order = 2) {

    if(order < 2) stop('order must be larger than 1')

    x = x[sample(nrow(x)),]

    xgraph = spdep::poly2nb(x)  %>%  graph_from_adj_list

    rm_ids    = ego(xgraph, order = 1, nodes = 1)[[1]]
    higher_nb = ego(xgraph, order = 2, nodes = 1)[[1]]
    out       = difference(higher_nb, rm_ids)

    i = 1
    while (TRUE) {
        if (i > length(out)) break

        id  = out[[i]]

        idi         = ego(xgraph, order = order-1, nodes = id)[[1]]
        diffi       = difference(idi, V(xgraph)[id])
        rm_ids      = union(rm_ids, diffi  )
        higher_idi  = ego(xgraph, order = order, nodes = id)[[1]] 
        higher_diff = difference(higher_idi ,  idi )
        out         = difference(union(out, higher_diff), rm_ids)
        
        i = i + 1
        }

    x[ c(1, out) , ]    
    }

find_contiguous <- function(x) {
    u = st_union(x)  %>% 
        st_cast("POLYGON")  %>% 
        st_as_sf
    u$gid = 1:nrow(u)    

    st_agr(u) = "constant"
    st_agr(x) = "constant"

    st_intersection(x, u)

    }

#' @export
st_thin.sf <- function(x, lag) {

    z = find_contiguous(x)  %>% setDT
    z[, n := .N, gid]

    xs = z[n > lag ]

    o = xs[, st_as_sf(.SD)  %>% thin_contiguous(order = lag), by = gid]

    o = rbind(o,  z[n <= lag]  )

    st_as_sf(o)


}    









