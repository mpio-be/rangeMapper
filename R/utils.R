

#' Thinning of polygonal grids
#'
#' Nearest neighbours spatial thinning of polygonal grids
#'
#' @param  x   an sf data.frame.
#' @param lag lag order. 
#' 
#' @return a thinned [sf::st_as_sf()] object.
#' 
#' @export
#' @md
#' 
#' @note
#' This function is still under development. 
#'
#' @references 
#' Based on SO answer: https://stackoverflow.com/questions/65907022/
#' 
#' @examples
#'\dontrun{
#' require(rangeMapper)
#' con = rmap_connect()
#' wrens = read_wrens()
#' rmap_add_ranges(con, x = wrens, ID = 'sci_name')
#' rmap_prepare(con, 'hex', cellsize=500)
#' rmap_save_map(con) 
#' x = rmap_to_sf(con)[, 'cell_id']
#' 
#' plot( st_thin(x,2) )
#' 
#' x = x[ ! x$cell_id  %in%  c(282,265) , ]
#' 
#' plot( st_thin(x,3) )
#' 
#' }

st_thin <- function(x, lag) UseMethod("st_thin")

thin_contiguous <- function(x, order = 2) {

    if(order < 2) stop('order must be larger than 1')

    x = x[sample(nrow(x)),]

    xgraph = spdep::poly2nb(x)  
    xgraph = igraph::graph_from_adj_list(xgraph)

    rm_ids    = igraph::ego(xgraph, order = 1, nodes = 1)[[1]]
    higher_nb = igraph::ego(xgraph, order = 2, nodes = 1)[[1]]
    out       = igraph::difference(higher_nb, rm_ids)

    i = 1
    while (TRUE) {
        if (i > length(out)) break

        id  = out[[i]]

        idi         = igraph::ego(xgraph, order = order-1, nodes = id)[[1]]
        diffi       = igraph::difference(idi, igraph::V(xgraph)[id])
        rm_ids      = igraph::union(rm_ids, diffi  )
        higher_idi  = igraph::ego(xgraph, order = order, nodes = id)[[1]] 
        higher_diff = igraph::difference(higher_idi ,  idi )
        out         = igraph::difference(igraph::union(out, higher_diff), rm_ids)
        
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

    z = find_contiguous(x)  
    z = split(z, z$gid)

    # large blocks
    z_large = z[sapply(z, nrow) > lag]

    o1 = lapply(z_large, thin_contiguous, order = lag)
    o1 = do.call(rbind, o1)

    o2 =  z[sapply(z, nrow) <= lag]
    o2 = do.call(rbind, o2)

    o = rbind(o1, o2)    


    o$gid = NULL

    o

    }    

