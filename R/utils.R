# ==========================================================================
# Unexported functions
# ==========================================================================


# is a db table empty?
# return logical vector of length 1.
is_empty <- function(con, tab) {
    o = dbGetQuery(con, glue('SELECT count(*) FROM (select 1 from {tab} limit 1)') ) %>% 
        as.logical  
    !o    
    }


# write to rmap_master table; overwrite when type and name exists
write_master <- function(con, type, name, source) {
    e = dbGetQuery(con, glue('SELECT type, name FROM rmap_master 
                            WHERE type = {shQuote(type)} AND name = {shQuote(name)}') )
    if(nrow(e)>0)
        dbExecute(con, glue('DELETE FROM rmap_master
                            WHERE type = {shQuote(type)} AND name = {shQuote(name)}') )

    x = data.frame(type, name, source)
    dbWriteTable(con, 'rmap_master', x, row.names = FALSE, append = TRUE)

    }

# does table exists in rmap_master?
exists_in_master <- function(con, nam) {
    exists = DBI::dbGetQuery(con, 
    glue::glue("SELECT name FROM rmap_master WHERE name='{nam}';") )  %>% 
    nrow  %>% 
    is_greater_than(0)   
    
    }

# get rmap_master joined with sqlite_master
get_master <- function(con) {
    x = dbGetQuery(con, 
        'SELECT m.pk, m.name, m.type rmap_type, s.type sqlite_type 
            FROM rmap_master m JOIN sqlite_master s on m.name = s.name')
    data.table(x)   
    }

# strip down string so sql does not complain
make_sql_nams <- function(s) {
    s  %>% 
    make.names %>%  
    gsub('\\W+','_', .)  %>% 
    gsub('^_|_$', '', .)
    }

drop_table_or_view <- function(x, con) {

    m = RSQLite::dbGetQuery(con, 
            glue('SELECT type, name FROM sqlite_master WHERE name = {shQuote(x)}')
        )

    if(nrow(m) > 0) {
        this = m$type 
        dbExecute(con, glue("DROP {this} IF EXISTS {x}") )

        }
}


talk <- function() {
    getOption('rmap.verbose')
}

