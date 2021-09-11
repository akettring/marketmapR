
library("tidyverse")
library("reshape2")
library("tidyquant")
library("ComplexHeatmap")
library("circlize")


###########################
# FUNCTIONS
###########################


# choose row labels based on recent average
get_labels <- function (df=NULL, n_top=NULL, n_labels=NULL, label_at=NULL) {

    # sort the list and get top/bottom values
    lab_list <- df %>% tail(n_top) %>% colMeans() %>% sort()
    top_labs <- lab_list %>% tail(n_labels) %>% names()
    bot_labs <- lab_list %>% head(n_labels) %>% names()

    # return either top, bottom, or both
    if (label_at=="both") {
        labs <- c(top_labs,bot_labs)
    } else if (label_at=="top") {
        labs <- top_labs
    } else if (label_at=="bottom") {
        labs <- bot_labs
    }

    # get the labels and their locations in the dataframe
    lab_locs <- which(colnames(df) %in% labs)                   # find the locations in the df
    labs <- colnames(df)[lab_locs]                              # re-sort based on locations
    return( list(labs, lab_locs) )
}


# group using sp500 index, combo of NYSE+NASDAQ, or don't
# note: combo index has almost as many categories as equities
# it is recomended to use the combo option with less than 50 equities

get_groups <- function(df=NULL, group_idx=NULL) {
    row_split <- NULL
    if (group_idx=="sp500") {
        df_exchange <- tq_index("sp500")
        temp_key <- df_exchange$sector
        names(temp_key) <- df_exchange$symbol
        row_split <- temp_key[names(df)]
    } else if (group_idx=="combo") {
        df_exchange <- rbind(
            tq_exchange("NYSE"), tq_exchange("NASDAQ") )
        temp_key <- df_exchange$industry
        names(temp_key) <- df_exchange$symbol
        row_split <- temp_key[names(df)]
    }
    return(row_split)
}



market_map <- function(df=NULL,             # dataframe that contains the variable of interest
                       valvar=NULL,         # column with variable of iterest
                       n_labels=0,          # number of labels to display
                       n_top=5,             # length of tail to consider for sorting labels
                       label_at="top",      # label at top, bottom, or both?
                       group_idx=NULL,      # group using "sp500" index, "combo" of NYSE+NASDAQ, or don't
                       detrend=FALSE ){     # detrend the time-series aginast market baseline fluctuations

    # if valvar isn't declared, use the last column of the dataframe
    if ( is.null(valvar) ){
        valvar <- colnames(df) %>% tail(1)
    }

    # cast the data for heatmapping
    df <- df %>% dcast(date~symbol, value.var = valvar)
    rownames(df) <- df$date
    df$date <- NULL
    df <- df %>% xts(order.by = as.Date(rownames(df)))

    # cleanup na's
    df <- df[rowSums(!is.na(df)) > 0,]                                # remove rows with *only* na
    df <- df %>% na.locf() %>% na.fill(median(df, na.rm = TRUE))      # fill remaining na

    # de-trend time series if desired
    if (detrend==TRUE){
        colMed <- apply(df,1,median)
        df <- sweep(df,1,colMed,"-")
    }

    # split columns by the month
    col_split <- df %>% index() %>% as.Date() %>% format("%y-%m")

    # split rows by sector
    row_split <- get_groups(df, group_idx)

    # row label annotations
    if (n_labels > 0){
        labs <- get_labels(df, n_top, n_labels, label_at)
        ha <- rowAnnotation(foo =
                anno_mark(
                    at = labs[[2]],
                    labels = labs[[1]],
                    link_width = unit(15, "mm"),
                    labels_gp = gpar(fontsize=6)
        ))
    } else {
        ha <-NULL
    }

    # colors of finance: red, black, green
    col_fun <- colorRamp2(
        c(min(df), median(df), max(df)),
        c("red", "black", "green"))

    # make the heatmap
    df %>% as.data.frame() %>% t() %>%
        Heatmap(.,
        row_split = row_split,
        column_split = col_split,
        show_column_names = FALSE,
        show_row_names=FALSE,
        right_annotation = ha,
        cluster_columns = FALSE,
        cluster_column_slices = FALSE,
        column_title_rot = 90,
        row_title_rot = 0,
        col=col_fun,
        row_gap = unit(0.5, "mm"),
        column_gap = unit(0.5, "mm"),
        name = valvar
        )

}


###########################
# USER DEFINED INPUTS
###########################

# # 1a. get the data
# symbol_list <- tq_index("SP500")$symbol %>% sample(50) # sp500 equities
# df <- tq_get(symbol_list, from = "2020-12-01")              # date range
# write.table(df, file = "demo/sp500.tsv", row.names=FALSE, sep="\t")

# 1b. load the data
df <- read.delim("demo/sp500.tsv", header=TRUE, sep="\t") %>% as_tibble()
df$date <- df$date %>% as.Date()


# 2. add the indicator to dataframe
df_plot <- df %>%
    na.omit(target.colnames="close") %>%                    # filter missing values
    group_by(symbol) %>%                                    # sort by symbol
    tq_mutate(                                              # technical indicator
        select = c(high, low, close),                                     # value to transform
        mutate_fun = stoch)


# 3. make the plot
png("demo/sp500.png",
    width = 11, height = 8.5, units = "in", res=600)


market_map(df_plot,
           valvar = "stoch",          # if valvar isnt defined, the last column in the df is used
           n_labels = 25,           # how many labels?
           n_top = 5,               # length of tail to consider for sorting labels
           label_at = "both",       # label both the top and bottom performing stocks
           group_idx = "sp500",     # for mapping non-sp500 equities, use "combo" (<50 stocks) or NULL (>50 stocks)
           detrend = TRUE           # removing baseline market shifts can sometimes make trends more clear
)

dev.off()

