
library("tidyverse")
library("reshape2")
library("tidyquant")
library("ComplexHeatmap")
library("circlize")


symbol_list <- tq_index("SP500")$symbol
    # %>% sample(100)
    # c("A","TMO", "DS")

df <- tq_get(symbol_list, from = "2020-12-01")

# mutate the series, applying the transformation, and add the new column
df <- df %>% group_by(symbol) %>%
    tq_mutate(
        select = close,
        mutate_fun = RSI,
        n = 14,
        col_rename = "RSI")

# cast the long data for heatmapping
df <- df %>% dcast(date~symbol, value.var = "RSI")
rownames(df) <- df$date
df$date <- NULL
df <- df %>% xts(order.by = as.Date(rownames(df)))
df <- df[rowSums(!is.na(df)) > 0,]                  # remove rows with *only* na
df <- df %>% na.locf() %>% na.fill(median(df, na.rm = TRUE))      # handle remaining na

# color: red, black, green
col_fun <- colorRamp2(
    c(min(df), median(df), max(df)),
    c("red", "black", "green"))

# split columns by the month
col_split <- df %>% index() %>% as.Date() %>% format("%y-%m")

# get the sector, use this to split the stonks
sp500 <- tq_index("SP500")
temp_key <- sp500$sector
names(temp_key) <- sp500$symbol
row_split <- temp_key[names(df)]

# get most recent avg, use this to choose labels
labs <- df %>% tail(5) %>% colMeans() %>% sort() %>% tail(50) %>% names()   # the list is sorted by magnitude
lab_locs <- which(colnames(df) %in% labs)                                   # find the locations in the df
labs <- colnames(df)[lab_locs]                                              # re-sort based on locations

ha <- rowAnnotation(foo =
        anno_mark(
            at = lab_locs,
            labels = labs,
            link_width = unit(15, "mm"),
            labels_gp = gpar(fontsize=6)
))

png("demo/hmap_rsi.png", width = 11, height = 8.5, units = "in", res=600)
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
    row_gap = unit(0.25, "mm"),
    column_gap = unit(0.25, "mm"),
    # border=TRUE,
    name = "RSI"
    )
dev.off()

