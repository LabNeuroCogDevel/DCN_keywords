library(dplyr)
library(readxl)
library(stringr)
library(ggplot2)
library(cowplot)

xls <- read_xlsx("DCN_EVISE_submission_data.xlsx")

xls_date <- function(x, n=1) {
   # if we have a date and min is near 2015, dont do anything
   if ("POSIXct" %in% class(x) &&
      min(as.numeric(strftime(x, "%Y")) - 2015) <= 2){
      return(x)
   }
   cat("XLS dates are off:", range(na.omit(x)), "\nfixing\n")
   x<- as.POSIXct( (as.numeric(x)+n)*60*60*24, origin="1899-12-30", tz="GMT")
   cat("new range:", range(na.omit(as.numeric(strftime(x, "%Y")))), "\n")
   return(x)
}



d <- xls %>%
   mutate(year=xls_date(`SbD Date`) %>% strftime("%Y")) %>%
   select(country=`Country Name`, year, keywoards=`Article Keywords`) %>%


# break up keywords
k <-
   xls$`Article Keywords` %>%
   strsplit("[;,]") %>% do.call(c, .) %>%
   tolower() %>%
   gsub("functional mri", "fmri", .) %>%
   gsub("event.related potentials?.*(\\(?erps?\\)?)?", "erp", .) %>%
   gsub("autism spectrum disorder", "asd", .) %>%
   gsub("^autism$", "asd", .) %>%
   gsub("brain", "", .) %>%
   str_trim

kdf <-
   data.frame(keyword=k) %>%
   # get a count of all the keywords
   group_by(keyword) %>% tally %>%
   # show only the top
   arrange(-n)

kdf %>% head(n=20) %>%
   # plot
   ggplot +
   aes(x=keyword, y=n) +
   geom_bar(stat="identity") +
   theme(axis.text.x = element_text(angle = 75, hjust = 1))
