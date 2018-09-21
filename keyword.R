library(dplyr)
library(readxl)
library(stringr)
library(ggplot2)
library(tidyr)
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

fix_word <- function(w) {
   w %>%
   tolower() %>%
   gsub("functional mri", "fmri", .) %>%
   gsub("event.related potentials?.*(\\(?erps?\\)?)?", "erp", .) %>%
   gsub("autism spectrum disorder", "asd", .) %>%
   gsub("^autism$", "asd", .) %>%
   gsub("brain", "", .) %>%
   str_trim # remove surrounding spaces
}

# clean up data
d <- xls %>%
   mutate(year=xls_date(`SbD Date`) %>% strftime("%Y")) %>%
   select(ref=`Article Editorial Ref`,
          year,
          country=`Country Name`,
          keywords=`Article Keywords`)

# by country
p_Country <-
   d %>% group_by(country) %>% tally %>% filter(n>2) %>%
   ggplot() +
   aes(x=country, y=n) +
   geom_bar(stat="identity") +
   theme(axis.text.x = element_text(angle = 75, hjust = 1)) +
   ggtitle("Countries with more than 2 publications")

print(p_Country)

# long format keywords
dkey <-
   d %>%
   # key holds a list of words, unnest makes row per list item
   mutate(key=strsplit(keywords, "[;,]")) %>%
   unnest %>%
   mutate(key=fix_word(key))


# break up keywords
kdf <-
   dkey %>%
   # get a count of all the keywords
   group_by(key, year) %>% tally %>%
   # show only the top
   arrange(-n)

p_keybyyear <-
   kdf %>% filter(n>6) %>%
   # plot
   ggplot +
   aes(x=key, y=n, fill=year) +
   geom_bar(stat="identity", position="dodge") +
   theme(axis.text.x = element_text(angle = 75, hjust = 1))

print(p_keybyyear)
