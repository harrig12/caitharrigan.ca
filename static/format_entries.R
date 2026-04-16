library(tidyverse)
library(htmltools)
library(lubridate)
library(googlesheets4)

gs4_auth('cait.harrigan@mail.utoronto.ca')

#--------------#
# Retrieve data
#--------------#

cv_entries <- read_sheet("1JKlSIuDrfC1wa4V1zf4Si_Tu34upiBEn1DlvFfDeFh0", 
                         sheet = 'new entries', col_types = "llcDDcccccccccc") %>%
  arrange(desc(pmax(year(end), year(begin), na.rm = T)), 
          desc(year(begin)), desc(month(begin))) %>%
  mutate(type = factor(type),
         when = format(begin, "%Om/%y"),
         end = ifelse(end >= now(), "present", format(end, "%Om/%y")),
         end = ifelse(when==end, NA, end),
         when = ifelse(is.na(end), when, str_c(when, " - ", end))
         ) %>%
  filter(!is.na(begin))

#--------------#
# Make buttons
#--------------#

make_buttons <- function(entry){
  buttons <- c()
  if (!is.na(entry['view'])) {
    buttons <- c(buttons, paste0('<a href="', entry['view'],'" class="btn btn-outline-secondary cv-btn"><i class="fa-solid fa-arrow-up-right-from-square"></i> view</a>'))
  }
  if (!is.na(entry['pdf'])) {
    buttons <-  c(buttons, paste0('<a href="', entry['pdf'],'" class="btn btn-outline-secondary cv-btn"><i class="fa-solid fa-file-pdf"></i> pdf</a>'))
  }
  if (!is.na(entry['code'])) {
    buttons <- c(buttons, paste0('<a href="', entry['code'],'" class="btn btn-outline-secondary cv-btn"><i class="fa-solid fa-code"></i> code</a>'))
  }
  return(buttons)
}

#--------------#
# Make entry
#--------------#

summarize_multi_year <- function(entries){
  entries %>%
    group_by(regular1, regular2, begin) %>%
    summarize(n=n()) %>%
    group_by(regular1) %>%
    mutate(n = ifelse(n==1, '', paste0(' x',n))) %>%
    summarise(regular2 = paste0(year(begin), n, collapse=', ')) %>%
    mutate(regular2 = paste0(regular1, ' (', regular2, ')')) %>%
    pull(regular2) %>%
    paste0(collapse = ', ')
}


format_info <- function(info){
  info %>%
    mutate(
      bold = paste0('<b class="entry-bold">', bold, '</b>'),
      italic = paste0('<i class="entry-italic">', italic, '</i>'),
    ) 
}

print_entry <- function(entry, squish=F, full_width=F){
  entry <- as_tibble_row(entry)
  
  lines <- ifelse(full_width, c('<div class="grid pub">'), c('<div class="grid cv-entry">'))
  lines <- c(lines, '<div class="g-col-1">')
  
  # entry info
  info <- entry %>% select(bold:italic)
  info <- format_info(info)[!is.na(info)]
  
  info <- ifelse(
    squish, 
    paste0(info, collapse = ', '),
    paste0(info, collapse = '</p><p class="entry-txt">')
  )
  
  lines <- c(lines, paste0('<p class="entry-txt">', info, '</p>'))
  lines <- c(lines, '</div> <div class="g-col-1">')
  # when
  if (!is.na(entry[['when']])){
    lines <- c(lines, '<span class="entry-when">', entry[['when']], '</span>')
  }
  # entry may have buttons 
  if( any(c(!is.na(entry[['view']]), !is.na(entry[['pdf']]), !is.na(entry[['code']]))) ){
    lines <- c(lines, make_buttons(entry))
  }
  lines <- c(lines, '</div> </div>')
  HTML(lines)
}


print_section <- function(entries, squish=F, full_width=F){
   paste(apply(entries, 1, print_entry, squish=squish, full_width=full_width), collapse = "")
}

