#' @title WikiCateg()
#' @description
#' function that extracts the categories of a Wikipedia page
#' @param pagetitle is the title of the page given in input by the user and has to be a string
#' @example WikiCateg("Machine learning") extracts the categories of the page "Machine learning"
#' @import tibble
#' @import dplyr
#' @import rvest
#' @import caret
#' @import readxl
#' @export
WikiCateg <- function(pagetitle) {
  pagetitle_no_space <- gsub(" ", "_", pagetitle)
  url <- paste("https://en.wikipedia.org/wiki/", pagetitle_no_space, sep = "")
  wiki<- html(url)
  categs_of_page <- wiki %>%
    html_nodes(".mw-normal-catlinks li a") %>%
    html_text()
  return(categs_of_page)
}


#' @title Scholar_is_it_a_method()
#' @description
#' function that web scrapes google scholar and determines whether a specific entity is a method or not
#' @param entity is given in input by the user and is a string
#' @example Scholar_is_it_a_method("Machine learning") will scrape google scholar and search for articles which have a phrase like "Machine learning is a method", if such a phrase is found, then the function returns "method"
#' @export
Scholar_is_it_a_method <- function(entity) {
  first_part_ofURL <- "https://scholar.google.com/scholar?hl=it&as_sdt=0%2C5&q=%22"
  last_part_ofURL <- "%22&btnG="
  entity_no_space <- gsub(" ", "+", entity)
  final_url <- paste(first_part_ofURL,entity_no_space,"+is+a+method", last_part_ofURL, sep = "")
  scholar <- html(final_url)
  searchResults <- scholar %>%
    html_nodes("#gs_ab_md div") %>%
    html_text()
  if (searchResults == "") {
    string <- "not a method"
  } else {
    string <- "method"
  }
  return(string)
}

#' @title is_it_aTech()
#' @description
#' function that predicts the class of a new entity through a pre trained KNN classifier
#' @param entity is a string and represents the entity a user wants to know if it is a technology or not
#' @example is_it_aTech("Machine learning") will return "technology" if the classifier predicts it's a technology, or "other" if the classifier predicts it is not a technology
#' @import tibble
#' @export
is_it_aTech <- function(entity) {
  my_model <- readRDS("myclassifier.rds")
  trainingdata_methodsvsTechs <- read_excel("trainingdata.xlsx")
  data <- trainingdata_methodsvsTechs
  data$class <- as.factor(data$class)
  new_data <- data %>%
    remove_rownames %>%
    column_to_rownames(var="entities")
  categories<- colnames(new_data)
  categories_final <- categories[categories != "class"]
  entity_1 <- gsub(" ", "_", entity, fixed=TRUE)
  url_wiki <- paste("https://en.wikipedia.org/wiki/", entity_1, sep = "")
  entity_page <- html(url_wiki)
  cat_of_entity <- entity_page %>%
    html_nodes(".mw-normal-catlinks li a") %>%
    html_text()
  summary_of_entity <- entity_page %>%
    html_nodes(".mw-parser-output > p:nth-of-type(3)") %>%
    html_text()
  one_hot <- c()
  for (c in categories_final) {
    if (c %in% cat_of_entity || grepl(tolower(c), tolower(summary_of_entity)) == TRUE) {
      one_hot <- c(1, one_hot)
    }else {
      one_hot <- c(0, one_hot)
    }
  }
  new_row <- data.frame(categs = categories_final, value = rev(one_hot))
  transposed <- t(new_row)
  colnames(transposed) = transposed[1, ]
  transposed = transposed[-1, ]
  transposed_x2 <- t(transposed)
  df <- as.data.frame(transposed_x2)
  for (col in categories_final) {
    df[, col] <- as.numeric(as.character(df[, col]))
  }
  prediction_newentry <- predict(my_model, newdata = df)
  return(prediction_newentry)
}


#' @title all_in_one()
#' @description
#' function that uses functions "Scholar_is_it_a_method()" and "is_it_aTech()" to determine whether an entity is a technology, method or neither of them
#' @param vector is a list of entities
#' @example
#' list <- c("Machine learning", "Microsoft office", "Graphene")
#' all_in_one(list) returns a dataframe with the type of entity returned by the function for each element of the input list
#' @export
all_in_one <- function(vector){
  results <- c()
  for (v in vector) {
    Sys.sleep(5)
    first_part_ofURL <- "https://scholar.google.com/scholar?hl=it&as_sdt=0%2C5&q=%22"
    last_part_ofURL <- "%22&btnG="
    entity_no_space <- gsub(" ", "+", v)
    final_url <- paste(first_part_ofURL,entity_no_space,"+is+a+method", last_part_ofURL, sep = "")
    scholar <- html(final_url)
    searchResults <- scholar %>%
      html_nodes("#gs_ab_md div") %>%
      html_text()
    if (searchResults != "") {
      results <- c("method", results)
    } else {
      results <- c(is_it_aTech(v), results)
    }
  }
  dataframe <- data.frame(entity= vector, type = rev(results))
  dataframe$type <- as.character(dataframe$type)
  results_df <- dataframe %>%
    mutate(type = ifelse(type == "2", "Technology", ifelse(type == "1", "other", type)))
  return(results_df)
}

