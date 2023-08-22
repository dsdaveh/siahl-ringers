set.seed(19)

library(randomNames)
#library(lexicon)
library(words)
library(Zoo)

randomNames(n,
            gender,
            ethnicity,
            which.names="both",
            name.order="last.first",
            name.sep=", ",
            sample.with.replacement=TRUE,
            return.complete.data=FALSE)


randomNames(5, which.names = "both", sample.with.replacement = FALSE, 
            name.order = "first.last", name.sep = " ")


generate_pseudonyms <- function() {
    # Generate a random first name
    first_name <- randomNames(1, which.names = "first")
    
    # Get the first letter of the first name
    first_letter <- substr(first_name, 1, 1)
    
    # Get color, animal, and plant names starting with the same letter
    colors <- crayon::crayon_colors[stringr::str_starts(names(crayon::crayon_colors), first_letter, ignore.case = TRUE)]
    animals <- crayon::crayon_animals[stringr::str_starts(names(crayon::crayon_animals), first_letter, ignore.case = TRUE)]
    plants <- crayon::crayon_plants[stringr::str_starts(names(crayon::crayon_plants), first_letter, ignore.case = TRUE)]
    
    # Generate alliterative pseudonyms
    if(length(colors) > 0 & length(animals) > 0 & length(plants) > 0) {
        color <- sample(colors, 1)
        animal <- sample(animals, 1)
        plant <- sample(plants, 1)
        return(list(
            color_animal = paste(color, animal, sep = "-"),
            color_plant = paste(color, plant, sep = "-")
        ))
    } else {
        # If we didn't find any matches, generate new pseudonyms
        return(generate_pseudonyms())
    }
}

# Generate pseudonyms
pseudonyms <- generate_pseudonyms()
print(pseudonyms$color_animal)
print(pseudonyms$color_plant)

# From: https://www.kaggle.com/datasets/chaibapat/sport-team-nicknames?resource=download
kaggle_team_names <- tibble(raw = read_lines('football_nicknames')) %>% 
    mutate(the_names = str_extract(raw, 'The \\w+') %>% str_replace('The ', '') %>% 
               snakecase::to_title_case(),
           first_letter = str_extract(the_names, '^.')) %>% 
    drop_na() %>% 
    select(-raw)  %>% 
    unique() %>% 
    arrange(the_names) 
    


# From: https://www.kaggle.com/datasets/pybear/animalsnames
kaggle_animals <- tibble(raw = readLines('animals_names.txt') %>% 
                             str_split(pattern = ", *") %>% 
                             unlist())
kaggle_animals <- tibble(raw = read_csv('animals_names.txt', col_names = FALSE))

# From: https://gist.github.com/hugsy/8910dc78d208e40de42deb29e62df913#file-english-adjectives-txt
adjectives <- tibble(adjective = readLines('english-adjectives.txt'))


