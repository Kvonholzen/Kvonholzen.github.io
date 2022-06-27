
# get information about what forms are available in this language
admin_info <- get_administration_data(language = wordbank_lang)
form_list <- as.list(unique(admin_info$form))


# go through each form type and extract comprehension and production information
aoa.mat <- data.frame(matrix(NA, nrow=0, ncol=10))

for(i in 1:length(form_list)){
  
  f.data <- get_instrument_data(language = wordbank_lang,
                                form = form_list[i],
                                administrations = TRUE,
                                iteminfo = TRUE)
  
# Get Age of Acquisition

# At what age do 50% of children understand this word?    
  aoa.comp <- fit_aoa(f.data, 
          measure = "understands", 
          method = "empirical", 
          proportion = 0.5)

  aoa.comp$aoa_type <- "Wordbank_AoA_understands"

# At what age do 50% of children understand this word?    
  aoa.prod <- fit_aoa(f.data, 
                      measure = "produces", 
                      method = "empirical", 
                      proportion = 0.5)  
  
  aoa.prod$aoa_type <- "Wordbank_AoA_produces"
  
  aoa.cp <- rbind(aoa.comp, aoa.prod)
  
  aoa.cp$form <- form_list[i]
  
  aoa.mat <- rbind(aoa.mat, aoa.cp)
  
  
}




aoa.m <- aoa.mat %>%
  filter(category == "food_drink" | category == "body_parts" | category == "action_words") %>% # narrow down to just nouns and verbs, correct semantic categories
  ungroup(num_item_id)%>%
  select(definition, category, lexical_class, uni_lemma, aoa_type, form, aoa) %>% # only columns we're interested in
  pivot_wider(names_from = form, values_from = aoa) ## make wider so that we have one column for each form


# identify the lowest AoA value for each word found in the forms
aoa.m <- aoa.m %>%
  nest(-definition, -category, -lexical_class, -uni_lemma, -aoa_type) %>% # nest rest of columns apart from id columns
  mutate(min_aoa = map(data, min, na.rm = T)) %>%  # identify the lowest AoA value, putting it in the column min_aoa
  unnest(cols = c(data, min_aoa)) %>%
  select(definition, category, lexical_class, uni_lemma, aoa_type, min_aoa)%>% # only columns we're interested in
  pivot_wider(names_from = aoa_type, values_from = min_aoa) %>% # make wider so we have one column for understands and one for produces
  filter(is.finite(Wordbank_AoA_understands)) # filter out the rows with Inf values, which result when both understands and produces don't have an AoA 
                                 #(because there is no point at which AoA is above 50% in the measured ages)


body_parts <- aoa.m %>%
  filter(category == "body_parts")


food_drink <- aoa.m %>%
  filter(category == "food_drink")

verbs <- aoa.m %>%
  filter(category == "action_words")



