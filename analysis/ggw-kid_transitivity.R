makeMbyS <- function(selectPredicate = c("hunger", "feelings", "thinking"),
                  selectSubid) {
  tempM <- NULL
  
  # make alphabetized list of characters, cycle through to fill in alphabetized pairs
  tempM <- dd %>%
    filter(predicate %in% selectPredicate &
             subid %in% selectSubid) %>%
    mutate(character1 = array(),
           character2 = array(),
           leftCharacter = factor(leftCharacter),
           rightCharacter = factor(rightCharacter))
  
  charsort = sort(levels(tempM$leftCharacter), decreasing = TRUE)
  
  for(i in 1:length(charsort)) {
    tempM <- tempM %>%
      mutate(
        character1 = 
          ifelse(leftCharacter == charsort[i] |
                   rightCharacter == charsort[i],
                 as.character(charsort[i]),
                 as.character(character1)),
        character2 = 
          ifelse(character1 == leftCharacter,
                 as.character(rightCharacter),
                 as.character(leftCharacter))) %>%
      mutate(character1 = factor(character1),
             character2 = factor(character2))
  }
  
  # count number of times each character is chosen over every other character
  tempM <- tempM %>%
    select(predicate, subid, leftCharacter, rightCharacter, 
           character1, character2, responseNum) %>%
    mutate(chooseChar = ifelse(responseNum == 0, NA,
                               ifelse((as.character(character1) == as.character(leftCharacter) 
                                       & responseNum < 0) |
                                        (as.character(character1) == as.character(rightCharacter) 
                                         & responseNum > 0), "char1", "char2"))) %>%
    select(-leftCharacter, -rightCharacter, -responseNum)
  
  # name rows and columns with character names
  characterNames <- with(subset(dd, phase == "test"), levels(factor(leftCharacter)))
  
  # create empty dataframe
  tempDf <- data.frame(matrix(0, nrow = length(characterNames), ncol = length(characterNames)))
  names(tempDf) = characterNames
  rownames(tempDf) = characterNames
  
  # fill in frequency data: choose row character over column character
  for (i in 1:length(characterNames)) {
    char1 <- characterNames[i]
    
    for (j in 1:length(characterNames)) {
      char2 <- characterNames[j]
      
      countA <- tempM %>%
        filter(character1 == char1 & character2 == char2 & chooseChar == "char1") %>%
        count()
      
      countB <- tempM %>%
        filter(character1 == char1 & character2 == char2 & chooseChar == "char2") %>%
        count()
      
      tempDf[char1, char2] <- tempDf[char1, char2] + countA
      tempDf[char2, char1] <- tempDf[char2, char1] + countB
    }
  }
  
  # set diagonal to NA
  diag(tempDf) <- NA
  
  return(tempDf)
  
}

transByAgeGroup <- function(selectAgeGroup) {
  tempD <- dd %>% 
    filter(ageGroup %in% selectAgeGroup) %>%
    select(subid) %>%
    mutate(subid = factor(subid))
  
  subids <- levels(tempD$subid)
  
  listM <- NULL
  violations <- data.frame(subid = numeric(0),
                           violations = numeric(0),
                           tests = numeric())
  
  for(i in 1:length(subids)) {
    tempSubid <- subids[i]
    tempM <- makeMbyS(selectSubid = tempSubid)
    tempTrans <- strans(tempM)
    tempViolations <- tempTrans$weak
    tempTests <- tempTrans$n.tests
    listM[[i]] <- tempM
    violations[i, "subid"] <- tempSubid
    violations[i, "violations"] <- tempViolations
    violations[i, "tests"] <- tempTests
  }
    
  violations <- violations %>%
    mutate(propViol = violations/tests)
  
  return(violations)
}

childViolations <- transByAgeGroup(selectAgeGroup = "children")
adultViolations <- transByAgeGroup(selectAgeGroup = "adults")

childViolations <- childViolations %>%
  mutate(ageGroup = "children")

adultViolations <- adultViolations %>%
  mutate(ageGroup = "adults")

allViolations <- full_join(childViolations, adultViolations)

ggplot(data = allViolations, aes(x = violations)) + 
  geom_histogram() +
  facet_grid(ageGroup ~ .) +
  theme_bw() +
  theme(text = element_text(size = 20)) +
  labs(title = "Participant-level violations of weak stochastic transitivity\n",
       x = "\nNumber of violations per participant\n(out of 120 tests)",
       y = "Count of participants\n") +
  scale_x_continuous(breaks = seq(0, 35, 5))
