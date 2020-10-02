prune_respondents <- function(data_all, qnames)
# Remove respondents depending on skipped responses.
# data_all: dataframe from survey data
# qnames: names of knowledge questions
# Returns: pruned dataframe
{
    
    skip_thresh <- 0 # exclude when more knowledge questions skipped

    N <- nrow( data_all ) # number of overall respondents

    # filter for consent
    consent_filt <- (data_all$Consent == 1)

    # filter for gender (don't include if no response)
    gender_filt <- data_all$Gender %in% c(1,2)

    # filter for undergraduate degree
    ugrad_filt <- !is.na( data_all$"Undergrad degree" )

    # filter for skipped questions
    is_na <- rep(0,N) # initialise with zeros
    for (qq in q_names)
    {
        is_na <- is_na + is.na(data_all[[qq]]) # find missing values for this question
    }

    is_na_all <- (is_na == 20) # get respondents who skipped all methods questions

    data_skipped <- data_all[which(is_na_all),] # get data for respondents who skipped all methods questions

    is_na_filt <- (is_na<=skip_thresh) # find respondents who skipped too much

    all_filt <- consent_filt & gender_filt & ugrad_filt & is_na_filt # combine all filters

    data <- data_all[which(all_filt),] # reduce data frame according to filters

    n_consent <- length(which(consent_filt))
    n_gender <- length(which(gender_filt))
    n_ugrad <- length(which(ugrad_filt))
    n_na <- length(which(is_na_filt))
    n_all <- length(which(all_filt))
    n_skip_all <- length(which(is_na==20))

    print( sprintf("Consented: %d - Gender responses: %d - Ugrad responses: %d - Enough responses: %d",
                                                                    n_consent, n_gender, n_ugrad, n_na) )
    print( sprintf("Remaining respondents: %d", n_all) )

    print( sprintf("Number of respondents who skipped all knowledge questions: %d.", n_skip_all))

    # some awkward entries
    if (data$Age[[34]] == "40br") # I assume typo
    {
        data$Age[[34]] = "40.000000"
    }

    return_list <- list("data"=data, "data_skipped"=data_skipped)

    return( return_list )
} # prune_respondents()


get_groups <- function(data)
# Create indices for subgroups of respondents (undergrad degrees etc.).
# data: data frame of survey data
# Returns: groups, data
# groups: list, with strings as indices to subgroups
# data: pruned data from input, reduced to respondents with classifiable undergrad degrees
{
    groups <- list()

    ### UNDERGRADUATE DEGREE
    ugrad <- data$"Undergrad degree"

    # for broader groups, include some respondents who provided string as input
    cnt_mods <- 0 # count how many will be modified
    for (uu in 1:length(ugrad))
    {        
        if ( any(grep('linguist', tolower(ugrad[uu])) == 1) | any(grep('psychology', tolower(ugrad[uu])) == 1) | any(grep('therapy', tolower(ugrad[uu])) == 1)
             | any(grep('language', tolower(ugrad[uu])) == 1))
        {
            data$"Undergrad degree"[uu] <- "1" # "Psychology" (see below)
            cnt_mods <- cnt_mods + 1
            next
        }

        if ( any(grep('engineer', tolower(ugrad[uu])) == 1) | any(grep('statistic', tolower(ugrad[uu])) == 1) | any(grep('artifical intelligence', tolower(ugrad[uu])) == 1)
             | any(grep('chemistry', tolower(ugrad[uu])) == 1) | any(grep('electronic', tolower(ugrad[uu])) == 1) | any(grep('natural sci', tolower(ugrad[uu])) == 1))
        {
            data$"Undergrad degree"[uu] <- "4" # "Physics"
            cnt_mods <- cnt_mods + 1
            next
        }

        if ( any(grep('neuroscience', tolower(ugrad[uu])) == 1) | any(grep('physiology', tolower(ugrad[uu])) == 1) | any(grep('medicine', tolower(ugrad[uu])) == 1)
             | any(grep('biomedical science', tolower(ugrad[uu])) == 1) | any(grep('health', tolower(ugrad[uu])) == 1))
        {
            data$"Undergrad degree"[uu] <- "8" # "Biology"
            cnt_mods <- cnt_mods + 1
            next
        }
    }
    cat("\n")
    print(sprintf("Modified undergraduate degree responses for %d respondents.", cnt_mods))
    cat("\n")    

    ugrad <- data$"Undergrad degree" # get updated/modified entries

    # note: these sub-groups won't be used in analysis
    groups[["ugrad_psych"]] <- Vectorize(isTRUE)(ugrad == "1") # numbers are strings here
    groups[["ugrad_cogsci"]] <- Vectorize(isTRUE)(ugrad == "2") # Vectorize turns NAs to FALSE
    groups[["ugrad_cogneu"]] <- Vectorize(isTRUE)(ugrad == "3")
    groups[["ugrad_phys"]] <- Vectorize(isTRUE)(ugrad == "4")
    groups[["ugrad_math"]] <- Vectorize(isTRUE)(ugrad == "5")
    groups[["ugrad_comp"]] <- Vectorize(isTRUE)(ugrad == "6")
    groups[["ugrad_biomed"]] <- Vectorize(isTRUE)(ugrad == "7")
    groups[["ugrad_biol"]] <- Vectorize(isTRUE)(ugrad == "8")
    groups[["ugrad_med"]] <- Vectorize(isTRUE)(ugrad == "9")
    groups[["ugrad_other"]] <- Vectorize(isTRUE)(nchar(ugrad) > 1) # not a single digit string

    # BROADER UNDERGRAD GROUPS (will be used in analysis)
    groups[["ugrad_group_psych"]] <- groups[["ugrad_psych"]] | groups[["ugrad_cogsci"]] | groups[["ugrad_cogneu"]]
    groups[["ugrad_group_meth"]] <- groups[["ugrad_phys"]] | groups[["ugrad_math"]] | groups[["ugrad_comp"]] | groups[["ugrad_biomed"]]
    groups[["ugrad_group_biol"]] <- groups[["ugrad_med"]] | groups[["ugrad_biol"]]

    # those who provided classifiable undergrad specification
    valid_ugrad <- groups$ugrad_group_meth | groups$ugrad_group_psych | groups$ugrad_group_biol

    data <- data[valid_ugrad,] # reduce data to valid respondents, use this to create new groups

    n_pp <- nrow(data) # number of participants here

    # reduce already-created groups
    groups[["ugrad_group_psych"]] <- groups[["ugrad_group_psych"]][valid_ugrad]
    groups[["ugrad_group_meth"]] <- groups[["ugrad_group_meth"]][valid_ugrad]
    groups[["ugrad_group_biol"]] <- groups[["ugrad_group_biol"]][valid_ugrad]

    ### All
    groups[["All"]] <- Vectorize(isTRUE)(c(1:n_pp)>0) # all TRUE

    ### GENDER
    groups[["males"]] <- Vectorize(isTRUE)(data$Gender == 1)
    groups[["females"]] <- Vectorize(isTRUE)(data$Gender == 2)

    ### EXPERTISE
    groups[["expert_yes"]] <- Vectorize(isTRUE)(data$Expertise == 1)
    groups[["expert_sortof"]] <- Vectorize(isTRUE)(data$Expertise == 2)
    groups[["expert_no"]] <- Vectorize(isTRUE)(data$Expertise == 3)
    groups[["expert_dk"]] <- Vectorize(isTRUE)(data$Expertise == 4)

    ### INTERNET USE
    groups[["internet_yes"]] <- Vectorize(isTRUE)(data$Internet == 1)
    groups[["internet_no"]] <- Vectorize(isTRUE)(data$Internet == 2)
     
    ## Researcher type (undergrad, PhD etc.)
    res_type <- data[["Research area"]]
    len <- length(res_type)   

    # Take care of "Other" option (Master's, RAs)
    for (uu in 1:len)
    {
        groups[["researcher_undgrad"]][uu] <- FALSE
        groups[["researcher_phd"]][uu] <- FALSE
        groups[["researcher_postdoc"]][uu] <- FALSE
        groups[["researcher_resass"]][uu] <- FALSE
        groups[["researcher_skipped"]][uu] <- FALSE
        # skipped?
        if ( is.na(res_type[[uu]]) )
        {
            groups[["researcher_skipped"]][uu] <- TRUE
        }
        else
        {
            # undergrads, incl. master, mphil etc.
            if ((res_type[[uu]]=="1") | ( any(grep('master', tolower(res_type[[uu]])) == 1) |
                                any(grep('Msc', tolower(res_type[[uu]])) == 1) |
                                any(grep('MPhil', tolower(res_type[[uu]])) == 1) ))
            {
                groups[["researcher_undgrad"]][uu] <- TRUE
                next
            } 
            else if ( res_type[[uu]]=="2" | any(grep('phd', tolower(res_type[[uu]])) == 1) )
            {
                groups[["researcher_phd"]][uu] <- TRUE
                next
            }
            else if (res_type[[uu]]=="3")
            {
                groups[["researcher_postdoc"]][uu] <- TRUE
                next
            }            
            else if ( any(grep('research assistant', tolower(res_type[[uu]])) == 1) )
            {
                groups[["researcher_resass"]][uu] <- TRUE
                next
            }
        }
    }

    ## gender by researcher type
    groups[["researcher_undgrad_males"]] = groups[["researcher_undgrad"]] & groups[["males"]]
    groups[["researcher_undgrad_females"]] = groups[["researcher_undgrad"]] & groups[["females"]]
    groups[["researcher_phd_males"]] = groups[["researcher_phd"]] & groups[["males"]]
    groups[["researcher_phd_females"]] = groups[["researcher_phd"]] & groups[["females"]]
    groups[["researcher_postdoc_males"]] = groups[["researcher_postdoc"]] & groups[["males"]]
    groups[["researcher_postdoc_females"]] = groups[["researcher_postdoc"]] & groups[["females"]]    
    groups[["researcher_resass_males"]] = groups[["researcher_resass"]] & groups[["males"]]
    groups[["researcher_resass_females"]] = groups[["researcher_resass"]] & groups[["females"]]

    ## Training needs
    groups[["needs_alot"]] <- Vectorize(isTRUE)(data$"Training needs" == 1)
    groups[["needs_sign"]] <- Vectorize(isTRUE)(data$"Training needs" == 2)
    groups[["needs_alit"]] <- Vectorize(isTRUE)(data$"Training needs" == 3)
    groups[["needs_notaa"]] <- Vectorize(isTRUE)(data$"Training needs" == 4)
    groups[["needs_dk"]] <- Vectorize(isTRUE)(data$"Training needs" == 5)
    groups[["needs_skpd"]] <- Vectorize(isTRUE)(is.na(data$"Training needs"))

    # Gender by undergraduate degree
    groups[["ugrad_group_psych_males"]] <- groups[["ugrad_group_psych"]] & groups[["males"]]
    groups[["ugrad_group_psych_females"]] <- groups[["ugrad_group_psych"]] & groups[["females"]]
    groups[["ugrad_group_meth_males"]] <- groups[["ugrad_group_meth"]] & groups[["males"]]
    groups[["ugrad_group_meth_females"]] <- groups[["ugrad_group_meth"]] & groups[["females"]]
    groups[["ugrad_group_biol_males"]] <- groups[["ugrad_group_biol"]] & groups[["males"]]
    groups[["ugrad_group_biol_females"]] <- groups[["ugrad_group_biol"]] & groups[["females"]]

    # Expertise by Gender
    groups[["expert_yes_males"]] <- groups[["expert_yes"]] & groups[["males"]]
    groups[["expert_yes_females"]] <- groups[["expert_yes"]] & groups[["females"]]
    groups[["expert_sortof_males"]] <- groups[["expert_sortof"]] & groups[["males"]]
    groups[["expert_sortof_females"]] <- groups[["expert_sortof"]] & groups[["females"]]
    groups[["expert_no_males"]] <- groups[["expert_no"]] & groups[["males"]]
    groups[["expert_no_females"]] <- groups[["expert_no"]] & groups[["females"]]
    groups[["expert_dk_males"]] <- groups[["expert_dk"]] & groups[["males"]]
    groups[["expert_dk_females"]] <- groups[["expert_dk"]] & groups[["females"]]

    # Expertise by undergraduate degree
    groups[["expert_yes_psych"]] <- groups[["expert_yes"]] & groups[["ugrad_group_psych"]]
    groups[["expert_yes_meth"]] <- groups[["expert_yes"]] & groups[["ugrad_group_meth"]]
    groups[["expert_yes_biol"]] <- groups[["expert_yes"]] & groups[["ugrad_group_biol"]]
    groups[["expert_sortof_psych"]] <- groups[["expert_sortof"]] & groups[["ugrad_group_psych"]]
    groups[["expert_sortof_meth"]] <- groups[["expert_sortof"]] & groups[["ugrad_group_meth"]]
    groups[["expert_sortof_biol"]] <- groups[["expert_sortof"]] & groups[["ugrad_group_biol"]]
    groups[["expert_no_psych"]] <- groups[["expert_no"]] & groups[["ugrad_group_psych"]]
    groups[["expert_no_meth"]] <- groups[["expert_no"]] & groups[["ugrad_group_meth"]]
    groups[["expert_no_biol"]] <- groups[["expert_no"]] & groups[["ugrad_group_biol"]]
    groups[["expert_dk_psych"]] <- groups[["expert_dk"]] & groups[["ugrad_group_psych"]]
    groups[["expert_dk_meth"]] <- groups[["expert_dk"]] & groups[["ugrad_group_meth"]]
    groups[["expert_dk_biol"]] <- groups[["expert_dk"]] & groups[["ugrad_group_biol"]]

    # group counts
    for (name in names(groups))
    {
        print( sprintf("%s: %d", name, length( which(groups[[name]]) ) ) )
    }

    return_list <- list("groups"=groups, "data" = data)

    return( return_list )
} # get_groups()


get_demographics <- function(groups, data_all)
{
    # Compute basic demographics, e.g. age.
    # groups:  list, with strings as indices to subgroups, from get_groups()
    # data_all: dataframe with survey data, from prune_respondents()
    # Returns: list, demos with demographic values (age etc.)

    demos <- list()
    
    # General
    demos['N_all'] <- nrow( data_all ) # number of respondents
    demos['N_males'] <- length(which(groups[["males"]]))
    demos['N_females'] <- length(which(groups[["females"]]))
    demos['n_int_males'] <- length(which(groups[["internet_yes"]] & groups[["males"]]))
    demos['n_int_females'] <- length(which(groups[["internet_yes"]] & groups[["females"]]))
    demos['n_int'] <- demos[['n_int_males']] + demos[['n_int_females']]

    # Current degree/programme
    demos['n_ugrad_males'] <- length(which(groups[["researcher_undgrad_males"]]))
    demos['n_ugrad_females'] <- length(which(groups[["researcher_undgrad_females"]]))
    demos['n_phd_males'] <- length(which(groups[["researcher_phd_males"]]))
    demos['n_phd_females'] <- length(which(groups[["researcher_phd_females"]]))
    demos['n_pd_males'] <- length(which(groups[["researcher_postdoc_males"]]))
    demos['n_pd_females'] <- length(which(groups[["researcher_postdoc_females"]]))
    demos['n_ra_males'] <- length(which(groups[["researcher_resass_males"]]))
    demos['n_ra_females'] <- length(which(groups[["researcher_resass_females"]]))

    # undergraduate degree
    demos['n_psych'] <- length(which(groups[["ugrad_group_psych"]]))
    demos['n_meth'] <- length(which(groups[["ugrad_group_meth"]]))
    demos['n_biol'] <- length(which(groups[["ugrad_group_biol"]]))

    demos['n_psych_males'] <- length(which(groups[["ugrad_group_psych_males"]]))
    demos['n_psych_females'] <- length(which(groups[["ugrad_group_psych_females"]]))
    demos['n_meth_males'] <- length(which(groups[["ugrad_group_meth_males"]]))
    demos['n_meth_females'] <- length(which(groups[["ugrad_group_meth_females"]]))
    demos['n_biol_males'] <- length(which(groups[["ugrad_group_biol_males"]]))
    demos['n_biol_females'] <- length(which(groups[["ugrad_group_biol_females"]]))

    # Age
    demos['mean_age'] <- mean( as.numeric( data_all$Age ), na.rm=T )
    demos['mean_age_males'] <- mean( as.numeric( data_all$Age[ groups[["males"]] ]), na.rm=T )
    demos['mean_age_females'] <- mean( as.numeric( data_all$Age[ groups[["females"]] ]), na.rm=T )
    demos['sd_age'] <- sd( as.numeric( data_all$Age ), na.rm=T )
    demos['sd_age_males'] <- sd( as.numeric( data_all$Age[ groups[["males"]] ]), na.rm=T )
    demos['sd_age_females'] <- sd( as.numeric( data_all$Age[ groups[["females"]] ]), na.rm=T )
    demos['age_psych_males'] <- mean( as.numeric( data_all$Age[ groups[["ugrad_group_psych_males"]] ]), na.rm=T )
    demos['age_psych_females'] <- mean( as.numeric( data_all$Age[ groups[["ugrad_group_psych_females"]] ]), na.rm=T )
    demos['age_meth_males'] <- mean( as.numeric( data_all$Age[ groups[["ugrad_group_meth_males"]] ]), na.rm=T )
    demos['age_meth_females'] <- mean( as.numeric( data_all$Age[ groups[["ugrad_group_meth_females"]] ]), na.rm=T )
    demos['age_biol_males'] <- mean( as.numeric( data_all$Age[ groups[["ugrad_group_biol_males"]] ]), na.rm=T )
    demos['age_biol_females'] <- mean( as.numeric( data_all$Age[ groups[["ugrad_group_biol_females"]] ]), na.rm=T )

    # self-rated expertise, in %
    demos['n_exp_yes_psych'] <- 100 * length(which(groups[["expert_yes_psych"]])) / demos[['n_psych']]
    demos['n_exp_sortof_psych'] <- 100 * length(which(groups[["expert_sortof_psych"]])) / demos[['n_psych']]
    demos['n_exp_no_psych'] <- 100 * length(which(groups[["expert_no_psych"]])) / demos[['n_psych']]
    demos['n_exp_yes_meth'] <- 100 * length(which(groups[["expert_yes_meth"]])) / demos[['n_meth']]
    demos['n_exp_sortof_meth'] <- 100 * length(which(groups[["expert_sortof_meth"]])) / demos[['n_meth']]
    demos['n_exp_no_meth'] <- 100 * length(which(groups[["expert_no_meth"]])) / demos[['n_meth']]
    demos['n_exp_yes_biol'] <- 100 * length(which(groups[["expert_yes_biol"]])) / demos[['n_biol']]
    demos['n_exp_sortof_biol'] <- 100 * length(which(groups[["expert_sortof_biol"]])) / demos[['n_biol']]
    demos['n_exp_no_biol'] <- 100 * length(which(groups[["expert_no_biol"]])) / demos[['n_biol']]

    demos['n_exp_yes_males'] <-  100 * length(which(groups[["expert_yes_males"]])) / demos[['N_males']]
    demos['n_exp_sortof_males'] <-  100 * length(which(groups[["expert_sortof_males"]])) / demos[['N_males']]
    demos['n_exp_no_males'] <-  100 * length(which(groups[["expert_no_males"]])) / demos[['N_males']]
    demos['n_exp_yes_females'] <-  100 * length(which(groups[["expert_yes_females"]])) / demos[['N_females']]
    demos['n_exp_sortof_females'] <-  100 * length(which(groups[["expert_sortof_females"]])) / demos[['N_females']]
    demos['n_exp_no_females'] <-  100 * length(which(groups[["expert_no_females"]])) / demos[['N_females']]

    print( sprintf('Age: %.1f (%.1f)', demos[['mean_age']], demos[['sd_age']]))
    print( sprintf('Males: %.1f (%.1f)', demos[['mean_age_males']], demos[['sd_age_males']]))
    print( sprintf('Females: %.1f (%.1f)', demos[['mean_age_females']], demos[['sd_age_females']]))

    return( demos )
} # demographics()


get_all_Results <- function(data_all, q_meth_names, correct, groups_all)
{
    # Create list with all results for groups and questions.
    # data_all: data frame, columns: questions, rows: respondents
    # q_meth_names: names of questions used to refer to columns in data
    # correct: data frame with correct response codes, same column names as data_all
    # groups_all: list with indices of respondents (rows of data) for different groups
    # Returns: list with results, entries are output from get_indiv_Results()

    Results <- list() # for different respondent groups

    print ("Gender")
    Results[["sex"]][["All"]] <- get_indiv_Results(data_all, q_meth_names, correct, groups_all[["All"]])
    Results[["sex"]][["males"]] <- get_indiv_Results(data_all, q_meth_names, correct, groups_all[["males"]])
    Results[["sex"]][["females"]] <- get_indiv_Results(data_all, q_meth_names, correct, groups_all[["females"]])

    print ("Degree")
    Results[["degree"]][["ugrad_group_psych"]] <- get_indiv_Results(data_all, q_meth_names, correct, groups_all[["ugrad_group_psych"]])
    Results[["degree"]][["ugrad_group_meth"]] <- get_indiv_Results(data_all, q_meth_names, correct, groups_all[["ugrad_group_meth"]])
    Results[["degree"]][["ugrad_group_biol"]] <- get_indiv_Results(data_all, q_meth_names, correct, groups_all[["ugrad_group_biol"]])

    print ("Degree by Gender")
    Results[["deg_sex"]][["ugrad_group_psych_males"]] <- get_indiv_Results(data_all, q_meth_names, correct, groups_all[["ugrad_group_psych_males"]])
    Results[["deg_sex"]][["ugrad_group_meth_males"]] <- get_indiv_Results(data_all, q_meth_names, correct, groups_all[["ugrad_group_meth_males"]])
    Results[["deg_sex"]][["ugrad_group_biol_males"]] <- get_indiv_Results(data_all, q_meth_names, correct, groups_all[["ugrad_group_biol_males"]])
    Results[["deg_sex"]][["ugrad_group_psych_females"]] <- get_indiv_Results(data_all, q_meth_names, correct, groups_all[["ugrad_group_psych_females"]])
    Results[["deg_sex"]][["ugrad_group_meth_females"]] <- get_indiv_Results(data_all, q_meth_names, correct, groups_all[["ugrad_group_meth_females"]])
    Results[["deg_sex"]][["ugrad_group_biol_females"]] <- get_indiv_Results(data_all, q_meth_names, correct, groups_all[["ugrad_group_biol_females"]])

    print ("Expertise")
    Results[["Expertise"]][["expert_yes"]] <- get_indiv_Results(data_all, q_meth_names, correct, groups_all[["expert_yes"]])
    Results[["Expertise"]][["expert_sortof"]] <- get_indiv_Results(data_all, q_meth_names, correct, groups_all[["expert_sortof"]])
    Results[["Expertise"]][["expert_no"]] <- get_indiv_Results(data_all, q_meth_names, correct, groups_all[["expert_no"]])
    # not enough trials for "dont know"

    print ("Expertise by Gender")
    Results[["ExpGend"]][["expert_yes_males"]] <- get_indiv_Results(data_all, q_meth_names, correct,
                                                                    groups_all[["expert_yes_males"]])
    Results[["ExpGend"]][["expert_sortof_males"]] <- get_indiv_Results(data_all, q_meth_names, correct,
                                                                    groups_all[["expert_sortof_males"]])
    Results[["ExpGend"]][["expert_no_males"]] <- get_indiv_Results(data_all, q_meth_names, correct,
                                                                    groups_all[["expert_no_males"]])
    Results[["ExpGend"]][["expert_yes_females"]] <- get_indiv_Results(data_all, q_meth_names, correct,
                                                                    groups_all[["expert_yes_females"]])
    Results[["ExpGend"]][["expert_sortof_females"]] <- get_indiv_Results(data_all, q_meth_names, correct,
                                                                    groups_all[["expert_sortof_females"]])
    Results[["ExpGend"]][["expert_no_females"]] <- get_indiv_Results(data_all, q_meth_names, correct,
                                                                    groups_all[["expert_no_females"]])

    print ("Expertise by Undergraduate Degree")
    Results[["ExpUgrad"]][["expert_yes_psych"]] <- get_indiv_Results(data_all, q_meth_names, correct,
                                                                    groups_all[["expert_yes_psych"]])
    Results[["ExpUgrad"]][["expert_sortof_psych"]] <- get_indiv_Results(data_all, q_meth_names, correct,
                                                                    groups_all[["expert_sortof_psych"]])
    Results[["ExpUgrad"]][["expert_no_psych"]] <- get_indiv_Results(data_all, q_meth_names, correct,
                                                                    groups_all[["expert_no_psych"]])
    Results[["ExpUgrad"]][["expert_yes_meth"]] <- get_indiv_Results(data_all, q_meth_names, correct,
                                                                    groups_all[["expert_yes_meth"]])
    Results[["ExpUgrad"]][["expert_sortof_meth"]] <- get_indiv_Results(data_all, q_meth_names, correct,
                                                                    groups_all[["expert_sortof_meth"]])
    Results[["ExpUgrad"]][["expert_no_meth"]] <- get_indiv_Results(data_all, q_meth_names, correct,
                                                                    groups_all[["expert_no_meth"]])
    Results[["ExpUgrad"]][["expert_yes_biol"]] <- get_indiv_Results(data_all, q_meth_names, correct,
                                                                    groups_all[["expert_yes_biol"]])
    Results[["ExpUgrad"]][["expert_sortof_biol"]] <- get_indiv_Results(data_all, q_meth_names, correct,
                                                                    groups_all[["expert_sortof_biol"]])
    Results[["ExpUgrad"]][["expert_no_biol"]] <- get_indiv_Results(data_all, q_meth_names, correct,
                                                                    groups_all[["expert_no_biol"]])

    # Training needs
    Results[["Training needs"]][["All"]] <- get_training_needs(data_all, groups_all[["All"]])
    Results[["Training needs"]][["males"]] <- get_training_needs(data_all, groups_all[["males"]])
    Results[["Training needs"]][["females"]] <- get_training_needs(data_all, groups_all[["females"]])
    Results[["Training needs"]][["Undgrad All"]] <- get_training_needs(data_all, groups_all[["researcher_undgrad"]])
    Results[["Training needs"]][["Undgrad males"]] <- get_training_needs(data_all, groups_all[["researcher_undgrad_males"]])
    Results[["Training needs"]][["Undgrad females"]] <- get_training_needs(data_all, groups_all[["researcher_undgrad_females"]])
    Results[["Training needs"]][["PhD All"]] <- get_training_needs(data_all, groups_all[["researcher_phd"]])
    Results[["Training needs"]][["PhD males"]] <- get_training_needs(data_all, groups_all[["researcher_phd_males"]])
    Results[["Training needs"]][["PhD females"]] <- get_training_needs(data_all, groups_all[["researcher_phd_females"]])
    Results[["Training needs"]][["Postdoc All"]] <- get_training_needs(data_all, groups_all[["researcher_postdoc"]])
    Results[["Training needs"]][["Postdoc males"]] <- get_training_needs(data_all, groups_all[["researcher_postdoc_males"]])
    Results[["Training needs"]][["Postdoc females"]] <- get_training_needs(data_all, groups_all[["researcher_postdoc_females"]])
    Results[["Training needs"]][["ugrad_group_meth"]] <- get_training_needs(data_all, groups_all[["ugrad_group_meth"]])
    Results[["Training needs"]][["ugrad_group_meth_males"]] <- get_training_needs(data_all, groups_all[["ugrad_group_meth_males"]])
    Results[["Training needs"]][["ugrad_group_meth_females"]] <- get_training_needs(data_all, groups_all[["ugrad_group_meth_females"]])
    Results[["Training needs"]][["ugrad_group_psych"]] <- get_training_needs(data_all, groups_all[["ugrad_group_psych"]])
    Results[["Training needs"]][["ugrad_group_psych_males"]] <- get_training_needs(data_all, groups_all[["ugrad_group_psych_males"]])
    Results[["Training needs"]][["ugrad_group_psych_females"]] <- get_training_needs(data_all, groups_all[["ugrad_group_psych_females"]])
    Results[["Training needs"]][["ugrad_group_biol"]] <- get_training_needs(data_all, groups_all[["ugrad_group_biol"]])
    Results[["Training needs"]][["ugrad_group_biol_males"]] <- get_training_needs(data_all, groups_all[["ugrad_group_biol_males"]])
    Results[["Training needs"]][["ugrad_group_biol_females"]] <- get_training_needs(data_all, groups_all[["ugrad_group_biol_females"]])
    

    # Researcher type
    Results[["Research area"]][["All"]] <- get_indiv_Results(data_all, q_meth_names, correct, groups_all[["All"]])
    Results[["Research area"]][["males"]] <- get_indiv_Results(data_all, q_meth_names, correct,
                                                                    groups_all[["males"]])
    Results[["Research area"]][["females"]] <- get_indiv_Results(data_all, q_meth_names, correct,
                                                                    groups_all[["females"]])

    Results[["Research area"]][["Undgrad All"]] <- get_indiv_Results(data_all, q_meth_names, correct, groups_all[["researcher_undgrad"]])
    Results[["Research area"]][["Undgrad males"]] <- get_indiv_Results(data_all, q_meth_names, correct,
                                                                    groups_all[["researcher_undgrad_males"]])
    Results[["Research area"]][["Undgrad females"]] <- get_indiv_Results(data_all, q_meth_names, correct,
                                                                    groups_all[["researcher_undgrad_females"]])
    Results[["Research area"]][["PhD All"]] <- get_indiv_Results(data_all, q_meth_names, correct, groups_all[["researcher_phd"]])
    Results[["Research area"]][["PhD males"]] <- get_indiv_Results(data_all, q_meth_names, correct,
                                                                    groups_all[["researcher_phd_males"]])
    Results[["Research area"]][["PhD females"]] <- get_indiv_Results(data_all, q_meth_names, correct,
                                                                    groups_all[["researcher_phd_females"]])
    Results[["Research area"]][["Postdoc All"]] <- get_indiv_Results(data_all, q_meth_names, correct, groups_all[["researcher_postdoc"]])
    Results[["Research area"]][["Postdoc males"]] <- get_indiv_Results(data_all, q_meth_names, correct,
                                                                    groups_all[["researcher_postdoc_males"]])
    Results[["Research area"]][["Postdoc females"]] <- get_indiv_Results(data_all, q_meth_names, correct,
                                                                    groups_all[["researcher_postdoc_females"]])

    # Future area counts
    Results[["Research area cnts"]][["All"]] <- get_research_area(data_all, groups_all[["All"]])
    Results[["Research area cnts"]][["males"]] <- get_research_area(data_all, groups_all[["males"]])
    Results[["Research area cnts"]][["females"]] <- get_research_area(data_all, groups_all[["females"]])

    # Future area counts
    Results[["Future area cnts"]][["All"]] <- get_future_area(data_all, groups_all[["All"]])
    Results[["Future area cnts"]][["males"]] <- get_future_area(data_all, groups_all[["males"]])
    Results[["Future area cnts"]][["females"]] <- get_future_area(data_all, groups_all[["females"]])

    return(Results)

} # get_all_Results()

get_indiv_Results <- function(data, q_names, correct, idx)
{
# Get response accuracies from data for questions in q_names.
# data: data frame, columns: questions, rows: respondents
# q_names: names of questions used to refer to columns in data
# correct: data frame with correct response codes, same column names as data
# idx: indices of respondents (rows of data) to take into account
# Returns: list with four elements, Results (counts) and Results_frac (fractions), sum_counts, sum_frac
# e.g. Results_out$frac[[qq]] (qq: question name) will return for values for Corr/Err/NoI/Skp

    Results_out <- list() # to be returned by this function

    # sub-groups of questions
    # NOTE: indices refer to q_meth_names (not q_names)
    # for reference:
    # qgroup_names <- list(linalg = "Linear Algebra", calc = "Calculus", progr = "Programming", signal = "Signal
    # Analysis", phys = "Physics", stats = "Statistics")
    qgroup <- list(AllQs = c(1:18), linalg = c(12,13), calc = c(9,10,11), progr = c(14,15,16), signal = c(2,3,4,7,8), phys = c(17,18), stats = c(1,5,6))
    # qgroup <- list(AllQs = c(1:18))
    qgroup_names <- names(qgroup)
    
    # reduce data frame to relevant respondents
    data <- data[idx,]

    n_pp <- length(which(idx)) # number of participants here

	noidea <- 5 # response code for "No idea"

	# initialise data frame for results
	Results_counts <- as.data.frame(c(1:4)) # data frame for number of responses
	names(Results_counts)[1] <- q_names[1]

	# response categories: Correct, Error, No Idea, Skipped
    resp_cats <- c("Cor", "Err", "NoI", "Skd")
	row.names(Results_counts) <- resp_cats

    # initialise data frame for fraction of responses
	Results_frac <- Results_counts

    # indices to participants with corr/incorr/ni/skp responses per question
    Results_inds <- list()

    # data frame for results for individual participants across questions
    Results_indiv <- list()
    r_categs <- c("Cor", "Err", "NoI") # response categories
    for (qq in qgroup_names)
    {
        for (rr in r_categs)
        {
            Results_indiv[[qq]][[rr]] <- matrix(0,n_pp,1) # initialise
        }
    }

    for (QQ in qgroup_names) # per question group
    {
        QQ_inds <- unlist(qgroup[QQ]) # indices to questions in this group
        n_qq <- length(QQ_inds) # number of questions considered
        a_qq <- 1/n_qq - 0.0000000001 # glm() didn't work if some values exactly 1 ###

        q_names_now <- q_names[QQ_inds] # names of questions in this group

    	for (qq in q_names_now)
    	{
    		values_now <- data[[qq]] # get data for current question from data frame
    		correct_now <- correct[[qq]] # this is the correct response for this question (1-4)
    		resp_vals <- c(1,2,3,4) # all four response options
    		incorr_now <- resp_vals[-correct_now] # values of incorrect responses (removes one element from vector)

            # indices of participants with correct responses
            cor_inds <- which( data[[qq]] == correct_now )
            # ... incorrect responses
            err_inds <- which( data[[qq]] %in% incorr_now)
            # ... no idea responses
            noi_inds <- which( data[[qq]] == noidea )
            # ... skipped responses
            skd_inds <- which( is.na( data[[qq]] ) )

            # indices to particants with corr/err/no/skd responses
            # needed for regression analysis
            Results_inds[[qq]][["Cor"]] <- list(cor_inds)
            Results_inds[[qq]][["Err"]] <- list(err_inds)
            Results_inds[[qq]][["NoI"]] <- list(noi_inds)
            Results_inds[[qq]][["Skd"]] <- list(skd_inds)

            # update individual results
            Results_indiv[[QQ]][["Cor"]][cor_inds] <- Results_indiv[[QQ]][["Cor"]][cor_inds] + a_qq
            Results_indiv[[QQ]][["Err"]][err_inds] <- Results_indiv[[QQ]][["Err"]][err_inds] + a_qq
            Results_indiv[[QQ]][["NoI"]][noi_inds] <- Results_indiv[[QQ]][["NoI"]][noi_inds] + a_qq

    		# all responses per question
    		Results_counts["Cor",qq] <- length( cor_inds ) # reponse correct	
    		Results_counts["Err",qq] <- length( err_inds ) # incorrect responses
    		Results_counts["NoI",qq] <- length( noi_inds ) # response "no idea"
    		Results_counts["Skd",qq] <- length( skd_inds ) # response skipped

    		n_good_resp <- Results_counts["Cor",qq] + Results_counts["Err",qq] + Results_counts["NoI",qq] # number of people who responded to this question
    		n_resp <- n_good_resp + Results_counts["Skd",qq] # all responses, incl. skipped

    		# turn into fractions
    		Results_frac[[qq]] <- Results_counts[[qq]]/n_resp
    	} # qq
    } # QQ

    # summary of results across all questions
    Sum_counts <- as.data.frame(c(1:4)) # data frame for number of responses
    names(Sum_counts) <- "sum"
    row.names(Sum_counts) <- resp_cats
    for (rr in resp_cats)
    {
        Sum_counts[rr, "sum"] <- sum( Results_counts[rr,] )
    }
    all_counts <- sum( Sum_counts)
    Sum_frac <- Sum_counts / all_counts

    # summary of results across sub-groups of questions
    Sum_qgroup_counts <- as.data.frame(c(1:4)) # data frame for number of responses
    names(Sum_qgroup_counts)[1] <- qgroup_names[1]
    row.names(Sum_qgroup_counts) <- c("Cor", "Err", "NoI", "Skd")
    Sum_qgroup_frac <- Sum_qgroup_counts
    for (qq in qgroup_names)
    {
        for (rr in resp_cats)
        {
            Sum_qgroup_counts[rr, qq] <- sum(Results_counts[rr,qgroup[[qq]]])
        }
        all_counts <- sum(Sum_qgroup_counts[, qq])
        Sum_qgroup_frac[, qq] <- Sum_qgroup_counts[, qq] / all_counts
    }
    
	Results_out$counts <- Results_counts
	Results_out$frac <- Results_frac
    Results_out$sum_counts <- Sum_counts
    Results_out$sum_frac <- Sum_frac
    Results_out$sum_qgroup_frac <- Sum_qgroup_frac

    Results_out$inds <- Results_inds
    Results_out$indiv <- Results_indiv

	return( Results_out )
} # get_indiv_Results


get_training_needs <- function(data_ori, idx)
{
# Plot responses for question "Training needs".
# data_ori: data frame, rows: respondents
# idx: indices of respondents (rows of data) to take into account
# Returns: list with six elements, Results (fractions)

    # response options:   
    # 1: a lot
    # 2: significantly
    # 3: a little
    # 4: not at all
    # 5: don't know
    # 6: skipped (NA)

    # get data for valid respondents
    data <- data_ori[["Training needs"]][idx]
    len <- length(data)

    ### get data into vector for bar plot
    res_vec <- matrix(0,6,1) # vector: number of response options

    for (cc in c(1:5))
    {
        res_vec[cc] <- length(which(data==cc)) / len
    }

    res_vec[6] <- length(which(is.na(data))) / len

    Results <- res_vec

    return( Results )
} # get_training_needs()


get_research_area <- function(data_ori, idx)
{
# Get counts for question "Research area".
# data_ori: data frame, rows: respondents
# idx: indices of respondents (rows of data) to take into account
# Returns: list with six elements, Results

    # response options:   
    # 1: Undergraduate student
    # 2: PhD student
    # 3: Post-doc
    # 4: Other (please specify)
    # added/changed below: 4: master's, 5: research assistant

    # get data for valid respondents
    data <- data_ori[["Research area"]][idx]
    len <- length(data)

    ### get data into vector for bar plot
    res_vec <- matrix(0,6,1) # vector: number of response options

    # Take care of "Other" option (master's, RAs)
    for (uu in 1:len)
    {
        # skipped?
        if ( is.na(data[[uu]]) )
        {
            data[[uu]] = 6
        }
        else
        {
            # master
            if (data[[uu]]=="1")
            {
                data[[uu]] = 1
            } 
            else if ( data[[uu]]=="2" | any(grep('phd', tolower(data[[uu]])) == 1) )
            {
                data[[uu]] = 2
                next
            }
            else if (data[[uu]]=="3")
            {
                data[[uu]] = 3
            }
            else if ( any(grep('master', tolower(data[[uu]])) == 1) |
                                any(grep('Msc', tolower(data[[uu]])) == 1) |
                                any(grep('MPhil', tolower(data[[uu]])) == 1) )
            {
                data[[uu]] = 4
                next
            }
            else if ( any(grep('research assistant', tolower(data[[uu]])) == 1) )
            {
                data[[uu]] = 5
            }
        }
    }

    for (cc in c(1:6))
    {
        res_vec[cc] <- length(which(data==cc))
        res_vec[cc] <- res_vec[cc] / len # fraction
    }

    Results <- res_vec

    return( Results )

} # get_research_area()


get_future_area <- function(data_ori, idx)
{
# Plot responses for question "Future area".
# data_ori: data frame, rows: respondents
# idx: indices of respondents (rows of data) to take into account
# Returns: list with seven elements, Results

    # response options:   
    # 1: Psychology
    # 2: Basic cognitive science
    # 3: Basic cognitive neuroscience
    # 4: Clinical neuroscience, neuropsychology, medicine
    # 5: Don't know
    # 6: Other (please specify)
    # 7: skipped

    # get data for valid respondents
    data <- data_ori[["Future area"]][idx]
    len <- length(data)

    ### get data into vector for bar plot
    res_vec <- matrix(0,7,1) # vector: number of response options

    # Take care of "Other" option
    for (uu in 1:len)
    {
        # skipped?
        if ( is.na(data[[uu]]) )
        {
            data[[uu]] = 7
        }
        else
        {
            if (data[[uu]]=="1")
            {
                data[[uu]] = 1
            } 
            else if ( data[[uu]]=="2")
            {
                data[[uu]] = 2
            }
            else if ( (data[[uu]]=="3") | any(grep('neuro', tolower(data[[uu]])) == 1) |
                        any(grep('imaging', tolower(data[[uu]])) == 1) )
            {
                data[[uu]] = 3
                next
            }
            else if ( (data[[uu]]=="4") | any(grep('medi', tolower(data[[uu]])) == 1)  | any(grep('physiol', tolower(data[[uu]])) == 1) )
            {
                data[[uu]] = 4
                next
            }
            else if (data[[uu]]=="5")
            {
                data[[uu]] = 5
            }
            else # Other
            {
                data[[uu]] = 6
            }                       
        }
    }

    for (cc in c(1:7))
    {
        res_vec[cc] <- length(which(data==cc))
        res_vec[cc] <- res_vec[cc] / len # fraction
    }   

    Results <- res_vec

    return( Results )

} # get_future_area()


plot_general_questions <- function(data, groups, my_title, bar_names, bar_legend, colours)
{
# Plot data to bar graph for "Training needs" question.
# data: list of data frames with data to plot
# groups: string, indices to data for respondent group(s) to plot
# my_title: string, title for plot
# bar_names: list of string, names of individual bars in plot
# bar_legend: list of strings, what to use as legend in bar graphs, for items in "groups"
# colours: vector of string, colour palette for ggplot (scale_fill_manual)
# Returns: (nothing)

    n_groups <- length(groups)

    n_bars <- length(data[[1]])-1 # -1 to skip "skipped"
    
    vals <- 100*data[[groups[1]]][1:n_bars] # initialise what will become dataframe

    group <- rep(bar_legend[1], n_bars) # groups as row names, use specified legend names

    bars <- bar_names # bar names within groups

    if (n_groups > 1) # append only if necessary
    {
        for (gg in c(2:n_groups)) # concatenate data for remaining groups
        {
            vals <- c(vals, 100*data[[groups[gg]]][1:n_bars]) # as %
            
            group <- c(group, rep(bar_legend[gg], n_bars))

            bars <- c(bars, bar_names)
        }
    }

    # turn NaN into 0 before plotting
    vals[is.nan(vals)] <- 0

    data_plot <- data.frame( group, bars, vals )

    # make order of factor levels explicit for plotting
    data_plot$bars <- factor(data_plot$bars, levels=data_plot$bars[1:n_bars])
    data_plot$group <- factor(data_plot$group, levels=data_plot$group[seq(1,n_bars*n_groups,n_bars)])

    fsize <- 7
    if (n_groups > 2) # decrease numbers in bar graphs for dense plots
    {
        fsize <- 4
    }

    p <- ggplot(data=data_plot, aes(x=bars, y=vals, fill=group, ymax=max(vals))) + geom_bar(stat="identity", position=position_dodge()) +
                labs(title=my_title) + theme_bw() + theme(text=element_text(size=24), legend.justification = c(1, 1), legend.position = c(1, 1)) +
                scale_fill_manual(values=colours) + geom_text(aes(label=round(vals)), vjust=3, color="brown", position = position_dodge(0.9), size=fsize)

    print(p)

    # output displayed in command window
    cat("\n")
    print(my_title)
    print(data_plot)

} # plot_general_questions()

#               (Results[["Research area"]], groups, "sum", restype, "Researcher type", bar_legend, colours_3)
plot_bargraphs <- function(data, groups, quest, restype, my_title, bar_legend, colours)
{
# Plot data to bar graph.
# data: list of data frames with data to plot [assumes Corr/Err/NI to be fastest running dimension]
# groups: string, indices to data for respondent group(s) to plot
# quest: string, the question (or subgroups of questions) for which results to be plotted
# restype: type of response to be plotted (e.g. "counts", "frac", "sum_counts")
# my_title: string, title for plot
# bar_legend: what to use as legend in bar graphs, for items in "groups"
# colours: vector of string, colour palette for ggplot (scale_fill_manual)
# Returns: (nothing)

    n_groups <- length(groups)

    # response types to plot (assumed to be present in this sequence in data)
    resp_names <- c("Corr", "Err", "No idea", "Skipped")

    # which response categories to plot (e.g. no "skipped")
    to_plot <- c(1,2,3)
    n_plot <- length(to_plot)

    for (rr in restype) # counts and fractions
    {
        
        perf <- c(100*data[[groups[1]]][[rr]][[quest]][to_plot]) # initialise list to become dataframe for plotting
        resp <- resp_names[to_plot] # response type row names, fastest running dimension
        group <- rep(bar_legend[1], n_plot) # groups as row names, use specified legend names

        if (n_groups > 1) # append only if necessary
        {
            for (gg in c(2:n_groups)) # concatenate data for remaining groups
            {

                perf <- c(perf, 100*data[[groups[gg]]][[rr]][[quest]][to_plot]) # as %
                
                resp <- c(resp, resp_names[to_plot])
                
                group <- c(group, rep(bar_legend[gg], n_plot))
            }
        }

        # turn Nan to zero before plotting
        perf[is.nan(perf)] <- 0

        group <- factor(group)
        resp <- factor(resp)
        perf <- as.numeric(perf)

        data_plot <- data.frame( group, resp, perf )

        # make order of factor levels explicit for plotting
        data_plot$resp <- factor(data_plot$resp, levels=data_plot$resp[1:n_plot])
        data_plot$group <- factor(data_plot$group, levels=data_plot$group[seq(1,n_plot*n_groups,n_plot)])

        fsize <- 7
        if (n_groups > 3) # decrease numbers in bar graphs for dense plots
        {
            fsize <- 4
        }

        p <- ggplot(data=data_plot, aes(x=resp, y=perf, fill=group, ymax=max(perf))) + geom_bar(stat="identity", position=position_dodge()) +
                    labs(title=my_title) + theme_bw() + theme(text=element_text(size=24), legend.justification = c(1, 1), legend.position = c(1, 1)) +
                    scale_fill_manual(values=colours) + geom_text(aes(label=round(perf)), vjust=3, color="brown", position = position_dodge(0.9), size=fsize)

        print(p)

        # output plotted data in command window
        cat("\n")
        print(my_title)
        print(data_plot)

    }
} # plot_bargraphs()


plot_demographics <- function(demos, pdf_name)
{
# Plot bargraphs for demographic information from demographics().
# demos: list with demographics from demographics()
# pdf_name: string, filename for PDF output
# Returns: (nothing)

    # 2 alternating (e.g. "male/female")
    colours_2 <- c("#56B4E9", "#0072B2", "#E69F00", "#D55E00", "#F0E442", "#CC79A7", "#000000", "#999999")

    # 3 alternating (e.g. "all/male/female")
    colours_3 <- c("#000000", "#56B4E9", "#0072B2", "#666666", "#E69F00", "#D55E00", "#999999", "#F0E442", "#CC79A7")

    # 3 alternating (e.g. "psych/meth/biol")
    colours_3b <- c("#56B4E9", "#E69F00", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#000000", "#666666", "#999999")

    pdf(pdf_name, onefile=TRUE)
    cat("\n")
    print ( sprintf("Plotting to PDF: %s", pdf_name) )

    # number of male/female respondents
    groups <- c('Total', 'Males', 'Females') # used for x-labelling in figure

    group_names <- c('N_all', 'N_males', 'N_females') # indices to 'demos'

    colors <- c("black", "violetred4", "blue")

    counts <- as.numeric(demos[group_names])

    data_plot <- data.frame(groups, counts)

    # make order of factor levels explicit for plotting
    data_plot$groups <- factor(data_plot$groups, levels=data_plot$groups[1:3])

    p <- ggplot(data=data_plot, aes(x=groups, y=counts, fill=groups, ymax=max(counts))) + geom_bar(stat="identity", position=position_dodge()) + 
                labs(title="# Participants") + theme_bw() + theme(text=element_text(size=24), legend.justification = c(1, 1), legend.position = c(1, 1)) +
                scale_fill_manual(values=colours_3) + geom_text(aes(label=counts), vjust=3, color="brown", position = position_dodge(0.9), size=7)

    print(p)

    # number of respondents with different undergraduate degrees
    res_stat <- c('UG', 'UG', 'PhD', 'PhD', 'PD', 'PD', 'RA', 'RA')

    gender <- c('Male', 'Female', 'Male', 'Female', 'Male', 'Female', 'Male', 'Female')

    group_names <- c('n_ugrad_males', 'n_ugrad_females', 'n_phd_males', 'n_phd_females', 'n_pd_males', 'n_pd_females', 'n_ra_males', 'n_ra_females')

    counts <- as.numeric(demos[group_names])

    data_plot <- data.frame(gender, res_stat, counts)

    # make order of factor levels explicit for plotting
    data_plot$gender <- factor(data_plot$gender, levels=data_plot$gender[1:2])
    data_plot$res_stat <- factor(data_plot$res_stat, levels=data_plot$res_stat[c(1,3,5,7)])

    p <- ggplot(data=data_plot, aes(x=res_stat, y=counts, fill=gender, ymax=max(counts))) + geom_bar(stat="identity", position=position_dodge()) +
                labs(title="# Participants") + theme_bw() + theme(text=element_text(size=24), legend.justification = c(1, 1), legend.position = c(1, 1)) +
                scale_fill_manual(values=colours_2) + geom_text(aes(label=counts), vjust=3, color="brown", position = position_dodge(0.9), size=5)

    print(p)

    # number of respondents who rate themselves at different expertise levels
    ugrad <- c('Psych', 'Psych', 'Psych', 'Meth', 'Meth', 'Meth', 'Biol', 'Biol', 'Biol')

    expertise <- c('Expert', 'Sort of', 'No expert', 'Expert', 'Sort of', 'No expert', 'Expert', 'Sort of', 'No expert')

    group_names <- c('n_exp_yes_psych', 'n_exp_sortof_psych', 'n_exp_no_psych', 'n_exp_yes_meth', 'n_exp_sortof_meth', 'n_exp_no_meth',
                     'n_exp_yes_biol', 'n_exp_sortof_biol', 'n_exp_no_biol')

    fracs <- as.numeric(demos[group_names])

    data_plot <- data.frame(ugrad, expertise, fracs)

    # make order of factor levels explicit for plotting
    data_plot$ugrad <- factor(data_plot$ugrad, levels=data_plot$ugrad[c(1,4,7)])
    data_plot$expertise <- factor(data_plot$expertise, levels=data_plot$expertise[1:3])

    p <- ggplot(data=data_plot, aes(x=expertise, y=fracs, fill=ugrad)) + geom_bar(stat="identity", position=position_dodge()) +
                labs(title="% Participants") + theme_bw() + theme(text=element_text(size=24), legend.justification = c(0, 1), legend.position = c(0, 1)) +
                scale_fill_manual(values=colours_3b) + geom_text(aes(label=round(fracs)), vjust=3, color="brown", position = position_dodge(0.9), size=7)

    print(p)

    # as previous, for gender
    gender <- c('Male', 'Female', 'Male', 'Female', 'Male', 'Female')

    expertise <- c('Expert', 'Sort of', 'No expert', 'Expert', 'Sort of', 'No expert')

    group_names <- c('n_exp_yes_males', 'n_exp_sortof_males', 'n_exp_no_males', 'n_exp_yes_females', 'n_exp_sortof_females', 'n_exp_no_females')

    fracs <- as.numeric(demos[group_names])

    data_plot <- data.frame(gender, expertise, fracs)

    # make order of factor levels explicit for plotting
    data_plot$gender <- factor(data_plot$gender, levels=data_plot$gender[1:2])
    data_plot$expertise <- factor(data_plot$expertise, levels=data_plot$expertise[1:3])

    p <- ggplot(data=data_plot, aes(x=expertise, y=fracs, fill=gender, ymax=max(fracs))) + geom_bar(stat="identity", position=position_dodge()) +
                labs(title="% Participants") + theme_bw() + theme(text=element_text(size=24), legend.justification = c(0, 1), legend.position = c(0, 1)) +
                scale_fill_manual(values=colours_2) + geom_text(aes(label=round(fracs)), vjust=3, color="brown", position = position_dodge(0.9), size=7)

    print(p)
    
    # number of respondents in different current research positions
    ugrad <- c('Psych', 'Psych', 'Meth', 'Meth', 'Biol', 'Biol')

    gender <- c('Male', 'Female', 'Male', 'Female', 'Male', 'Female')

    group_names <- c('n_psych_males', 'n_psych_females', 'n_meth_males', 'n_meth_females', 'n_biol_males', 'n_biol_females')

    counts <- as.numeric(demos[group_names])

    data_plot <- data.frame(gender, ugrad, counts)

    # make order of factor levels explicit for plotting
    data_plot$gender <- factor(data_plot$gender, levels=data_plot$gender[1:2])
    data_plot$ugrad <- factor(data_plot$ugrad, levels=data_plot$ugrad[c(1,3,5)])

    p <- ggplot(data=data_plot, aes(x=ugrad, y=counts, fill=gender, ymax=max(counts))) + geom_bar(stat="identity", position=position_dodge()) +
                labs(title="# Participants") + theme_bw() + theme(text=element_text(size=24), legend.justification = c(1, 1), legend.position = c(1, 1)) +
                scale_fill_manual(values=colours_2) + geom_text(aes(label=counts), vjust=3, color="brown", position = position_dodge(0.9), size=7)

    print(p)

    group_names <- c('age_psych_males', 'age_psych_females', 'age_meth_males', 'age_meth_females', 'age_biol_males', 'age_biol_females')

    age <- as.numeric(demos[group_names])

    data_plot <- data.frame(gender, ugrad, age)

    # make order of factor levels explicit for plotting
    data_plot$gender <- factor(data_plot$gender, levels=data_plot$gender[1:2])
    data_plot$ugrad <- factor(data_plot$ugrad, levels=data_plot$ugrad[c(1,3,5)])

    p <- ggplot(data=data_plot, aes(x=ugrad, y=age, fill=gender, ymax=max(age))) + geom_bar(stat="identity", position=position_dodge()) +
                labs(title="Age (yrs)") + theme_bw() + theme(text=element_text(size=24)) +
                scale_fill_manual(values=colours_2) + geom_text(aes(label=round(age,1)), vjust=3, color="brown", position = position_dodge(0.9), size=7)

    print(p)

    dev.off()
} # plot_demographics()


get_dep_var <- function(Results_sub, qq, categ)
{
# Create dependent variable for logistic_regression.
# Results_sub: sub structure of Results (e.g. Results[["sex"]][["All"]])
# qq: string, name of questions
# categ: string, response category "Cor"/"Err"/"NoI"/"Skd"
# Returns: dv, dependent variable as factor

    n <- nrow(Results_sub[["indiv"]][["AllQs"]][["Cor"]]) # number of participants, doesn't depend on qq or categ

    tmp <- matrix(0,n,1)
    tmp[ Results_sub[["inds"]][[qq]][[categ]][[1]] ] <- 1

    dv <- factor( tmp )

    return( dv )
} # get_dep_var()


binomial_regression <- function(dv, iv, family="binomial")
{
# Compute logistic regression using glm and family="binomial".
# result computed for dv vs columns in iv
# dv: data frame, dependent variable
# iv: data frame, independent variables
# family: string, family of error distribution for glm()
# Returns: stat_list (list)
    
    stat_list <- list()
    
    # combine in data frame
    data_glm <- cbind(dv, iv)

    # formula for glm(): dv versus first column
    frm <- paste("dv ~ ", names(iv)[1])

    n_names <- length(names(iv))
    for (nn in c(2:n_names)) # if more than one column in iv
    {
        frm <- paste(frm, "+ ", names(iv)[nn])
    }

    print("Binomial GLM formula:")
    print(frm)

    frm <- as.formula(frm)

    # compute logistic regression model
    glm_out <- glm(formula=frm, data=data_glm, family=family)

    # get p-value
    p_fit <- coef(summary(glm_out))[,4]
    coef_fit <- exp(coef(glm_out))

    stat_list[["glm_out"]] <- glm_out
    stat_list[["p"]] <- p_fit
    stat_list[["coef"]] <- coef_fit

    return(stat_list)
} # binomial_regression()


multinomial_regression <- function(dv, iv_groups)
{
# Compute multinomial logistic regression using multinom from nnet.
# result computed for dv vs first column in iv
# dv: data frame, dependent variable
# iv_groups: dict of data frames with independent variables
#            each dict contains group of variables whose significant is
#            to be tested separately in model comparison
# Returns: stat_list (list)
    
    stat_list <- list()

    group_names <- names(iv_groups)
    n_groups <- length(iv_groups)
    
    # combine in data frame
    data_glm <- dv
    for (gg in group_names)
    {
        data_glm <- cbind(data_glm, iv_groups[gg])
    }

    # formulas for regression: dv versus other columns
    frm <- vector("list", n_groups+1)

    # create different models with different predictors for model comparison
    for (mm in c(1:(n_groups+1)))
    {
        print(mm)
        frm[mm] <- "dv ~ "
    }
   
    for (ff in c(1:(n_groups+1))) # per formula
    {
        pred_idx <- c(1:n_groups) # which predictor groups to include
        if (ff>1) # include everything for first formula
        {
            pred_idx <- pred_idx[-(ff-1)] # remove appropriate predictor group
        }

        n_preds <- length(pred_idx)

        for (pp in c(1:n_preds)) # across chosen predictor groups
        {
            print(pp)
            n_iv <- length(iv_groups[group_names[pred_idx[pp]]])
            print(group_names[pred_idx[pp]])
            # beware of [[]]
            iv_names <- names(iv_groups[[group_names[pred_idx[pp]]]])
            print(iv_names)
       
            # add predictor groups to appropriate formulas
            for (ii in c(1:n_iv))
            {
                if ((pp>1) || (ii>1))
                {
                    frm[ff] <- paste(frm[ff], "+ ")
                }
                frm[ff] <- paste(frm[ff], iv_names[ii])               
            }
        }
    }
   
    print( sprintf("Formula %s vs %s", frm[1], frm[2]) )

    frm1 <- as.formula(frm[[1]])
    frm2 <- as.formula(frm[[2]])

    # testing multinom ###
    print("Multinomial regression.")
    # mnr_out <- multinom(formula=frm, data=data_glm)
    # refLevel specifies the element of response variable to use as reference

    # http://dwoll.de/rexrepos/posts/regressionMultinom.html#model-comparisons---likelihood-ratio-tests
    vglm_out1 <- vglm(formula=frm1, data=data_glm, family=multinomial(refLevel=1))
    vglm_out2 <- vglm(formula=frm2, data=data_glm, family=multinomial(refLevel=1))

    vglm_stats <- lrtest(vglm_out1, vglm_out2)

    return(vglm_stats)
} # multinomial_regression()


ordered_logistic_regression <- function(dv, iv_groups)
{
# Compute ordered logistic regression using polr from MASS.
# (result computed for dv vs first column in iv ???)
# dv: data frame, dependent variable
# iv_groups: dict of data frames with independent variables
#            each dict contains group of variables whose significance is
#            to be tested separately in model comparison
# Returns: output of polr()

# With help from:
# https://stats.idre.ucla.edu/r/dae/ordinal-logistic-regression/
    
    stat_list <- list()

    group_names <- names(iv_groups)
    n_groups <- length(iv_groups)
    
    # combine dependent and independent variables in data frame
    data_glm <- dv
    for (gg in group_names)
    {
        data_glm <- cbind(data_glm, iv_groups[gg])
    }
   
    # add predictor groups to appropriate formulas
    frm <- "dv ~ "
    for (ii in c(1:n_groups))
    {
        if (ii>1)
        {
            frm <- paste(frm, "+ ")
        }
        frm <- paste(frm, names(iv_groups[[group_names[ii]]]))
    }
   
    print( sprintf("Formula %s", frm) )

    frm1 <- as.formula(frm)

    # testing multinom ###
    print("Ordered logistic regression.")

    model <- polr(formula=frm1, data=data_glm, Hess=TRUE)

    return(model)
} # ordered_logistic_regression()


plot_Results <- function(Results, fig_outdir)
{
    # Plot results to figures (PDF).
    # Results: list with results from get_all_Results(), entries are output from get_indiv_Results()
    # fig_outdir: directory for figures

    # colours for bargraphs in ggplot:
    # color-blind-friendly palette (re-arranged from http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/)
    # original: cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

    # 2 alternating (e.g. "male/female")
    colours_2 <- c("#56B4E9", "#0072B2", "#E69F00", "#D55E00", "#F0E442", "#CC79A7", "#000000", "#999999")

    # 3 alternating (e.g. "all/male/female")
    colours_3 <- c("#000000", "#56B4E9", "#0072B2", "#666666", "#E69F00", "#D55E00", "#999999", "#F0E442", "#CC79A7")

    # 3 alternating (e.g. "psych/meth/biol")
    colours_3b <- c("#56B4E9", "#E69F00", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#000000", "#666666", "#999999")


    # SUMMARY PLOTS across ALL QUESTIONS
    # restype <- c("sum_counts", "sum_frac") # summary across all questions
    restype <- c("sum_frac")

    pdf_name <- sprintf("%sSummaries_General.pdf", fig_outdir) # avoid some problems with long filenames
    pdf(pdf_name, onefile=TRUE)
    cat("\n")
    print ( sprintf("Plotting to PDF: %s", pdf_name) )

    # All
    # Results$sex$All$sum_frac$sum
    
    # All
    groups <- c("All")
    bar_legend <- c("All")
    plot_bargraphs(Results[["sex"]], groups, "sum", restype, "All", bar_legend, colours_3)

    # gender
    groups <- c("males", "females")
    bar_legend <- c("Males", "Females")
    plot_bargraphs(Results[["sex"]], groups, "sum", restype, "Gender", bar_legend, colours_2)

    # degree
    groups <- c("ugrad_group_psych", "ugrad_group_meth", "ugrad_group_biol")
    bar_legend <- c("Psych", "Meth", "Biol")
    plot_bargraphs(Results[["degree"]], groups, "sum", restype, "Undergraduate Degree", bar_legend, colours_3b)

    # degree-by-gender
    groups <- c("ugrad_group_psych_males", "ugrad_group_psych_females", "ugrad_group_meth_males", "ugrad_group_meth_females", "ugrad_group_biol_males", "ugrad_group_biol_females")
    bar_legend <- c("Psych M", "Psych F", "Meth M", "Meth F", "Biol M", "Biol F")
    plot_bargraphs(Results[["deg_sex"]], groups, "sum", restype, "Degree-by-Gender", bar_legend, colours_2)

    # expertise
    groups <- c("expert_yes", "expert_sortof", "expert_no")
    bar_legend <- c("Expert", "Sort of Expert", "No Expert")
    plot_bargraphs(Results[["Expertise"]], groups, "sum", restype, "Expertise", bar_legend, colours_3b)

    # gender-by-expertise
    groups <- c("expert_yes_males", "expert_yes_females", "expert_sortof_males", "expert_sortof_females", "expert_no_males", 
                "expert_no_females")
    bar_legend <- c("Expert M", "Expert F", "Sort of Expert M", "Sort of Expert F", "No Expert M",
                    "No Expert F")
    plot_bargraphs(Results[["ExpGend"]], groups, "sum", restype, "Expertise-by-Gender", bar_legend, colours_2)

    # undergrad-by-expertise
    groups <- c("expert_yes_psych", "expert_yes_meth", "expert_yes_biol", "expert_sortof_psych", "expert_sortof_meth", "expert_sortof_biol",
                "expert_no_psych",  "expert_no_meth",  "expert_no_biol")
    bar_legend <- c("Expert Psych", "Expert Meth", "Expert Biol", "S-o-E Psych", "S-o-E Meth", "S-o-E Biol",
                    "No Expert Psych", "No Expert Meth", "No Expert Biol")
    plot_bargraphs(Results[["ExpUgrad"]], groups, "sum", restype, "Expertise-by-Ugrad", bar_legend, colours_3b)


    # Researcher type
    groups <- c("Undgrad All", "Undgrad males", "Undgrad females", "PhD All", "PhD males", "PhD females",
                    "Postdoc All", "Postdoc males", "Postdoc females")
    bar_legend <- c("Ugrad All", "Ugrad M", "Ugrad F", "PhD All", "PhD M", "PhD F",
                    "Pdoc All", "Pdoc M", "Pdoc F")
    plot_bargraphs(Results[["Research area"]], groups, "sum", restype, "Researcher type", bar_legend, colours_3)

    # Training needs
    groups <- c("All", "males", "females")
    bar_legend <- c("All", "males", "females")
    bar_names <- c("A lot", "Signif.", "A little", "No", "DK")
    plot_general_questions(Results[["Training needs"]], groups, "Training needs", bar_names, bar_legend, colours_3)

    groups <- c("Undgrad All", "PhD All", "Postdoc All")
    bar_legend <- c("Ugrad All", "PhD All", "Pdoc All")
    bar_names <- c("A lot", "Signif.", "A little", "No", "DK")
    plot_general_questions(Results[["Training needs"]], groups, "Training needs", bar_names, bar_legend, colours_3b)

    groups <- c("Undgrad All", "Undgrad males", "Undgrad females", "PhD All", "PhD males", "PhD females",
                    "Postdoc All", "Postdoc males", "Postdoc females")
    bar_legend <- c("Ugrad All", "Ugrad males", "Ugrad females", "PhD All", "PhD males", "PhD females",
                    "Pdoc All", "Pdoc males", "Pdoc females")
    bar_names <- c("A lot", "Signif.", "A little", "No", "DK")
    plot_general_questions(Results[["Training needs"]], groups, "Training needs", bar_names, bar_legend, colours_3)

    groups <- c("ugrad_group_psych_males", "ugrad_group_psych_females", "ugrad_group_meth_males", "ugrad_group_meth_females", "ugrad_group_biol_males", "ugrad_group_biol_females")
    bar_legend <- c("Ugrads Psych M", "Ugrads Psych F", "Ugrads Meth M", "Ugrads Meth F", "Ugrads Biol M", "Ugrads Biol F")
    bar_names <- c("A lot", "Signif.", "A little", "No", "DK")
    plot_general_questions(Results[["Training needs"]], groups, "Training needs", bar_names, bar_legend, colours_2)

    # Researcher type counts
    groups <- c("All", "males", "females")
    bar_legend <- c("All", "males", "females")
    bar_names <- c("Ugrad", "PhD", "Pdoc", "Master's", "RA")
    plot_general_questions(Results[["Research area cnts"]], groups, "Researcher type", bar_names, bar_legend, colours_3)

    # Future area cnts
    groups <- c("All", "males", "females")
    bar_legend <- c("All", "males", "females")
    bar_names <- c("Psych", "CogSci", "CogNsci", "CliNsci", "DK", "Other")
    plot_general_questions(Results[["Future area cnts"]], groups, "Future area", bar_names, bar_legend, colours_3)

    dev.off()

    # SUMMARY PLOTS across SUB-GROUPS OF QUESTIONS
    pdf_name <- sprintf("%sSummaries_QuestionGroups.pdf", fig_outdir) # avoid some problems with long filenames
    pdf(pdf_name, onefile=TRUE)
    cat("\n")
    print ( sprintf("Plotting to PDF: %s", pdf_name) )

    # restype <- c("sum_counts", "sum_frac") # summary across all questions
    restype <- c("sum_qgroup_frac")

    # subgroups of questions
    qgroup_names <- list(linalg = "Linear Algebra", calc = "Calculus", progr = "Programming", signal = "Signal Analysis",
                    phys = "Physics", stats = "Statistics")

    for (qq in names(qgroup_names))
    {
        # gender
        groups <- c("All", "males", "females")
        bar_legend <- c("All", "males", "females")
        title <- sprintf("Gender | %s", qgroup_names[qq])
        plot_bargraphs(Results[["sex"]], groups, qq, restype, title, bar_legend, colours_3)

        # degree
        groups <- c("ugrad_group_psych", "ugrad_group_meth", "ugrad_group_biol")
        bar_legend <- c("Psych", "Meth", "Biol")
        title <- sprintf("Undergraduate Degree | %s", qgroup_names[qq])
        plot_bargraphs(Results[["degree"]], groups, qq, restype, title, bar_legend, colours_3b)

        # degree-by-sex
        groups <- c("ugrad_group_psych_males", "ugrad_group_psych_females", "ugrad_group_meth_males", "ugrad_group_meth_females", "ugrad_group_biol_males", "ugrad_group_biol_females")
        bar_legend <- c("Psych M", "Psych F", "Meth M", "Meth F", "Biol M", "Biol F")
        title <- sprintf("Degree-by-Gender | %s", qgroup_names[qq])
        plot_bargraphs(Results[["deg_sex"]], groups, qq, restype, title, bar_legend, colours_2)

        # expertise
        groups <- c("expert_yes", "expert_sortof", "expert_no")
        bar_legend <- c("Expert", "Sort of Expert", "No Expert")
        title <- sprintf("Expertise | %s", qgroup_names[qq])        
        plot_bargraphs(Results[["Expertise"]], groups, qq, restype, title, bar_legend, colours_3b)
    }
    dev.off()


    # types of results to be plotted
    # restype <- c("counts", "frac")
    restype <- c("frac")

    # plot into one PDF
    pdf_name <- sprintf("%sAllplots.pdf", fig_outdir) # avoid some problems with long filenames
    pdf(pdf_name, onefile=TRUE)
    cat("\n")
    print ( sprintf("Plotting to PDF: %s", pdf_name) )

    for (qq in q_meth_names)
    {
        filestem <- "all"
        groups <- c("males", "females")
        bar_legend <- c("Males", "Females")
        plot_bargraphs(Results[["sex"]], groups, qq, restype, qq, bar_legend, colours_2)

        filestem <- "degree"
        groups <- c("ugrad_group_psych", "ugrad_group_meth", "ugrad_group_biol")
        bar_legend <- c("Psych", "Meth", "Biol")
        plot_bargraphs(Results[["degree"]], groups, qq, restype, qq, bar_legend, colours_3b)

        filestem <- "deg_sex"
        groups <- c("ugrad_group_psych_males", "ugrad_group_psych_females", "ugrad_group_meth_males", "ugrad_group_meth_females", "ugrad_group_biol_males", "ugrad_group_biol_females")
        bar_legend <- c("Psych M", "Psych F", "Meth M", "Meth F", "Biol M", "Biol F")
        plot_bargraphs(Results[["deg_sex"]], groups, qq, restype, qq, bar_legend, colours_2)

        filestem <- "expertise"
        groups <- c("expert_yes", "expert_sortof", "expert_no")
        bar_legend <- c("Expert", "Sort of Expert", "No Export")
        plot_bargraphs(Results[["Expertise"]], groups, qq, restype, qq, bar_legend, colours_3b)

    }

    # closes all plot windows:
    dev.off()
    graphics.off()

} # plot_Results()


get_Stats <- function(Results, groups_all)
{
# Compute logistic regression stats for results from get_all_Results().
# Results: list with results from get_all_Results(), entries are output from get_indiv_Results()
# groups_all: list with indices of respondents (rows of data) for different groups
# Returns: stat_list, list of results from logistic regression models
    
    ### STATISTICS
    stat_list <- list() # collect results from logistic regression

    # response categories
    # categs <- c("Cor", "Err", "NoI")
    categs <- c("Cor")

    n_pps <- length(groups_all$All)

    # factor undergraduate degree
    iv_ugrad <- matrix(0,n_pps,1)
    iv_ugrad[which(groups_all[["ugrad_group_psych"]])] = 1
    iv_ugrad[which(groups_all[["ugrad_group_biol"]])] = 2
    iv_ugrad[which(groups_all[["ugrad_group_meth"]])] = 3

    iv_ugrad <- data.frame( iv_ugrad )

    # factor researcher type
    iv_restyp <- matrix(0,n_pps,1)
    iv_restyp[which(groups_all[["researcher_undgrad"]])] = 1
    iv_restyp[which(groups_all[["researcher_phd"]])] = 2
    iv_restyp[which(groups_all[["researcher_postdoc"]])] = 3
    iv_restyp[which(groups_all[["researcher_resass"]])] = 4
    iv_restyp[which(groups_all[["researcher_skipped"]])] = 5

    iv_restyp <- data.frame( iv_restyp )

    # factor gender
    iv_gender <- 1*groups_all[["males"]]
    iv_gender <- data.frame(iv_gender)

    ## STATS (Logistic multiple regression)
    cat("\n######################\n")

    # INDEPENDENT VARIABLES
    iv <- cbind(iv_gender, iv_ugrad, iv_restyp)

    # classic binomial regression for individual questions
    # because dependent variable is 0 or 1

    cat("\n###\nIndividual questions.\n###\n")

    for (qq in q_meth_names)
    {
        cat(sprintf("\n###\n %s\n###\n", qq))

        for (cc in categs) # Cor|Err|NoI
        {
            # DEPENDENT VARIABLE
            dv <- get_dep_var(Results[["sex"]][["All"]], qq, cc)
            dv <- data.frame(dv)
            
            model <- binomial_regression(dv, iv)

            stat_list[[qq]][[cc]] <- model

            print( sprintf("Gender - Coef: %f, p: %f", model[["coef"]][2], model[["p"]][2]) )
            print( sprintf("Ugrad - Coef: %f, p: %f", model[["coef"]][3], model[["p"]][3]) )
            print( sprintf("Res Type - Coef: %f, p: %f", model[["coef"]][4], model[["p"]][4]) )
        }
    }

    # organise IVs in groups that can be tested in model comparison
    iv_groups <- list()
    # beware of [[]]
    iv_groups[['gend']] <- iv_gender
    iv_groups[['ugrad']] <- iv_ugrad
    iv_groups[['restyp']] <- iv_restyp

    # stats for subgroups of questions

    cat("\n###\nSub-groups of questions.\n###\n")

    # ordered logistic regression for subgroups of questions
    # because dependent variables can have multiple ordered levels

    qgroup_names <- list(linalg = "Linear Algebra", calc = "Calculus", progr = "Programming", signal = "Signal Analysis",
                    phys = "Physics", stats = "Statistics")

    for (qq in names(qgroup_names))
    {
        cat(sprintf("\n###\n %s\n###\n", qq))

        for (cc in categs) # Cor|Err|NoI
        {
            dv <- array( Results[["sex"]][["All"]][["indiv"]][[qq]][[cc]] )
            dv <- as.factor(dv)
            dv <- data.frame(dv)
            
            model <- ordered_logistic_regression(dv, iv_groups)        

            ## store table
            (ctable <- coef(summary(model)))

            ## calculate and store p values
            p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2

            ## combined table
            (ctable <- cbind(ctable, "p value" = p))

            print(ctable)

            stat_list[[qq]][[cc]] <- ctable

            # # confidence intervals
            # (ci <- confint(model)) # default method gives profiled CIs
            
            # print(ci)
        }
    }

    cat("\n###\nAcross all questions.\n###\n")

    # ordered logistic regression for subgroups of questions
    # because dependent variables can have multiple ordered levels

    for (cc in categs)
    {
        cat("\n")
        print( cc )

        dv <- array( Results[["sex"]][["All"]][["indiv"]][["AllQs"]][[cc]] )
        dv <- as.factor(dv)
        dv <- data.frame(dv)

        model <- ordered_logistic_regression(dv, iv_groups)    

        ## store table
        (ctable <- coef(summary(model)))

        ## calculate and store p values
        p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2

        ## combined table
        (ctable <- cbind(ctable, "p value" = p))

        print(ctable)

        stat_list[[qq]][[cc]] <- ctable

        # confidence intervals
        (ci <- confint(model)) # default method gives profiled CIs
        
        print(ci)
    }

    return( stat_list )

} # get_Stats()