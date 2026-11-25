# ==============================================================================
# rv_treatments.R -- the randomized contrast(s) behind each study card, as code
# ==============================================================================
# One entry per card id: a function of the harmonized data returning a list of
# contrasts. Each contrast has a name, a 0/1/NA treatment vector (NA = not in
# this contrast), and `primary`: the target construct(s) the card expects it to
# move. Every measured construct (targets and placebos) is estimated under
# every contrast; placebos are the false-positive check.
#
# Conjoint attribute contrasts are defined on the PAIR SHOWN (either profile
# carries the level), never on the chosen profile: the choice is
# post-treatment, so conditioning on it would break randomization (R2, point 3).
# Where a draft card phrased the contrast on the chosen profile, the pair-shown
# version is used and listed in revision/deviations.md.
# ==============================================================================

pair_has <- function(d, attr, levels) {
  a <- d[[paste0("chosen_", attr)]]
  b <- d[[paste0("other_", attr)]]
  out <- as.integer(a %in% levels | b %in% levels)
  out[is.na(a) & is.na(b)] <- NA
  out
}
pair_differs <- function(d, attr) {
  a <- d[[paste0("chosen_", attr)]]
  b <- d[[paste0("other_", attr)]]
  out <- as.integer(a != b)
  out[is.na(a) | is.na(b)] <- NA
  out
}
arm_vs <- function(x, treated, reference) {
  out <- rep(NA_integer_, length(x))
  out[x %in% treated] <- 1L
  out[x %in% reference] <- 0L
  out
}
ctr <- function(name, treat, primary, note = "") {
  list(name = name, treat = treat, primary = primary, note = note)
}

TREATMENTS <- list(
  sktw_naturalization_2024 = function(d) list(
    ctr("cue_shown", as.integer(d$treatment), "civic_commitment",
        "social-integration attribute shown in all tasks vs withheld")),
  aks21_c1_hire = function(d) list(
    ctr("unemployed_in_pair", pair_has(d, "occupation", "Unemployed"), "employment_occupation")),
  aks21_c1_neighbor = function(d) list(
    ctr("nk_only_colleagues_in_pair",
        pair_has(d, "social_networks", "Has only North Korean defector-migrant colleagues"),
        "social_integration_ties")),
  aks21_c1_vote = function(d) list(
    ctr("kwp_member_in_pair", pair_has(d, "former_kwp_member", "Yes"), "north_korea_regime_ties")),
  aks21_c2_nkbusiness = function(d) list(
    ctr("discrimination_rationale_in_pair",
        pair_has(d, "policy_rationale", "It will help North Koreans who face discrimination"),
        "anti_discrimination_help"),
    ctr("local_business_rationale_in_pair",
        pair_has(d, "policy_rationale", "It will improve local business conditions"),
        "local_business_conditions")),
  aks21_c3_unification = function(d) list(
    ctr("liberal_democracy_in_pair",
        pair_has(d, "political_system", "North-South integration under liberal democratic system"),
        "political_system_democracy"),
    ctr("authoritarian_in_pair",
        pair_has(d, "political_system", grep("authoritarian", unique(c(d$chosen_political_system, d$other_political_system)), value = TRUE)),
        "political_system_democracy")),
  aks21_c4_nsa = function(d) {
    lv <- unique(c(d$chosen_rationale, d$other_rationale))
    list(
      ctr("security_rationale_in_pair", pair_has(d, "rationale", grep("national security", lv, value = TRUE)),
          "national_security_threat"),
      ctr("democracy_rationale_in_pair", pair_has(d, "rationale", grep("liberal democracy", lv, value = TRUE)),
          "liberal_democracy_rights"))
  },
  ifes2023_de_jobtraining = function(d) list(
    ctr("saxony_in_pair", pair_has(d, "birth_origin", "Saxony, DE"), "east_origin_reasoning")),
  ifes2023_sk_jobtraining = function(d) list(
    ctr("dprk_in_pair", pair_has(d, "birth_origin", "North Hamgyong, DPRK"), "nk_origin_reasoning")),
  ifes2023_de_politician = function(d) list(
    ctr("diversity_differs", pair_differs(d, "diversity"), "diversity_reasoning")),
  ifes2023_sk_politician = function(d) list(
    ctr("democracy_differs", pair_differs(d, "democracy"), "democracy_reasoning")),
  ifes2023_nk_politician = function(d) list(
    ctr("democracy_differs", pair_differs(d, "democracy"), "democracy_reasoning")),
  ifes2023_nk_natid = function(d) list(
    ctr("institutions_differs", pair_differs(d, "political_institutions"), "civic_law_criteria")),
  ifes2023_de_natid = function(d) list(
    ctr("religion_attribute_shown", as.integer(d$treatment), "religious_criteria")),
  prc2026_q22_national = function(d) list(
    ctr("patriotic_often_in_pair", pair_has(d, "patriotism", "Patriotic, often expresses"),
        "patriotic_expression")),
  roc2026_q12_national = function(d) list(
    ctr("patriotic_often_in_pair", pair_has(d, "patriotism", "Patriotic, often expresses"),
        "patriotic_expression")),
  prc2026_q60_ngo = function(d) list(
    ctr("national_security_vs_control", arm_vs(d$treatment, "National security", "Control"),
        c("foreign_influence_concern", "state_alignment")),
    ctr("private_security_vs_control", arm_vs(d$treatment, "Private security", "Control"),
        character(0), "secondary arm"),
    ctr("public_morality_vs_control", arm_vs(d$treatment, "Public morality", "Control"),
        character(0), "secondary arm")),
  roc2024_q74_national = function(d) list(
    ctr("taiwanese_identity_in_pair", pair_has(d, "sentiment", "Identifies as Taiwanese"),
        "national_self_identification"),
    ctr("democracy_stance_in_pair",
        pair_has(d, "politics", c("Thinks democracy is good despite flaws",
                                  "Thinks democracy is chaotic, prefers one-party rule")),
        "democratic_commitment")),
  rok2024_q40_election = function(d) {
    sc <- as.integer((d$chosen_scandal != "None") | (d$other_scandal != "None"))
    list(
      ctr("presidential_frame", arm_vs(d$treatment, "presidential", "general"),
          "programmatic_policy_reasoning"),
      ctr("allegation_in_pair", sc, "scandal_mention"))
  },
  nwo26_exp2 = function(d) list(
    ctr("procedure_violation", arm_vs(d$profile_procedure,
        c("executive_overreach", "external_imposition", "procedural_neglect"), "control"),
        "procedural_legitimacy"),
    ctr("unequal_vs_even_application", arm_vs(d$profile_implementation, "unequal_application", "even_handed"),
        "equal_treatment_in_application")),
  nwo26_exp3_comprehension = function(d) {
    key <- paste(d$country, d$cn_variant)
    charter <- c("US v1", "DE v1", "DE v2", "KR v1", "KR v3")
    none <- c("US v2", "US v4", "DE v3", "KR v2")
    list(ctr("charter_keyed_variant", arm_vs(key, charter, none), "founding_charter",
             "within the treatment arm only (no control-arm text)"))
  }
)

# --- External datasets with more than two arms or a moderator -----------------
# Binary 0/1 designs use the default (treatment column, every target primary).
TREATMENTS <- c(TREATMENTS, list(
  ofosu2024_malawi = function(d) list(
    ctr("discrimination_vs_progress_video", arm_vs(d$treatment, "low_viability", "high_viability"),
        c("discrimination_low_viability", "women_progress")),
    ctr("discrimination_vs_control", arm_vs(d$treatment, "low_viability", "control"), character(0), "secondary"),
    ctr("progress_vs_control", arm_vs(d$treatment, "high_viability", "control"), character(0), "secondary")),
  bisgaard2019_study1 = function(d) list(
    ctr("pos_vs_neg_gov_supporters", ifelse(d$gov %in% 1, arm_vs(d$treatment, "pos", "neg"), NA),
        c("incumbent_responsibility", "other_actor_responsibility")),
    ctr("pos_vs_neg_opposition", ifelse(d$gov %in% 0, arm_vs(d$treatment, "pos", "neg"), NA),
        c("incumbent_responsibility", "other_actor_responsibility")),
    ctr("pos_vs_neg_pooled", arm_vs(d$treatment, "pos", "neg"), character(0), "no pooled sign theorized")),
  bisgaard2019_study3 = function(d) list(
    ctr("pos_vs_neg_gov_supporters", ifelse(d$gov %in% 1, as.integer(d$treatment), NA),
        c("incumbent_responsibility", "other_actor_responsibility")),
    ctr("pos_vs_neg_opposition", ifelse(d$gov %in% 0, as.integer(d$treatment), NA),
        c("incumbent_responsibility", "other_actor_responsibility"))),
  bisgaard2019_study4 = function(d) list(
    ctr("pos_vs_neg_gov_supporters", ifelse(d$gov %in% 1, as.integer(d$treatment), NA),
        c("incumbent_responsibility", "other_actor_responsibility")),
    ctr("pos_vs_neg_opposition", ifelse(d$gov %in% 0, as.integer(d$treatment), NA),
        c("incumbent_responsibility", "other_actor_responsibility"))),
  dur2026_china = function(d) list(
    ctr("rival_vs_home_cooperation", arm_vs(d$treatment, "rival", "home"), c("negative_relations", "security_concern")),
    ctr("rival_vs_control", arm_vs(d$treatment, "rival", "control"), character(0), "secondary")),
  dur2026_us = function(d) list(
    ctr("adversary_vs_home_cooperation", arm_vs(d$treatment, "adversary", "home"), c("negative_relations", "security_concern")),
    ctr("adversary_vs_control", arm_vs(d$treatment, "adversary", "control"), character(0), "secondary")),
  ziegler2022_kane = function(d) list(
    ctr("disunited_vs_united", arm_vs(d$treatment, "disunited", "united"), c("supporters_upset", "supporters_pleased")),
    ctr("disunited_vs_control", arm_vs(d$treatment, "disunited", "control"), "supporters_upset", "secondary")),
  hartnett2022_whytrump = function(d) list(
    ctr("margin_above_median", as.integer(d$treatment_binary), character(0),
        "authors expect no effect: a null-expected check")),
  davies2013_audiencecosts = function(d) list(
    ctr("threat_not_carried_out_vs_nothing", arm_vs(d$treatment, c("threat", "escalated_threat"), "control"),
        "inconsistency_credibility")),
  marx2025_genderlanguage = function(d) list(
    ctr("active_gnl_vs_control", arm_vs(d$treatment, "active_gnl", "control"), "gnl_rejection"),
    ctr("passive_gnl_vs_control", arm_vs(d$treatment, "passive_gnl", "control"), character(0), "secondary"),
    ctr("technical_vs_control", arm_vs(d$treatment, "technical", "control"), character(0), "placebo request arm")),
  # --- wave 3 (registered 17 Nov 2026) ---------------------------------------
  andre2022_primemech = function(d) list(
    ctr("costs_vs_control", arm_vs(d$treatment, "costs", "control"), "firm_cost_channel"),
    ctr("demand_vs_control", arm_vs(d$treatment, "demand", "control"), "demand_channel")),
  # H3a-d of the preregistration: coded menus cue politics; vegan and ethnic
  # point left, meat and traditional point right. One primary contrast per
  # target; the single-menu contrasts are secondary.
  scaduto2023_foodpolitics = function(d) list(
    ctr("coded_vs_base", arm_vs(d$treatment, c("vegan", "meat", "traditional", "ethnic"), "base"), "food_political_link"),
    ctr("left_coded_vs_base", arm_vs(d$treatment, c("vegan", "ethnic"), "base"), "progressive_cosmopolitan_link"),
    ctr("right_coded_vs_base", arm_vs(d$treatment, c("meat", "traditional"), "base"), "tradition_nation_link"),
    ctr("left_vs_right_coded", arm_vs(d$treatment, c("vegan", "ethnic"), c("meat", "traditional")), character(0), "secondary"),
    ctr("vegan_vs_base", arm_vs(d$treatment, "vegan", "base"), character(0), "secondary"),
    ctr("ethnic_vs_base", arm_vs(d$treatment, "ethnic", "base"), character(0), "secondary"),
    ctr("meat_vs_base", arm_vs(d$treatment, "meat", "base"), character(0), "secondary"),
    ctr("traditional_vs_base", arm_vs(d$treatment, "traditional", "base"), character(0), "secondary")),
  hahn2024_oeq_pos = function(d) list(
    ctr("end_vs_beginning", as.integer(d$treatment), "closed_item_aspect"),
    ctr("motivating_vs_neutral_cue", as.integer(d$cue), character(0), "secondary; the authors expect little effect on content")),
  hahn2024_oeq_neg = function(d) list(
    ctr("end_vs_beginning", as.integer(d$treatment), "closed_item_aspect"),
    ctr("motivating_vs_neutral_cue", as.integer(d$cue), character(0), "secondary; the authors expect little effect on content")),
  # Andre 2025: one primary contrast per card; the comparisons with the
  # no-information arm are secondary (the information states the target).
  andre2025_shallowmeritocracy_attention = function(d) list(
    ctr("attention_vs_treatment", arm_vs(d$treatment, c("attention", "eqrates_attention"), c("treatment", "eqrates_treatment")),
        "rate_shapes_effort", "pooled over the two pay-rate conditions"),
    ctr("attention_vs_treatment_unequal_rates", arm_vs(d$treatment, "attention", "treatment"), character(0), "secondary"),
    ctr("attention_vs_treatment_equal_rates", arm_vs(d$treatment, "eqrates_attention", "eqrates_treatment"), character(0), "secondary"),
    ctr("treatment_vs_control", arm_vs(d$treatment, "treatment", "control"), character(0), "secondary; the author expects no difference"),
    ctr("eqtreatment_vs_eqcontrol", arm_vs(d$treatment, "eqrates_treatment", "eqrates_control"), character(0), "secondary")),
  andre2025_shallowmeritocracy_cf = function(d) list(
    ctr("high_vs_low", arm_vs(d$treatment, "high", "low"), "disadvantage_shaped_effort"),
    ctr("high_vs_none", arm_vs(d$treatment, "high", "none"), character(0), "secondary"),
    ctr("low_vs_none", arm_vs(d$treatment, "low", "none"), character(0), "secondary")),
  andre2025_shallowmeritocracy_cfadv = function(d) list(
    ctr("low_vs_high", arm_vs(d$treatment, "low", "high"), "advantage_shaped_effort"),
    ctr("low_vs_none", arm_vs(d$treatment, "low", "none"), character(0), "secondary"),
    ctr("high_vs_none", arm_vs(d$treatment, "high", "none"), character(0), "secondary")),
  andre2025_shallowmeritocracy_costs = function(d) list(
    ctr("tedious_vs_not", as.integer(d$treatment), "work_was_unpleasant")),
  andre2025_shallowmeritocracy_vignettes = function(d) list(
    ctr("high_vs_low", arm_vs(d$treatment, "high", "low"), "circumstances_shaped_choice"),
    ctr("high_vs_none", arm_vs(d$treatment, "high", "none"), character(0), "secondary"),
    ctr("low_vs_none", arm_vs(d$treatment, "low", "none"), character(0), "secondary")),
  andre2025_macronarratives_provision = function(d) list(
    ctr("pudem_vs_baseline", arm_vs(d$treatment, "pudem", "baseline"), "pent_up_demand_cause"),
    ctr("energy_vs_baseline", arm_vs(d$treatment, "energy", "baseline"), "energy_crisis_cause")),
  andre2025_macronarratives_mp = function(d) list(
    ctr("mp_vs_energy", as.integer(d$treatment), c("monetary_policy_cause", "energy_crisis_cause"),
        "energy is the active control, so its narrative is expected to fall")),
  sperber2024_zambia = function(d) list(
    ctr("religious_vs_infoonly_secular", arm_vs(d$treatment, "religious", c("infoonly", "secular")), "religious_identity"),
    ctr("religious_vs_infoonly", arm_vs(d$treatment, "religious", "infoonly"), character(0), "secondary; do-file Table 5 trackC"),
    ctr("religious_vs_secular", arm_vs(d$treatment, "religious", "secular"), character(0), "secondary; do-file test trackB=trackC")),
  andre2024_stockmodels_pricecosts = function(d) list(
    ctr("cost_frames_vs_price_info", arm_vs(d$treatment, c("number_of_shares", "purchase_cost"), "price_info"), "prices_as_investor_costs"),
    ctr("price_info_vs_control", arm_vs(d$treatment, "price_info", "control"), "price_reference"),
    ctr("number_of_shares_vs_price_info", arm_vs(d$treatment, "number_of_shares", "price_info"), character(0), "secondary"),
    ctr("purchase_cost_vs_price_info", arm_vs(d$treatment, "purchase_cost", "price_info"), character(0), "secondary")),
  andre2024_stockmodels_attention = function(d) list(
    ctr("price_reaction_vs_control", arm_vs(d$treatment, "price_reaction", "control"), "price_response_reference"),
    ctr("trader_reaction_vs_control", arm_vs(d$treatment, "trader_reaction", "control"), character(0), "secondary")),
  # Carey et al. 2025, Study 1: each arm has its own hypothesis and item.
  nyhan2024_targetedcorrections_seats2022 = function(d) list(
    ctr("prebunking_vs_placebo", arm_vs(d$treatment, "prebunking", "placebo"), "election_safeguards"),
    ctr("credible_vs_placebo", arm_vs(d$treatment, "credible", "placebo"), character(0), "secondary")),
  nyhan2024_targetedcorrections_seats2020 = function(d) list(
    ctr("credible_vs_placebo", arm_vs(d$treatment, "credible", "placebo"), "republican_or_judicial_sources"),
    ctr("prebunking_vs_placebo", arm_vs(d$treatment, "prebunking", "placebo"), character(0), "secondary")),
  # Task-level: the senator's side and behavior are randomized per vignette;
  # "agree" is the respondent's pre-treatment left-right side matching it.
  krishnarajan2022_rationalizing = function(d) list(
    ctr("undem_agree_vs_disagree", arm_vs(d$treatment, "undemocratic_agree", "undemocratic_disagree"),
        c("democratic_norm_violation", "policy_or_country_benefit")),
    ctr("regular_disagree_vs_agree", arm_vs(d$treatment, "regular_disagree", "regular_agree"), character(0), "secondary"),
    ctr("undemocratic_vs_regular", arm_vs(d$treatment, c("undemocratic_agree", "undemocratic_disagree", "undemocratic_neutral"),
        c("regular_agree", "regular_disagree", "regular_neutral")), character(0), "secondary; positive control")),
  zarpli2023_turkeysanctions = function(d) list(
    ctr("uncertain_vs_certain", arm_vs(d$treatment, c("uncertain", "uncertain_cost"), c("certain", "certain_cost")),
        "sender_ulterior_motives", "pooled over the cost factor (article Table 2)"),
    ctr("uncertain_vs_certain_nocost", arm_vs(d$treatment, "uncertain", "certain"), character(0), "secondary; the authors' Rmd contrast (T3 vs T1)"),
    ctr("uncertain_vs_control", arm_vs(d$treatment, c("uncertain", "uncertain_cost"), "control"), character(0), "secondary; article Table 3"),
    ctr("cost_vs_nocost", arm_vs(d$treatment, c("certain_cost", "uncertain_cost"), c("certain", "uncertain")), character(0),
        "secondary; positive control for the placebo economic_consequences")),
  # TESS studies (factorials: the primary contrast pools over the other factors)
  levine2009_donate = function(d) list(
    ctr("financial_vs_general", arm_vs(d$treatment, c("financial_donate", "financial_volunteer"), c("general_donate", "general_volunteer")), "personal_financial_constraint"),
    ctr("financial_vs_general_donation_only", arm_vs(d$treatment, "financial_donate", "general_donate"), character(0), "secondary"),
    ctr("nonfinancial_vs_general", arm_vs(d$treatment, c("nonfinancial_donate", "nonfinancial_volunteer"), c("general_donate", "general_volunteer")), character(0), "secondary")),
  banaji2019_surgeon = function(d) list(
    ctr("daughter_or_child_vs_son", arm_vs(d$treatment, c("daughter", "child"), "son"), "mother_answer"),
    ctr("child_vs_son", arm_vs(d$treatment, "child", "son"), character(0), "secondary"),
    ctr("daughter_vs_son", arm_vs(d$treatment, "daughter", "son"), character(0), "secondary")),
  scherer2015_screening = function(d) list(
    ctr("high_vs_low_risk", arm_vs(d$risk_feedback, "high", "low"), "cancer_worry_risk"),
    ctr("harms_vs_no_harms", arm_vs(d$treatment, c("high_harms", "low_harms"), c("high_noharms", "low_noharms")), "test_harms"),
    ctr("high_risk_vs_control", arm_vs(d$treatment, c("high_harms", "high_noharms"), "control"), character(0), "secondary")),
  koblitz2012_mood = function(d) list(
    ctr("affirmation_vs_none", as.integer(d$affirmation), "implementation_plan", "pooled over the emotion arms"),
    ctr("happiness_affirmation_vs_none", arm_vs(d$treatment, "happiness_sa", "happiness_nosa"), character(0), "secondary")),
  corstange2016_refugees = function(d) list(
    ctr("democracy_vs_control", arm_vs(d$treatment, "democracy", "control"), "democracy_dictatorship"),
    ctr("extremism_vs_control", arm_vs(d$treatment, "extremism", "control"), "religious_extremism"),
    ctr("sectarian_vs_control", arm_vs(d$treatment, "sectarian", "control"), "sectarian_religious_groups"),
    ctr("competing_vs_control", arm_vs(d$treatment, c("democracy_extremism", "democracy_sectarian", "extremism_sectarian", "all_three"), "control"), character(0), "secondary")),
  johnson2021_algofair = function(d) list(
    ctr("parent_requests_vs_other", arm_vs(d$treatment, "parent_requests", c("counselor_discretion", "simple_rule", "weighted_lottery")), "parental_knowledge")),
  weisshaar2019_applicant = function(d) list(
    ctr("bm_or_ww_vs_white_man", arm_vs(d$treatment, c("black_man", "white_woman"), "white_man"), "stereotyped_shortcoming"),
    ctr("black_man_vs_white_man", arm_vs(d$treatment, "black_man", "white_man"), character(0), "secondary"),
    ctr("white_woman_vs_white_man", arm_vs(d$treatment, "white_woman", "white_man"), character(0), "secondary"),
    ctr("black_woman_vs_white_man", arm_vs(d$treatment, "black_woman", "white_man"), character(0), "secondary; authors expect a smaller penalty"),
    ctr("unqualified_vs_qualified", as.integer(d$qualified == 0), character(0), "secondary; positive control for placebo qualification_reference")),
  schnabel2015_service = function(d) list(
    ctr("gay_vs_interracial", arm_vs(d$couple, "gay", "interracial"), "disapproves_couple"),
    ctr("religious_vs_nonreligious", arm_vs(d$reason, "religious", "nonreligious"), character(0), "secondary"),
    ctr("self_employed_vs_chain", arm_vs(d$business, "self_employed", "corporate_chain"), character(0), "secondary")),
  calarco2019_vignette = function(d) list(
    ctr("light_vs_none", arm_vs(d$treatment, "light", "none"), "drinking_scrutiny"),
    ctr("heavy_vs_none", arm_vs(d$treatment, "heavy", "none"), character(0), "secondary"),
    ctr("warning_in_drinking_arms", ifelse(d$treatment %in% c("light", "heavy"), d$warning, NA), character(0), "secondary"),
    ctr("low_vs_high_status_light", ifelse(d$treatment == "light", as.integer(d$status == "low"), NA), character(0), "secondary")),
  # Two public conjoints with an explain-your-choice item after the first
  # pair: contrasts on the pair shown, as for the author's own conjoints.
  hopkins2013_conjoint = function(d) list(
    ctr("party_label_differs", pair_differs(d, "party"), "partisanship_ideology"),
    ctr("governor_vs_president", arm_vs(d$treatment, "governor", "president"), character(0), "secondary; the proposal expects similar criteria across offices"),
    ctr("mayor_vs_president", arm_vs(d$treatment, "mayor", "president"), character(0), "secondary")),
  chu2020_collegerank = function(d) list(
    ctr("rank_differs", pair_differs(d, "rank"), "ranking_prestige"),
    ctr("top25_in_pair", pair_has(d, "rank", "Top 25"), character(0), "secondary; the direction for cost_affordability is inferred, not stated")),
  braman2016_legitimacy = function(d) list(
    ctr("no_vs_clear_authority", arm_vs(d$treatment, "no_authority", "clear_authority"), "lacks_constitutional_authority"),
    ctr("unclear_vs_clear_authority", arm_vs(d$treatment, "unclear_authority", "clear_authority"), character(0), "secondary")),
  pepin2016_couples = function(d) list(
    ctr("unequal_vs_equal_earnings", arm_vs(d$treatment, c("man_higher", "woman_higher"), "equal"), "earnings_breadwinning"),
    ctr("man_vs_woman_higher", arm_vs(d$treatment, "man_higher", "woman_higher"), character(0), "secondary; hedged expectation")),
  pepin2016_couples_leisure = function(d) list(
    ctr("unequal_vs_equal_earnings", arm_vs(d$treatment, c("man_higher", "woman_higher"), "equal"), "earnings_breadwinning"),
    ctr("man_vs_woman_higher", arm_vs(d$treatment, "man_higher", "woman_higher"), character(0), "secondary; hedged expectation")),
  # Price and efficacy are randomized independently; each hypothesis (H1 costs,
  # H2 efficacy) has its own target, and each target is the other's placebo.
  musgrave2026_sanctionswtp = function(d) list(
    ctr("price_high_vs_low", arm_vs(d$treatment, c("usd100", "usd200"), c("usd010", "usd025")),
        "cost_to_americans", "pooled over efficacy; $0.50 arm left out"),
    ctr("efficacy_high_vs_low", as.integer(d$efficacy_high), "sanctions_effectiveness", "pooled over price"),
    ctr("price_max_vs_min", arm_vs(d$treatment, "usd200", "usd010"), character(0), "secondary")),
  aarslew2026_electoralintegrity = function(d) list(
    ctr("violation_vs_control", arm_vs(d$treatment, c("absentee", "misinformation"), "control"), "election_cheating",
        "violation arms pooled as in the article"),
    ctr("absentee_vs_control", arm_vs(d$treatment, "absentee", "control"), character(0), "secondary"),
    ctr("misinformation_vs_control", arm_vs(d$treatment, "misinformation", "control"), character(0), "secondary")),
  # Black respondents: the authors expect derogation to be recognized and
  # penalized whoever the speaker is, so derogatory vs neutral is primary and
  # the speaker contrast carries no expected direction.
  bonilla2024_sourcecues_s3 = function(d) list(
    ctr("derog_vs_neutral", arm_vs(d$treatment, c("white_derog", "black_derog"), c("white_neutral", "black_neutral")), "rejects_as_racist",
        "partly mechanical: only the derogatory statement makes a claim about Black people"),
    ctr("white_vs_black_derogator", arm_vs(d$treatment, "white_derog", "black_derog"), character(0), "secondary; authors expect no source-cue effect among Black respondents"),
    ctr("white_vs_black_neutral", arm_vs(d$treatment, "white_neutral", "black_neutral"), character(0), "design-based placebo: no derogation")),
  bonilla2024_sourcecues_s4 = function(d) list(
    ctr("derog_vs_neutral", arm_vs(d$treatment, c("white_derog", "muslim_derog"), c("white_neutral", "muslim_neutral")), "rejects_as_bigoted",
        "partly mechanical; the round-2 statement type is the reverse of round 1, so order is part of the contrast"),
    ctr("white_vs_muslim_derogator", arm_vs(d$treatment, "white_derog", "muslim_derog"), character(0), "secondary; authors expect no source-cue effect among Black respondents"),
    ctr("white_vs_muslim_neutral", arm_vs(d$treatment, "white_neutral", "muslim_neutral"), character(0), "design-based placebo: no derogation")),
  sirovica2022_cowcalf = function(d) list(
    ctr("si_vs_ns", arm_vs(d$treatment, "si", "ns"), "maternal_separation_concern"),
    ctr("sg_vs_ns", arm_vs(d$treatment, "sg", "ns"), character(0), "secondary"),
    ctr("fc_vs_ns", arm_vs(d$treatment, "fc", "ns"), character(0), "secondary")),
  # Each argument arm has its own hypothesis (H1A-H3A) and its own target.
  sagan2024_atomicarguments = function(d) list(
    ctr("illegal_vs_baseline", arm_vs(d$treatment, "illegal", "baseline"), "law_reference"),
    ctr("precedent_vs_baseline", arm_vs(d$treatment, "precedent", "baseline"), "precedent_reference"),
    ctr("effect_vs_baseline", arm_vs(d$treatment, "effect", "baseline"), "effectiveness_reference"),
    ctr("illegal_counter_vs_baseline", arm_vs(d$treatment, "illegal_counter", "baseline"), character(0), "secondary"),
    ctr("precedent_counter_vs_baseline", arm_vs(d$treatment, "precedent_counter", "baseline"), character(0), "secondary"),
    ctr("effect_counter_vs_baseline", arm_vs(d$treatment, "effect_counter", "baseline"), character(0), "secondary")),
  sukin2024_patronreassurance = function(d) list(
    ctr("high_vs_low_risk", arm_vs(d$treatment,
        c("bilateral_summit", "in_theater_deployment", "deterrent_deployment", "multilateral_summit"),
        c("sanctions", "reiterate_guarantee")), "escalation_risk"),
    ctr("deployments_vs_low_risk", arm_vs(d$treatment, c("in_theater_deployment", "deterrent_deployment"),
        c("sanctions", "reiterate_guarantee")), character(0), "secondary"),
    ctr("summits_vs_low_risk", arm_vs(d$treatment, c("bilateral_summit", "multilateral_summit"),
        c("sanctions", "reiterate_guarantee")), character(0), "secondary")),
  # The proposal states both expectations: federal-income-tax information
  # raises, and all-taxes information lowers, mentions of means-tested programs.
  # The second contrast is coded control = 1 so that the expected sign of the
  # target (increase) holds for both.
  williamson2017_taxes = function(d) list(
    ctr("federal_income_tax_vs_control", arm_vs(d$treatment, "federal_income_tax", "control"), "means_tested_upset"),
    ctr("control_vs_all_taxes", arm_vs(d$treatment, "control", "all_taxes"), "means_tested_upset",
        "the proposal expects all-taxes information to lower mentions")),
  cohen2019_collegemajor = function(d) list(
    ctr("lawyer_vs_factory_parents", as.integer(d$treatment), "social_experiences_valued"),
    ctr("no_debt_vs_debt", arm_vs(d$financing, "no_debt", "debt_30k"), "social_experiences_valued", "the proposal states the same direction for debt"),
    ctr("emily_vs_michael", arm_vs(d$gender, "emily", "michael"), character(0), "independently randomized factor; check")),
  pogacar2023_inactioninertia = function(d) list(
    ctr("big_missed_vs_control", arm_vs(d$treatment, "big_missed", "control"), "against_current_deal"),
    ctr("big_missed_vs_small_missed", arm_vs(d$treatment, "big_missed", "small_missed"), character(0), "secondary; H3a names this comparison too"),
    ctr("small_missed_vs_control", arm_vs(d$treatment, "small_missed", "control"), character(0), "secondary")),
  # Schaffner's expectation covers any prejudiced Trump quote. The primary
  # contrast is the quote about the group asked about, pooled over the other
  # quote (the paper's Figure 1). No quote is about Millennials, so that card
  # compares any prejudiced quote with the neutral-only arm.
  ces2016_uma_mexicans = function(d) list(
    ctr("mexico_quote", arm_vs(d$treatment, c("mexico", "both"), c("neutral", "thugs")), c("offensive_comment", "negative_comment")),
    ctr("thugs_quote", arm_vs(d$treatment, c("thugs", "both"), c("neutral", "mexico")), character(0), "secondary; the quote is not about this group")),
  ces2016_uma_blacks = function(d) list(
    ctr("thugs_quote", arm_vs(d$treatment, c("thugs", "both"), c("neutral", "mexico")), c("offensive_comment", "negative_comment")),
    ctr("mexico_quote", arm_vs(d$treatment, c("mexico", "both"), c("neutral", "thugs")), character(0), "secondary; the quote is not about this group")),
  ces2016_uma_millennials = function(d) list(
    ctr("any_prejudiced_quote", arm_vs(d$treatment, c("thugs", "mexico", "both"), "neutral"), c("offensive_comment", "negative_comment")),
    ctr("mexico_quote", arm_vs(d$treatment, c("mexico", "both"), c("neutral", "thugs")), character(0), "secondary"),
    ctr("thugs_quote", arm_vs(d$treatment, c("thugs", "both"), c("neutral", "mexico")), character(0), "secondary")),
  # 2 x 2 plus a baseline arm. The authors test H2 and H4 within the four
  # treatment arms, each factor pooled over the other; the baseline is left out.
  porumbescu2023_blame = function(d) list(
    ctr("chinese_vs_covid", arm_vs(d$treatment, c("chinese_negative", "chinese_positive"), c("covid_negative", "covid_positive")),
        "chinese_people_blame", "H2; pooled over framing"),
    ctr("negative_vs_positive", arm_vs(d$treatment, c("chinese_negative", "covid_negative"), c("chinese_positive", "covid_positive")),
        "federal_blame", "H4; pooled over virus name")),
  # Each mechanism is defined by the authors as a difference between two arms
  # (online appendix A.2-A.4): incompetence = engage and fail vs engage and
  # succeed; inconsistency = back down vs engage; belligerence = engage vs
  # stay out.
  nomikos2019_audiencecosts = function(d) list(
    ctr("engage_fail_vs_engage_succeed", arm_vs(d$treatment, "engage_fail", "engage_succeed"), "failed_outcome_incompetence"),
    ctr("back_down_vs_engage_fail", arm_vs(d$treatment, "back_down", "engage_fail"), "inconsistency_backing_down"),
    ctr("engage_fail_vs_stay_out", arm_vs(d$treatment, "engage_fail", "stay_out"), "belligerence_military_force"),
    ctr("back_down_vs_stay_out", arm_vs(d$treatment, "back_down", "stay_out"), character(0), "secondary; the conventional audience-cost contrast")),
  # Table 1 of the article pairs each hypothesis with two conditions.
  dill2022_kettlesofhawks = function(d) list(
    ctr("nuc100k_vs_eff9045", arm_vs(d$treatment, "nuc100k", "eff9045"), "civilian_casualties"),
    ctr("target_civ_vs_eff9090", arm_vs(d$treatment, "target_civ", "eff9090"), "civilian_immunity"),
    ctr("eff9045_vs_eff9090", arm_vs(d$treatment, "eff9045", "eff9090"), "military_effectiveness"),
    ctr("christian_vs_nuc100k", arm_vs(d$treatment, "christian", "nuc100k"), character(0), "secondary")),
  # Low-cost and high-cost versions were fielded as separate samples, so the
  # estimator stratifies on level (EXTRA_STRATA in rv_config.R).
  miller2016_humanfinancialcosts = function(d) list(
    ctr("human_vs_financial", arm_vs(d$treatment, c("human_prospective", "human_sunk"),
        c("financial_prospective", "financial_sunk")), "american_lives"),
    ctr("sunk_vs_prospective", arm_vs(d$treatment, c("human_sunk", "financial_sunk"),
        c("human_prospective", "financial_prospective")), "recoup_past_investment"),
    ctr("human_sunk_vs_financial_sunk", arm_vs(d$treatment, "human_sunk", "financial_sunk"),
        character(0), "secondary; H2's sunk-cost comparison")),
  # The authors expect the legal phrase ("objectively reasonable") to give more
  # positive descriptions than "average", and "average" to match the blank prompt.
  spruill2022_legalreasonable_s1 = function(d) list(
    ctr("reasonable_vs_average", arm_vs(d$treatment, "reasonable", "average"), c("favorable_portrayal", "unfavorable_portrayal")),
    ctr("reasonable_vs_blank", arm_vs(d$treatment, "reasonable", "blank"), character(0), "secondary"),
    ctr("average_vs_blank", arm_vs(d$treatment, "average", "blank"), character(0), "secondary; the authors expect no difference")),
  spruill2022_legalreasonable_s2 = function(d) list(
    ctr("reasonable_vs_average", as.integer(d$treatment), c("favorable_portrayal", "unfavorable_portrayal"))),
  # Each frame names the consideration it emphasizes; each is compared with
  # the control arm. The other frame's target serves as a placebo.
  yeung2022_ubiframing = function(d) list(
    ctr("opportunity_vs_control", arm_vs(d$treatment, "opportunity", "control"), "equal_opportunity_self_reliance"),
    ctr("limit_government_vs_control", arm_vs(d$treatment, "limit_government", "control"), "limiting_government")),
  # H1 compares perspective taking with reading the opposing view only (the
  # paper's placebo; "disagreement" in the data). The weekend-essay arm is the
  # paper's control.
  muradova2022_empathy = function(d) list(
    ctr("perspective_taking_vs_disagreement", arm_vs(d$treatment, "perspective_taking", "disagreement"), "opposing_consideration"),
    ctr("disagreement_vs_control", arm_vs(d$treatment, "disagreement", "control"), character(0), "secondary"),
    ctr("perspective_taking_vs_control", arm_vs(d$treatment, "perspective_taking", "control"), character(0), "secondary")),
  # 2 x 2 (couple's sexuality by celebrity) plus a control arm with no couple.
  # The author's expectation concerns the sexuality factor.
  turnbulldugarte2022_surrogacy = function(d) list(
    ctr("gay_vs_straight_couple", arm_vs(d$treatment, c("celeb_gay", "ordinary_gay"), c("celeb_straight", "ordinary_straight")), "parents_sexuality"),
    ctr("gay_couple_vs_control", arm_vs(d$treatment, c("celeb_gay", "ordinary_gay"), "control"), character(0), "secondary"),
    ctr("celebrity_vs_ordinary", arm_vs(d$treatment, c("celeb_gay", "celeb_straight"), c("ordinary_gay", "ordinary_straight")), character(0), "secondary; placebo factor")),
  # The article uses the open answers as a manipulation check for the
  # intergroup prime (group-based reasons); the second target comes from the
  # preregistered hypothesis for the closed item. The two intragroup arms
  # fielded only in the United States are in no contrast.
  leipziger2026_groupinequality = function(d) list(
    ctr("intergroup_vs_control", arm_vs(d$treatment, "intergroup", "control"), c("group_based_reasons", "circumstances_beyond_control")),
    ctr("intergroup_vs_interpersonal", arm_vs(d$treatment, "intergroup", "interpersonal"), character(0), "secondary; reported in the article; preregistered H3"),
    ctr("interpersonal_vs_control", arm_vs(d$treatment, "interpersonal", "control"), character(0), "secondary; preregistered H2")),
  # The open item is the authors' manipulation check, coded for the value each
  # frame stresses. Experiment 2 has two freedom frames and no security frame.
  thomas2022_ubireframing_e1 = function(d) list(
    ctr("freedom_vs_control", arm_vs(d$treatment, "freedom", "control"), "freedom_mention"),
    ctr("security_vs_control", arm_vs(d$treatment, "security", "control"), "security_mention")),
  thomas2022_ubireframing_e2 = function(d) list(
    ctr("freedom_vs_control", arm_vs(d$treatment, "freedom", "control"), "freedom_mention"),
    ctr("freedom_plus_vs_control", arm_vs(d$treatment, "freedom_plus", "control"), character(0), "secondary; the article reports both freedom frames against control"))
))
