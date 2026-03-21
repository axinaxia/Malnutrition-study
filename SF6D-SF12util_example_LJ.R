# SF-12 -> SF-6D(SF-12) -> SG utility (Brazier & Roberts 2004, model 4)
# Inputs expected (one row per person):
#   pf_mod     = SF-12 physical functioning (moderate activities): 1=limited a lot, 2=limited a little, 3=not limited at all
#   rp_limit   = SF-12 role-physical: yes/no item "limited in kind of work/activities due to physical health"
#               (recommended coding: 1=Yes (limited), 0=No (not limited))
#   re_less    = SF-12 role-emotional: yes/no item "accomplished less due to emotional problems"
#               (recommended coding: 1=Yes, 0=No)
#   sf_interf  = SF-12 social functioning interference: typically 1=all of the time ... 5=none of the time
#   pain_int   = SF-12 pain interference with normal work: often 1=not at all ... 5=extremely
#               (some datasets use 6 levels; handled below)
#   mh_down    = SF-12 mental health "downhearted and low": often 1=all ... 6=none
#   vit_energy = SF-12 vitality/energy: often 1=all ... 6=none
#
# Returns:
#   sf6d_state: character code "PF RLSF PAIN MH VIT" concatenated
#   utility_sg: numeric utility (SG-based) per Brazier & Roberts 2004 model (4)

sf12_to_sf6d <- function(
    data,
    pf_mod     = "pf_mod",
    rp_limit   = "rp_limit",
    re_less    = "re_less",
    sf_interf  = "sf_interf",
    pain_int   = "pain_int",
    mh_down    = "mh_down",
    vit_energy = "vit_energy",
    strict = TRUE
) {
  d <- data
  
  # Helper: check columns
  cols <- c(pf_mod, rp_limit, re_less, sf_interf, pain_int, mh_down, vit_energy)
  missing_cols <- setdiff(cols, names(d))
  if (length(missing_cols) > 0) {
    stop("Missing columns: ", paste(missing_cols, collapse = ", "))
  }
  
  # ---- 1) Map SF-12 responses -> SF-6D(SF-12) dimension levels (Table 2) ----
  # PF (3 levels): Table 2 says:
  #   1 = not limited, 2 = limited a little, 3 = limited a lot
  # Typical SF-12 coding: 1 limited a lot, 2 limited a little, 3 not limited
  PF <- d[[pf_mod]]
  PF_lvl <- ifelse(is.na(PF), NA_integer_,
                   ifelse(PF == 3, 1L,
                          ifelse(PF == 2, 2L,
                                 ifelse(PF == 1, 3L, NA_integer_))))
  
  # Role limitations (4 levels) from two yes/no items (Table 2):
  #   1 = none (no phys, no emot)
  #   2 = physical only
  #   3 = emotional only
  #   4 = both physical and emotional
  RP <- d[[rp_limit]]
  RE <- d[[re_less]]
  # allow common encodings: 1/0 or "Yes"/"No"
  to01 <- function(x) {
    if (is.logical(x)) return(ifelse(is.na(x), NA_integer_, ifelse(x, 1L, 0L)))
    if (is.numeric(x)) return(ifelse(is.na(x), NA_integer_,
                                     ifelse(x %in% c(1, "1"), 1L,
                                            ifelse(x %in% c(0, "0"), 0L, NA_integer_))))
    if (is.character(x)) {
      x2 <- trimws(tolower(x))
      return(ifelse(is.na(x), NA_integer_,
                    ifelse(x2 %in% c("yes","y","true","t","1"), 1L,
                           ifelse(x2 %in% c("no","n","false","f","0"), 0L, NA_integer_))))
    }
    return(rep(NA_integer_, length(x)))
  }
  RP01 <- to01(RP)
  RE01 <- to01(RE)
  
  RL_lvl <- ifelse(is.na(RP01) | is.na(RE01), NA_integer_,
                   ifelse(RP01 == 0 & RE01 == 0, 1L,
                          ifelse(RP01 == 1 & RE01 == 0, 2L,
                                 ifelse(RP01 == 0 & RE01 == 1, 3L,
                                        ifelse(RP01 == 1 & RE01 == 1, 4L, NA_integer_)))))
  
  # Social functioning (5 levels): SF-12 typically 1=all ... 5=none
  # Table 2: 1=none, 2=little, 3=some, 4=most, 5=all
  SF <- d[[sf_interf]]
  SF_lvl <- ifelse(is.na(SF), NA_integer_,
                   ifelse(SF == 5, 1L,
                          ifelse(SF == 4, 2L,
                                 ifelse(SF == 3, 3L,
                                        ifelse(SF == 2, 4L,
                                               ifelse(SF == 1, 5L, NA_integer_))))))
  
  # Pain interference (Table 2 has 5 levels):
  # 1 not at all, 2 a little bit, 3 moderately, 4 quite a bit, 5 extremely
  PA <- d[[pain_int]]
  # If a dataset uses 6 categories, collapse the top-end to 5 levels (conservative):
  #   1..5 -> 1..5, and 6 -> 5
  PA2 <- ifelse(is.na(PA), NA_integer_,
                ifelse(PA %in% 1:5, as.integer(PA),
                       ifelse(PA == 6, 5L, NA_integer_)))
  PAIN_lvl <- PA2
  
  # Mental health downhearted (SF-12 often 1=all ... 6=none)
  # Table 2 has 5 levels (none, little, some, most, all).
  # We collapse 6 response options by merging "good bit" with "most" (common practice):
  #   6 none -> 1
  #   5 little -> 2
  #   4 some -> 3
  #   3 good bit -> 4
  #   2 most -> 4
  #   1 all -> 5
  MH <- d[[mh_down]]
  MH_lvl <- ifelse(is.na(MH), NA_integer_,
                   ifelse(MH == 6, 1L,
                          ifelse(MH == 5, 2L,
                                 ifelse(MH == 4, 3L,
                                        ifelse(MH %in% c(2,3), 4L,
                                               ifelse(MH == 1, 5L, NA_integer_))))))
  
  # Vitality/energy (SF-12 often 1=all ... 6=none)
  # Table 2 has 5 levels (all, most, some, little, none).
  # We collapse 6 options by merging "good bit" with "most":
  #   1 all -> 1
  #   2 most -> 2
  #   3 good bit -> 2
  #   4 some -> 3
  #   5 little -> 4
  #   6 none -> 5
  VIT <- d[[vit_energy]]
  VIT_lvl <- ifelse(is.na(VIT), NA_integer_,
                    ifelse(VIT == 1, 1L,
                           ifelse(VIT %in% c(2,3), 2L,
                                  ifelse(VIT == 4, 3L,
                                         ifelse(VIT == 5, 4L,
                                                ifelse(VIT == 6, 5L, NA_integer_))))))
  
  # Optional strict range checking
  if (strict) {
    bad <- which(
      (!is.na(PF_lvl)   & !(PF_lvl   %in% 1:3)) |
        (!is.na(RL_lvl)   & !(RL_lvl   %in% 1:4)) |
        (!is.na(SF_lvl)   & !(SF_lvl   %in% 1:5)) |
        (!is.na(PAIN_lvl) & !(PAIN_lvl %in% 1:5)) |
        (!is.na(MH_lvl)   & !(MH_lvl   %in% 1:5)) |
        (!is.na(VIT_lvl)  & !(VIT_lvl  %in% 1:5))
    )
    if (length(bad) > 0) {
      stop("Found out-of-range mapped levels in rows: ", paste(head(bad, 20), collapse = ", "),
           if (length(bad) > 20) " ..." else "")
    }
  }
  
  # ---- 2) Apply Brazier & Roberts (2004) SF-6D(SF-12) SG algorithm: Table 4, model (4) ----
  # Utility = 1 - (sum of decrements for non-baseline levels) - MOST*decrement
  # (Coefficients below are the model (4) "parsimonious consistent" estimates.)   [oai_citation:3‡ResearchGate](https://www.researchgate.net/profile/Ariel-Linden/post/How_does_one_calculate_utility_values_using_QOL_instruments/attachment/59d63d62c49f478072ea872e/AS%3A273759624204295%401442280788202/download/Brazier_2004.pdf)
  
  # Decrements (note: all are utility *losses*)
  dec_PF  <- c(`1`=0.000, `2`=0.0, `3`=0.045)
  # RL is aggregated as RL234 (one decrement applied whenever RL level is 2,3,or 4)
  dec_RL234 <- 0.063
  # Social functioning decrements (levels 2..5 vs 1)
  dec_SF  <- c(`1`=0.000, `2`=0.063, `3`=0.066, `4`=0.081, `5`=0.093)
  # Pain decrements (levels 2..5 vs 1)
  dec_PAIN <- c(`1`=0.000, `2`=0.00, `3`=0.042, `4`=0.077, `5`=0.137)
  # Mental health: MH23 aggregated (applies to level 2 or 3), plus separate for 4 and 5
  dec_MH <- function(mh_lvl) {
    ifelse(is.na(mh_lvl), NA_real_,
           ifelse(mh_lvl == 1, 0.000,
                  ifelse(mh_lvl %in% c(2,3), 0.059,
                         ifelse(mh_lvl == 4, 0.113,
                                ifelse(mh_lvl == 5, 0.134, NA_real_)))))
  }
  # Vitality: VIT234 aggregated, plus separate VIT5
  dec_VIT <- function(vit_lvl) {
    ifelse(is.na(vit_lvl), NA_real_,
           ifelse(vit_lvl == 1, 0.000,
                  ifelse(vit_lvl %in% c(2,3,4), 0.078,
                         ifelse(vit_lvl == 5, 0.106, NA_real_))))
  }
  
  # MOST indicator definition as described in the paper:
  # "most severe" = PF level 3; RL levels 3 or 4; SF levels 4 or 5; PAIN levels 4 or 5;
  # MH levels 4 or 5; VIT level 5  [oai_citation:4‡ResearchGate](https://www.researchgate.net/profile/Ariel-Linden/post/How_does_one_calculate_utility_values_using_QOL_instruments/attachment/59d63d62c49f478072ea872e/AS%3A273759624204295%401442280788202/download/Brazier_2004.pdf)
  MOST <- ifelse(
    is.na(PF_lvl) | is.na(RL_lvl) | is.na(SF_lvl) | is.na(PAIN_lvl) | is.na(MH_lvl) | is.na(VIT_lvl),
    NA_integer_,
    as.integer(
      (PF_lvl == 3) |
        (RL_lvl %in% c(3,4)) |
        (SF_lvl %in% c(4,5)) |
        (PAIN_lvl %in% c(4,5)) |
        (MH_lvl %in% c(4,5)) |
        (VIT_lvl == 5)
    )
  )
  
  # Compute decrements
  d_pf   <- ifelse(is.na(PF_lvl), NA_real_, dec_PF[as.character(PF_lvl)])
  d_rl   <- ifelse(is.na(RL_lvl), NA_real_, ifelse(RL_lvl == 1, 0.000, dec_RL234))
  d_sf   <- ifelse(is.na(SF_lvl), NA_real_, dec_SF[as.character(SF_lvl)])
  d_pain <- ifelse(is.na(PAIN_lvl), NA_real_, dec_PAIN[as.character(PAIN_lvl)])
  d_mh   <- dec_MH(MH_lvl)
  d_vit  <- dec_VIT(VIT_lvl)
  
  # MOST decrement (model 4)
  dec_MOST <- 0.077
  
  utility <- 1.0 - (d_pf + d_rl + d_sf + d_pain + d_mh + d_vit) - ifelse(is.na(MOST), NA_real_, MOST * dec_MOST)
  
  # SF-6D state code (PF-RL-SF-PAIN-MH-VIT)
  sf6d_state <- paste0(PF_lvl, RL_lvl, SF_lvl, PAIN_lvl, MH_lvl, VIT_lvl)
  
  # Return
  out <- data.frame(
    PF_lvl = PF_lvl,
    RL_lvl = RL_lvl,
    SF_lvl = SF_lvl,
    PAIN_lvl = PAIN_lvl,
    MH_lvl = MH_lvl,
    VIT_lvl = VIT_lvl,
    MOST = MOST,
    sf6d_state = sf6d_state,
    utility_sg = utility,
    stringsAsFactors = FALSE
  )
  out
}

# ---- Example usage ----
df <- data.frame(
  pf_mod = c(3, 2, 1),
  rp_limit = c(0, 1, 1),
  re_less = c(0, 0, 1),
  sf_interf = c(5, 3, 1),
  pain_int = c(1, 3, 5),
  mh_down = c(6, 4, 1),
  vit_energy = c(2, 5, 6)
)

res <- sf12_to_sf6d(df)
cbind(df, res)