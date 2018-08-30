.onLoad <- function(libname, pkgname){

  message(paste("Loading", pkgname))

  message("Setting Paths")
  setpaths()

  library(magrittr)

  options(dplyr.print_max = 1000)
  options(dplyr.show_progress = FALSE)

  # Set knitr options
  knitr::opts_chunk$set(eval = TRUE,
                        comment = NA,
                        tidy = FALSE,
                        warning = FALSE,
                        message = FALSE,
                        echo = FALSE,
                        results = 'asis',
                        dpi = 600,
                        sanitize = FALSE,
                        fig.width = 6.75, fig.height = 6.75, # Default figure widths
                        dev = c('jpeg'),
                        dev.args = list(pdf = list(NULL),
                                        tiff = list(compression = 'lzw'),
                                        png = list(NULL)),
                        error = FALSE)

  knitr::opts_knit$set(progress = TRUE,
                       verbose = FALSE)

  # Add Acronym Defs
  acronyms <<-
    tibble::tribble(
      ~Acronym, ~Alternate, ~Definition,
      "A-OTM", NA,"AquaTEC OTM",
      "OTM", NA, "Online Toxicity Monitor",
      "USEPA", NA, "United States Environmetnal Protection Agency",
      "PUB", NA, "Singapore PUB",
      "ZWEEC", NA, "ZWEEC Analytics PTE LTD",
      "MOU", NA, "Memorandum of Understanding",
      "CRADA", NA, "Cooperative Research and Development Agreement",
      "AARF", NA, "AWBERC Aquatic Research Facility",
      "QAM", NA, "Quality Assurance Manager",
      "HASP", NA, "Health and Safety Plan",
      "nh3", "NH~3~", "Ammonia-N",
      "nh4", "NH~4~^+^", "Ammonium",
      "TOC", NA, "Total Organic Carbon",
      "DOC", NA, "Dissolved Organic Carbon",
      "DO", NA, "Dissolved Oxygen",
      "YSI", NA, "Yellow Springs International Model 6820 Environmental Monitoring Sonde",
      "OP/C", NA, "Abraxis 96-well Organophosphate/Carbamate ELISA Kit (PN 550055)",
      "ACh-E", NA, "Acetyl Cholinesterase",
      "ATC", NA, "acetylthiocholine",
      "DTNB", NA, "5,5'-Dithio-bis(2-Nitrobenzoic Acid)",
      "MDL", NA, "Method Detection Limit",
      "MC-ADDA", NA, "Abraxis 96-well MC-ADDA ELISA Kit (PN 520011, Abraxis Inc., Warminster, PA)",
      "Na2EDTA", "Na~2~EDTA", "Disodium Ethylenediamme Tetraacetate",
      "USEPA-FTP", NA, "USEPA Science FTP Site",
      "ISE", NA, "Ion Specific Electrode",
      "IDL", NA, "Instrument Detection Limit",
      "LRB", NA, "Laboratory Reagent Blank",
      "MSDS", NA, "Material Safety Data Sheet",
      "EDL", NA, "Estimated Detection Limit",
      "QCS", NA, "Quality Control Sample",
      "GF/F", NA, "Whatman Glass Fiber Filter",
      "HCl", NA, "Hydrochloric Acid",
      "AAS", NA, "Aqueous Acetone Solution",
      "BAAS", NA, "Buffered Aqueous Acetone Solution",
      "SSS", NA, "Chlorophyll Stock Standard Solution",
      "LDR", NA, "Linear Detection Range",
      "PAR", NA, "Photosynthetically Active Radiation",
      "CAAS", NA, "Cyanotoxin Automated Assay System (Abraxis Inc., Warminster, PA)",
      "STX", NA, "Saxitoxin",
      "STX-ELISA", NA, "Abraxis 96-well Saxitoxin ELISA Kit (PN 52255B, Abraxis Inc., Warminster, PA)",
      "SS", NA, "S::can Spectrolyzer",
      "HFD", NA, "On-line High Frequency Data",
      "PPIT", NA, "Protein Phosphotase Inhibition Toxicity Assay",
      "IP", NA, "Internet Protocol Address",
      "CoC", NA, "Chain of Custody",
      "MMPB", NA, "3-methoxy-2-methyl-4-phenylbutyric acid",
      "DI", NA, "Deionized Water",
      "ASTM1", NA, "ASTM Type I Water",
      "IDP", NA, "Initial Demonstration of Performance",
      "IDC", NA, "Initial Demonstration of Capability",
      "MP", NA, "Microcystin Producer",
      "LH", "Lake Harsha", "William H. Harsha Lake",
      "USACE", NA, "United States Army Corps of Engineers",
      "CHAB", NA, "Cyanobacterial Harmful Algal Bloom",
      "PCR", NA, "Polymerase Chain Reaction",
      "qPCR", NA, "Quantitative Polymerase Chain Reaction",
      "RT-qPCR", NA, "Reverse Transcription Polymerase Chain Reaction",
      "RNA", NA, "Ribonucleic Acid",
      "RLT Plus", NA, "RLT Plus Buffer (QIAGEN Valencia CA)",
      "HF", NA, "High Frequency",
      "MC", NA, "Microcystin",
      "ESF", NA, "USEPA Experimental Streams Facility",
      "GFC", NA, "Glass Film Control",
      "HPW", NA, "High Purity Water",
      "ANA", NA, "Anatoxin-a",
      "ANA-ELISA", NA, "Abraxis 96-well Anatoxin-a ELISA Kit (PN 520060, Abraxis Inc., Warminster, PA)",
      "LFB", NA, "Laboratory Fortified Blank",
      "LFSM", NA, "Laboratory Fortified Sample Matrix",
      "LFSMD", NA, "Laboratory Fortified Sample Matrix Duplicate",
      "LCMRL", NA, "Lowest Concentration Minimum Reporting Level",
      "LOW-CV", NA, "Low-range Calibration Verification",
      "PDS", NA, "Primary Dilution Standard",
      "EC50", "EC~50~" , "50% Effective Concentration",
      "MRL", NA, "Minimum Reporting Level",
      "CV", NA, "Coefficient of Variation",
      "PETG", NA, "Polyethylene Terephthalate Glycol-Modified",
      "Abraxis", NA, "Abraxis, Inc., Warminster, PA",
      "CYL", NA, "Cylindrospermopsin",
      "AOA", NA, "BBE Algae Online Analyzer",
      "ATX", NA, "Anatoxin",
      "PAM", NA, "Pulsed, Amplitude Modulated Fluorometry",
      "WIZ", NA, "Water *in-situ* Analyser (SYSTEA S.p.a. - Via Paduni, 2A 03012 - ANAGNI P.Iva)",
      "TRP", NA, "Total Reactive Phosphorous",
      "NO2_3", NA, "Nitrate-Nitrite",
      "PA", NA, "Precision and Accuracy",
      "ASB", NA, "Acceptable System Background",
      "SOP", NA, "Standard Operating Procedure",
      "QA", NA, "Quality Assurance",
      "QC", NA, "Quality Control",
      "ASTM", NA, "American Society for Testing and Materials International",
      "RSD", NA, "Percent Relative Standard Deviation",
      "PI", NA, "Principle Investigator",
      'HR~PIR~', NA, 'Half Range for the Prediction of Interval of Results',
      'PIR', NA, 'Prediction of Interval of Results',
      'PSP', NA, 'Paralytic Shellfish Poisons',
      'GTX', NA, 'Gonyautoxins',
      'NRCC', NA, 'National Research Council Canada',
      '546AP', NA, 'Abraxis Method 546 Accessory Pack (PN 520013, Abraxis Inc., Warminster, PA)',
      'ELISA', NA, 'Enzyme-Linked Immunosorbent Assay',
      "IPA", NA, "Isopropyl Alcohol",
      "LL4", NA, "Four parameter log-logistic function",
      "LT-MRL", NA, "Long-term Minimum Reporting Limit",
      "LT-MDL", NA, "Long-term Minimum Detection Limit",
      "RPD", NA, "Relative Percent Difference"
    ) %>%
    dplyr::mutate(Include = FALSE) %>%
    # Alphabetize acronyms
    dplyr::arrange(Acronym)

  invisible()

}

.onAttach <- function(libname, pkgname) {
  env <- as.environment(paste0("package:", pkgname))
  env[[".conflicts.OK"]] <- TRUE
  suppressPackageStartupMessages((get("library", baseenv()))("gfuns"))
}
