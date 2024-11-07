# Load packages
pacman::p_load(dplyr, stringr, quanteda, readxl)

dict_RA <-  quanteda::dictionary(
  file = "C:\\Users\\ischn\\Desktop\\RoteListe\\regime_dictionary_v1.cat")

# Add new entries in dictionary
new_entries <- list(
  methotrexat = "mtx",
  MELPHALAN = c("mel","fbm", "hd_melphalan"),
  Zoledronsaeure = c("zoledron","zoledronat"),
  temsirolimus = "temsirolismus",
  vemurafenib = "vemurafinib",
  Ibandronsaeure = c("ibandronat","Ibandronsäure"),
  lanreotid = "lanreotide",
  Fluorouracil = c("FU", "5-FU", "5_FU", "5FU"),
  `Gemcitabin, Oxaliplatin` = "gemox",
  Paclitaxel = "nabpaclitaxel",
  Streptozocin = "streptozotocin",
  `oxaliplatin, fluorouracil, folsaeure`="off",
  Irinotecan = "onivyde",
  Panitumumab = "panitu",
  Pembrolizumab = c("pembrol","pembrolizumab_erhal"),
  dactinomycin = "actinomycin",
  pixantron = "pixantrone",
  Lenalidomid = "revlimid",
  interferon = "roferon",
  Tretinoin = c("atra","all_trans_retinsaeure", "retinsaeure"),
  folsaeure = c("folinsaeure","folins", "folinsaeu", "folinsäu", "folin", "folinsäure", "LEUKOVORIN", "LEUCOVORIN", "Leuk", "fs"),
  pomalidomid = "pomalidomide",
  `Epirubicin, Cyclophosphamid`= "ec",
  Kombi = "Kombinationen",
  paclitaxel = c("nab_paclitaxel", "Abraxane"),
  carboplatin = c("carbo","carbo_schema", "carbopl"),
  Galunisertib = "ly2157299",
  Olaparib = "lynparza",
  `Cyclophosphamide, Etoposide, Bleomycin`= "CED",
  Trametinib = "mekinist",
  Atezolizumab = "mpdl328da0",
  Mitomycin = "mytomycin",
  Fludarabine = "flu",
  gemcitabin =c("gemcitabine","gemzar"),
  Imatinib= "glivec",
  Alemtuzumab = "campath",
  `Enfortumab Vedotin` = "Enfortumab",
  `Polatuzumab Vedotin` = "Polatuzumab",
  neuroblastom = "nb",
  Cytarabin = c("arac","ara", "hd_arac", "ldac"),
  `Tretinoin, Arsentrioxid` = "ato",
  Bicalutamid = "casodex",
  Bevacizumab = "avastin",
  `Fluorouracil, Epirubicin, Cyclophosphamid` = "fec",
  Cyclophosphamid = c("cyclo", "cy", "endoxan"),
  Trastuzumab = "herceptin",
  Mycophenolsaeure = "mycophenolatmofetil",
  Hydroxycarbamid = "litalir",
  Lomustin = "ccnu",
  Azacitidin = "vidaza",
  `Donor Lymphocyte Infusion` = "DLI",
  `Daunoxome, Fludarabin, Hochdosis-Cytarabin` = "DNX/FLA",
  `Methotrexat, Cytarabin, Thiotepa, Rituximab` = "MATRix",
  Topotecan = "Topo",
  Busulfan = "bu",
  Mercaptopurin = "mp",
  Paclitaxel = "taxol",
  `Doxorubicin, Cyclophosphamid` = "ac",
  `Gemtuzumab ozogamicin` = "gemtuzumab",
  carmustin = "bcnu",
  Doxorubicin = c("adriamycin", "adria", "hydroxydaunomycin", "doxo", "caelyx", "myocet", "transdrug", "doxorub", "ADRIAMYCIN"),
  FLUDARABIN = "FLUDARABINE",
  Amilomer = "embocept",
  Docetaxel = c("doc","taxotere"),
  capecitabin = c("capecitabine","xeloda"),
  Paclitaxel = c("pacli","poliglumex", "pac"),
  Tamoxifen = "tam",
  bevacizumab = c("beva","bevaci", "bev"),
  cisplatin = "cispl",
  Cytarabin = c("ara_c","depocyte", "depocyt"),
  `Brentuximab-Vedotin` = "brentuximab",
  cisplatin = c("cis","cisp"),
  dexamethason = c("dexa", "dexame"),
  etoposid = c("eto","etoposidphosphat", "etopsid"),
  `Cytarabin, Mitoxantron`=  "ham",
  Irinotecan = "irino",
  `carmustin, Etoposid, Cytarabin, Melphalan` = "beam",
  `Rituximab, carmustin, Etoposid, Cytarabin, Melphalan` = "r_beam",
  `Rituximab, Methotrexat` = "r_mtx",
  `highdose Methotrexat` = "hd_mtx",
  Alendronsaeure = "Alendronat",
  Buserelin = "Buserelinacetat",
  Teniposide = "VM26",
  Rituximab = "rituximab_monotherapie",
  Octreotid = "sandostatin",
  Sunitinib = "sutent",
  Dacarbazin = "dtic",
  Vinorelbin = "navelbine",
  `Docetaxel, Doxorubicin, Cyclophosphamid` = "tac",
  Cetuximab = "cetuxi",
  `Carboplatin,Etoposid,Vincristin` = "cev",
  valproinsaeure = "valproat",
  carboplatin = c("auc","cisplat"),
  `transarterielle Embolisation` = "TAE",
  `Bleomycin, Etoposid, Doxorubicin, Cyclophosphamid, Vincristin, Procarbazin und Prednison` = c("beacopp","bea"),
  `Cyclophosphamid, Vincristin, Procarbazin, Prednison` = "copp",
  `fludarabin, amsacrin, cytarabin` = "flamsa",
  Gemcitabin = c("gem","gemc"),
  Temozolomid ="Tmz",
  Mitomycin = "mmc" ,
  Oxaliplatin = "oxali",
  `Polatuzumab vedotin` = "polatuzumab",
  Temozolomid = "temodal",
  `Tegafur, gimeracil, oteracil` = "teysuno",
  Treosulfan = c("tre", "treo"),
  `Ifosfamid, Carboplatin, Etoposid` = "ice",
  Ifosfamid = "Ifos",
  Paclitaxel = "nabpacli",
  Pembrolizumab = "pembro",
  Pertuzumab = "pertu",
  `adriamycin, cyclophosphamid, etoposid` = "ace",
  mitomycin = "mitom",
  DACARBAzIN = "DACARBACIN",
  Anastrozol = "ARIMIDEX",
  Thiotepa = "tt",
  Triamcinolon  = "tbi"
)

# Arrange Entries in alphabetical order
new_entries <- new_entries[order(names(new_entries))]


additional_entries <- list(
  ABBREVIATIONS = new_entries
)

# Check if "REGIME" exists in the dictionary
if ("REGIME" %in% names(dict_RA)) {
  # Combine existing REGIME with additional entries
  dict_RA  <- c(dict_RA, additional_entries)
}

#add all caps

# Save the updated dictionary
saveRDS(dict_RA, "C:\\Users\\ischn\\Desktop\\RoteListe\\RA_dictionary_v2.cat")
