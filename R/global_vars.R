CENTILOID_EQUATION <- "100 * (SUVR - YC_0) / (AD_100 - YC_0)"

MASKS <- list(
  CTX = list(FILE = "ctx_2mm.nii", AD_100 = NULL, YC_0 = NULL),
  CG = list(FILE = "CerebGry_2mm.nii", AD_100 = 2.428, YC_0 = 1.170),
  WC = list(FILE = "WhlCbl_2mm.nii", AD_100 = 2.076, YC_0 = 1.009),
  WCB = list(FILE = "WhlCblBrnStm_2mm.nii", AD_100 = 1.962, YC_0 = 0.959),
  PONS = list(FILE = "Pons_2mm.nii", AD_100 = 1.535, YC_0 = 0.761)
)
