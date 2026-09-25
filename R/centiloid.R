get_centiloid <- function(pet, id) {
  # Retrieve package paths for masks
  masks <- purrr::map(
    MASKS,
    \(mask) {
      mask$FILE <- system.file(
        file.path("voi", mask$FILE),
        package = "kuadrc.pet"
      )
      mask
    }
  )

  # fmt: skip
  ctx <- as.numeric(fslr::fslstats(pet, opts = sprintf("-k %s -M", masks$CTX$FILE),  verbose = FALSE))
  masks <- masks[names(masks) != "CTX"]
  purrr::imap_dfr(masks, \(x, y) {
    # fmt: skip
    mask_mean <- as.numeric(fslr::fslstats(pet, opts = sprintf("-k %s -M", x$FILE), verbose = FALSE))
    AD_100 <- x$AD_100
    YC_0 <- x$YC_0
    SUVR <- ctx / mask_mean

    centiloid_value <-
      rlang::eval_tidy(
        rlang::parse_expr(CENTILOID_EQUATION),
        data.frame(SUVR, AD_100, YC_0)
      )

    # fmt: skip
    data.frame(ID = id, MASK = y, SUVR = unname(SUVR), CENTILOID = centiloid_value)
  })
}
