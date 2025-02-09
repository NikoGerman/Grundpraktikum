pkgs <- c("dplyr",
  "tidyr",
  "ggplot2",
  "ggrepel",
  "patchwork",
  "forcats",
  "checkmate",
  "viridis",
  "readxl",
  "DiagrammeR",
  "DiagrammeRsvg",
  "rsvg")

installed <- rownames(installed.packages())

cat("Versuche die benötigen Packages direkt zu installieren:")
for (pkg in pkgs) {
  if(any(installed == pkg)) {
    next
  } else {
    cat("Package <", pkg, "> wird installiert...\n", sep = "")
    install.packages(pkg)
  }
}
