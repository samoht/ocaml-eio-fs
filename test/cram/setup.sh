# Compiler versions disagree about whether Cmdliner quotes values in errors.
# Strip only balanced single quotes so the transcript checks the diagnostic,
# not the compiler-dependent presentation.
normalize_quotes() {
  sed "s/'\\([^']*\\)'/\\1/g"
}
