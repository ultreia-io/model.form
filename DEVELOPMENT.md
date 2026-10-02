# Maintainer guide

`develop` is the integration branch. `main` contains releases. Implementation branches use `feature/`.

## Local verification

Run from the package root with development dependencies installed:

```sh
Rscript -e 'roxygen2::roxygenise(); rmarkdown::render("README.Rmd"); testthat::test_local()'
mkdir -p build
(cd build && R CMD build --no-manual ..)
R CMD check --no-manual --output=build build/model.form_0.1.0.tar.gz
Rscript tools/build-site.R
```

The site includes a native coverage page at `docs/articles/coverage.html`, using the shared theme and navigation.

Coverage CSV and XML stay in `docs/coverage/`. Archives and check output stay in `build/`.

Run `Rscript tools/coverage.R` to refresh coverage data alone; rebuild the site to refresh its page.

`roxygen2`, `rcmdcheck`, and `pkgload` are development tools, not runtime dependencies.

Review coverage gaps for missing behaviour. Check installed resources, runnable examples, and website navigation.

Keep the YAML schema authoritative. Semantic invariants live in R and must have focused behavioural tests.

## Release

After code review, update DESCRIPTION and NEWS to the same version on `develop`, then run **Perform Release**.

The workflow runs checks and coverage, publishes the source archive and checksum, and deploys the released documentation.

Verify the workflow and release assets. If publication fails, rerun the failed job.

## License

LGPL-2.1. See [LICENSE](LICENSE).
