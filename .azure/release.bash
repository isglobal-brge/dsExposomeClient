#!/bin/bash

# Azure Pipeline predefined environment variables
# - BUILD_REASON: we check on "PullRequest
# - BUILD_REPOSTIORY_NAME: we use to determine the package that is build
# - BUILD_REPOSITORY_LOCALPATH: we need go back to the directory that contains the sources after the loop
# - AGENT_HOMEDIRECTORY: we need this to store the docker credentials
# - SYSTEM_PULLREQUEST_PULLREQUESTID: PullRequestID from GitHub
# - SYSTEM_PULLREQUEST_TARGETBRANCH: PullRequest target branch e.g. main
# Additional environment variables to make sure the release works
# - GITHUB_TOKEN: password used to push to github
# - R_LIBS_USER: home directory user libraries

git remote set-url origin "https://${GITHUB_TOKEN}@github.com/${BUILD_REPOSITORY_NAME}.git"
git checkout -f master
git fetch --tags
Rscript -e "withr::with_libpaths(new = '${R_LIBS_USER}', git2r::config(user.email = 'xavier.escriba@isglobal.org', user.name = 'Azure Pipeline'))"
RELEASE_SCOPE="patch"
Rscript -e "withr::with_libpaths(new = '${R_LIBS_USER}', usethis::use_version('${RELEASE_SCOPE}'))"
TAG=$(grep Version DESCRIPTION | head -n1 | cut -d':' -f2 | xargs)
PACKAGE=$(grep Package DESCRIPTION | head -n1 | cut -d':' -f2 | xargs)
Rscript -e "withr::with_libpaths(new = '${R_LIBS_USER}', devtools::document())"
Rscript -e "withr::with_libpaths(new = '${R_LIBS_USER}', pkgdown::build_site())"
git commit -a -m "[ci skip] Created release: ${TAG}"
echo "Releasing ${PACKAGE} ${TAG}"
R CMD build .
Rscript -e "withr::with_libpaths(new = '${R_LIBS_USER}', devtools::check_built(path = './${PACKAGE}_${TAG}.tar.gz', force_suggests = TRUE))"
git tag "${TAG}"
echo "Creating new development version for R-package: [ ${BUILD_REPOSITORY_NAME} ]" # create this version on the `dev` branch
# Rebase `dev` branch to `master`
git checkout dev
git rebase master
# Use dev-versioning number (-9000)
Rscript -e "withr::with_libpaths(new = '${R_LIBS_USER}', usethis::use_version('dev'))"
git commit -a -m 'Increment dev-version number'
# Push `dev`/`master`
git push --tags origin master
git push --tags origin dev
