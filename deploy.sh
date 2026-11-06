#!/bin/bash
# Publish the app: checks first, then upload and compare.
#   ./deploy.sh           syntax + helper tests, then upload (fast)
#   ./deploy.sh --full    additionally the browser smoke test (about 1 minute)
set -e
cd "$(dirname "$0")"
SERVER=root@139.59.214.176
DIR=/srv/shiny-server/sample-apps/fairy

echo "1/5 syntax of app.R"
Rscript -e 'invisible(parse("app.R"))'

echo "2/5 tests of the helper functions"
Rscript -e 'testthat::test_file("tests/test-app-helpers.R", reporter = "summary", stop_on_failure = TRUE)'

if [ "$1" = "--full" ]; then
  echo "3/5 smoke test in a headless browser"
  NOT_CRAN=true Rscript -e 'testthat::test_file("tests/smoke.R", reporter = "summary", stop_on_failure = TRUE)'
else
  echo "3/5 smoke test skipped (use --full to run it)"
fi

echo "4/5 git: everything committed, then push"
if [ -n "$(git status --porcelain)" ]; then
  echo "There are uncommitted changes: commit first."; exit 1
fi
git push -q

echo "5/5 upload and restart"
scp -q app.R $SERVER:$DIR/app.R
if [ -d www ]; then scp -qr www $SERVER:$DIR/; ssh $SERVER "chown -R shiny:shiny $DIR/www && chmod -R a+rX $DIR/www"; fi
ssh $SERVER "cd $DIR && chown shiny:shiny app.R && chmod 644 app.R && touch restart.txt"
LOCAL=$(md5 -q app.R 2>/dev/null || md5sum app.R | cut -d' ' -f1)
REMOTE=$(ssh $SERVER "md5sum $DIR/app.R" | cut -d' ' -f1)
if [ "$LOCAL" = "$REMOTE" ]; then echo "Deployed, md5 identical: $LOCAL"; else echo "md5 DIFFERENT: $LOCAL vs $REMOTE"; exit 1; fi
