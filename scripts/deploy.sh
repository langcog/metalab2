#!/bin/bash

echo "===== START NEW DEPLOYMENT ====="
echo "==> Page update at "$(date -u +%Y-%m-%dT%H:%M:%SZ)

echo "==> Change working directory."
BASENAME=$(dirname "$0")
cd $BASENAME/metalab2

echo "==> Cleaning current git branch"
git fetch origin
git clean -qdf
git reset --hard

echo "==> Switching to and updating master branch."
git checkout master
git clean -qdf
git reset --hard origin/master

echo "==> Setting proper directory for script execution."
cd scripts

echo "==> Installing needed packages."
sudo Rscript packageInstaller.R

echo "==> Script main_builder.R execution"
Rscript main_builder.R

echo "==> Moving back to the root directory"
cd ..

echo "==> Updating shinyapps."
find shinyapps/* -type d -exec touch {}/restart.txt \;
cp -R shinyapps/* /srv/shiny-server/
find /srv/shiny-server/ -name "restart.txt" -exec touch {} \;

echo "==> Copying shiny apps' data to shiny server."
cp -R metadata/ /srv/shiny-server/common/
cp -R data/ /srv/shiny-server/common/

echo "==> Move .gitignore-gh-pages to the rendered site."
cp .gitignore-gh-pages rendered/

echo "==> Switching to gh-pages branch."
git checkout gh-pages

echo "==> Replacing entire repo content with rendered site folder."
# Copy the git history into the rendered/ folder
cp -Rf .git rendered/
# Delete everything except the rendered/ folder
find . -maxdepth 1 ! -name 'rendered' -exec rm -rf {} +
# Move everything from the rendered/ folder into current folder
mv rendered/* .
mv rendered/.* .
rm -rf rendered
# Use proper gitignore file
mv .gitignore-gh-pages .gitignore

echo "==> Pushing new site to GitHub."
git add -A
commitMessage="Page update on "$(date -u +%Y-%m-%dT%H:%M:%SZ)
git commit -m "$commitMessage"
git push origin gh-pages

git checkout master
