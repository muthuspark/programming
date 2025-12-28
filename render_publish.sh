#!/bin/bash
set -e  # Exit on any error

hugo
echo "Removing pagination redirect pages..."
rm -rf public/page public/tags/page public/posts/page
echo "Running pagefind..."
npx -y pagefind --site public
echo "Building site with Hugo..."
echo "Copying ads.txt..."
cp ads.txt public/ads.txt
git add .
current_date=$(date +"%Y-%m-%d")
commit_message=${1:-"Release $current_date"}
git commit -m "$commit_message"
git push
echo "Deployment completed successfully!"