pushd %~dp0 && npm run build && pushd dist && git add . && git commit -m "Publish new version" && git push -u origin gh-pages && popd && popd
