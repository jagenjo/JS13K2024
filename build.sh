echo "minifying"
rm compo/*
rm compo.zip
cat src/indexFinal.html > temp.html
cat src/microgl.js >> temp.html
cat src/game.js >> temp.html
minify temp.html > compo/index.html
rm temp.html
#minify src/microgl.js > compo/microgl.js
#minify src/game.js > compo/game.js
#minify src/index.html > compo/index.html
cp src/*.xbin compo
#cp src/*.png compo
echo "compressing..."
zip -9 compo.zip -r compo
ls -l compo.zip
