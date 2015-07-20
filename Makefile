out: src
	mkdir -p out
	hastec src/Petting.hs -o out/Petting.js -Wall -O # broken: --opt-minify
	cp src/*.css out
	cp src/*.gif out
	cp src/*.html out
