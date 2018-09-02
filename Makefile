all:
	./private/elm make ./src/Main.elm --output ./dist/app.js

build.log: ./src/*
	make all | tee ./build.log

clean:
	rm -rf ./dist/
	mkdir -p ./dist/
	mkdir -p ./logs/
	touch ./build.log

watch:
	(while true; do make -q build.log || make build.log ; sleep 1; done) | grep -v 'make\[1\]'

.PHONY: all clean
