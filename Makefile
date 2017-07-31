all: main.js
clean:
	rm -f main.js
main.js:
	elm make --warn --output main.js --yes Main.elm
	closure-compiler --js main.js --js_output_file main.min.js
.PHONY: clean

