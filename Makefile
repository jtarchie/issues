all: main.js
clean:
	rm -f main.js
main.js:
	elm make --warn --output main.js --yes Main.elm
.PHONY: clean

