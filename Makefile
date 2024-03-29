public/index.html: public/elm.js

public/elm.js: $(wildcard src/*.elm) $(wildcard src/**/*.elm)
	elm make src/Main.elm --output=public/elm.js

.PHONY=clean
clean:
	rm public/elm.js

prod:
	elm make src/Main.elm --optimize --output=elm.js
	uglifyjs elm.js --compress "pure_funcs=[F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9],pure_getters,keep_fargs=false,unsafe_comps,unsafe" | uglifyjs --mangle --output=public/elm.js
	rm elm.js
