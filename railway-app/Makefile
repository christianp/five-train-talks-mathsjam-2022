DIRNAME=$(notdir $(CURDIR))

ELMS=$(wildcard src/*.elm)

app.js: src/RailwayApp.elm $(ELMS)
	-elm make $< --output=$@ 2> error.txt
	@cat error.txt

upload: app.js index.html style.css
	rsync -avz . clpland:~/domains/somethingorotherwhatever.com/html/$(DIRNAME)
	@echo "Uploaded to https://somethingorotherwhatever.com/$(DIRNAME)"
