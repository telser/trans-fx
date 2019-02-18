docs:
	pandoc --mathjax -s -c style.css --to html -o docs/intro.html trans-fx-demo/src/Control/FX/Demo/Intro.lhs

.PHONY: docs
