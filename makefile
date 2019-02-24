docs:
	pandoc --mathjax -s -c style.css --to html -o docs/index.html trans-fx-demo/src/Control/FX/Demo.lhs
	pandoc --mathjax -s -c style.css --to html -o docs/intro.html trans-fx-demo/src/Control/FX/Demo/Intro.lhs
	pandoc --mathjax -s -c style.css --to html -o docs/compose.html trans-fx-demo/src/Control/FX/Demo/Compose.lhs

.PHONY: docs
