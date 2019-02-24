docs:
	pandoc --mathjax -s -c style.css --to html -o docs/index.html trans-fx-demo/src/Control/FX/Demo.lhs
	pandoc --mathjax -s -c style.css --to html -o docs/basic-effects.html trans-fx-demo/src/Control/FX/Demo/BasicEffects.lhs
	pandoc --mathjax -s -c style.css --to html -o docs/doing-io.html trans-fx-demo/src/Control/FX/Demo/DoingIO.lhs

.PHONY: docs
