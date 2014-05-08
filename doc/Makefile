wc: final.pdf
	@echo "Word count:"
	@pdftotext final.pdf
	@wc -w final.txt

final.pdf: final.tex lexed.png parsed.png task_dependencies.png
	@pdflatex final.tex

interim.pdf: interim.tex task_dependencies.png
	@pdflatex interim.tex

lexed.png: lexed.dot
	@dot -Kfdp -Tpng lexed.dot -o lexed.png

parsed.png: parsed.dot
	@dot -Kfdp -Tpng parsed.dot -o parsed.png

task_dependencies.png: task_dependencies.dot
	@dot -Tpng task_dependencies.dot -o task_dependencies.png

.PHONY: clean
clean:
	@rm -f *.pdf *.txt *.png *.aux *.log
	@echo CLEANED
