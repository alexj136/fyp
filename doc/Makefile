wc: final.pdf
	@echo "Word count:"
	@pdftotext final.pdf
	@wc -w final.txt
	@rm final.txt

final.pdf: final.tex lexed.png parsed.png task_dependencies.png
	@pdflatex final.tex

lexed.png: lexed.dot
	@dot -Kfdp -Tpng lexed.dot -o lexed.png

parsed.png: parsed.dot
	@dot -Kfdp -Tpng parsed.dot -o parsed.png

task_dependencies.png: task_dependencies.dot
	@dot -Tpng task_dependencies.dot -o task_dependencies.png

clean:
	@rm -f final.pdf lexed.png parsed.png task_dependencies.png
	@echo CLEANED
