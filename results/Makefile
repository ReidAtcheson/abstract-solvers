




all : abstract_cg_results.pdf one_cg_results.pdf

cg_results :
	python3 plot_cg_results.py

abstract_cg_results.pdf : cg_results
	gs -dBATCH -dNOPAUSE -q -sDEVICE=pdfwrite -sOutputFile=abstract_cg.pdf \
		abstract_cg_1.pdf abstract_cg_2.pdf abstract_cg_3.pdf \
		abstract_cg_4.pdf abstract_cg_5.pdf \
		abstract_cg_6.pdf abstract_cg_7.pdf abstract_cg_8.pdf \
		abstract_cg_9.pdf abstract_cg_10.pdf

one_cg_results.pdf : cg_results
	gs -dBATCH -dNOPAUSE -q -sDEVICE=pdfwrite -sOutputFile=one_cg.pdf \
		one_cg_1.pdf one_cg_2.pdf one_cg_3.pdf \
		one_cg_4.pdf one_cg_5.pdf \
		one_cg_6.pdf one_cg_7.pdf one_cg_8.pdf \
		one_cg_9.pdf one_cg_10.pdf



.PHONY : clean



clean :
	rm -rf ./*.dat
	rm -rf ./*.pdf
