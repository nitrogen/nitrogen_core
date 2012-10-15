all: compile

compile:
	./rebar compile

clean:
	./rebar clean

docs:  
	utils/make_docs/make_docs.el
	@(cd doc;perl add_disqus.pl)

doc: docs

vim:
	utils/vim-headers/add_vim.sh
