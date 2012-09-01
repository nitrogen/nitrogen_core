all: compile

compile:
	./rebar compile

clean:
	./rebar clean

docs:  
	./make_docs.el
	@(cd doc;perl add_disqus.pl)

doc: docs
