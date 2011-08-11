all: compile

compile:
	./rebar compile

clean:
	./rebar clean

docs:  
	./make_docs.el

doc: docs
