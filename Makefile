DIALYZER_OPTS=-Werror_handling -Wrace_conditions -Wunmatched_returns

all: init compile xref eunit

init:
	./rebar get-deps compile 

compile:
	./rebar compile skip_deps=true

xref:
	./rebar xref skip_deps=true

clean:
	./rebar clean skip_deps=true

eunit:
	./rebar eunit skip_deps=true

edoc:
	./rebar doc skip_deps=true
	find doc -name '*.html' | xargs sed -i.orig 's/ISO-8859-1/UTF-8/'
	ERL_LIBS=deps/edown deps/edown/make_doc
	sed -i.org 's_http://github.com/esl/.*doc_doc_' README.md && rm README.md.org

start: compile
	erl -pz ebin

build_plt:
	dialyzer --build_plt --apps erts kernel stdlib -r ebin

dialyzer:
	dialyzer --src -r src/ $(DIALYZER_OPTS)
