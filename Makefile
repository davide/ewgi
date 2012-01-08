ERL		?= erl
EBIN_DIRS	:= $(wildcard lib/*/ebin)
APP		:= ewgi

all: erl

erl: lib
        make -C all

docs:
	@erl -noshell -run edoc_run application '$(APP)' '"."' '[]'

clean:
	@echo "removing:"
	@rm -fv ebin/*.beam

lib:
	@mkdir lib

dialyzer: erl
	@dialyzer -c ebin

test: erl
	@$(ERL) -pa $(EBIN_DIRS) -pa ebin -noinput +B \
	-eval 'case lists:member(error, ewgi_test:test()) of true -> halt(1); _ -> halt(0) end.'
