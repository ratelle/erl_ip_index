REBAR=./rebar

all: get-deps compile

get-deps:
	$(REBAR) u-d

compile:
	$(REBAR) compile

clean:
	@$(REBAR) clean

CFLAGS=-Wall -Wextra -pedantic -std=gnu11 -g
CXXFLAGS=-Wall -Wextra -pedantic -std=gnu++11 -g
INDEXED_EWAH_O := c_src/ewah_bitmap.o c_src/ewah_rlw.o c_src/indexed_ewah.o

check: all eunit qc

eunit:
	$(REBAR) eunit

qc:
	$(REBAR) qc


.PHONY: all compile get-deps clean check eunit qc check_valgrind
