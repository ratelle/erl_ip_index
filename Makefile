REBAR=./rebar3

all: compile

compile:
	$(REBAR) compile

clean:
	@$(REBAR) clean

CFLAGS=-Wall -Wextra -pedantic -std=gnu11 -g
CXXFLAGS=-Wall -Wextra -pedantic -std=gnu++11 -g
INDEXED_EWAH_O := c_src/ewah_bitmap.o c_src/ewah_rlw.o c_src/indexed_ewah.o

check: all eunit proper dialyzer

eunit:
	$(REBAR) eunit

dialyzer:
	$(REBAR) dialyzer

proper:
	$(REBAR) proper

AFL_FUZZ ?= afl-fuzz
AFL_CC ?= afl-gcc
AFL_CXX ?= afl-g++
FUZZ_SKELETON ?= fuzz_skeleton

fuzz:
	$(REBAR) clean
	CC=$(AFL_CC) CXX=$(AFL_CXX) $(REBAR) compile
	$(AFL_FUZZ) $(AFL_FLAGS) -i fuzz/samples -o fuzz/findings -- \
	  $(FUZZ_SKELETON) priv/ip_index_nif.so fuzz/build_index_nif.term

.PHONY: all compile get-deps clean check eunit qc check_valgrind fuzz
