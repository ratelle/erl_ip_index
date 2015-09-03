REBAR=./rebar

all: deps compile

deps:
	@$(REBAR) update-deps
	@$(REBAR) get-deps

compile:
	$(REBAR) compile

fast:
	$(REBAR) compile skip_deps=true

clean:
	@rm -rf deps
	@$(REBAR) clean

CFLAGS=-Wall -Wextra -pedantic -std=gnu11 -g
CXXFLAGS=-Wall -Wextra -pedantic -std=gnu++11 -g
INDEXED_EWAH_O := c_src/ewah_bitmap.c c_src/ewah_rlw.c c_src/indexed_ewah.c

check_valgrind: $(INDEXED_EWAH_O) test/standalone.cpp
	$(CXX) $(CXXFLAGS) -Ic_src $^ -o test/standalone
	valgrind ./test/standalone 3000000 0 32

.PHONY: all deps compile clean check_valgrind
