REBAR=./rebar

all: compile

compile:
	$(REBAR) compile

clean:
	@$(REBAR) clean

CFLAGS=-Wall -Wextra -pedantic -std=gnu11 -g
CXXFLAGS=-Wall -Wextra -pedantic -std=gnu++11 -g
INDEXED_EWAH_O := c_src/ewah_bitmap.o c_src/ewah_rlw.o c_src/indexed_ewah.o

check_valgrind: $(INDEXED_EWAH_O) test/standalone.cpp
	$(CXX) $(CXXFLAGS) -Ic_src $^ -o test/standalone
	valgrind ./test/standalone 3000000 0 32

.PHONY: all compile clean check_valgrind
