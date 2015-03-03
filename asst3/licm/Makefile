all: licm.so

CXXFLAGS = -std=c++11 -rdynamic $(shell llvm-config --cxxflags) -g -O0

licm.o: licm.cpp dominance.h
dominance.o: dominance.cpp dominance.h

licm.so: licm.o dominance.o
	$(CXX) -dylib -shared $^ -o $@
clean:
	rm -f *.o *~ *.so
