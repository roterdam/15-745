all: liveness.so available.so

CXXFLAGS = -std=c++11 -rdynamic $(shell llvm-config --cxxflags) -g -O0

dataflow.o: dataflow.h dataflow.cpp

available-support.o: available-support.cpp available-support.h	

%.so: %.o dataflow.o available-support.o
	$(CXX) -dylib -shared $^ -o $@

clean:
	rm -f *.o *~ *.so

.PHONY: clean all
