all: dataflow.so dce.so

CXXFLAGS = -std=c++11 -rdynamic $(shell llvm-config --cxxflags) -g -O0

dataflow.o: dataflow.h dataflow.cpp

%.so: %.o
	$(CXX) -dylib -shared $^ -o $@
clean:
	rm -f *.o *~ *.so
