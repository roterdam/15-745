// I made a fallback framework in case you guys hate the template-based one.

class TransferFunction {
 public:
  ~TransferFunction() { }
  virtual BitVector& operator()(const BitVector&) const = 0;
};

class TransferFunctionBuilder {
 public:
  ~TransferFunctionBuilder() { }
  virtual TransferFunction *makeInstTransferFn(const Instruction *) const = 0;
  virtual TransferFunction *makeBlockTransferFn(const BasicBlock *) const = 0;
};

typedef BitVector& (*MeetFunction)(const BitVector&, const BitVector&);
typedef DenseMap<Instruction *, BitVector *> DataMap;
enum class FlowDirection { FORWARD, BACKWARD };

struct DataflowConfiguration {
  FlowDirection dir;
  TransferFunctionBuilder *fnBuilder;
  MeetFunction meetWith;
  BitVector top;
  BitVector boundary_state;
};

DataMap *dataflow(const Function& code, const DataflowConfiguration& config);
