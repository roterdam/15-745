namespace dataflow {

/*
  Note the weird class def line for LivenessTransferFunction.
*/
class LivenessTransferFunction :
      public TransferFunction<LivenessTransferFunction> {
 public:
  LivenessTransferFunction(const Instruction *inst) { }
  ~LivenessTransferFunction() { }
 
  LivenessTransferFunction& thenCall(const LivenessTransferFunction& other) {
    return *this;
  }
  
  BitVector& operator()(BitVector& bv) const {
    return bv;
  }
};


void foo(const Function& code) {
  DataflowParameters<LivenessTransferFunction> configParams;
  configParams.dir = FlowDirection::FORWARD;

  auto out = dataflow(code, configParams);
}

} // namespace dataflow
