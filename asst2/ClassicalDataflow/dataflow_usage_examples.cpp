namespace dataflow {

/*
  Note the weird class def line for LivenessTransferFunction.
*/
class LivenessTransferFunction : public TransferFunction<LivenessTransferFunction> {
 public:
  LivenessTransferFunction(const Instruction *inst) { }
  ~LivenessTransferFunction() { }
 
  void compose(const LivenessTransferFunction& other) { }
  
  BitVector operator()(const BitVector& bv) const {
    BitVector bv2 = bv;
    return bv2;
  }
};


void foo(const Function& code) {
  DataflowParameters<LivenessTransferFunction> configParams;
  configParams.dir = FlowDirection::FORWARD;

  auto out = dataflow(code, configParams);
}

} // namespace dataflow
