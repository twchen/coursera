val in1, in2, sum, carry = new Wire
halfAdder(in1, in2, sum, carry)
probe("in1", in1)
probe("in2", in2)
probe("sum", sum)
probe("carry", carry)

in1 setSignal true
run()

in2 setSignal true
run()
