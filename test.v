module test (clk);
  input clk; // Clock is required to get initial activation
  always @(posedge clk) begin
    Test_0();
  end
endmodule
