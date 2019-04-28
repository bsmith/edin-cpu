# This is intended to just simulate an 8-bit ALU implemented with a pair of LUTs and fast-carry logic
# This is driven by a microcode harness to store the fibonacci sequence in memory

define_register("P", 8);
define_register("Q", 8);
define_register("R", 8);

#define_memory("MS", 8, 8);

#define_counter("MAR", 8);

# XXX use quote_sub here!
sub ALU_fast_carry {
  my $p_low = ($_[0] >> 4) & 0xf;
  my $q_low = ($_[0] >> 0) & 0xf;
  return((($p_low + $q_low) >> 4) & 1);
}
define_logic("ALU_fast_carry", 8, 1, \&ALU_fast_carry);

sub ALU {
  my $cin = ($_[0] >> 8) & 1;
  my $p = ($_[0] >> 4) & 0xf;
  my $q = ($_[0] >> 0) & 0xf;
  return(($p + $q + $cin) & 0x1f);
}
define_logic("ALU_high", 9, 5, \&ALU);
define_logic("ALU_low", 9, 5, \&ALU);

define_logic("zero", 0, 1, sub { return 0 });

timed_log "Defined gates";

## 0 -> ALU_fast_carry.in[8]; # thinko - fast carry doesn't have cin.  Add it? TODO?
#define_net("zero.out", "ALU_fast_carry.in[8]", 1);
define_net("P.out[0:3]", "ALU_fast_carry.in[0:3]", 4);
define_net("Q.out[0:3]", "ALU_fast_carry.in[4:7]", 4);
# 0 -> ALU_low.in[8];
define_net("zero.out", "ALU_low.in[8]", 1);
define_net("P.out[0:3]", "ALU_low.in[0:3]", 4);
define_net("Q.out[0:3]", "ALU_low.in[4:7]", 4);
define_net("ALU_fast_carry.out", "ALU_high.in[8]", 1);
define_net("P.out[4:7]", "ALU_high.in[0:3]", 4);
define_net("Q.out[4:7]", "ALU_high.in[4:7]", 4);

define_net("Q.out", "P.in", 8);
define_net("ALU_high.out[0:3]", "Q.in[4:7]", 4);
define_net("ALU_low.out[0:3]", "Q.in[0:3]", 4);
define_net("ALU_high.out[0:3]", "R.in[4:7]", 4);
define_net("ALU_low.out[0:3]", "R.in[0:3]", 4);

on_reset_latches sub {
  reset_latch("P", 1);
  reset_latch("Q", 1);
  reset_latch("R", 1);
};

on_check sub {
  if ($t > 0) {
    my $r = read_latch("R");
    our ($fib_a, $fib_b);
    ($fib_a, $fib_b) = (1, 1) unless defined $fib_a and defined $fib_b;
    ($fib_a, $fib_b) = ($fib_b, ($fib_a + $fib_b) & 0xff);
    if ($fib_b == $r) {
      timed_log "check okay: $fib_b\n" if ($t % $DUMP_INTERVAL) == 0;
    } else {
      timed_log ">>> NOT OKAY: want: $fib_b;  got: $r <<<\n";
      dump_state();
      $not_okay++;
      last if $not_okay > 100;
    }
  }
};

1;
