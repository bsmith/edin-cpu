#!/usr/bin/perl

use 5.024;
use strict;
use warnings;
use Time::HiRes qw(time);
use Sys::Hostname qw(hostname);
use Cwd qw(getcwd);
use String::ShellQuote qw(shell_quote_best_effort);

my $ST;
BEGIN { $ST = time }

sub timed_log {
  @_ = sprintf("BLANK MESSAGE at %s line %s", (caller)[1,2]) unless @_;
  push @_, "\n" unless defined $_[-1] and $_[-1] =~ /\R$/;
  print sprintf("[%.6f] ", time - $ST), @_;
}

#say "EDIN proto 1";
print <<'END';
 _____ ____ ___ _   _                   _          _ 
| ____|  _ \_ _| \ | |  _ __  _ __ ___ | |_ ___   / |
|  _| | | | | ||  \| | | '_ \| '__/ _ \| __/ _ \  | |
| |___| |_| | || |\  | | |_) | | | (_) | || (_) | | |
|_____|____/___|_| \_| | .__/|_|  \___/ \__\___/  |_|
                       |_|                           
END
say scalar(localtime);
say shell_quote_best_effort(hostname(), getcwd(), "$0");
say "args: ", shell_quote_best_effort(@ARGV);

# "Library" code
our $t = 0;
our $max_tpd;

our %registers;
our %logics;

sub define_register {
  my ($name, $width) = @_;
  die if defined($registers{$name});

  my $mask = 0xffffffff >> (32 - $width);
  $registers{$name} = {
    name => $name,
    width => $width,
    mask => $mask,
    latched => 0x5a5a5a5a & $mask,
    input => undef,
    # TODO: track "definedness"
  };
  timed_log sprintf "Defined latch %s with mask %08x\n", $name, $mask;
  return $name;
}

sub read_latch {
  my ($name) = @_;
  die unless defined $registers{$name};
#  die if $registers{$name}{latched} & ~$registers{$name}{mask};
  return $registers{$name}{latched} & $registers{$name}{mask};
}

# TODO: a version that just does bitfields - and keeps track of definedness separately
sub input_latch {
  my ($name, $input) = @_;
  die unless defined $registers{$name};
#  die if $input & ~$registers{$name}{mask};
  die if defined $registers{$name}{input};
  $registers{$name}{input} = $input;
  return;
}

sub set_latches {
  for my $name (sort keys %registers) {
    $registers{$name}{latched} = $registers{$name}{input} & $registers{$name}{mask};
    $registers{$name}{input} = undef;
  }
}

sub define_logic {
  my ($name, $in_bits, $out_bits, $func) = @_;
  die if defined($logics{$name});

  $logics{$name} = {
    name => $name,
    in_bits => $in_bits,
    out_bits => $out_bits,
    func => $func,
  };
  timed_log sprintf "Defined logic %s with %s in and %s out bits\n", $name, $in_bits, $out_bits;
  return $name;
}

sub run_logic {
  my ($name, $input) = @_;
  die unless defined $logics{$name};
# die if $input & ~$logics{$name}{in_mask};
  return $logics{$name}{func}->($input);
}

sub dump_state {
  my @lines;
  for my $latch_name (sort keys %registers) {
    push @lines, sprintf(q{"%s": %x}, $latch_name, $registers{$latch_name}{latched});
  }
  unshift @lines, qq{"_t": $t};
  print "{ ", join(",\n  ", @lines), " }\n";
}

# This is intended to just simulate an 8-bit ALU implemented with a pair of LUTs and fast-carry logic
# This is driven by a microcode harness to store the fibonacci sequence in memory

define_register("P", 8);
define_register("Q", 8);
define_register("R", 8);

#define_memory("MS", 8, 8);

#define_counter("MAR", 8);

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

timed_log "Defined gates";

# The simulation procedes as follows
# Time 0 is the initial "random" state of the gates
# Then we loop over the ticks and the tick is numbered on the gates being set at the end.
#   Wiring from latch outputs is evaluated
#   Latches are set - defining their values for time 't'
# As wiring is evaluated, we accumulate an estimate of propagation time
# Memory for write works as a latch, but for reading works like logic

# TODO: simulate pull up on MAR._reset
# TODO: declarative wiring (netlists)
#       define_net(driver, driven)
# also able to specify bit-ranges
# TODO: generate Perl or C code (with use integer)
# TODO: B::Concise,-exec dump of generated perl

timed_log "Start of run\n";

die unless $t == 0;
dump_state();
while ($t < 100) {
  local $max_tpd = 0;

  # wiring for main ALU block
  my $p = read_latch("P");
  my $q = read_latch("Q");
  my $fast_carry_bit = run_logic("ALU_fast_carry", (($p & 0xf) << 4) | ($q & 0xf));
  my $alu_low = run_logic("ALU_low", (($p & 0xf) << 4) | ($q & 0xf));
  my $alu_high = run_logic("ALU_high", (($fast_carry_bit & 1) << 8) | ($p & 0xf0) | (($q >> 4) & 0xf));

  if ($t == 0) {
    input_latch("P", 1);
    input_latch("Q", 1);
    input_latch("R", 1);
#    input_latch("MAR._reset", 0);
  } else {
    input_latch("P", read_latch("Q"));
    input_latch("Q", (($alu_high & 0xf) << 4) | ($alu_low & 0xf));
    input_latch("R", (($alu_high & 0xf) << 4) | ($alu_low & 0xf));
  }

  # latching
  $t++;
  set_latches();
  
  dump_state();

  # checking
  if ($t > 1) {
    my $r = read_latch("R");
    our ($fib_a, $fib_b);
    ($fib_a, $fib_b) = (1, 1) unless defined $fib_a and defined $fib_b;
    ($fib_a, $fib_b) = ($fib_b, ($fib_a + $fib_b) & 0xff);
    if ($fib_b == $r) {
      print "check okay: $fib_b\n";
    } else {
      print ">>> NOT OKAY: want: $fib_b;  got: $r <<<\n";
    }
  }
}

timed_log "End of run\n";
