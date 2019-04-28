#!/usr/bin/perl

# proto1.pl reaches a performance of about 17 k ticks/second
#   loop speed of about 26 k ticks/second
# this version emits a compiled loop body - hopefully faster!
# the first revision that kept loops in the body but inlined the four subs
#   had a loop speed of about 29 k ticks/second, but an overall speed of 6 k
# second revision with logics and latches loops removed, but apply_nets kept
#   had a loop speed of about 35 k ticks/second, but an overall speed of 3 k
# third revision with lexical wiring vars and 256 iterations
#   had a loop speed of about 125 k ticks/second, but an overall speed of 6.5 k

use 5.024;
use strict;
use warnings;
use Time::HiRes qw(time);
use Sys::Hostname qw(hostname);
use Cwd qw(getcwd);
use String::ShellQuote qw(shell_quote_best_effort);
use Carp qw(croak);
use B::Concise ();
use B::Deparse ();

my $ST;
BEGIN { $ST = time }

BEGIN { $Carp::Verbose = !!1 }

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
		 comp_pl version!
END
say scalar(localtime);
say shell_quote_best_effort(hostname(), getcwd(), "$0");
say "args: ", shell_quote_best_effort(@ARGV);

# "Library" code
# First the circuit definition
our %signals;
our %registers;
our %logics;
# nets is a map from driver signal to driven signal to net with bitmask
our %nets;

# N set bits
sub bit_mask {
  return 0xffffffff >> (32 - $_[0]);
}

# input: name
# input: name[0:3]
# returns: (name, mask, shift, length)
sub parse_name {
  my ($input) = @_;
  $input =~ m{^ ((?![0-9])[a-zA-Z0-9_.'-]+) (?:\[ (\d+)(?::(\d+))? \])? $}ax;
  croak "parse_name($input): not parsed" unless defined $1;
  my ($name, $first, $last) = ($1, $2, $3);
  croak "parse_name($input): signal not defined" unless defined $signals{$name};
  if (defined $first and not defined $last) {
    $last = $first;
  }
  $first //= 0;
  $last //= $signals{$name}{width} - 1;
  die "parse_name($input): last=$last width=$signals{$name}{width}" if $last >= $signals{$name}{width};
  die unless $first <= $last;
  my $length = $last - $first + 1;
  my $mask = bit_mask($length) << $first;
  die unless defined $signals{$name};
  return ($name, $mask, $first, $length);
}

sub define_signal {
  my ($name, $width, $direction) = @_;
  die if defined($signals{$name});
  die unless $direction =~ m{^(?:in|out)$};

  $signals{$name} = {
    name => $name,
    width => $width,
    pullup => undef,
    direction => $direction,
  };
  timed_log sprintf "Defined signal %s of width %s\n", $name, $width;
  return $name;
}

# TODO: allow for bitfields?
sub set_pullup {
  my ($name, $enabled) = @_;
  die unless defined($signals{$name});
  if ($enabled) {
    $signals{$name}{pullup} = bit_mask($signals{$name}{width});
  } else {
    $signals{$name}{pullup} = undef;
  }
  return;
}

sub define_register {
  my ($name, $width) = @_;
  die if defined($registers{$name});

  # should I use buses or muxes?  TODO
  define_signal($name . ".in", $width, "in");
#  define_signal($name . "._wr", 1);
#  define_signal($name . ".oe", 1);
  define_signal($name . ".out", $width, "out");
#  set_pullup($name . ".oe", 1); 
#  set_pullup($name . "._wr", 1); 

  my $mask = 0xffffffff >> (32 - $width);
  $registers{$name} = {
    name => $name,
    width => $width,
    mask => $mask,
#    latched => 0x5a5a5a5a & $mask,
#    input => undef,
    # TODO: track "definedness"
  };
  timed_log sprintf "Defined register %s with mask %08x\n", $name, $mask;
  return $name;
}

sub define_logic {
  my ($name, $in_bits, $out_bits, $func) = @_;
  die if defined($logics{$name});

  define_signal($name . ".in", $in_bits, "in");
  define_signal($name . ".out", $out_bits, "out");

  $logics{$name} = {
    name => $name,
    in_bits => $in_bits,
    out_bits => $out_bits,
    in_mask => bit_mask($in_bits),
    out_mask => bit_mask($out_bits),
    func => $func,
  };
  timed_log sprintf "Defined logic %s with %s in and %s out bits\n", $name, $in_bits, $out_bits;
  return $name;
}

sub define_net {
  my ($driver, $driven, $width) = @_;
  my ($driver_name, $driver_mask, $driver_shift, $driver_width) = parse_name($driver);
  my ($driven_name, $driven_mask, $driven_shift, $driven_width) = parse_name($driven);
  croak "define_net($driver, $driven, $width): driver/driven width mismatch" unless $driver_width == $driven_width;
  croak "define_net($driver, $driven, $width): declared width mismatch" unless $driver_width == $width;

  croak "define_net($driver, $driven, $width): net already defined" if defined $nets{$driver_name}{$driven_name};

  # TODO: use from/to instead of driver/driven
  # TODO: handle multiple mappings for the same pair better
  #   maybe just an array and resolve it later, like we do for multiple drivers
  $nets{$driver_name}{$driven_name} = {
      driver_mask => $driver_mask,
      driver_shift => $driver_shift,
      driven_mask => $driven_mask,
      driven_shift => $driven_shift,
      width => $width,
    };

  return;
}

# Preparing logics
our @logics_order;
sub tsort_logics {
  my %logic_marks;
  @logics_order = ();

  my $visit;
  $visit = sub {
    my ($logic_name) = @_;
    if ($logic_marks{$logic_name}) {
      die "loop" if $logic_marks{$logic_name} eq 'TEMP';
      return;
    }
    $logic_marks{$logic_name} = "TEMP";
    for my $driven_signal (keys %{$nets{$logic_name . ".out"}}) {
      my ($driven_block) = split /\./, $driven_signal;
      if (defined $logics{$driven_block}) {
        print "$logic_name drives logic $driven_block\n"; # DEBUG
        $visit->($driven_block);
      } else {
        print "$logic_name drives non-logic $driven_block\n"; # DEBUG
      }
    }
    $logic_marks{$logic_name} = "PERM";
    unshift @logics_order, $logic_name;
  };

  for my $logic_name (sort keys %logics) {
    $visit->($logic_name);
  }
  $visit = undef;

  return;
}

our %LUTs;
sub generate_luts {
  my $total_words = 0;
  for my $logic_name (sort keys %logics) {
    my $logic = $logics{$logic_name};
    my $out_mask = bit_mask($logic->{out_bits});
    my $in_max = $logic->{in_bits} == 0 ? 0 : bit_mask($logic->{in_bits});
    $total_words += $in_max + 1;
    printf "LUT %s for 0..%s masked %08x\n", $logic_name, $in_max, $out_mask; # DEBUG
    for my $in (0..$in_max) {
      $LUTs{$logic_name}[$in] = $logic->{func}($in) & $out_mask;
    }
  }
  printf "LUTs consume %d words\n", $total_words;
}

sub run_logic {
  my ($name, $input) = @_;
  croak "run_logic($name, undef): input is undef" unless defined $input;
  croak "run_logic($name, $input): logic not found" unless defined $logics{$name};
  die if $input & ~$logics{$name}{in_mask};
  return $LUTs{$name}[$input] if defined $LUTs{$name};
  return $logics{$name}{func}->($input & $logics{$name}{in_mask}) & $logics{$name}{out_mask};
}

# Running a simulation
our $t = 0;
our $max_tpd;

# currently latched values
# signal values are entirely internal to run_wiring! XXX
# TODO: yeah, but should be examinable externally?
# TODO: add "probe" latches - these have a .in wired to some other signal, but no .out needed
our %latches;
# new inputs to latches are found on wiring

# fill in initial values for the latches in the state hash (%latches)
# clear inputs
sub setup_state {
  %latches = ();
  for my $name (sort keys %registers) {
    $latches{$name} = 0x5a5a5a5a & $registers{$name}{mask};
  }
  return;
}

sub reset_latch {
  my ($name, $value) = @_;
  die if $value & ~$registers{$name}{mask};
  $latches{$name} = $value & $registers{$name}{mask};
  return;
}

sub dump_state {
  my @lines;
  for my $latch_name (sort keys %registers) {
    push @lines, sprintf(q{"%s": %x}, $latch_name, $latches{$latch_name});
  }
  unshift @lines, qq{"_t": $t};
  print "{ ", join(",\n  ", @lines), " }\n";
  return;
}

sub read_latch {
  my ($name) = @_;
  die unless defined $registers{$name};
  die if $latches{$name} & ~$registers{$name}{mask};
  return $latches{$name} & $registers{$name}{mask};
}

# Simulator step generator code

our %vars;
our %wiring_vars;
our $uniq_gen = 0;
sub reset_generator {
  %vars = ();
  %wiring_vars = ();
  $uniq_gen = 0;
}

sub get_var {
  my ($id) = @_;
  $id =~ s/[^a-zA-Z0-9]/_/g;
  $id = '_' . $id if $id =~ /^[0-9]/;
  if (defined $vars{$id}) {
    $id .= $uniq_gen++;
    die if defined $vars{$id};
  }
  $vars{$id} = $_[0];
  return $id;
}

sub gen_setup_wiring {
  my %driven_wires;
  for my $driver_name (keys %nets) {
    for my $driven_name (keys %{$nets{$driver_name}}) {
      $driven_wires{$driven_name}++;
    }
  }
  
  my $code = '';
  for my $driven_name (sort keys %driven_wires) {
    my $varname = $wiring_vars{$driven_name} = '$' . get_var($driven_name);
    $code .= 'my ' . $varname . " = \n";
  }
  $code .= "  0;\n";
  return $code;
}

sub gen_apply_nets {
  my ($driver_name, $value) = @_;
  my $code = '';

  $code .= sprintf "# %s driving %s\n",
    $driver_name, join(", ", sort keys %{$nets{$driver_name}});

  # TODO: instead of the die below, properly track which bits are driven
  # this allows checking we're not driving anything twice
  # then for logic inputs we can check they're driven
  # only really important after OE is implemented for 3-state logic since
  # without that, you can statically check this
  # TODO: that would be a good optimisation: only runtime check 3-state buses

  for my $driven_name (sort keys %{$nets{$driver_name}}) {
    my $net = $nets{$driver_name}{$driven_name};
    my $template = <<'END';
#      die if !!!wiring_driven!!! & !!!driven_mask!!!;
      !!!wiring_driven!!! |= ((!!!value!!!) & !!!driver_mask!!!) >> !!!driver_shift!!! << !!!driven_shift!!!;
END
    $template =~ s{!!!wiring_driven!!!}{$wiring_vars{$driven_name}}ge;
    $template =~ s{!!!driver_mask!!!}{sprintf "0x%08x", $net->{driver_mask}}ge;
    $template =~ s{!!!driver_shift!!!}{$net->{driver_shift}}ge;
    $template =~ s{!!!driven_mask!!!}{sprintf "0x%08x", $net->{driven_mask}}ge;
    $template =~ s{!!!driven_shift!!!}{$net->{driven_shift}}ge;
    $template =~ s{!!!value!!!}{$value}ge;
    $code .= $template;
  }

  return $code;
}

sub gen_latches_to_wires {
  my $code;
  for my $name (sort keys %registers) {
    my $value_template = q{ $latches{!!!name!!!} & !!!mask!!! };
    $value_template =~ s{!!!name!!!}{'"' . quotemeta($name) . '"'}ge;
    $value_template =~ s{!!!mask!!!}{sprintf "0x%08x", $registers{$name}{mask}}ge;
    $code .= gen_apply_nets($name . ".out", $value_template);
  }
  return $code;
}

sub gen_run_wiring {
  my $code;
  for my $logic_name (@logics_order) {
    my $template = '';
    my $output_var = '$' . get_var($logic_name . '_output');
    if (defined $LUTs{$logic_name}) {
      if ($logics{$logic_name}{in_bits} == 0) {
#	$template .= 'my !!!output_var!!! = ' . $LUTs{$logic_name}[0] . ";\n";
	$output_var = $LUTs{$logic_name}[0];
      } else {
	$template .= 'my !!!output_var!!! = $LUTs{!!!logic_name!!!}[!!!wiring_in!!!];' . "\n";
      }
    } else {
      if ($logics{$logic_name}{in_bits} == 0) {
        $template .= <<'END';
          my !!!output_var!!! = run_logic(!!!logic_name!!!, 0);
END
      } else {
        $template .= <<'END';
          my !!!output_var!!! = run_logic(!!!logic_name!!!, !!!wiring_in!!!);
END
      }
    }
    $template =~ s{!!!output_var!!!}{$output_var}g;
    $template =~ s{!!!logic_name!!!}{'"' . quotemeta($logic_name) . '"'}ge;
    $template =~ s{!!!wiring_in!!!}{$wiring_vars{$logic_name . ".in"}}ge;
    $template .= gen_apply_nets($logic_name . ".out", $output_var);
    $code .= $template;
  }
  return $code;
}

sub gen_set_latches {
  my $code;
  for my $name (sort keys %registers) {
    my $template = <<'END';
      $latches{!!!name!!!} = !!!wiring_in!!! & !!!mask!!!;
END
    $template =~ s{!!!name!!!}{'"' . quotemeta($name) . '"'}ge;
    $template =~ s{!!!wiring_in!!!}{$wiring_vars{$name . ".in"}}ge;
    $template =~ s{!!!mask!!!}{sprintf "0x%08x", $registers{$name}{mask}}ge;
    $code .= $template;
  }
  return $code;
}

# TODO proper template function
# does Sub::Quote have something?
# even want something that abstracts $latches{!!!main!!!} so I can flip these to lexicals?
# maybe using String::Tagged to mark holes and their parts-of-speech

our $simulator;
sub generate_simulator {
  reset_generator();

  my %blocks = (
    setup_wiring => \&gen_setup_wiring,
    latches_to_wires => \&gen_latches_to_wires,
    run_wiring => \&gen_run_wiring,
    set_latches => \&gen_set_latches,
  );
  my $template = <<'END';
    sub {
      use integer;

      # setup wiring for this simulation step
      !!!setup_wiring!!!

      # copy latch values into latch.out signals
      !!!latches_to_wires!!!

      # use topo order to evaluate logic blocks
      !!!run_wiring!!!

      # copy latch.in signals into latch state
      $t++;
      !!!set_latches!!!

      return;
    }
END

  # XXX magic to fix up indentation to copy indent of first line
  $template =~ s{!!!([^!]+)!!!}{($blocks{$1} // sub { die "gen $1" })->()}ge;

  timed_log "Template for simulator";
  print $template;

  local $@;
  my $rv = eval $template;
  if (not $rv) {
    die $@;
  }
  $simulator = $rv;

  timed_log "Dump of simulator";
  B::Concise::compile("-exec", $simulator)->();

  timed_log "Deparse of simulator";
  print B::Deparse->new->coderef2text($simulator), "\n";

  return;
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

timed_log "Defined nets";

tsort_logics();
timed_log "Logics sorted as: ", join(", ", @logics_order);

generate_luts();
timed_log "Generated LUTs";

generate_simulator();
timed_log "Generated simulator";

# The simulation procedes as follows
# Time 0 is the initial "random" state of the gates
# Then we loop over the ticks and the tick is numbered on the gates being set at the end.
#   Wiring from latch outputs is evaluated
#   Latches are set - defining their values for time 't'
# As wiring is evaluated, we accumulate an estimate of propagation time
# Memory for write works as a latch, but for reading works like logic

# TODO: simulate pull up on MAR._reset
# TODO: generate Perl or C code (with use integer)
# TODO: pullups

timed_log "Start of run\n";

# setup latch state vector
setup_state();

# hand tune some latches
reset_latch("P", 1);
reset_latch("Q", 1);
reset_latch("R", 1);

dump_state();

my $MAX_T = 250_000;
my $DUMP_INTERVAL = 1000;

my $st_loop = time;
my $not_okay = 0;
die unless $t == 0;
while ($t < $MAX_T) {
  local $max_tpd = 0;
  
  $simulator->();
  
  dump_state() if ($t % $DUMP_INTERVAL) == 0;

  # checking
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
}

if ($not_okay > 0) {
  timed_log ">>> RUN WAS NOT OKAY <<<\n";
}

dump_state();

our %scale_suffices = (
  -15 => "femto", -12 => "pico", -9 => "nano", -6 => "micro", -3 => "milli",
  3 => "kilo", 6 => "Mega", 9 => "Giga", 12 => "Tera", 15 => "Peta" );
sub num2human {
  my ($num) = @_;
  my ($mantissa, $exponent) = split /e/, sprintf "%.5e", $num;

  my $rem = $exponent % 3;
#  print "mantissa=$mantissa exponent=$exponent rem=$rem\n"; # DEBUG
  $exponent -= $rem;
  $mantissa *= 10**$rem;
  return $mantissa if $exponent == 0;
  my $suffix = substr $scale_suffices{$exponent} // "X", 0, 1;
  return substr($mantissa, 0, 5) . " " . $suffix . sprintf(" (%.3e)", $num);
}

my $et = time;
timed_log sprintf "loop  t=%s  elapsed=%s %s ticks/second\n", $t, num2human($et - $st_loop), num2human($t / ($et - $st_loop));
timed_log sprintf "total t=%s  elapsed=%s %s ticks/second\n", $t, num2human($et - $ST), num2human($t / ($et - $ST));

timed_log "End of run\n";
