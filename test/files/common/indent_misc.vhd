--------------------------------------------------------------------------------
-- File header:
--------------------------------------------------------------------------------

-- 1) Testing comments at unit space:
     -- before library comment
library ieee, work;
     -- after library comment
use ieee.std_logic_1164.all;
        -- use ieee.std_logic_1164.all;
   -- use ieee.std_logic_1164.all;

     -- before entity comment
entity foo is
end entity foo;
    -- after entity comment



-- 2) Testing case statements
architecture indent of indent_test is

begin

process (all)
begin
case var is
when A =>
out <= in;
when B =>
out <= in;
when C =>
out <= in;
when others =>
out <= in;
end case;
end process;

end architecture indent;


-- 3) Constant array initialization
package body foo is
constant ARRAY_CONSTANT : array_type :=
(
VALUE_0,
VALUE_1,
VALUE_2,
VALUE_3,
VALUE_4
);
end package body foo;


-- 4) Multiline procedural expression
package body foo is
procedure test_multiline_expression is
begin  -- procedure test_multiline_expression
signal_out <= signal_in1 and
signal_in2 and
signal_in3 and
signal_in4;
end procedure test_multiline_expression;
end package body foo;


-- 5) Block statement (https://peterfab.com/ref/vhdl/vhdl_renerta/mobile/source/vhd00012.htm)
---- Example 1
A1: OUT1 <= '1' after 5 ns;
LEVEL1 : block
begin
A2: OUT2 <= '1' after 5 ns;
A3: OUT3 <= '0' after 4 ns;
end block LEVEL1;
A1: OUT1 <= '1' after 5 ns;
A2: OUT2 <= '1' after 5 ns;
A3: OUT3 <= '0' after 4 ns;

---- Example 2
entity X_GATE is
generic (LongTime : Time; ShortTime : Time);
port (P1, P2, P3 : inout BIT);
end X_GATE;
architecture STRUCTURE of X_GATE is
-- global declarations of signal:
signal A, B : BIT;
begin
LEVEL1 : block
-- local declaration of generic parameters
generic (GB1, GB2 : Time);
-- local binding of generic parameters
generic map (GB1 => LongTime, GB2 => ShortTime);
-- local declaration of ports
port (PB1: in BIT; PB2 : inout BIT );
-- local binding of ports and signals
port map (PB1 => P1, PB2 => B);
-- local declarations:
constant Delay : Time := 1 ms;
signal S1 : BIT;
begin
S1 <= PB1 after Delay;
PB2 <= S1 after GB1, P1 after GB2;
end block LEVEL1;
end architecture STRUCTURE;

---- https://www.hdlworks.com/hdl_corner/vhdl_ref/VHDLContents/BlockStatement.htm
signal P, Q, R: std_logic;
-- ...
level1: block
port(A, B: in std_logic;
C: out std_logic);
port map(A => P, B => Q, C => R);
begin
C <= A and B;
end block level1;

---- Own example:
----   Copy of Example 2 and place block after the first concurrent
----   statement so that it is detected as a "block_statement"
entity block_test is
end block_test;
architecture arch of block_test is
-- global declarations of signal:
signal A, B : BIT;
begin
-- Extra statements wrt Example 2
S1 <= PB1 after Delay;
PB2 <= S1 after GB1, P1 after GB2;

LEVEL1 : block
-- local declaration of generic parameters
generic (GB1, GB2 : Time);
-- local binding of generic parameters
generic map (GB1 => LongTime, GB2 => ShortTime);
-- local declaration of ports
port (PB1: in BIT; PB2 : inout BIT );
-- local binding of ports and signals
port map (PB1 => P1, PB2 => B);
-- local declarations:
constant Delay : Time := 1 ms;
signal S1 : BIT;
begin
S1 <= PB1 after Delay;
PB2 <= S1 after GB1, P1 after GB2;
end block LEVEL1;
end architecture arch;


-- 6) Issue #6: Conditional signal assignment and selected signal assignment
-- (https://github.com/gmlarumbe/vhdl-ts-mode/issues/6)
library ieee;
use ieee.std_logic_1164.all;

entity traffic_light_fsm is
  port (
    clock, reset, button : in  std_logic;
    leds                 : out std_logic_vector(2 downto 0));  -- red, yellow, green
end entity traffic_light_fsm;

architecture rtl of traffic_light_fsm is
  type state_type is (RED, YELLOW, GREEN, RED_YELLOW);
  signal state_reg, state_next : state_type;
begin  -- architecture rtl

  REG : process (clock, reset) is
  begin  -- process REG
    if reset = '1' then                 -- asynchronous reset (active low)
      state_reg <= RED;
    elsif rising_edge(clock) then       -- rising clock edge
      state_reg <= state_next;
    end if;
  end process REG;

  NSL : state_next <= RED_YELLOW when state_reg = RED and button = '1' else
                      GREEN  when state_reg = RED_YELLOW else
                      YELLOW when state_reg = GREEN and button = '1' else
                      RED    when state_reg = YELLOW else
                      state_reg;

  OL : with state_reg select
    leds <=
    "100" when RED,
    "110" when RED_YELLOW,
    "001" when GREEN,
    "010" when YELLOW,
    "---" when others;

  -- INFO: Not present in #6, added for testing
  with state_reg select
    leds <= "100" when RED,
            "110" when RED_YELLOW,
            "001" when GREEN,
            "010" when YELLOW,
            "---" when others;

  -- INFO: Not present in #6, added for testing
  with state_reg select leds <= "100" when RED,
                                "110" when RED_YELLOW,
                                "001" when GREEN,
                                "010" when YELLOW,
                                "---" when others;

end architecture rtl;
