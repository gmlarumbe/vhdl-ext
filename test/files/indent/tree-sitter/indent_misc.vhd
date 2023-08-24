--------------------------------------------------------------------------------
-- File header:
--------------------------------------------------------------------------------

-- 1) Testing comments at unit space:
library ieee, work;
use ieee.std_logic_1164.all;
-- use ieee.std_logic_1164.all;
-- use ieee.std_logic_1164.all;

entity foo is
end entity foo;



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

