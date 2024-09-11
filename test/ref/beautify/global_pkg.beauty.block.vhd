library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;

package global is

-----------
-- Types --
-----------
-- (Converters <-> FSM connections)
    type conversion_op is (S2MM, MM2S);

    type conversion_req_t is record
        op_type : conversion_op;
        request : std_logic;
        size    : unsigned(9 downto 0);
        address : std_logic_vector(31 downto 0);
    end record conversion_req_t;

type conversion_rsp_t is record
s2mm_done : std_logic;
mm2s_done : std_logic;
end record conversion_rsp_t;

---------------
-- CONSTANTS --
---------------
-- AXI Lite regs
constant C_S_AXI_DATA_WIDTH : integer := 32;
constant C_S_AXI_ADDR_WIDTH : integer := 7;

-- AXI If Converters
constant C_M_AXI_BURST_LEN    : integer := 32;
constant C_M_AXI_ID_WIDTH     : integer := 1;
constant C_M_AXI_ADDR_WIDTH   : integer := 32;
constant C_M_AXI_DATA_WIDTH   : integer := 64;
constant C_M_AXI_AWUSER_WIDTH : integer := 0;
constant C_M_AXI_ARUSER_WIDTH : integer := 0;
constant C_M_AXI_WUSER_WIDTH  : integer := 0;
constant C_M_AXI_RUSER_WIDTH  : integer := 0;
constant C_M_AXI_BUSER_WIDTH  : integer := 0;

-- AXI Lite master
constant C_M_MEM_AXI_TARGET_SLAVE_BASE_ADDR : std_logic_vector := x"0000_0000";
constant C_M_MEM_AXI_ADDR_WIDTH             : integer          := 32;
constant C_M_MEM_AXI_DATA_WIDTH             : integer          := 32;

-- Pattern counters
constant PATTERN_COUNTER_DATA_WIDTH : integer := 64;
constant PATTERN : std_logic_vector(PATTERN_COUNTER_DATA_WIDTH-1 downto 0) := (others => '0');

-- Clock divider
constant DIV_FACTOR : integer := 16;

-- Input buffer
constant LEFT_CH_ST_BASE_ADDRESS  : std_logic_vector(31 downto 0) := x"1000_0000";
constant RIGHT_CH_ST_BASE_ADDRESS : std_logic_vector(31 downto 0) := x"2000_0000";


end package global;
