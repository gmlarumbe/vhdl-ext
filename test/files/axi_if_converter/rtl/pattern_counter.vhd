-------------------------------------------------------------------------------
-- Title      : Pattern Counters
-- Project    :
-------------------------------------------------------------------------------
-- File       : pattern_counter.vhd
-- Author     : Gonzalo Martinez Larumbe  <gonzalomlarumbe@gmail.com>
-- Company    :
-- Created    : 2020-02-12
-- Last update: 2020-02-12
-- Platform   : Debian 9.1
-- Standard   : VHDL'08
-------------------------------------------------------------------------------
-- Description:
-------------------------------------------------------------------------------
-- Copyright (c) 2020
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author  Description
-- 2020-02-12  1.0      larumbe Created
-------------------------------------------------------------------------------

library IEEE;
library xil_defaultlib;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;

entity pattern_counter is
    generic (
        DATA_WIDTH : integer                                 := 32;
        PATTERN    : std_logic_vector(DATA_WIDTH-1 downto 0) := (others => '0')
        );
    port (
        axis_clk    : in std_logic;
        axis_resetn : in std_logic;
        soft_reset  : in std_logic;

        axis_tvalid : in std_logic;
        axis_tdata  : in std_logic_vector(DATA_WIDTH-1 downto 0);
        axis_tready : in std_logic;

        count         : out unsigned(31 downto 0);
        pattern_count : out unsigned(31 downto 0)
        );

end entity pattern_counter;


architecture RTL of pattern_counter is
    signal count_i         : unsigned(31 downto 0);
    signal pattern_count_i : unsigned(31 downto 0);

begin

    count         <= count_i;
    pattern_count <= pattern_count_i;

    pattern_count_proc : process(axis_clk) is
    begin
        if(rising_edge(axis_clk)) then
            if(axis_resetn = '0' or soft_reset = '1') then
                count_i         <= (others => '0');
                pattern_count_i <= (others => '0');
            elsif (axis_tvalid = '1' and axis_tready = '1') then
                count_i <= count_i + '1';
                if (axis_tdata = PATTERN) then
                    pattern_count_i <= pattern_count_i + '1';
                end if;
            end if;
        end if;
    end process pattern_count_proc;


end architecture RTL;
