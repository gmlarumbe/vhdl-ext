-------------------------------------------------------------------------------
-- Title      : Clock Divider
-- Project    :
-------------------------------------------------------------------------------
-- File       : clk_div.vhd
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

library xil_defaultlib;
library IEEE;
use IEEE.STD_LOGIC_1164.all;
use IEEE.numeric_std.all;

entity clk_div is
    generic (
        DIV_FACTOR : integer := 8
        );
    port (
        clk        : in  std_logic;
        resetn     : in  std_logic;
        soft_reset : in  std_logic;
        clk_out    : out std_logic
        );
end entity clk_div;


architecture RTL of clk_div is
    signal cnt     : integer   := 0;
    signal clk_div : std_logic := '1';

begin

    div_proc : process(clk)
    begin
        if (rising_edge(clk)) then
            if (resetn = '0' or soft_reset = '1') then
                cnt     <= 0;
                clk_div <= '1';
            else
                cnt <= cnt+1;
                if (cnt = (DIV_FACTOR/2)-1) then
                    clk_div <= not clk_div;
                    cnt     <= 0;
                end if;
            end if;
        end if;
    end process;


    BUFG_inst : entity xil_defaultlib.BUFG
        port map (
            O => clk_out,
            I => clk_div
            );

end RTL;
