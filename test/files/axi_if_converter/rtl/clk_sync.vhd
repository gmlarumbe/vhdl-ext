-------------------------------------------------------------------------------
-- Title      : Clock Synchronizer
-- Project    :
-------------------------------------------------------------------------------
-- File       : clk_sync.vhd
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


entity clk_sync is
    port (
        clk         : in  std_logic;
        resetn      : in  std_logic;
        soft_reset  : in  std_logic;
        clk_fs      : in  std_logic;
        clk_fs_sync : out std_logic
        );
end clk_sync;


architecture RTL of clk_sync is

    signal clk_fs_ff : std_logic;

begin

    clk_fs_sync_proc : process (clk) is
    begin
        if (rising_edge(clk)) then
            if (resetn = '0' or soft_reset = '1') then
                clk_fs_sync <= '0';
                clk_fs_ff   <= '0';
            else
                clk_fs_ff   <= clk_fs;
                clk_fs_sync <= clk_fs_ff;
            end if;
        end if;
    end process clk_fs_sync_proc;


end architecture RTL;
