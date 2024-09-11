-------------------------------------------------------------------------------
-- Title      : Instances
-- Project    : AXI Interface Converter
-------------------------------------------------------------------------------
-- File       : instances.vhd
-- Author     : Gonzalo Larumbe  <gonzalomlarumbe@gmail.com>
-- Company    : gmlarumbe
-- Created    : 2023-01-28
-- Last update: 2023-12-21
-- Platform   : Debian
-- Standard   : VHDL'08
-------------------------------------------------------------------------------
-- Description:
-------------------------------------------------------------------------------
-- Copyright (c) 2023 Ericsson
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author  Description
-- 2023-01-28  1.0      Gonzalo Created
-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity instances is
end entity instances;

architecture RTL of instances is

    component block0 is
        generic (Generic0 : integer := 0;
                 Generic1 : integer := 0;
                 Generic2 : integer := 0);
        port (Port0 : in std_logic;
              Port1 : in std_logic;
              Port2 : in std_logic);
    end component block0;

    signal Port0 : std_logic;
    signal Port1 : std_logic;
    signal Port2 : std_logic;

begin  -- architecture RTL

    -- Component instantiation
    I_BLOCK0_0 : block0
        port map (
            Port0 => Port0,
            Port1 => Port1,
            Port2 => Port2
            );

    I_BLOCK0_1 : block0
        port map (
            Port0 => Port0,
            Port1 => Port1,
            Port2 => Port2);

    I_BLOCK0_2 : block0 generic map (
        Generic0 => 0,
        Generic1 => 0,
        Generic2 => 0
        ) port map (
            Port0 => Port0,
            Port1 => Port1,
            Port2 => Port2);

    -- Entity instantiation
    I_BLOCK1_0 : entity work.block1
        port map (
            Port0 => Port0,
            Port1 => Port1,
            Port2 => Port2
            );

    I_BLOCK1_1 : entity work.block1
        port map (
            Port0 => Port0,
            Port1 => Port1,
            Port2 => Port2
            );

    I_BLOCK1_2 : entity work.block1 generic map (
        Generic0 => 0,
        Generic1 => 0,
        Generic2 => 0
        ) port map (
            Port0 => Port0,
            Port1 => Port1,
            Port2 => Port2);

    I_BLOCK1_3 : entity work.block1
    generic map (
        Generic0 => 0,
        Generic1 => 0,
        Generic2 => 0
    ) port map (
        Port0 => Port0,
        Port1 => Port1,
        Port2 => Port2
    );

    -- Generate
    GEN_BLOCK1 :
    for I in 0 to 3 generate
        I_BLOCK1_GEN : entity work.block1 port map
            (Port0, Port1, Port2);
    end generate GEN_BLOCK1;



end architecture RTL;
