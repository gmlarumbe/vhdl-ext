-------------------------------------------------------------------------------
-- Title      : Misc Tests
-- Project    : vhdl-ext
-------------------------------------------------------------------------------
-- File       : misc.vhd
-- Author     : Gonzalo Larumbe  <gonzalomlarumbe@gmail.com>
-- Company    : gmlarumbe
-- Created    : 2023-08-30
-- Last update: 2023-09-26
-- Platform   : Archvm
-- Standard   : VHDL'08
-------------------------------------------------------------------------------
-- Description:
-------------------------------------------------------------------------------
-- Copyright (c) 2023 gmlarumbe
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author  Description
-- 2023-08-30  1.0      gonz    Created
-------------------------------------------------------------------------------

------------------------------------------------------------
-- Entity instantiation without library prefix
------------------------------------------------------------
entity misc is
end entity misc;

architecture RTL of misc is

begin  -- architecture RTL

    I_BLOCK1_0 : entity block1
        port map (
            Port0 => Port0,
            Port1 => Port1,
            Port2 => Port2
            );

end architecture RTL;

------------------------------------------------------------
-- Ports with bit-select and slice
------------------------------------------------------------

architecture port_bit_select of misc is

begin  -- architecture port_bit_select

    I_BLOCK1_0 : entity block1
        port map (
            Port0(0) => Port0(0),
            Port1(1) => Port1(1),
            Port2(i) => Port2(i)
            );

    I_BLOCK1_1 : entity block1
        port map (
            Port0(3 downto 0) => Port0,
            Port1(5 downto 1) => Port1,
            Port2(0 to 5) => Port2
            );

end architecture port_bit_select;
