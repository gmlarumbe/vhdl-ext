-------------------------------------------------------------------------------
-- Title      : Misc Tests
-- Project    : vhdl-ext
-------------------------------------------------------------------------------
-- File       : misc.vhd
-- Author     : Gonzalo Larumbe  <gonzalomlarumbe@gmail.com>
-- Company    : gmlarumbe
-- Created    : 2023-08-30
-- Last update: 2023-08-30
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
