-------------------------------------------------------------------------------
-- Title      : BUFG Model for vhdl-ext
-- Project    :
-------------------------------------------------------------------------------
-- File       : axi_if_converter.vhd
-- Author     : Gonzalo Martinez Larumbe  <gonzalomlarumbe@gmail.com>
-- Company    :
-- Created    : 2020-02-12
-- Last update: 2023-09-06
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
library ieee;
use ieee.std_logic_1164.all;

entity BUFG is
    port (
        O : out std_logic;
        I : in  std_logic
        );
end BUFG;

architecture RTL of BUFG is
begin
    O <= I;
end RTL;

