-------------------------------------------------------------------------------
-- Title      : Input Buffer Package
-- Project    :
-------------------------------------------------------------------------------
-- File       : input_buffer_pkg.vhd
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
use xil_defaultlib.global.C_M_AXI_BURST_LEN;

package input_buffer_types is

    type input_buffer_inputs_t is record
        start_burst_master_l : std_logic;
        bw_counter_l         : std_logic_vector(7 downto 0);
        wlast_l              : std_logic;
        write_done_l         : std_logic;
        short_burst_l        : std_logic;
        send_size_l          : unsigned(9 downto 0);

        start_burst_master_r : std_logic;
        bw_counter_r         : std_logic_vector(7 downto 0);
        wlast_r              : std_logic;
        write_done_r         : std_logic;
        short_burst_r        : std_logic;
        send_size_r          : unsigned(9 downto 0);
    end record;

    type input_buffer_outputs_t is record
        buffer_size_l : unsigned(10 downto 0);
        bram_ptr_l    : std_logic_vector(31 downto 0);
        buffer_size_r : unsigned(10 downto 0);
        bram_ptr_r    : std_logic_vector(31 downto 0);
    end record;

    type bram_read_pointer_t is record
        dina : std_logic_vector(63 downto 0);
        ena  : std_logic;
        wea  : std_logic_vector(0 downto 0);
        head : unsigned(10 downto 0);
        tail : unsigned(10 downto 0);
    end record;

    type output_reg is array (0 to C_M_AXI_BURST_LEN) of std_logic_vector(63 downto 0);
    constant OUTPUT_REG_DEFAULT_VALUE : std_logic_vector(63 downto 0) := x"DEAD_BEEF_BEBA_CAFE";

end package input_buffer_types;




library IEEE;
library xil_defaultlib;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;
use xil_defaultlib.input_buffer_types.all;

package input_buffer_logic is

    procedure init_bram_logic (signal bram_pointer : out bram_read_pointer_t);

    procedure stream_to_bram (
        signal s_axis_tvalid  : in    std_logic;
        signal s_axis_tdata   : in    std_logic_vector(63 downto 0);
        signal bram_pointer   : inout bram_read_pointer_t;
        signal overflow_error : out   std_logic
        );

end package input_buffer_logic;



package body input_buffer_logic is

    procedure init_bram_logic (signal bram_pointer : out bram_read_pointer_t) is
    begin
        bram_pointer.dina <= x"0000_0000_0000_0000";
        bram_pointer.wea  <= b"0";
        bram_pointer.ena  <= '0';
        bram_pointer.head <= to_unsigned(0, 11);
        bram_pointer.tail <= to_unsigned(0, 11);
    end init_bram_logic;


    procedure stream_to_bram (
        signal s_axis_tvalid  : in    std_logic;
        signal s_axis_tdata   : in    std_logic_vector(63 downto 0);
        signal bram_pointer   : inout bram_read_pointer_t;
        signal overflow_error : out   std_logic
        ) is
    begin
        if (s_axis_tvalid = '1') then
            if (bram_pointer.tail + 1) = bram_pointer.head then
                overflow_error <= '1';
            else
                bram_pointer.ena  <= '1';
                bram_pointer.wea  <= "1";
                bram_pointer.dina <= s_axis_tdata;
                bram_pointer.tail <= bram_pointer.tail + 1;
            end if;

        else
            bram_pointer.ena  <= '0';
            bram_pointer.wea  <= "0";
            bram_pointer.dina <= (others => '0');
        end if;

    end stream_to_bram;


end package body input_buffer_logic;
