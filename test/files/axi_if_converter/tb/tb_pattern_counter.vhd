-------------------------------------------------------------------------------
-- Title      : Pattern Counters Testbench
-- Project    :
-------------------------------------------------------------------------------
-- File       : tb_pattern_counter.vhd
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
use IEEE.math_real.all;
use xil_defaultlib.global_sim.all;


entity tb_pattern_counter is
end entity tb_pattern_counter;


architecture TB of tb_pattern_counter is

    constant DATA_ITERATIONS : integer := 32;
    constant SEED1_VALUE     : integer := 1;
    constant SEED2_VALUE     : integer := 999;

    constant DATA_WIDTH : integer                                 := 32;
    constant PATTERN    : std_logic_vector(DATA_WIDTH-1 downto 0) := (others => '0');

    signal axis_clk      : std_logic                               := '1';
    signal axis_resetn   : std_logic                               := '1';
    signal soft_reset    : std_logic                               := '0';
    signal axis_tvalid   : std_logic                               := '0';
    signal axis_tdata    : std_logic_vector(DATA_WIDTH-1 downto 0) := (others => '0');
    signal axis_tready   : std_logic                               := '0';
    signal count         : unsigned(31 downto 0);
    signal pattern_count : unsigned(DATA_WIDTH-1 downto 0);

    signal stop_clock : std_logic := '0';

begin

    -- Clock Gen
    axis_clk <= (not axis_clk and not stop_clock) after AXI_CLK_T / 2;

    -- Instance
    DUT : entity xil_defaultlib.pattern_counter
        generic map(
            DATA_WIDTH => DATA_WIDTH,
            PATTERN    => PATTERN
            )
        port map (
            axis_clk    => axis_clk,
            axis_resetn => axis_resetn,
            soft_reset  => soft_reset,

            axis_tvalid => axis_tvalid,
            axis_tdata  => axis_tdata,
            axis_tready => axis_tready,

            count         => count,
            pattern_count => pattern_count
            );


    main : process
        variable seed1 : integer := SEED1_VALUE;
        variable seed2 : integer := SEED2_VALUE;

        impure function rand_slv(len : integer) return std_logic_vector is
            variable r   : real;
            variable slv : std_logic_vector(len - 1 downto 0);
        begin
            for i in slv'range loop
                uniform(seed1, seed2, r);
                slv(i) := '1' when r > 0.5 else '0';
            end loop;
            return slv;
        end function;

        procedure random_data is
        begin
            axis_tvalid <= '1';
            for i in 1 to DATA_ITERATIONS loop
                axis_tdata <= rand_slv(DATA_WIDTH);
                wait for (AXI_CLK_T);
            end loop;
            axis_tvalid <= '0';
        end procedure random_data;

        procedure pattern_data is
        begin
            axis_tvalid <= '1';
            axis_tdata  <= PATTERN;
            for i in 1 to DATA_ITERATIONS loop
                wait for (AXI_CLK_T);
            end loop;
            axis_tvalid <= '0';
        end procedure pattern_data;

    begin
        wait for(AXI_CLK_T);
        axis_tready <= '1';
        wait for (3*AXI_CLK_T);
        axis_resetn <= '0';
        wait for (3*AXI_CLK_T);
        axis_resetn <= '1';
        wait for (3*AXI_CLK_T);

        random_data;
        wait for (3*AXI_CLK_T);
        pattern_data;
        wait for (3*AXI_CLK_T);

        end_test_and_stop_clock(stop_clock);

    end process main;


end architecture TB;
