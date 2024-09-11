-------------------------------------------------------------------------------
-- Title      : Core FSM Testbench
-- Project    :
-------------------------------------------------------------------------------
-- File       : tb_core_fsm.vhd
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
use xil_defaultlib.global.all;
use xil_defaultlib.global_sim.all;

entity tb_core_fsm is
end tb_core_fsm;

architecture TB of tb_core_fsm is

    signal clk        : std_logic := '0';
    signal resetn     : std_logic := '0';
    signal clk_fs     : std_logic := '0';
    signal soft_reset : std_logic := '0';

    signal system_enable  : std_logic := '0';
    signal system_running : std_logic := '0';

    signal conv_op          : std_logic             := '0';
    signal conv_req         : conversion_req_t;
    signal conv_rsp         : conversion_rsp_t;
    signal internal_error   : std_logic             := '0';
    signal pattern_req      : std_logic             := '0';
    signal pattern_len      : unsigned(9 downto 0);
    signal pattern_finished : std_logic             := '0';
    signal pattern_tlast    : std_logic             := '0';
    signal buffer_size      : unsigned(10 downto 0) := (others => '0');
    signal bram_ptr         : std_logic_vector(31 downto 0);
    signal read_size        : unsigned(15 downto 0) := (others => '0');

    signal stop_clock : std_logic := '0';


begin

    -- Clock generation
    clk <= (not clk and not stop_clock) after AXI_CLK_T/2;


    DUT : entity xil_defaultlib.core_fsm
        port map (
            clk    => clk,
            resetn => resetn,
            clk_fs => clk_fs,

            soft_reset     => soft_reset,
            system_enable  => system_enable,
            system_running => system_running,

            conv_op        => conv_op,
            conv_req       => conv_req,
            conv_rsp       => conv_rsp,
            internal_error => internal_error,

            pattern_req      => pattern_req,
            pattern_len      => pattern_len,
            pattern_finished => pattern_finished,
            pattern_tlast    => pattern_tlast,

            buffer_size => buffer_size,
            bram_ptr    => bram_ptr,
            read_size   => read_size
            );


    main : process

        procedure init_values is
        begin
            conv_rsp.s2mm_done <= '0';
            conv_rsp.mm2s_done <= '0';
        end procedure init_values;


        procedure reset_system is
        begin
            wait for (10*AXI_CLK_T);
            resetn <= '1';
            wait until rising_edge(clk);
            wait for (10*AXI_CLK_T);
        end procedure reset_system;


        procedure restart_system (constant OP : conversion_op) is
        begin
            wait until rising_edge(clk);
            system_enable <= '0';
            wait for (5*AXI_CLK_T);
            if (OP = S2MM) then
                conv_op <= '0';
            else
                conv_op <= '1';
            end if;
            wait for (5*AXI_CLK_T);
            system_enable <= '1';
            wait until rising_edge(clk);
        end procedure restart_system;


        procedure s2mm_rx (
            signal buffer_size : inout unsigned(10 downto 0);
            constant SIZE      : in    natural
            ) is
        begin
            for i in 1 to SIZE loop
                buffer_size <= buffer_size + 1;
                wait for (AXI_CLK_T);
            end loop;
        end procedure s2mm_rx;


        procedure s2mm_consume (
            signal buffer_size : inout unsigned(10 downto 0);
            signal conv_rsp    : out   conversion_rsp_t;
            constant SIZE      : in    natural
            ) is
            constant BURST_LEN     : natural := 32;
            variable NUM_TRANSFERS : natural := 0;
        begin
            NUM_TRANSFERS := integer(ceil(real(SIZE)/real(BURST_LEN)));

            for i in 1 to NUM_TRANSFERS loop
                for j in 1 to BURST_LEN loop
                    buffer_size <= buffer_size - 1;
                    wait for (AXI_CLK_T);
                end loop;
                conv_rsp.s2mm_done <= '1';
                wait until rising_edge(clk);
                conv_rsp.s2mm_done <= '0';
            end loop;

        end procedure s2mm_consume;


        procedure s2mm (
            signal conv_rsp : out conversion_rsp_t;
            constant SIZE   : in  natural
            ) is
        begin
            wait for (5*AXI_CLK_T);
            s2mm_rx(buffer_size, SIZE);
            wait for (20*AXI_CLK_T);
            s2mm_consume(buffer_size, conv_rsp, SIZE);
            wait for (5*AXI_CLK_T);
        end procedure s2mm;


        procedure mm2s (
            signal conv_rsp  : out conversion_rsp_t;
            signal read_size : out unsigned(15 downto 0);
            constant SIZE    : in  natural
            ) is
        begin
            read_size          <= to_unsigned(SIZE, 16);
            -- Wait for response
            wait for (50*AXI_CLK_T);
            -- Send response
            conv_rsp.mm2s_done <= '1';
            wait until rising_edge(clk);
            conv_rsp.mm2s_done <= '0';
            wait for (5*AXI_CLK_T);
        end procedure mm2s;


    begin

        -- Init test
        init_values;
        reset_system;
        -- S2MM
        restart_system(S2MM);
        s2mm(conv_rsp, 32);
        s2mm(conv_rsp, 128);
        -- MM2S
        restart_system(MM2S);
        mm2s(conv_rsp, read_size, 32);
        mm2s(conv_rsp, read_size, 128);
        -- End of tets
        wait for (50*AXI_CLK_T);
        end_test_and_stop_clock(stop_clock);

    end process;


end TB;
