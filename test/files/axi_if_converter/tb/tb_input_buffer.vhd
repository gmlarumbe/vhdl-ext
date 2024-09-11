-------------------------------------------------------------------------------
-- Title      : Input Buffer Testbench
-- Project    :
-------------------------------------------------------------------------------
-- File       : tb_input_buffer.vhd
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
use xil_defaultlib.input_buffer_types.all;
use xil_defaultlib.global.all;
use xil_defaultlib.global_sim.all;


entity tb_input_buffer is
end tb_input_buffer;

architecture TB of tb_input_buffer is

    constant RX_DATA_ITERATIONS : integer := 32;

    type tb_step_t is (
        INIT,
        RX_DATA_L,
        RX_DATA_BOTH,
        FINISH
        );

    signal axi_clk    : std_logic := '1';
    signal resetn     : std_logic := '1';
    signal soft_reset : std_logic := '0';
    signal stop_clock : std_logic := '0';
    signal test_stage : tb_step_t := INIT;


    signal input_buffer_in : input_buffer_inputs_t := (
        start_burst_master_l => '0',
        bw_counter_l         => x"00",
        wlast_l              => '0',
        write_done_l         => '0',
        short_burst_l        => '0',
        send_size_l          => (others => '0'),

        start_burst_master_r => '0',
        bw_counter_r         => x"00",
        wlast_r              => '0',
        write_done_r         => '0',
        short_burst_r        => '0',
        send_size_r          => (others => '0')
        );

    signal input_buffer_out : input_buffer_outputs_t;

    -- RX Data
    signal s_axis_lch_aclk    : std_logic;
    signal s_axis_lch_aresetn : std_logic;
    signal s_axis_lch_tdata   : std_logic_vector(63 downto 0);
    signal s_axis_lch_tvalid  : std_logic;
    signal s_axis_lch_tkeep   : std_logic_vector(7 downto 0);
    signal s_axis_lch_tlast   : std_logic;
    signal s_axis_lch_tready  : std_logic;

    signal s_axis_rch_aclk    : std_logic;
    signal s_axis_rch_aresetn : std_logic;
    signal s_axis_rch_tdata   : std_logic_vector(63 downto 0);
    signal s_axis_rch_tvalid  : std_logic;
    signal s_axis_rch_tkeep   : std_logic_vector(7 downto 0);
    signal s_axis_rch_tlast   : std_logic;
    signal s_axis_rch_tready  : std_logic;

    -- Transmit to next stage
    signal m_axis_lch_aclk    : std_logic;
    signal m_axis_lch_aresetn : std_logic;
    signal m_axis_lch_tdata   : std_logic_vector(63 downto 0);
    signal m_axis_lch_tvalid  : std_logic;
    signal m_axis_lch_tkeep   : std_logic_vector(7 downto 0);
    signal m_axis_lch_tlast   : std_logic;
    signal m_axis_lch_tready  : std_logic;

    signal m_axis_rch_aclk    : std_logic;
    signal m_axis_rch_aresetn : std_logic;
    signal m_axis_rch_tdata   : std_logic_vector(63 downto 0);
    signal m_axis_rch_tvalid  : std_logic;
    signal m_axis_rch_tkeep   : std_logic_vector(7 downto 0);
    signal m_axis_rch_tlast   : std_logic;
    signal m_axis_rch_tready  : std_logic;

    -- Error signals
    signal bram_overflow_error       : std_logic;
    signal out_reg_underflow_error_l : std_logic;
    signal out_reg_overflow_error_l  : std_logic;
    signal out_reg_underflow_error_r : std_logic;
    signal out_reg_overflow_error_r  : std_logic;

    -- Auxiliary signals
    signal tb_aux_buffer_l : std_logic := '0';
    signal tb_aux_buffer_r : std_logic := '0';


begin

    -- Clock Generation
    axi_clk         <= (not axi_clk and not stop_clock) after AXI_CLK_T / 2;
    -- Clocks and signals
    s_axis_lch_aclk <= axi_clk;
    s_axis_rch_aclk <= axi_clk;
    m_axis_lch_aclk <= axi_clk;
    m_axis_rch_aclk <= axi_clk;

    s_axis_lch_aresetn <= resetn;
    s_axis_rch_aresetn <= resetn;
    m_axis_lch_aresetn <= resetn;
    m_axis_rch_aresetn <= resetn;

    DUT : entity xil_defaultlib.input_buffer
        generic map (
            LEFT_CH_BASE_ADDRESS  => LEFT_CH_ST_BASE_ADDRESS,
            RIGHT_CH_BASE_ADDRESS => RIGHT_CH_ST_BASE_ADDRESS,
            C_M_AXI_BURST_LEN     => C_M_AXI_BURST_LEN
            )
        port map (
            inputs     => input_buffer_in,
            outputs    => input_buffer_out,
            soft_reset => soft_reset,

            -- RX Data
            s_axis_lch_aclk    => s_axis_lch_aclk,
            s_axis_lch_aresetn => s_axis_lch_aresetn,
            s_axis_lch_tdata   => s_axis_lch_tdata,
            s_axis_lch_tvalid  => s_axis_lch_tvalid,
            s_axis_lch_tkeep   => s_axis_lch_tkeep,
            s_axis_lch_tlast   => s_axis_lch_tlast,
            s_axis_lch_tready  => s_axis_lch_tready,

            s_axis_rch_aclk    => s_axis_rch_aclk,
            s_axis_rch_aresetn => s_axis_rch_aresetn,
            s_axis_rch_tdata   => s_axis_rch_tdata,
            s_axis_rch_tvalid  => s_axis_rch_tvalid,
            s_axis_rch_tkeep   => s_axis_rch_tkeep,
            s_axis_rch_tlast   => s_axis_rch_tlast,
            s_axis_rch_tready  => s_axis_rch_tready,

            -- Transmit to next stage
            m_axis_lch_aclk    => m_axis_lch_aclk,
            m_axis_lch_aresetn => m_axis_lch_aresetn,
            m_axis_lch_tdata   => m_axis_lch_tdata,
            m_axis_lch_tvalid  => m_axis_lch_tvalid,
            m_axis_lch_tkeep   => m_axis_lch_tkeep,
            m_axis_lch_tlast   => m_axis_lch_tlast,
            m_axis_lch_tready  => m_axis_lch_tready,

            m_axis_rch_aclk    => m_axis_rch_aclk,
            m_axis_rch_aresetn => m_axis_rch_aresetn,
            m_axis_rch_tdata   => m_axis_rch_tdata,
            m_axis_rch_tvalid  => m_axis_rch_tvalid,
            m_axis_rch_tkeep   => m_axis_rch_tkeep,
            m_axis_rch_tlast   => m_axis_rch_tlast,
            m_axis_rch_tready  => m_axis_rch_tready,

            -- Error signals
            bram_overflow_error       => bram_overflow_error,
            out_reg_underflow_error_l => out_reg_underflow_error_l,
            out_reg_overflow_error_l  => out_reg_overflow_error_l,
            out_reg_underflow_error_r => out_reg_underflow_error_r,
            out_reg_overflow_error_r  => out_reg_overflow_error_r
            );

    ---------------
    --- Stimuli ---
    ---------------
    main : process
        procedure start_from_reset is
        begin
            wait for 3 * AXI_CLK_T;
            resetn <= '0';
            wait for 3 * AXI_CLK_T;
            resetn <= '1';
            wait for 3 * AXI_CLK_T;
        end start_from_reset;


        procedure rx_data_lch is
        begin
            for i in 0 to RX_DATA_ITERATIONS loop
                s_axis_lch_tvalid <= '1';
                s_axis_lch_tdata  <= std_logic_vector((x"0123_4567_89AB_CDEF") - to_unsigned(i, 64));
                wait for (AXI_CLK_T);
                s_axis_lch_tdata  <= std_logic_vector((x"FFFF_FFFF_FFFF_FFFF") + to_unsigned(i, 64));
                wait for (AXI_CLK_T);
                s_axis_lch_tdata  <= std_logic_vector((x"FEDC_BA98_7654_3210") - to_unsigned(i, 64));
                wait for (AXI_CLK_T);
                s_axis_lch_tvalid <= '0';
                wait for 15*AXI_CLK_T;
            end loop;
        end rx_data_lch;


    begin
        test_stage <= INIT;
        start_from_reset;

        test_stage <= RX_DATA_L;
        rx_data_lch;

        test_stage      <= RX_DATA_BOTH;
        wait for (AXI_CLK_T);
        tb_aux_buffer_l <= '1';
        tb_aux_buffer_r <= '1';
        wait for (AXI_CLK_T);
        tb_aux_buffer_l <= '0';
        tb_aux_buffer_r <= '0';
        wait for (AXI_CLK_T);

        wait for (300 * AXI_CLK_T);
        test_stage <= FINISH;
        end_test_and_stop_clock(stop_clock);
    end process;


    channel_l_ready_proc : process is
    begin
        if (tb_aux_buffer_l = '1') then
            s_axis_lch_tready                    <= '1';
            wait for (5 * AXI_CLK_T);
            input_buffer_in.start_burst_master_l <= '1';
            input_buffer_in.send_size_l          <= to_unsigned(32, 10);
            wait for (2 * AXI_CLK_T);
            input_buffer_in.start_burst_master_l <= '0';
            wait for (AXI_CLK_T);

            for i in 0 to C_M_AXI_BURST_LEN-1 loop
                input_buffer_in.bw_counter_l <= std_logic_vector(to_unsigned(i, 8));
                wait for (AXI_CLK_T);
            end loop;

            s_axis_lch_tready <= '0';
            wait for (100*AXI_CLK_T);

            s_axis_lch_tready                    <= '1';
            wait for (5 * AXI_CLK_T);
            input_buffer_in.start_burst_master_l <= '1';
            input_buffer_in.send_size_l          <= to_unsigned(7, 10);
            wait for (2 * AXI_CLK_T);
            input_buffer_in.start_burst_master_l <= '0';
            wait for (AXI_CLK_T);

            for i in 0 to C_M_AXI_BURST_LEN-1 loop
                input_buffer_in.bw_counter_l <= std_logic_vector(to_unsigned(i, 8));
                wait for (AXI_CLK_T);
            end loop;

            s_axis_lch_tready <= '0';

        end if;

        wait on tb_aux_buffer_l;
    end process channel_l_ready_proc;


    channel_r_ready_proc : process is
    begin
        if (tb_aux_buffer_r = '1') then
            s_axis_rch_tready                    <= '1';
            wait for (5 * AXI_CLK_T);
            input_buffer_in.start_burst_master_r <= '1';
            wait for (2 * AXI_CLK_T);
            input_buffer_in.start_burst_master_r <= '0';
            wait for (AXI_CLK_T);

            for i in 0 to C_M_AXI_BURST_LEN-1 loop
                input_buffer_in.bw_counter_r <= std_logic_vector(to_unsigned(i, 8));
                wait for (AXI_CLK_T);
            end loop;

            s_axis_rch_tready <= '0';
        end if;

        wait on tb_aux_buffer_r;
    end process channel_r_ready_proc;


    wlast_l_proc : process is
    begin
        if (input_buffer_in.bw_counter_l = x"1F") then
            input_buffer_in.wlast_l <= '1';
            wait for (2*AXI_CLK_T);
            input_buffer_in.wlast_l <= '0';
        end if;
        wait on input_buffer_in.bw_counter_l;
    end process wlast_l_proc;


    wlast_r_proc : process is
    begin
        if (input_buffer_in.bw_counter_r = x"1F") then
            input_buffer_in.wlast_r <= '1';
            wait for (AXI_CLK_T);
            input_buffer_in.wlast_r <= '0';
        end if;
        wait on input_buffer_in.bw_counter_r;
    end process wlast_r_proc;


end TB;
