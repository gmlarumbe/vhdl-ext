-------------------------------------------------------------------------------
-- Title      : Core FSM
-- Project    :
-------------------------------------------------------------------------------
-- File       : core_fsm.vhd
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
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;
use xil_defaultlib.global.all;

entity core_fsm is
    port (
        clk            : in  std_logic;
        resetn         : in  std_logic;
        clk_fs         : in  std_logic;
        soft_reset     : in  std_logic;
        system_enable  : in  std_logic;
        system_running : out std_logic;

        conv_op        : in  std_logic;
        conv_req       : out conversion_req_t;
        conv_rsp       : in  conversion_rsp_t;
        internal_error : in  std_logic;

        pattern_req      : out std_logic;
        pattern_len      : out unsigned(9 downto 0);
        pattern_finished : in  std_logic;
        pattern_tlast    : in  std_logic;

        buffer_size : in unsigned(10 downto 0);
        bram_ptr    : in std_logic_vector(31 downto 0);
        read_size   : in unsigned(15 downto 0)
        );

end core_fsm;

architecture RTL of core_fsm is

    type fsm_states is (
        IDLE,
        REQ_S2MM,
        WAIT_S2MM,
        REQ_MM2S,
        WAIT_MM2S
        );
    signal state : fsm_states;

    signal s2mm_write_ptr      : std_logic_vector(31 downto 0);
    signal mm2s_read_ptr       : std_logic_vector(31 downto 0);
    signal s2mm_write_req_size : unsigned(9 downto 0);
    signal mm2s_read_req_size  : unsigned(9 downto 0);

    constant S2MM_WRITE_SIZE : natural := 32;
    constant MM2S_READ_SIZE  : natural := 32;

begin


    fsm_axi_full : process (clk)
    begin
        if (rising_edge(clk)) then
            if (resetn = '0' or soft_reset = '1') then
                state          <= IDLE;
                system_running <= '0';

                conv_req            <= (request => '0', op_type => S2MM, size => (others => '0'), address => (others => '0'));
                s2mm_write_ptr      <= (others  => '0');
                mm2s_read_ptr       <= (others  => '0');
                s2mm_write_req_size <= (others  => '0');
                mm2s_read_req_size  <= (others  => '0');

            else
                -- Default outputs
                conv_req.request <= '0';
                conv_req.size    <= (others => '0');
                conv_req.address <= (others => '0');
                system_running   <= '1';

                -- FSM
                case state is
                    when IDLE =>
                        system_running <= '0';
                        if (system_enable) then
                            if (conv_op = '0' and buffer_size >= S2MM_WRITE_SIZE) then
                                state <= REQ_S2MM;
                            elsif (conv_op = '1' and mm2s_read_ptr(15 downto 0) < std_logic_vector(read_size)) then
                                state <= REQ_MM2S;
                            end if;
                        end if;


                    when REQ_S2MM =>
                        if (buffer_size >= S2MM_WRITE_SIZE) then
                            conv_req.op_type <= S2MM;
                            conv_req.request <= '1';
                            conv_req.size    <= to_unsigned(S2MM_WRITE_SIZE, 10);
                            conv_req.address <= s2mm_write_ptr;

                            s2mm_write_req_size <= to_unsigned(S2MM_WRITE_SIZE, 10);
                            state               <= WAIT_S2MM;
                        end if;


                    when WAIT_S2MM =>
                        if (conv_rsp.s2mm_done) then
                            s2mm_write_ptr <= std_logic_vector(unsigned(s2mm_write_ptr) + s2mm_write_req_size);
                            state          <= IDLE;
                        end if;


                    when REQ_MM2S =>
                        conv_req.op_type <= MM2S;
                        conv_req.request <= '1';
                        conv_req.size    <= read_size(9 downto 0);
                        conv_req.address <= mm2s_read_ptr;

                        mm2s_read_req_size <= read_size(9 downto 0);
                        state              <= WAIT_MM2S;


                    when WAIT_MM2S =>
                        if (conv_rsp.mm2s_done) then
                            mm2s_read_ptr <= std_logic_vector(unsigned(mm2s_read_ptr) + mm2s_read_req_size);
                            state         <= IDLE;
                        end if;

                end case;

                if (not system_enable) then
                    state <= IDLE;
                end if;

            end if;
        end if;
    end process fsm_axi_full;


end RTL;
