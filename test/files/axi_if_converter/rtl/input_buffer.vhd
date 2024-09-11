-------------------------------------------------------------------------------
-- Title      : Input Buffer
-- Project    :
-------------------------------------------------------------------------------
-- File       : input_buffer.vhd
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
use xil_defaultlib.input_buffer_logic.all;
use xil_defaultlib.global.all;

entity input_buffer is
    generic (
        C_M_AXI_BURST_LEN     : integer                       := C_M_AXI_BURST_LEN;
        LEFT_CH_BASE_ADDRESS  : std_logic_vector(31 downto 0) := LEFT_CH_ST_BASE_ADDRESS;
        RIGHT_CH_BASE_ADDRESS : std_logic_vector(31 downto 0) := RIGHT_CH_ST_BASE_ADDRESS
        );
    port (
        inputs     : in  input_buffer_inputs_t;
        outputs    : out input_buffer_outputs_t;
        soft_reset : in  std_logic;

        -- RX Data
        s_axis_lch_aclk    : in  std_logic;
        s_axis_lch_aresetn : in  std_logic;
        s_axis_lch_tdata   : in  std_logic_vector(63 downto 0);
        s_axis_lch_tvalid  : in  std_logic;
        s_axis_lch_tkeep   : in  std_logic_vector(7 downto 0);
        s_axis_lch_tlast   : in  std_logic;
        s_axis_lch_tready  : out std_logic;

        s_axis_rch_aclk    : in  std_logic;
        s_axis_rch_aresetn : in  std_logic;
        s_axis_rch_tdata   : in  std_logic_vector(63 downto 0);
        s_axis_rch_tvalid  : in  std_logic;
        s_axis_rch_tkeep   : in  std_logic_vector(7 downto 0);
        s_axis_rch_tlast   : in  std_logic;
        s_axis_rch_tready  : out std_logic;

        -- Transmit to next stage
        m_axis_lch_aclk    : in  std_logic;
        m_axis_lch_aresetn : in  std_logic;
        m_axis_lch_tdata   : out std_logic_vector(63 downto 0);
        m_axis_lch_tvalid  : out std_logic;
        m_axis_lch_tkeep   : out std_logic_vector(7 downto 0);
        m_axis_lch_tlast   : out std_logic;
        m_axis_lch_tready  : in  std_logic;

        m_axis_rch_aclk    : in  std_logic;
        m_axis_rch_aresetn : in  std_logic;
        m_axis_rch_tdata   : out std_logic_vector(63 downto 0);
        m_axis_rch_tvalid  : out std_logic;
        m_axis_rch_tkeep   : out std_logic_vector(7 downto 0);
        m_axis_rch_tlast   : out std_logic;
        m_axis_rch_tready  : in  std_logic;

        -- Error signals
        bram_overflow_error       : out std_logic;
        out_reg_underflow_error_l : out std_logic;
        out_reg_overflow_error_l  : out std_logic;
        out_reg_underflow_error_r : out std_logic;
        out_reg_overflow_error_r  : out std_logic
        );
end input_buffer;



architecture RTL of input_buffer is

    component blk_mem_gen_0
        port (
            -- BRAM Write
            clka  : in  std_logic;
            ena   : in  std_logic;
            wea   : in  std_logic_vector(0 downto 0);
            addra : in  std_logic_vector(10 downto 0);
            dina  : in  std_logic_vector(63 downto 0);
            -- BRAM Read
            clkb  : in  std_logic;
            rstb  : in  std_logic;
            enb   : in  std_logic;
            addrb : in  std_logic_vector(10 downto 0);
            doutb : out std_logic_vector(63 downto 0)
            );
    end component;

    procedure bram_logic (
        signal s_axis_tvalid   : in    std_logic;
        signal s_axis_tdata    : in    std_logic_vector(63 downto 0);
        signal read_bram_enb   : in    std_logic;
        signal bram_pointer    : inout bram_read_pointer_t;
        signal overflow_error  : out   std_logic;
        signal address_write_d : out   std_logic_vector(10 downto 0)
        ) is
    begin
        if (read_bram_enb = '1') then
            bram_pointer.head <= bram_pointer.head + to_unsigned(1, 11);
        end if;

        stream_to_bram(s_axis_tvalid, s_axis_tdata, bram_pointer, overflow_error);
        address_write_d <= std_logic_vector(bram_pointer.tail);
    end procedure bram_logic;


    procedure bram_logic_rst (
        signal bram_pointer    : out bram_read_pointer_t;
        signal overflow_error  : out std_logic;
        signal address_write_d : out std_logic_vector(10 downto 0)
        ) is
    begin
        init_bram_logic(bram_pointer);
        overflow_error  <= '0';
        address_write_d <= (others => '0');
    end procedure bram_logic_rst;


    procedure read_to_output_reg_logic (
        signal start_burst_master    : in    std_logic;
        signal wlast                 : in    std_logic;
        signal read_size             : in    unsigned(9 downto 0);
        signal idx                   : inout unsigned(10 downto 0);
        signal idx_bram              : inout unsigned(10 downto 0);
        signal bram_to_buffer        : in    std_logic_vector(63 downto 0);
        signal read_bram_enb         : inout std_logic;
        signal last_word_out_reg     : inout std_logic;
        signal load_output_reg       : out   std_logic;
        signal output_reg_out_tvalid : out   std_logic
        ) is
    begin
        if (start_burst_master = '1') then
            read_bram_enb <= '1';
        end if;

        if (read_bram_enb = '1') then
            load_output_reg       <= '1';
            output_reg_out_tvalid <= '1';
            idx                   <= idx + 1;
            idx_bram              <= idx_bram + 1;

            if (idx = read_size-1) then
                read_bram_enb     <= '0';
                last_word_out_reg <= '1';
            end if;

        elsif (last_word_out_reg = '1') then
            idx               <= (others => '0');
            last_word_out_reg <= '0';
            load_output_reg   <= '0';

        elsif (wlast = '1') then
            output_reg_out_tvalid <= '0';
        end if;
    end procedure read_to_output_reg_logic;


    procedure read_to_output_reg_logic_rst (
        signal idx                   : out unsigned(10 downto 0);
        signal idx_bram              : out unsigned(10 downto 0);
        signal read_bram_enb         : out std_logic;
        signal output_reg_out_tvalid : out std_logic;
        signal load_output_reg       : out std_logic;
        signal last_word_out_reg     : out std_logic
        ) is
    begin
        idx                   <= (others => '0');
        idx_bram              <= (others => '0');
        output_reg_out_tvalid <= '0';
        read_bram_enb         <= '0';
        load_output_reg       <= '0';
        last_word_out_reg     <= '0';
    end procedure read_to_output_reg_logic_rst;


    procedure bram_pointer_position_calc (
        signal write_done            : in    std_logic;
        signal read_size             : in    unsigned(9 downto 0);
        signal bram_pointer_position : inout std_logic_vector(31 downto 0)
        ) is
    begin
        if (write_done = '1') then
            bram_pointer_position <= std_logic_vector(unsigned(bram_pointer_position) + (resize(read_size, 32) sll 3));
        end if;
    end procedure bram_pointer_position_calc;


    procedure bram_pointer_position_rst (
        constant CH_BASE_ADDRESS     : in  std_logic_vector(31 downto 0);
        signal bram_pointer_position : out std_logic_vector(31 downto 0)
        ) is
    begin
        bram_pointer_position <= CH_BASE_ADDRESS;
    end procedure bram_pointer_position_rst;


    procedure load_output_reg (
        constant OUTPUT_REG_DEFAULT_VALUE : in  std_logic_vector(63 downto 0);
        signal output_reg_out_tvalid      : in  std_logic;
        signal idx                        : in  unsigned(10 downto 0);
        signal bram_to_buffer             : in  std_logic_vector(63 downto 0);
        signal load_output_reg            : in  std_logic;
        signal output_reg                 : out output_reg;
        signal out_reg_underflow_error    : out std_logic;
        signal out_reg_overflow_error     : out std_logic
        ) is
    begin
        if (output_reg_out_tvalid = '1') then
            if (load_output_reg = '1') then
                if (idx <= 0) then
                    out_reg_underflow_error <= '1';
                elsif (idx > 32) then
                    out_reg_overflow_error <= '1';
                else
                    output_reg(to_integer(idx-1)) <= bram_to_buffer;
                end if;
            end if;
        else
            output_reg <= (others => OUTPUT_REG_DEFAULT_VALUE);
        end if;
    end procedure load_output_reg;


    procedure load_output_reg_rst (
        constant OUTPUT_REG_DEFAULT_VALUE : in  std_logic_vector(63 downto 0);
        signal output_reg                 : out output_reg;
        signal out_reg_underflow_error    : out std_logic;
        signal out_reg_overflow_error     : out std_logic
        ) is
    begin
        output_reg              <= (others => OUTPUT_REG_DEFAULT_VALUE);
        out_reg_underflow_error <= '0';
        out_reg_overflow_error  <= '0';
    end procedure load_output_reg_rst;


    signal reset_bram_l          : std_logic;
    signal reset_bram_r          : std_logic;
    signal bram_a_addrb          : std_logic_vector(10 downto 0);
    signal bram_b_addrb          : std_logic_vector(10 downto 0);
    signal read_bram_enb_l       : std_logic;
    signal read_bram_enb_r       : std_logic;
    signal bram_pointer_l        : bram_read_pointer_t;
    signal bram_pointer_r        : bram_read_pointer_t;
    signal bram_to_buffer_l      : std_logic_vector(63 downto 0);
    signal bram_to_buffer_r      : std_logic_vector(63 downto 0);
    signal bram_overflow_error_l : std_logic;
    signal bram_overflow_error_r : std_logic;

    signal output_reg_l              : output_reg;
    signal output_reg_r              : output_reg;
    signal output_reg_out_tvalid_l   : std_logic;
    signal output_reg_out_tvalid_l_d : std_logic;
    signal output_reg_out_tvalid_r   : std_logic;
    signal output_reg_out_tvalid_r_d : std_logic;
    signal last_word_out_reg_l       : std_logic;
    signal last_word_out_reg_r       : std_logic;
    signal load_output_reg_l         : std_logic;
    signal load_output_reg_r         : std_logic;

    signal bram_ptr_pos_l    : std_logic_vector(31 downto 0);
    signal bram_ptr_pos_r    : std_logic_vector(31 downto 0);
    signal address_write_d_l : std_logic_vector(10 downto 0);
    signal address_write_d_r : std_logic_vector(10 downto 0);

    signal idx_l      : unsigned(10 downto 0);
    signal idx_l_bram : unsigned(10 downto 0);
    signal idx_r      : unsigned(10 downto 0);
    signal idx_r_bram : unsigned(10 downto 0);

    attribute keep                      : string;
    attribute keep of output_reg_l      : signal is "true";
    attribute keep of output_reg_r      : signal is "true";
    attribute ram_style                 : string;
    attribute ram_style of output_reg_l : signal is "distributed";
    attribute ram_style of output_reg_r : signal is "distributed";

begin

    reset_bram_l <= not s_axis_lch_aresetn;
    reset_bram_r <= not s_axis_rch_aresetn;

    input_buffer_l : blk_mem_gen_0
        port map (
            clka  => s_axis_lch_aclk,
            ena   => bram_pointer_l.ena,
            wea   => bram_pointer_l.wea,
            addra => address_write_d_l,
            dina  => bram_pointer_l.dina,

            clkb  => s_axis_lch_aclk,
            rstb  => reset_bram_l,
            enb   => read_bram_enb_l,
            addrb => bram_a_addrb,
            doutb => bram_to_buffer_l
            );


    input_buffer_r : blk_mem_gen_0
        port map (
            clka  => s_axis_rch_aclk,
            ena   => bram_pointer_r.ena,
            wea   => bram_pointer_r.wea,
            addra => address_write_d_r,
            dina  => bram_pointer_r.dina,

            clkb  => s_axis_rch_aclk,
            rstb  => reset_bram_r,
            enb   => read_bram_enb_r,
            addrb => bram_b_addrb,
            doutb => bram_to_buffer_r
            );


    ----------------
    -- COMB LOGIC --
    ----------------
    outputs.buffer_size_l <= bram_pointer_l.tail - bram_pointer_l.head;
    outputs.bram_ptr_l    <= bram_ptr_pos_l;
    m_axis_lch_tdata      <= output_reg_l(to_integer(unsigned(inputs.bw_counter_l)));
    m_axis_lch_tvalid     <= output_reg_out_tvalid_l and output_reg_out_tvalid_l_d;

    outputs.buffer_size_r <= bram_pointer_r.tail - bram_pointer_r.head;
    outputs.bram_ptr_r    <= bram_ptr_pos_r;
    m_axis_rch_tdata      <= output_reg_r(to_integer(unsigned(inputs.bw_counter_r)));
    m_axis_rch_tvalid     <= output_reg_out_tvalid_r and output_reg_out_tvalid_r_d;

    -- BRAMs Address read/write management
    bram_a_addrb <= std_logic_vector(idx_l_bram);
    bram_b_addrb <= std_logic_vector(idx_r_bram);

    -- Overflow management
    bram_overflow_error <= bram_overflow_error_l or bram_overflow_error_r;


    --------------------
    -- Undriven Signals --
    --------------------
    s_axis_lch_tready <= '1';
    s_axis_rch_tready <= '1';
    m_axis_lch_tkeep  <= (others => '0');
    m_axis_lch_tlast  <= '0';
    m_axis_rch_tkeep  <= (others => '0');
    m_axis_rch_tlast  <= '0';

    ---------------
    -- SEQ LOGIC --
    ---------------
    axi_bram_logic_l : process(s_axis_lch_aclk)
    begin
        if (rising_edge(s_axis_lch_aclk)) then
            if (s_axis_lch_aresetn = '0' or soft_reset = '1') then
                bram_logic_rst(
                    bram_pointer    => bram_pointer_l,
                    overflow_error  => bram_overflow_error_l,
                    address_write_d => address_write_d_l
                    );
            else
                bram_logic(
                    s_axis_tvalid   => s_axis_lch_tvalid,
                    s_axis_tdata    => s_axis_lch_tdata,
                    read_bram_enb   => read_bram_enb_l,
                    bram_pointer    => bram_pointer_l,
                    overflow_error  => bram_overflow_error_l,
                    address_write_d => address_write_d_l
                    );
            end if;
        end if;
    end process axi_bram_logic_l;


    axi_bram_logic_r : process(s_axis_rch_aclk)
    begin
        if (rising_edge(s_axis_rch_aclk)) then
            if (s_axis_rch_aresetn = '0' or soft_reset = '1') then
                bram_logic_rst(
                    bram_pointer    => bram_pointer_r,
                    overflow_error  => bram_overflow_error_r,
                    address_write_d => address_write_d_r
                    );
            else
                bram_logic(
                    s_axis_tvalid   => s_axis_rch_tvalid,
                    s_axis_tdata    => s_axis_rch_tdata,
                    read_bram_enb   => read_bram_enb_r,
                    bram_pointer    => bram_pointer_r,
                    overflow_error  => bram_overflow_error_r,
                    address_write_d => address_write_d_r
                    );
            end if;
        end if;
    end process axi_bram_logic_r;


    read_to_output_reg_l : process (s_axis_lch_aclk) is
    begin
        if (rising_edge(s_axis_lch_aclk)) then
            if (s_axis_lch_aresetn = '0' or soft_reset = '1') then
                read_to_output_reg_logic_rst(
                    idx                   => idx_l,
                    idx_bram              => idx_l_bram,
                    read_bram_enb         => read_bram_enb_l,
                    output_reg_out_tvalid => output_reg_out_tvalid_l,
                    load_output_reg       => load_output_reg_l,
                    last_word_out_reg     => last_word_out_reg_l
                    );
            else
                read_to_output_reg_logic(
                    start_burst_master    => inputs.start_burst_master_l,
                    wlast                 => inputs.wlast_l,
                    read_size             => inputs.send_size_l,
                    idx                   => idx_l,
                    idx_bram              => idx_l_bram,
                    bram_to_buffer        => bram_to_buffer_l,
                    read_bram_enb         => read_bram_enb_l,
                    last_word_out_reg     => last_word_out_reg_l,
                    load_output_reg       => load_output_reg_l,
                    output_reg_out_tvalid => output_reg_out_tvalid_l
                    );
            end if;
        end if;
    end process read_to_output_reg_l;


    read_to_output_reg_r : process (s_axis_rch_aclk) is
    begin
        if (rising_edge(s_axis_rch_aclk)) then
            if (s_axis_rch_aresetn = '0' or soft_reset = '1') then
                read_to_output_reg_logic_rst(
                    idx                   => idx_r,
                    idx_bram              => idx_r_bram,
                    read_bram_enb         => read_bram_enb_r,
                    output_reg_out_tvalid => output_reg_out_tvalid_r,
                    load_output_reg       => load_output_reg_r,
                    last_word_out_reg     => last_word_out_reg_r
                    );
            else
                read_to_output_reg_logic(
                    start_burst_master    => inputs.start_burst_master_r,
                    wlast                 => inputs.wlast_r,
                    read_size             => inputs.send_size_r,
                    idx                   => idx_r,
                    idx_bram              => idx_r_bram,
                    bram_to_buffer        => bram_to_buffer_r,
                    read_bram_enb         => read_bram_enb_r,
                    last_word_out_reg     => last_word_out_reg_r,
                    load_output_reg       => load_output_reg_r,
                    output_reg_out_tvalid => output_reg_out_tvalid_r
                    );
            end if;
        end if;
    end process read_to_output_reg_r;


    bram_ptr_pos_l_proc : process (s_axis_lch_aclk) is
    begin
        if (rising_edge(s_axis_lch_aclk)) then
            if (s_axis_lch_aresetn = '0' or soft_reset = '1') then
                bram_pointer_position_rst(
                    CH_BASE_ADDRESS       => LEFT_CH_BASE_ADDRESS,
                    bram_pointer_position => bram_ptr_pos_l
                    );
            else
                bram_pointer_position_calc(
                    write_done            => inputs.write_done_l,
                    read_size             => inputs.send_size_l,
                    bram_pointer_position => bram_ptr_pos_l
                    );
            end if;
        end if;
    end process bram_ptr_pos_l_proc;


    bram_ptr_pos_r_proc : process (s_axis_rch_aclk) is
    begin
        if (rising_edge(s_axis_rch_aclk)) then
            if (s_axis_rch_aresetn = '0' or soft_reset = '1') then
                bram_pointer_position_rst(
                    CH_BASE_ADDRESS       => RIGHT_CH_BASE_ADDRESS,
                    bram_pointer_position => bram_ptr_pos_r
                    );
            else
                bram_pointer_position_calc(
                    write_done            => inputs.write_done_r,
                    read_size             => inputs.send_size_r,
                    bram_pointer_position => bram_ptr_pos_r
                    );
            end if;
        end if;
    end process bram_ptr_pos_r_proc;


    output_reg_loading_l : process (s_axis_lch_aclk) is
    begin
        if (rising_edge(s_axis_lch_aclk)) then
            if (s_axis_lch_aresetn = '0' or soft_reset = '1') then
                load_output_reg_rst(
                    OUTPUT_REG_DEFAULT_VALUE => OUTPUT_REG_DEFAULT_VALUE,
                    output_reg               => output_reg_l,
                    out_reg_underflow_error  => out_reg_underflow_error_l,
                    out_reg_overflow_error   => out_reg_overflow_error_l
                    );
            else
                load_output_reg(
                    OUTPUT_REG_DEFAULT_VALUE => OUTPUT_REG_DEFAULT_VALUE,
                    output_reg_out_tvalid    => output_reg_out_tvalid_l,
                    idx                      => idx_l,
                    bram_to_buffer           => bram_to_buffer_l,
                    load_output_reg          => load_output_reg_l,
                    output_reg               => output_reg_l,
                    out_reg_underflow_error  => out_reg_underflow_error_l,
                    out_reg_overflow_error   => out_reg_overflow_error_l
                    );
            end if;
        end if;
    end process output_reg_loading_l;



    output_reg_loading_r : process (s_axis_rch_aclk) is
    begin
        if (rising_edge(s_axis_rch_aclk)) then
            if (s_axis_rch_aresetn = '0' or soft_reset = '1') then
                load_output_reg_rst(
                    OUTPUT_REG_DEFAULT_VALUE => OUTPUT_REG_DEFAULT_VALUE,
                    output_reg               => output_reg_r,
                    out_reg_underflow_error  => out_reg_underflow_error_r,
                    out_reg_overflow_error   => out_reg_overflow_error_r
                    );
            else
                load_output_reg(
                    OUTPUT_REG_DEFAULT_VALUE => OUTPUT_REG_DEFAULT_VALUE,
                    output_reg_out_tvalid    => output_reg_out_tvalid_r,
                    idx                      => idx_r,
                    bram_to_buffer           => bram_to_buffer_r,
                    load_output_reg          => load_output_reg_r,
                    output_reg               => output_reg_r,
                    out_reg_underflow_error  => out_reg_underflow_error_r,
                    out_reg_overflow_error   => out_reg_overflow_error_r
                    );
            end if;
        end if;
    end process output_reg_loading_r;


    delay_output_reg_valid_l : process (s_axis_lch_aclk) is
    begin
        if (rising_edge(s_axis_lch_aclk)) then
            if (s_axis_lch_aresetn = '0' or soft_reset = '1') then
                output_reg_out_tvalid_l_d <= '0';
            else
                output_reg_out_tvalid_l_d <= output_reg_out_tvalid_l;
            end if;
        end if;
    end process delay_output_reg_valid_l;


    delay_output_reg_valid_r : process (s_axis_rch_aclk) is
    begin
        if (rising_edge(s_axis_rch_aclk)) then
            if (s_axis_rch_aresetn = '0' or soft_reset = '1') then
                output_reg_out_tvalid_r_d <= '0';
            else
                output_reg_out_tvalid_r_d <= output_reg_out_tvalid_r;
            end if;
        end if;
    end process delay_output_reg_valid_r;


end RTL;
