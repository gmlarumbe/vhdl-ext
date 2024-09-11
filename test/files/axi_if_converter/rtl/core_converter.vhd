-------------------------------------------------------------------------------
-- Title      : Core Converter
-- Project    :
-------------------------------------------------------------------------------
-- File       : core_converter.vhd
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

library ieee;
library xil_defaultlib;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use xil_defaultlib.global.all;

entity core_converter is
    generic (
        C_M_AXI_BURST_LEN    : integer := 256;
        C_M_AXI_ID_WIDTH     : integer := 1;
        C_M_AXI_ADDR_WIDTH   : integer := 32;
        C_M_AXI_DATA_WIDTH   : integer := 64;
        C_M_AXI_AWUSER_WIDTH : integer := 0;
        C_M_AXI_ARUSER_WIDTH : integer := 0;
        C_M_AXI_WUSER_WIDTH  : integer := 0;
        C_M_AXI_RUSER_WIDTH  : integer := 0;
        C_M_AXI_BUSER_WIDTH  : integer := 0
        );
    port (
        soft_reset : in  std_logic;
        conv_req   : in  conversion_req_t;
        conv_rsp   : out conversion_rsp_t;

        fb_wr_burst_start : out std_logic;
        fb_bw_counter     : out std_logic_vector(7 downto 0);
        fb_wlast          : out std_logic;
        fb_reduced_burst  : out std_logic;
        fb_awlen          : out std_logic_vector(7 downto 0);
        fb_burst_done     : out std_logic;

        pattern_req      : in  std_logic            := '0';
        pattern_len      : in  unsigned(9 downto 0) := (others => '0');
        pattern_finished : out std_logic;
        pattern_tlast    : in  std_logic            := '0';

        s_axis_aclk    : in  std_logic;
        s_axis_aresetn : in  std_logic;
        s_axis_tready  : out std_logic;
        s_axis_tdata   : in  std_logic_vector(63 downto 0) := (others => '0');
        s_axis_tvalid  : in  std_logic                     := '0';
        s_axis_tkeep   : in  std_logic_vector(7 downto 0)  := (others => '0');
        s_axis_tlast   : in  std_logic                     := '0';

        m_axis_aclk    : in  std_logic := '0';
        m_axis_aresetn : in  std_logic := '0';
        m_axis_tdata   : out std_logic_vector(63 downto 0);
        m_axis_tvalid  : out std_logic;
        m_axis_tkeep   : out std_logic_vector(7 downto 0);
        m_axis_tlast   : out std_logic;
        m_axis_tready  : in  std_logic := '0';
        m_axis_tdest   : out std_logic;

        m_axi_aclk    : in  std_logic;
        m_axi_aresetn : in  std_logic;
        m_axi_awid    : out std_logic_vector(C_M_AXI_ID_WIDTH-1 downto 0);
        m_axi_awaddr  : out std_logic_vector(C_M_AXI_ADDR_WIDTH-1 downto 0);
        m_axi_awlen   : out std_logic_vector(7 downto 0);
        m_axi_awsize  : out std_logic_vector(2 downto 0);
        m_axi_awburst : out std_logic_vector(1 downto 0);
        m_axi_awlock  : out std_logic;
        m_axi_awcache : out std_logic_vector(3 downto 0);
        m_axi_awprot  : out std_logic_vector(2 downto 0);
        m_axi_awqos   : out std_logic_vector(3 downto 0);
        m_axi_awuser  : out std_logic_vector(C_M_AXI_AWUSER_WIDTH-1 downto 0);
        m_axi_awvalid : out std_logic;
        m_axi_awready : in  std_logic;
        m_axi_wdata   : out std_logic_vector(C_M_AXI_DATA_WIDTH-1 downto 0);
        m_axi_wstrb   : out std_logic_vector(C_M_AXI_DATA_WIDTH/8-1 downto 0);
        m_axi_wlast   : out std_logic;
        m_axi_wuser   : out std_logic_vector(C_M_AXI_WUSER_WIDTH-1 downto 0);
        m_axi_wvalid  : out std_logic;
        m_axi_wready  : in  std_logic;
        m_axi_bid     : in  std_logic_vector(C_M_AXI_ID_WIDTH-1 downto 0);
        m_axi_bresp   : in  std_logic_vector(1 downto 0);
        m_axi_buser   : in  std_logic_vector(C_M_AXI_BUSER_WIDTH-1 downto 0);
        m_axi_bvalid  : in  std_logic;
        m_axi_bready  : out std_logic;
        m_axi_arid    : out std_logic_vector(C_M_AXI_ID_WIDTH-1 downto 0);
        m_axi_araddr  : out std_logic_vector(C_M_AXI_ADDR_WIDTH-1 downto 0);
        m_axi_arlen   : out std_logic_vector(7 downto 0);
        m_axi_arsize  : out std_logic_vector(2 downto 0);
        m_axi_arburst : out std_logic_vector(1 downto 0);
        m_axi_arlock  : out std_logic;
        m_axi_arcache : out std_logic_vector(3 downto 0);
        m_axi_arprot  : out std_logic_vector(2 downto 0);
        m_axi_arqos   : out std_logic_vector(3 downto 0);
        m_axi_aruser  : out std_logic_vector(C_M_AXI_ARUSER_WIDTH-1 downto 0);
        m_axi_arvalid : out std_logic;
        m_axi_arready : in  std_logic;
        m_axi_rid     : in  std_logic_vector(C_M_AXI_ID_WIDTH-1 downto 0);
        m_axi_rdata   : in  std_logic_vector(C_M_AXI_DATA_WIDTH-1 downto 0);
        m_axi_rresp   : in  std_logic_vector(1 downto 0);
        m_axi_rlast   : in  std_logic;
        m_axi_ruser   : in  std_logic_vector(C_M_AXI_RUSER_WIDTH-1 downto 0);
        m_axi_rvalid  : in  std_logic;
        m_axi_rready  : out std_logic;

        internal_error : out std_logic
        );

end core_converter;



architecture RTL of core_converter is

    function clogb2 (bit_depth : integer) return integer is
        variable depth : integer := bit_depth;
        variable count : integer := 1;
    begin
        for clogb2 in 1 to bit_depth loop
            if (bit_depth <= 2) then
                count := 1;
            else
                if (depth <= 1) then
                    count := count;
                else
                    depth := depth / 2;
                    count := count + 1;
                end if;
            end if;
        end loop;
        return (count);
    end;


    type fsm_state is (
        IDLE,
        WRITE_BURST_SIZE_CALC,
        WRITE_INITIATE,
        WRITING_TO_MEM,
        READ_BURST_SIZE_CALC,
        READ_INITIATE,
        READING_FROM_MEM
        );

    signal state : fsm_state;

    signal axi_awlen   : std_logic_vector(7 downto 0);
    signal axi_awvalid : std_logic;
    signal axi_wdata   : std_logic_vector(C_M_AXI_DATA_WIDTH-1 downto 0);
    signal axi_wlast   : std_logic;
    signal axi_wlast_i : std_logic;
    signal axi_wvalid  : std_logic;
    signal axi_bready  : std_logic;
    signal axi_arlen   : std_logic_vector(7 downto 0);
    signal axi_arvalid : std_logic;
    signal axi_rready  : std_logic;

    signal req_d     : std_logic;
    signal req_dd    : std_logic;
    signal req_pulse : std_logic;

    signal rd_burst_size_calc_start  : std_logic;
    signal rd_burst_size_calc_active : std_logic;
    signal rd_burst_size_calc_done   : std_logic;
    signal rd_burst_start            : std_logic;
    signal rd_burst_start_active     : std_logic;
    signal rd_burst_start_done       : std_logic;
    signal base_wr_addr              : std_logic_vector(C_M_AXI_ADDR_WIDTH-1 downto 0);
    signal burst_wr_addr             : std_logic_vector(C_M_AXI_ADDR_WIDTH-1 downto 0);
    signal burst_wr_addr_end         : std_logic_vector(C_M_AXI_ADDR_WIDTH-1 downto 0);
    signal transaction_wr_size       : std_logic_vector(9 downto 0);
    signal read_start                : std_logic;
    signal read_done                 : std_logic;
    signal burst_write_counter       : std_logic_vector(7 downto 0);
    signal transaction_wr_counter    : std_logic_vector(11 downto 0);
    signal wr_burst_size             : std_logic_vector(7 downto 0);
    signal wr_short_burst_4kb        : std_logic;
    signal strobe_burst              : std_logic;
    signal strobe_len                : std_logic_vector(7 downto 0);

    signal wr_burst_size_calc_start  : std_logic;
    signal wr_burst_size_calc_active : std_logic;
    signal wr_burst_size_calc_done   : std_logic;
    signal wr_burst_start            : std_logic;
    signal wr_burst_start_active     : std_logic;
    signal wr_burst_start_done       : std_logic;
    signal base_rd_addr              : std_logic_vector(C_M_AXI_ADDR_WIDTH-1 downto 0);
    signal burst_rd_addr             : std_logic_vector(C_M_AXI_ADDR_WIDTH-1 downto 0);
    signal burst_rd_addr_end         : std_logic_vector(C_M_AXI_ADDR_WIDTH-1 downto 0);
    signal transaction_rd_size       : std_logic_vector(9 downto 0);
    signal write_start               : std_logic;
    signal write_done                : std_logic;
    signal burst_read_counter        : std_logic_vector(7 downto 0);
    signal transaction_rd_counter    : std_logic_vector(11 downto 0);
    signal rd_burst_size             : std_logic_vector(7 downto 0);

    signal pattern_finished_i : std_logic;
    signal pattern_cnt        : unsigned(9 downto 0);


begin

    -- Write fixed signals
    m_axi_awid    <= (others => '0');
    m_axi_awburst <= "01";
    m_axi_awlock  <= '0';
    m_axi_awcache <= "0010";
    m_axi_awprot  <= "000";
    m_axi_awqos   <= x"0";
    m_axi_wuser   <= (others => '0');
    -- Read fixed signals
    m_axi_arid    <= (others => '0');
    m_axi_aruser  <= (others => '1');
    m_axi_arburst <= "01";
    m_axi_arlock  <= '0';
    m_axi_arcache <= "0010";
    m_axi_arprot  <= "000";
    m_axi_arqos   <= x"0";
    -- Read/write logic
    m_axi_awaddr  <= burst_wr_addr;
    m_axi_awlen   <= std_logic_vector(unsigned(axi_awlen) -1);
    m_axi_awsize  <= std_logic_vector(to_unsigned(clogb2((C_M_AXI_DATA_WIDTH/8)-1), 3));
    m_axi_awuser  <= (others => '1');
    m_axi_awvalid <= axi_awvalid;
    m_axi_wdata   <= axi_wdata;
    m_axi_wlast   <= axi_wlast;
    m_axi_wvalid  <= axi_wvalid;
    m_axi_bready  <= axi_bready;
    m_axi_araddr  <= burst_rd_addr;
    m_axi_arlen   <= std_logic_vector(unsigned(axi_arlen) -1);
    m_axi_arsize  <= std_logic_vector(to_unsigned(clogb2((C_M_AXI_DATA_WIDTH/8)-1), 3));
    m_axi_arvalid <= axi_arvalid;
    m_axi_rready  <= axi_rready;

    ----------------------------
    -- Master stream interface -
    ----------------------------
    m_axis_tdata <= (others => '0') when pattern_req = '1' else
                    m_axi_rdata when (axi_rready = '1' and m_axi_rvalid = '1' and (unsigned(transaction_rd_size)-1) >= unsigned(transaction_rd_counter)) else
                    (others => '0');

    m_axis_tvalid <= '1' when (pattern_req = '1' and pattern_finished_i = '0') else
                     m_axi_rvalid when (axi_rready = '1' and (unsigned(transaction_rd_size)-1) >= unsigned(transaction_rd_counter)) else
                     '0';

    m_axis_tlast <= '1' when (pattern_tlast = '1' and m_axi_rvalid = '1' and axi_rready = '1' and (unsigned(transaction_rd_size)-1) = unsigned(transaction_rd_counter)) else
                    '0';

    axi_rready <=
        m_axis_tready when (read_start = '1' and (unsigned(transaction_rd_size)-1) >= unsigned(transaction_rd_counter)) else
        '1'           when (read_start = '1' and (unsigned(burst_read_counter) <= C_M_AXI_BURST_LEN-1)) else
        '0';

    m_axis_tdest <= '0';
    m_axis_tkeep <= x"FF";


    ----------------------------
    -- Slave stream interface --
    ----------------------------
    axi_wdata <= s_axis_tdata when ((axi_wvalid = '1') and ((unsigned(transaction_wr_size)-1) >= unsigned(transaction_wr_counter))) else
                 (others => '0');

    axi_wvalid <=
        s_axis_tvalid when ((m_axi_wready = '1') and ((unsigned(transaction_wr_size)-1) >= unsigned(transaction_wr_counter))) else
        '1'           when ((write_start = '1') and (unsigned(burst_write_counter) <= C_M_AXI_BURST_LEN-1)) else
        '0';

    s_axis_tready <= m_axi_wready when ((m_axi_wready = '1') and ((unsigned(transaction_wr_size)-1) >= unsigned(transaction_wr_counter))) else
                     '0';


    -- Feedback to input buffer
    fb_wr_burst_start <= wr_burst_start;
    fb_bw_counter     <= burst_write_counter;
    fb_wlast          <= axi_wlast;
    fb_reduced_burst  <= (wr_short_burst_4kb or strobe_burst);
    fb_awlen          <= strobe_len when (strobe_burst = '1')     else axi_awlen;
    fb_burst_done     <= write_done when (state = WRITING_TO_MEM) else '0';

    -- Other signals
    pattern_finished <= pattern_finished_i;
    internal_error   <= (axi_rready and m_axi_rvalid and m_axi_rresp(1)) or
                      (axi_bready and m_axi_bvalid and m_axi_bresp(1));

    -- Internal signals comb logic
    axi_wlast <= (axi_wlast_i) and (m_axi_wready);
    req_pulse <= '1' when req_dd = '0' and req_d = '1' else '0';


    ---------
    -- FSM --
    ---------
    fsm_proc : process (m_axi_aclk) is
    begin
        if (rising_edge(m_axi_aclk)) then
            if (m_axi_aresetn = '0' or soft_reset = '1') then
                state                    <= IDLE;
                conv_rsp.s2mm_done       <= '0';
                conv_rsp.mm2s_done       <= '0';
                wr_burst_size_calc_start <= '0';
                rd_burst_size_calc_start <= '0';
                base_wr_addr             <= (others => '0');
                transaction_wr_size      <= (others => '0');
                transaction_rd_size      <= (others => '0');
                wr_burst_start           <= '0';
                write_start              <= '0';
                base_rd_addr             <= (others => '0');
                rd_burst_start           <= '0';
                read_start               <= '0';
            else
                case (state) is
                    when IDLE =>
                        write_start        <= '0';
                        wr_burst_start     <= '0';
                        conv_rsp.s2mm_done <= '0';
                        conv_rsp.mm2s_done <= '0';

                        if (req_pulse = '1') then
                            if (conv_req.op_type = S2MM) then
                                state               <= WRITE_BURST_SIZE_CALC;
                                base_wr_addr        <= conv_req.address;
                                transaction_wr_size <= std_logic_vector(conv_req.size);
                            else
                                state               <= READ_BURST_SIZE_CALC;
                                base_rd_addr        <= conv_req.address;
                                transaction_rd_size <= std_logic_vector(conv_req.size);
                            end if;
                        end if;


                    when WRITE_BURST_SIZE_CALC =>
                        if (wr_burst_size_calc_done = '0' and wr_burst_size_calc_active = '0' and wr_burst_size_calc_start = '0') then
                            wr_burst_size_calc_start <= '1';
                        else
                            wr_burst_size_calc_start <= '0';
                        end if;
                        if (wr_burst_size_calc_done = '1')then
                            state <= WRITE_INITIATE;
                        end if;


                    when WRITE_INITIATE =>
                        if (wr_burst_start_done = '0' and wr_burst_start_active = '0' and wr_burst_start = '0') then
                            wr_burst_start <= '1';
                        else
                            wr_burst_start <= '0';
                        end if;

                        if (wr_burst_start_done = '1') then
                            state <= WRITING_TO_MEM;
                        end if;


                    when WRITING_TO_MEM =>
                        if (write_done = '0') then
                            write_start <= '1';
                        else
                            write_start <= '0';
                        end if;

                        if (write_done = '1') then
                            if (unsigned(transaction_wr_counter) >= unsigned(transaction_wr_size)) then
                                conv_rsp.s2mm_done <= '1';
                                state              <= IDLE;
                            else
                                state              <= WRITE_BURST_SIZE_CALC;
                                conv_rsp.s2mm_done <= '0';
                            end if;
                        else
                            conv_rsp.s2mm_done <= '0';
                        end if;


                    when READ_BURST_SIZE_CALC =>
                        if (rd_burst_size_calc_done = '0' and rd_burst_size_calc_active = '0' and rd_burst_size_calc_start = '0') then
                            rd_burst_size_calc_start <= '1';
                        else
                            rd_burst_size_calc_start <= '0';
                        end if;

                        if (rd_burst_size_calc_done = '1')then
                            state <= READ_INITIATE;
                        end if;


                    when READ_INITIATE =>
                        if (rd_burst_start_done = '0' and rd_burst_start_active = '0' and rd_burst_start = '0') then
                            rd_burst_start <= '1';
                        else
                            rd_burst_start <= '0';
                        end if;

                        if (rd_burst_start_done = '1') then
                            state <= READING_FROM_MEM;
                        end if;


                    when READING_FROM_MEM =>
                        if (read_done = '0') then
                            read_start <= '1';
                        else
                            read_start <= '0';
                        end if;

                        if (read_done = '1') then
                            if (unsigned(transaction_rd_counter) >= unsigned(transaction_rd_size)) then
                                conv_rsp.mm2s_done <= '1';
                                state              <= IDLE;
                            else
                                state <= READ_BURST_SIZE_CALC;
                            end if;
                        else
                            conv_rsp.mm2s_done <= '0';
                        end if;

                end case;
            end if;
        end if;
    end process fsm_proc;


    read_burst_size_calc_proc : process (m_axi_aclk) is
    begin
        if (rising_edge(m_axi_aclk)) then
            if (m_axi_aresetn = '0' or soft_reset = '1') then
                rd_burst_size_calc_active <= '0';
                rd_burst_size_calc_done   <= '0';
                rd_burst_size             <= (others => '0');
            elsif (state /= READ_BURST_SIZE_CALC) then
                rd_burst_size_calc_active <= '0';
                rd_burst_size_calc_done   <= '0';
            else
                if (rd_burst_size_calc_start = '1') then
                    rd_burst_size_calc_active <= '1';
                    burst_rd_addr_end         <= std_logic_vector((C_M_AXI_DATA_WIDTH/8)*to_unsigned(C_M_AXI_BURST_LEN, 8) + unsigned(burst_rd_addr));
                    rd_burst_size             <= std_logic_vector(to_unsigned(C_M_AXI_BURST_LEN, 8));
                    rd_burst_size_calc_done   <= '0';
                elsif (rd_burst_size_calc_active = '1') then
                    if ((burst_rd_addr_end(12) /= burst_rd_addr(12)) and
                        (burst_rd_addr_end(11 downto 0) /= std_logic_vector(to_unsigned(0, 12)))) then
                        rd_burst_size           <= (rd_burst_size srl 1);
                        burst_rd_addr_end       <= std_logic_vector((C_M_AXI_DATA_WIDTH/8)*unsigned(rd_burst_size srl 1) + unsigned(burst_rd_addr));
                        rd_burst_size_calc_done <= '0';
                    else
                        rd_burst_size_calc_active <= '0';
                        rd_burst_size_calc_done   <= '1';
                    end if;
                else
                    rd_burst_size_calc_active <= '0';
                    rd_burst_size_calc_done   <= '0';
                end if;
            end if;
        end if;
    end process read_burst_size_calc_proc;



    araddr_proc : process (m_axi_aclk) is
    begin
        if (rising_edge(m_axi_aclk)) then
            if (m_axi_aresetn = '0' or soft_reset = '1') then
                burst_rd_addr <= (others => '0');
            elsif (state = IDLE and req_pulse = '1' and conv_req.op_type = MM2S) then
                burst_rd_addr <= conv_req.address;
            elsif (read_done = '1' and state = READING_FROM_MEM) then
                burst_rd_addr <= std_logic_vector(unsigned(burst_rd_addr) + unsigned(rd_burst_size)*(C_M_AXI_DATA_WIDTH/8));
            end if;
        end if;
    end process araddr_proc;


    arvalid_proc : process (m_axi_aclk) is
    begin
        if (rising_edge(m_axi_aclk)) then
            if (m_axi_aresetn = '0' or state /= READ_INITIATE or soft_reset = '1') then
                rd_burst_start_done   <= '0';
                rd_burst_start_active <= '0';
                axi_arvalid           <= '0';
            else
                if (rd_burst_start = '1') then
                    rd_burst_start_active <= '1';
                end if;
                if (rd_burst_start_active = '1') then
                    axi_arvalid <= '1';
                    if (m_axi_arready = '1') then
                        rd_burst_start_active <= '0';
                        rd_burst_start_done   <= '1';
                    end if;
                end if;
                if (rd_burst_start_done = '1') then
                    axi_arvalid <= '0';
                end if;
            end if;
        end if;
    end process arvalid_proc;


    arlen_proc : process (m_axi_aclk) is
    begin
        if (rising_edge(m_axi_aclk)) then
            if (m_axi_aresetn = '0' or soft_reset = '1') then
                axi_arlen <= (others => '0');
            elsif (rd_burst_start = '1') then
                axi_arlen <= rd_burst_size;
            end if;
        end if;
    end process arlen_proc;


    rdata_counter_proc : process (m_axi_aclk) is
    begin
        if (rising_edge(m_axi_aclk)) then
            if (m_axi_aresetn = '0' or state /= READING_FROM_MEM or soft_reset = '1') then
                burst_read_counter <= (others => '0');
            elsif (axi_rready = '1' and m_axi_rvalid = '1') then
                burst_read_counter <= std_logic_vector(unsigned(burst_read_counter) + 1);
            end if;
        end if;
    end process rdata_counter_proc;


    rdata_total_counter_proc : process (m_axi_aclk) is
    begin
        if (rising_edge(m_axi_aclk)) then
            if (m_axi_aresetn = '0' or soft_reset = '1') then
                transaction_rd_counter <= (others => '0');
            else
                if (m_axi_rvalid = '1' and axi_rready = '1') then
                    transaction_rd_counter <= std_logic_vector(unsigned(transaction_rd_counter) + 1);
                end if;

                if (state = IDLE) then
                    transaction_rd_counter <= (others => '0');
                end if;
            end if;
        end if;
    end process rdata_total_counter_proc;


    r_done_proc : process (m_axi_aclk) is
    begin
        if (rising_edge(m_axi_aclk)) then
            if (m_axi_aresetn = '0' or state /= READING_FROM_MEM or soft_reset = '1') then
                read_done <= '0';
            elsif (burst_read_counter = std_logic_vector(unsigned(rd_burst_size)-1) and m_axi_rvalid = '1' and axi_rready = '1') then
                read_done <= '1';
            end if;
        end if;
    end process r_done_proc;


    write_burst_size_calc_proc : process (m_axi_aclk) is
    begin
        if (rising_edge(m_axi_aclk)) then
            if (m_axi_aresetn = '0' or soft_reset = '1') then
                wr_burst_size_calc_active <= '0';
                wr_burst_size_calc_done   <= '0';
                wr_burst_size             <= (others => '0');
                wr_short_burst_4kb        <= '0';
            elsif (state = IDLE) then
                wr_short_burst_4kb <= '0';
            elsif (state /= WRITE_BURST_SIZE_CALC) then
                wr_burst_size_calc_active <= '0';
                wr_burst_size_calc_done   <= '0';
            else
                if (wr_burst_size_calc_start = '1') then
                    wr_burst_size_calc_active <= '1';
                    wr_burst_size_calc_done   <= '0';
                    burst_wr_addr_end         <= std_logic_vector((C_M_AXI_DATA_WIDTH/8)*to_unsigned(C_M_AXI_BURST_LEN, 8) + unsigned(burst_wr_addr));
                    wr_burst_size             <= std_logic_vector (to_unsigned(C_M_AXI_BURST_LEN, 8));
                    wr_short_burst_4kb        <= '0';
                elsif (wr_burst_size_calc_active = '1') then
                    if (burst_wr_addr_end(12) /= burst_wr_addr(12) and
                        (burst_wr_addr_end(11 downto 0) /= std_logic_vector(to_unsigned(0, 12)))) then
                        wr_short_burst_4kb      <= '1';
                        wr_burst_size           <= (wr_burst_size srl 1);
                        burst_wr_addr_end       <= std_logic_vector((C_M_AXI_DATA_WIDTH/8)*unsigned(wr_burst_size srl 1) + unsigned(burst_wr_addr));
                        wr_burst_size_calc_done <= '0';
                    else
                        wr_burst_size_calc_active <= '0';
                        wr_burst_size_calc_done   <= '1';
                    end if;
                end if;
            end if;
        end if;
    end process write_burst_size_calc_proc;


    awaddr_proc : process (m_axi_aclk) is
    begin
        if (rising_edge(m_axi_aclk)) then
            if (m_axi_aresetn = '0' or soft_reset = '1') then
                burst_wr_addr <= (others => '0');
            elsif (state = IDLE and req_pulse = '1' and conv_req.op_type = S2MM) then
                burst_wr_addr <= conv_req.address;
            elsif (write_done = '1' and state = WRITING_TO_MEM) then
                burst_wr_addr <= std_logic_vector(unsigned(burst_wr_addr) + unsigned(wr_burst_size)*(C_M_AXI_DATA_WIDTH/8));
            end if;
        end if;
    end process awaddr_proc;


    strobe_burst_proc : process (m_axi_aclk) is
    begin
        if (rising_edge(m_axi_aclk)) then
            if (m_axi_aresetn = '0' or soft_reset = '1') then
                strobe_burst <= '0';
                strobe_len   <= (others => '0');
            elsif (state = IDLE) then
                strobe_burst <= '0';
                strobe_len   <= (others => '0');
            elsif (state = WRITE_BURST_SIZE_CALC) then
                if (wr_burst_size_calc_start = '1') then
                    strobe_burst <= '0';
                    strobe_len   <= (others => '0');
                else
                    if ((unsigned(transaction_wr_counter) + unsigned(wr_burst_size)) > unsigned(transaction_wr_size)) then
                        strobe_burst <= '1';
                        strobe_len   <= std_logic_vector(resize((resize(unsigned(transaction_wr_size), 11)-unsigned(transaction_wr_counter)), 8));
                    else
                        strobe_burst <= '0';
                        strobe_len   <= (others => '0');
                    end if;
                end if;
            end if;
        end if;
    end process strobe_burst_proc;


    awlen_proc : process (m_axi_aclk) is
    begin
        if (rising_edge(m_axi_aclk)) then
            if (m_axi_aresetn = '0' or soft_reset = '1') then
                axi_awlen <= (others => '0');
            elsif (wr_burst_start = '1') then
                axi_awlen <= wr_burst_size;
            end if;
        end if;
    end process awlen_proc;


    awvalid_proc : process (m_axi_aclk) is
    begin
        if (rising_edge(m_axi_aclk)) then
            if (m_axi_aresetn = '0' or soft_reset = '1' or state /= WRITE_INITIATE) then
                wr_burst_start_done   <= '0';
                wr_burst_start_active <= '0';
                axi_awvalid           <= '0';
            else
                if (wr_burst_start = '1') then
                    wr_burst_start_active <= '1';
                end if;

                if (wr_burst_start_active = '1') then
                    axi_awvalid <= '1';
                    if (m_axi_awready = '1') then
                        wr_burst_start_active <= '0';
                        wr_burst_start_done   <= '1';
                    end if;
                end if;

                if (wr_burst_start_done = '1') then
                    axi_awvalid <= '0';
                end if;
            end if;
        end if;
    end process awvalid_proc;


    wdata_counter_proc : process (m_axi_aclk) is
    begin
        if (rising_edge(m_axi_aclk)) then
            if (m_axi_aresetn = '0' or state /= WRITING_TO_MEM or soft_reset = '1') then
                burst_write_counter <= (others => '0');
            else
                if (m_axi_wready = '1' and axi_wvalid = '1') then
                    burst_write_counter <= std_logic_vector(unsigned(burst_write_counter) + 1);
                end if;
            end if;
        end if;
    end process wdata_counter_proc;


    wdata_total_counter_proc : process (m_axi_aclk) is
    begin
        if (rising_edge(m_axi_aclk)) then
            if (m_axi_aresetn = '0' or soft_reset = '1') then
                transaction_wr_counter <= (others => '0');
                m_axi_wstrb            <= (others => '0');
            elsif (state = IDLE) then
                transaction_wr_counter <= (others => '0');
            elsif (state /= WRITING_TO_MEM) then
                m_axi_wstrb <= (others => '0');
            else
                if (unsigned(transaction_wr_counter) < unsigned(transaction_wr_size)) then
                    m_axi_wstrb <= (others => '1');
                else
                    m_axi_wstrb <= (others => '0');
                end if;

                if (m_axi_wready = '1' and axi_wvalid = '1') then
                    transaction_wr_counter <= std_logic_vector(unsigned(transaction_wr_counter) + 1);
                    if ((unsigned(transaction_wr_counter) + 1) >= unsigned(transaction_wr_size)) then
                        m_axi_wstrb <= (others => '0');
                    end if;
                end if;

            end if;
        end if;
    end process wdata_total_counter_proc;


    wlast_proc : process (m_axi_aclk) is
    begin
        if (rising_edge(m_axi_aclk)) then
            if (m_axi_aresetn = '0' or state /= WRITING_TO_MEM or soft_reset = '1') then
                axi_wlast_i <= '0';
            else
                if (unsigned(wr_burst_size) > 2) then
                    if ((unsigned(burst_write_counter) = unsigned(wr_burst_size)-2) and (axi_wvalid = '1') and (m_axi_wready = '1')) then
                        axi_wlast_i <= '1';
                    else
                        axi_wlast_i <= '0';
                    end if;
                elsif (unsigned(wr_burst_size) = 2) then
                    if ((unsigned(burst_write_counter) = unsigned(wr_burst_size)-2) and (axi_wvalid = '1') and (m_axi_wready = '1')) then
                        axi_wlast_i <= '1';
                    else
                        axi_wlast_i <= '0';
                    end if;
                else
                    if ((unsigned(burst_write_counter) = unsigned(wr_burst_size)-1)) then
                        axi_wlast_i <= '1';
                    else
                        axi_wlast_i <= '0';
                    end if;
                end if;
            end if;
        end if;
    end process wlast_proc;


    bready_proc : process (m_axi_aclk) is
    begin
        if (rising_edge(m_axi_aclk)) then
            if (m_axi_aresetn = '0' or state /= WRITING_TO_MEM or soft_reset = '1') then
                axi_bready <= '0';
                write_done <= '0';
            else
                if (m_axi_bvalid = '1' and axi_bready = '0') then
                    axi_bready <= '1';
                else
                    axi_bready <= axi_bready;
                    if (axi_bready = '1') then
                        axi_bready <= '0';
                        if (write_done = '1') then
                            write_done <= '0';
                        else
                            write_done <= '1';
                        end if;
                    end if;
                end if;
            end if;
        end if;
    end process bready_proc;


    request_edge_detection_proc : process (m_axi_aclk) is
    begin
        if (rising_edge(m_axi_aclk)) then
            if (m_axi_aresetn = '0' or soft_reset = '1') then
                req_dd <= '0';
                req_d  <= '0';
            else
                req_d  <= conv_req.request;
                req_dd <= req_d;
            end if;
        end if;
    end process request_edge_detection_proc;


    pattern_cnt_proc : process (m_axi_aclk) is
    begin
        if (rising_edge(m_axi_aclk)) then
            if (m_axi_aresetn = '0' or soft_reset = '1') then
                pattern_finished_i <= '0';
                pattern_cnt        <= (others => '0');
            else
                if (m_axis_tready = '1' and pattern_req = '1' and pattern_finished_i = '0') then
                    if (pattern_cnt = pattern_len-1) then
                        pattern_finished_i <= '1';
                    else
                        pattern_cnt <= pattern_cnt + 1;
                    end if;
                end if;

                if (pattern_req = '0') then
                    pattern_finished_i <= '0';
                    pattern_cnt        <= (others => '0');
                end if;

            end if;
        end if;
    end process pattern_cnt_proc;


end RTL;
