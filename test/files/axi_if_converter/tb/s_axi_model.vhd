-------------------------------------------------------------------------------
-- Title      : Slave AXI full Memory Model
-- Project    :
-------------------------------------------------------------------------------
-- File       : s_axi_model.vhd
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
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity s_axi_model is
    generic (
        C_S_AXI_ID_WIDTH     : integer := 1;
        C_S_AXI_DATA_WIDTH   : integer := 64;
        C_S_AXI_ADDR_WIDTH   : integer := 10;
        C_S_AXI_AWUSER_WIDTH : integer := 0;
        C_S_AXI_ARUSER_WIDTH : integer := 0;
        C_S_AXI_WUSER_WIDTH  : integer := 0;
        C_S_AXI_RUSER_WIDTH  : integer := 0;
        C_S_AXI_BUSER_WIDTH  : integer := 0
        );
    port (
        S_AXI_ACLK     : in  std_logic;
        S_AXI_ARESETN  : in  std_logic;
        S_AXI_AWID     : in  std_logic_vector(C_S_AXI_ID_WIDTH-1 downto 0);
        S_AXI_AWADDR   : in  std_logic_vector(C_S_AXI_ADDR_WIDTH-1 downto 0);
        S_AXI_AWLEN    : in  std_logic_vector(7 downto 0);
        S_AXI_AWSIZE   : in  std_logic_vector(2 downto 0);
        S_AXI_AWBURST  : in  std_logic_vector(1 downto 0);
        S_AXI_AWLOCK   : in  std_logic;
        S_AXI_AWCACHE  : in  std_logic_vector(3 downto 0);
        S_AXI_AWPROT   : in  std_logic_vector(2 downto 0);
        S_AXI_AWQOS    : in  std_logic_vector(3 downto 0);
        S_AXI_AWREGION : in  std_logic_vector(3 downto 0);
        S_AXI_AWUSER   : in  std_logic_vector(C_S_AXI_AWUSER_WIDTH-1 downto 0);
        S_AXI_AWVALID  : in  std_logic;
        S_AXI_AWREADY  : out std_logic;
        S_AXI_WDATA    : in  std_logic_vector(C_S_AXI_DATA_WIDTH-1 downto 0);
        S_AXI_WSTRB    : in  std_logic_vector((C_S_AXI_DATA_WIDTH/8)-1 downto 0);
        S_AXI_WLAST    : in  std_logic;
        S_AXI_WUSER    : in  std_logic_vector(C_S_AXI_WUSER_WIDTH-1 downto 0);
        S_AXI_WVALID   : in  std_logic;
        S_AXI_WREADY   : out std_logic;
        S_AXI_BID      : out std_logic_vector(C_S_AXI_ID_WIDTH-1 downto 0);
        S_AXI_BRESP    : out std_logic_vector(1 downto 0);
        S_AXI_BUSER    : out std_logic_vector(C_S_AXI_BUSER_WIDTH-1 downto 0);
        S_AXI_BVALID   : out std_logic;
        S_AXI_BREADY   : in  std_logic;
        S_AXI_ARID     : in  std_logic_vector(C_S_AXI_ID_WIDTH-1 downto 0);
        S_AXI_ARADDR   : in  std_logic_vector(C_S_AXI_ADDR_WIDTH-1 downto 0);
        S_AXI_ARLEN    : in  std_logic_vector(7 downto 0);
        S_AXI_ARSIZE   : in  std_logic_vector(2 downto 0);
        S_AXI_ARBURST  : in  std_logic_vector(1 downto 0);
        S_AXI_ARLOCK   : in  std_logic;
        S_AXI_ARCACHE  : in  std_logic_vector(3 downto 0);
        S_AXI_ARPROT   : in  std_logic_vector(2 downto 0);
        S_AXI_ARQOS    : in  std_logic_vector(3 downto 0);
        S_AXI_ARREGION : in  std_logic_vector(3 downto 0);
        S_AXI_ARUSER   : in  std_logic_vector(C_S_AXI_ARUSER_WIDTH-1 downto 0);
        S_AXI_ARVALID  : in  std_logic;
        S_AXI_ARREADY  : out std_logic;
        S_AXI_RID      : out std_logic_vector(C_S_AXI_ID_WIDTH-1 downto 0);
        S_AXI_RDATA    : out std_logic_vector(C_S_AXI_DATA_WIDTH-1 downto 0);
        S_AXI_RRESP    : out std_logic_vector(1 downto 0);
        S_AXI_RLAST    : out std_logic;
        S_AXI_RUSER    : out std_logic_vector(C_S_AXI_RUSER_WIDTH-1 downto 0);
        S_AXI_RVALID   : out std_logic;
        S_AXI_RREADY   : in  std_logic
        );
end s_axi_model;

architecture RTL of s_axi_model is

    constant ADDR_LSB          : integer                                            := (C_S_AXI_DATA_WIDTH/32)+ 1;
    constant OPT_MEM_ADDR_BITS : integer                                            := 6;
    constant USER_NUM_MEM      : integer                                            := 1;
    constant LOW               : std_logic_vector (C_S_AXI_ADDR_WIDTH - 1 downto 0) := "0000000000";

    signal axi_awaddr  : std_logic_vector(C_S_AXI_ADDR_WIDTH-1 downto 0);
    signal axi_awready : std_logic;
    signal axi_wready  : std_logic;
    signal axi_bresp   : std_logic_vector(1 downto 0);
    signal axi_buser   : std_logic_vector(C_S_AXI_BUSER_WIDTH-1 downto 0);
    signal axi_bvalid  : std_logic;
    signal axi_araddr  : std_logic_vector(C_S_AXI_ADDR_WIDTH-1 downto 0);
    signal axi_arready : std_logic;
    signal axi_rdata   : std_logic_vector(C_S_AXI_DATA_WIDTH-1 downto 0);
    signal axi_rresp   : std_logic_vector(1 downto 0);
    signal axi_rlast   : std_logic;
    signal axi_ruser   : std_logic_vector(C_S_AXI_RUSER_WIDTH-1 downto 0);
    signal axi_rvalid  : std_logic;

    signal aw_wrap_en       : std_logic;
    signal ar_wrap_en       : std_logic;
    signal aw_wrap_size     : natural;
    signal ar_wrap_size     : natural;
    signal axi_awv_awr_flag : std_logic;
    signal axi_arv_arr_flag : std_logic;
    signal axi_awlen_cntr   : std_logic_vector(7 downto 0);
    signal axi_arlen_cntr   : std_logic_vector(7 downto 0);
    signal axi_arburst      : std_logic_vector(2-1 downto 0);
    signal axi_awburst      : std_logic_vector(2-1 downto 0);
    signal axi_arlen        : std_logic_vector(8-1 downto 0);
    signal axi_awlen        : std_logic_vector(8-1 downto 0);

    signal mem_address  : std_logic_vector(OPT_MEM_ADDR_BITS downto 0);
    signal mem_select   : std_logic_vector(USER_NUM_MEM-1 downto 0);
    type word_array is array (0 to USER_NUM_MEM-1) of std_logic_vector(C_S_AXI_DATA_WIDTH-1 downto 0);
    signal mem_data_out : word_array;

    signal i              : integer;
    signal j              : integer;
    signal mem_byte_index : integer;

    type BYTE_RAM_TYPE is array (0 to 255) of std_logic_vector(7 downto 0);
begin

    S_AXI_AWREADY <= axi_awready;
    S_AXI_WREADY  <= axi_wready;
    S_AXI_BRESP   <= axi_bresp;
    S_AXI_BUSER   <= (others => '0');
    S_AXI_BVALID  <= axi_bvalid;
    S_AXI_BID     <= s_axi_awid;
    S_AXI_ARREADY <= axi_arready;
    S_AXI_RDATA   <= axi_rdata;
    S_AXI_RRESP   <= axi_rresp;
    S_AXI_RLAST   <= axi_rlast;
    S_AXI_RUSER   <= axi_ruser;
    S_AXI_RVALID  <= axi_rvalid;
    S_AXI_RID     <= s_axi_arid;

    aw_wrap_size <= ((C_S_AXI_DATA_WIDTH)/8 * to_integer(unsigned(axi_awlen)));
    ar_wrap_size <= ((C_S_AXI_DATA_WIDTH)/8 * to_integer(unsigned(axi_arlen)));
    aw_wrap_en   <= '1' when (((axi_awaddr and std_logic_vector(to_unsigned(aw_wrap_size, C_S_AXI_ADDR_WIDTH))) xor std_logic_vector(to_unsigned(aw_wrap_size, C_S_AXI_ADDR_WIDTH))) = LOW) else '0';
    ar_wrap_en   <= '1' when (((axi_araddr and std_logic_vector(to_unsigned(ar_wrap_size, C_S_AXI_ADDR_WIDTH))) xor std_logic_vector(to_unsigned(ar_wrap_size, C_S_AXI_ADDR_WIDTH))) = LOW) else '0';

    -- axi_awready is asserted for one S_AXI_ACLK clock cycle when both
    -- S_AXI_AWVALID and S_AXI_WVALID are asserted. axi_awready is
    -- de-asserted when reset is low.
    awready_proc : process (S_AXI_ACLK)
    begin
        if rising_edge(S_AXI_ACLK) then
            if S_AXI_ARESETN = '0' then
                axi_awready      <= '0';
                axi_awv_awr_flag <= '0';
            else
                if (axi_awready = '0' and S_AXI_AWVALID = '1' and axi_awv_awr_flag = '0' and axi_arv_arr_flag = '0') then
                    axi_awv_awr_flag <= '1';
                    axi_awready      <= '1';
                elsif (S_AXI_WLAST = '1' and axi_wready = '1') then
                    axi_awv_awr_flag <= '0';
                else
                    axi_awready <= '0';
                end if;
            end if;
        end if;
    end process;


    -- axi_awaddr latching
    -- This process is used to latch the address when both
    -- S_AXI_AWVALID and S_AXI_WVALID are valid.
    awaddr_proc : process (S_AXI_ACLK)
    begin
        if rising_edge(S_AXI_ACLK) then
            if S_AXI_ARESETN = '0' then
                axi_awaddr     <= (others => '0');
                axi_awburst    <= (others => '0');
                axi_awlen      <= (others => '0');
                axi_awlen_cntr <= (others => '0');
            else
                if (axi_awready = '0' and S_AXI_AWVALID = '1' and axi_awv_awr_flag = '0') then
                    axi_awaddr     <= S_AXI_AWADDR(C_S_AXI_ADDR_WIDTH - 1 downto 0);
                    axi_awlen_cntr <= (others => '0');
                    axi_awburst    <= S_AXI_AWBURST;
                    axi_awlen      <= S_AXI_AWLEN;
                elsif((axi_awlen_cntr <= axi_awlen) and axi_wready = '1' and S_AXI_WVALID = '1') then
                    axi_awlen_cntr <= std_logic_vector (unsigned(axi_awlen_cntr) + 1);
                    case (axi_awburst) is
                        when "00" =>
                            axi_awaddr <= axi_awaddr;
                        when "01" =>
                            axi_awaddr(C_S_AXI_ADDR_WIDTH - 1 downto ADDR_LSB) <= std_logic_vector (unsigned(axi_awaddr(C_S_AXI_ADDR_WIDTH - 1 downto ADDR_LSB)) + 1);
                            axi_awaddr(ADDR_LSB-1 downto 0)                    <= (others => '0');
                        when "10" =>
                            if (aw_wrap_en = '1') then
                                axi_awaddr <= std_logic_vector (unsigned(axi_awaddr) - (to_unsigned(aw_wrap_size, C_S_AXI_ADDR_WIDTH)));
                            else
                                axi_awaddr(C_S_AXI_ADDR_WIDTH - 1 downto ADDR_LSB) <= std_logic_vector (unsigned(axi_awaddr(C_S_AXI_ADDR_WIDTH - 1 downto ADDR_LSB)) + 1);
                                axi_awaddr(ADDR_LSB-1 downto 0)                    <= (others => '0');
                            end if;
                        when others =>
                            axi_awaddr(C_S_AXI_ADDR_WIDTH - 1 downto ADDR_LSB) <= std_logic_vector (unsigned(axi_awaddr(C_S_AXI_ADDR_WIDTH - 1 downto ADDR_LSB)) + 1);
                            axi_awaddr(ADDR_LSB-1 downto 0)                    <= (others => '0');
                    end case;
                end if;
            end if;
        end if;
    end process;



    -- axi_wready is asserted for one S_AXI_ACLK clock cycle when both
    -- S_AXI_AWVALID and S_AXI_WVALID are asserted. axi_wready is
    -- de-asserted when reset is low.
    wready_proc : process (S_AXI_ACLK)
    begin
        if rising_edge(S_AXI_ACLK) then
            if S_AXI_ARESETN = '0' then
                axi_wready <= '0';
            else
                if (axi_wready = '0' and S_AXI_WVALID = '1' and axi_awv_awr_flag = '1') then
                    axi_wready <= '1';
                elsif (S_AXI_WLAST = '1' and axi_wready = '1') then
                    axi_wready <= '0';
                end if;
            end if;
        end if;
    end process;



    -- The write response and response valid signals are asserted by the slave
    -- when axi_wready, S_AXI_WVALID, axi_wready and S_AXI_WVALID are asserted.
    -- This marks the acceptance of address and indicates the status of
    -- write transaction.
    write_rsp_proc : process (S_AXI_ACLK)
    begin
        if rising_edge(S_AXI_ACLK) then
            if S_AXI_ARESETN = '0' then
                axi_bvalid <= '0';
                axi_bresp  <= "00";
            else
                if (axi_awv_awr_flag = '1' and axi_wready = '1' and S_AXI_WVALID = '1' and axi_bvalid = '0' and S_AXI_WLAST = '1') then
                    axi_bvalid <= '1';
                    axi_bresp  <= "00";
                elsif (S_AXI_BREADY = '1' and axi_bvalid = '1') then
                    axi_bvalid <= '0';
                end if;
            end if;
        end if;
    end process;



    -- axi_arready is asserted for one S_AXI_ACLK clock cycle when
    -- S_AXI_ARVALID is asserted. axi_awready is
    -- de-asserted when reset (active low) is asserted.
    -- The read address is also latched when S_AXI_ARVALID is
    -- asserted. axi_araddr is reset to zero on reset assertion.
    arready_proc : process (S_AXI_ACLK)
    begin
        if rising_edge(S_AXI_ACLK) then
            if S_AXI_ARESETN = '0' then
                axi_arready      <= '0';
                axi_arv_arr_flag <= '0';
            else
                if (axi_arready = '0' and S_AXI_ARVALID = '1' and axi_awv_awr_flag = '0' and axi_arv_arr_flag = '0') then
                    axi_arready      <= '1';
                    axi_arv_arr_flag <= '1';
                elsif (axi_rvalid = '1' and S_AXI_RREADY = '1' and (axi_arlen_cntr = axi_arlen)) then
                    axi_arv_arr_flag <= '0';
                else
                    axi_arready <= '0';
                end if;
            end if;
        end if;
    end process;


    --This process is used to latch the address when both
    --S_AXI_ARVALID and S_AXI_RVALID are valid.
    read_address_proc : process (S_AXI_ACLK)
    begin
        if rising_edge(S_AXI_ACLK) then
            if S_AXI_ARESETN = '0' then
                axi_araddr     <= (others => '0');
                axi_arburst    <= (others => '0');
                axi_arlen      <= (others => '0');
                axi_arlen_cntr <= (others => '0');
                axi_rlast      <= '0';
            else
                if (axi_arready = '0' and S_AXI_ARVALID = '1' and axi_arv_arr_flag = '0') then
                    axi_araddr     <= S_AXI_ARADDR(C_S_AXI_ADDR_WIDTH - 1 downto 0);
                    axi_arlen_cntr <= (others => '0');
                    axi_rlast      <= '0';
                    axi_arburst    <= S_AXI_ARBURST;
                    axi_arlen      <= S_AXI_ARLEN;
                elsif((axi_arlen_cntr <= axi_arlen) and axi_rvalid = '1' and S_AXI_RREADY = '1') then
                    axi_arlen_cntr <= std_logic_vector (unsigned(axi_arlen_cntr) + 1);
                    axi_rlast      <= '0';
                    case (axi_arburst) is
                        when "00" =>
                            axi_araddr <= axi_araddr;
                        when "01" =>
                            axi_araddr(C_S_AXI_ADDR_WIDTH - 1 downto ADDR_LSB) <= std_logic_vector (unsigned(axi_araddr(C_S_AXI_ADDR_WIDTH - 1 downto ADDR_LSB)) + 1);
                            axi_araddr(ADDR_LSB-1 downto 0)                    <= (others => '0');
                        when "10" =>
                            if (ar_wrap_en = '1') then
                                axi_araddr <= std_logic_vector (unsigned(axi_araddr) - (to_unsigned(ar_wrap_size, C_S_AXI_ADDR_WIDTH)));
                            else
                                axi_araddr(C_S_AXI_ADDR_WIDTH - 1 downto ADDR_LSB) <= std_logic_vector (unsigned(axi_araddr(C_S_AXI_ADDR_WIDTH - 1 downto ADDR_LSB)) + 1);
                                axi_araddr(ADDR_LSB-1 downto 0)                    <= (others => '0');
                            end if;
                        when others =>
                            axi_araddr(C_S_AXI_ADDR_WIDTH - 1 downto ADDR_LSB) <= std_logic_vector (unsigned(axi_araddr(C_S_AXI_ADDR_WIDTH - 1 downto ADDR_LSB)) + 1);
                            axi_araddr(ADDR_LSB-1 downto 0)                    <= (others => '0');
                    end case;
                elsif((axi_arlen_cntr = axi_arlen) and axi_rlast = '0' and axi_arv_arr_flag = '1') then
                    axi_rlast <= '1';
                elsif (S_AXI_RREADY = '1') then
                    axi_rlast <= '0';
                end if;
            end if;
        end if;
    end process;


    -- axi_rvalid is asserted for one S_AXI_ACLK clock cycle when both
    -- S_AXI_ARVALID and axi_arready are asserted. The slave registers
    -- data are available on the axi_rdata bus at this instance. The
    -- assertion of axi_rvalid marks the validity of read data on the
    -- bus and axi_rresp indicates the status of read transaction.axi_rvalid
    -- is deasserted on reset (active low). axi_rresp and axi_rdata are
    -- cleared to zero on reset (active low).
    rvalid_proc : process (S_AXI_ACLK)
    begin
        if rising_edge(S_AXI_ACLK) then
            if S_AXI_ARESETN = '0' then
                axi_rvalid <= '0';
                axi_rresp  <= "00";
            else
                if (axi_arv_arr_flag = '1' and axi_rvalid = '0') then
                    axi_rvalid <= '1';
                    axi_rresp  <= "00";
                elsif (axi_rvalid = '1' and S_AXI_RREADY = '1') then
                    axi_rvalid <= '0';
                end if;
            end if;
        end if;
    end process;



    -- Memory address signals generation
    gen_mem_sel : if (USER_NUM_MEM >= 1) generate
    begin
        mem_select  <= "1";
        mem_address <= axi_araddr(ADDR_LSB+OPT_MEM_ADDR_BITS downto ADDR_LSB) when axi_arv_arr_flag = '1' else
                       axi_awaddr(ADDR_LSB+OPT_MEM_ADDR_BITS downto ADDR_LSB) when axi_awv_awr_flag = '1' else
                       (others => '0');
    end generate gen_mem_sel;


    -- Internal memory generation
    bram_gen : for i in 0 to USER_NUM_MEM-1 generate
        signal mem_rden : std_logic;
        signal mem_wren : std_logic;
    begin
        mem_wren <= axi_wready and S_AXI_WVALID;
        mem_rden <= axi_arv_arr_flag;

        byte_bram_gen : for mem_byte_index in 0 to (C_S_AXI_DATA_WIDTH/8-1) generate
            signal byte_ram : BYTE_RAM_TYPE;
            signal data_in  : std_logic_vector(7 downto 0);
            signal data_out : std_logic_vector(7 downto 0);
        begin

            data_in  <= S_AXI_WDATA((mem_byte_index*8+7) downto mem_byte_index*8);
            data_out <= byte_ram(to_integer(unsigned(mem_address)));
            byte_ram_proc : process(S_AXI_ACLK) is
            begin
                if (rising_edge (S_AXI_ACLK)) then
                    if (S_AXI_ARESETN = '0') then
                        byte_ram(to_integer(unsigned(mem_address))) <= (others => '1');
                    elsif (mem_wren = '1' and S_AXI_WSTRB(mem_byte_index) = '1') then
                        byte_ram(to_integer(unsigned(mem_address))) <= data_in;
                    end if;
                end if;

            end process byte_ram_proc;
            process(S_AXI_ACLK) is
            begin
                if (rising_edge (S_AXI_ACLK)) then
                    if (S_AXI_ARESETN = '0') then
                        mem_data_out(i)((mem_byte_index*8+7) downto mem_byte_index*8) <= (others => '1');
                    elsif (mem_rden = '1') then
                        mem_data_out(i)((mem_byte_index*8+7) downto mem_byte_index*8) <= data_out;
                    end if;
                end if;
            end process;

        end generate byte_bram_gen;
    end generate bram_gen;



    -- Output register for memory read data
    out_register_proc : process(mem_data_out, axi_rvalid) is
    begin
        if (axi_rvalid = '1') then
            axi_rdata <= mem_data_out(0);
        else
            axi_rdata <= (others => '0');
        end if;
    end process;


end RTL;
