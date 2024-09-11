-------------------------------------------------------------------------------
-- Title      : AXI Lite Registers
-- Project    :
-------------------------------------------------------------------------------
-- File       : axi_lite_regs.vhd
-- Author     : Gonzalo Martinez Larumbe  <gonzalomlarumbe@gmail.com>
-- Company    :
-- Created    : 2020-02-12
-- Last update: 2020-02-16
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
library xil_defaultlib;


entity axi_lite_regs is
    generic (
        C_S_AXI_DATA_WIDTH : integer := 32;
        C_S_AXI_ADDR_WIDTH : integer := 7
        );
    port (
        soft_reset     : out std_logic;
        system_enable  : out std_logic;
        system_running : in  std_logic;

        conv_op_lch : out std_logic;
        conv_op_rch : out std_logic;
        read_size_l : out unsigned(15 downto 0);
        read_size_r : out unsigned(15 downto 0);

        -- Axi Lite Master R/W interface
        write_request : out std_logic;
        write_data    : out std_logic_vector(31 downto 0);
        write_address : out std_logic_vector(31 downto 0);
        write_done    : in  std_logic;

        read_request    : out std_logic;
        read_address    : out std_logic_vector(31 downto 0);
        read_data       : in  std_logic_vector (31 downto 0);
        read_data_valid : in  std_logic;

        transaction_error : in std_logic;

        -- Pattern counters
        count_lch         : in unsigned(31 downto 0);
        pattern_count_lch : in unsigned(31 downto 0);
        count_rch         : in unsigned(31 downto 0);
        pattern_count_rch : in unsigned(31 downto 0);

        bram_overflow_error       : in std_logic;
        out_reg_underflow_error_l : in std_logic;
        out_reg_overflow_error_l  : in std_logic;
        out_reg_underflow_error_r : in std_logic;
        out_reg_overflow_error_r  : in std_logic;

        -- Slave AXI interface
        s_axi_aclk    : in  std_logic;
        s_axi_aresetn : in  std_logic;
        s_axi_awaddr  : in  std_logic_vector(C_S_AXI_ADDR_WIDTH-1 downto 0);
        s_axi_awprot  : in  std_logic_vector(2 downto 0);
        s_axi_awvalid : in  std_logic;
        s_axi_awready : out std_logic;
        s_axi_wdata   : in  std_logic_vector(C_S_AXI_DATA_WIDTH-1 downto 0);
        s_axi_wstrb   : in  std_logic_vector((C_S_AXI_DATA_WIDTH/8)-1 downto 0);
        s_axi_wvalid  : in  std_logic;
        s_axi_wready  : out std_logic;
        s_axi_bresp   : out std_logic_vector(1 downto 0);
        s_axi_bvalid  : out std_logic;
        s_axi_bready  : in  std_logic;
        s_axi_araddr  : in  std_logic_vector(C_S_AXI_ADDR_WIDTH-1 downto 0);
        s_axi_arprot  : in  std_logic_vector(2 downto 0);
        s_axi_arvalid : in  std_logic;
        s_axi_arready : out std_logic;
        s_axi_rdata   : out std_logic_vector(C_S_AXI_DATA_WIDTH-1 downto 0);
        s_axi_rresp   : out std_logic_vector(1 downto 0);
        s_axi_rvalid  : out std_logic;
        s_axi_rready  : in  std_logic
        );

end axi_lite_regs;


architecture RTL of axi_lite_regs is

    signal axi_awaddr  : std_logic_vector(C_S_AXI_ADDR_WIDTH-1 downto 0);
    signal axi_awready : std_logic;
    signal axi_wready  : std_logic;
    signal axi_bresp   : std_logic_vector(1 downto 0);
    signal axi_bvalid  : std_logic;
    signal axi_araddr  : std_logic_vector(C_S_AXI_ADDR_WIDTH-1 downto 0);
    signal axi_arready : std_logic;
    signal axi_rdata   : std_logic_vector(C_S_AXI_DATA_WIDTH-1 downto 0);
    signal axi_rresp   : std_logic_vector(1 downto 0);
    signal axi_rvalid  : std_logic;

    constant ADDR_LSB          : integer := (C_S_AXI_DATA_WIDTH/32)+ 1;
    constant OPT_MEM_ADDR_BITS : integer := 4;

    signal slv_reg_rden : std_logic;
    signal slv_reg_wren : std_logic;
    signal reg_data_out : std_logic_vector(C_S_AXI_DATA_WIDTH-1 downto 0);
    signal byte_index   : integer;

    signal soft_reset_i        : std_logic;
    signal soft_reset_cnt      : integer;
    constant SOFT_RESET_CYCLES : integer := 50;

    -- Actual registers
    signal control_reg              : std_logic_vector(C_S_AXI_DATA_WIDTH-1 downto 0);
    signal status_reg               : std_logic_vector(C_S_AXI_DATA_WIDTH-1 downto 0);
    signal version_reg              : std_logic_vector(C_S_AXI_DATA_WIDTH-1 downto 0);
    signal converter_setup_reg      : std_logic_vector(C_S_AXI_DATA_WIDTH-1 downto 0);
    signal mm2s_size_reg            : std_logic_vector(C_S_AXI_DATA_WIDTH-1 downto 0);
    signal master_lite_wr_setup_reg : std_logic_vector(C_S_AXI_DATA_WIDTH-1 downto 0);
    signal master_lite_wr_add_reg   : std_logic_vector(C_S_AXI_DATA_WIDTH-1 downto 0);
    signal master_lite_wr_data_reg  : std_logic_vector(C_S_AXI_DATA_WIDTH-1 downto 0);
    signal master_lite_rd_setup_reg : std_logic_vector(C_S_AXI_DATA_WIDTH-1 downto 0);
    signal master_lite_rd_add_reg   : std_logic_vector(C_S_AXI_DATA_WIDTH-1 downto 0);
    signal master_lite_rd_data_reg  : std_logic_vector(C_S_AXI_DATA_WIDTH-1 downto 0);
    signal count_lch_reg            : std_logic_vector(C_S_AXI_DATA_WIDTH-1 downto 0);
    signal pattern_count_lch_reg    : std_logic_vector(C_S_AXI_DATA_WIDTH-1 downto 0);
    signal count_rch_reg            : std_logic_vector(C_S_AXI_DATA_WIDTH-1 downto 0);
    signal pattern_count_rch_reg    : std_logic_vector(C_S_AXI_DATA_WIDTH-1 downto 0);

    attribute keep                             : boolean;
    attribute keep of control_reg              : signal is true;
    attribute keep of status_reg               : signal is true;
    attribute keep of version_reg              : signal is true;
    attribute keep of converter_setup_reg      : signal is true;
    attribute keep of mm2s_size_reg            : signal is true;
    attribute keep of master_lite_wr_setup_reg : signal is true;
    attribute keep of master_lite_wr_add_reg   : signal is true;
    attribute keep of master_lite_wr_data_reg  : signal is true;
    attribute keep of master_lite_rd_setup_reg : signal is true;
    attribute keep of master_lite_rd_add_reg   : signal is true;
    attribute keep of master_lite_rd_data_reg  : signal is true;
    attribute keep of count_lch_reg            : signal is true;
    attribute keep of pattern_count_lch_reg    : signal is true;
    attribute keep of count_rch_reg            : signal is true;
    attribute keep of pattern_count_rch_reg    : signal is true;

    -- Bit aliases
    alias BIT_ENABLE     : std_logic is control_reg(0);
    alias BIT_SOFT_RESET : std_logic is control_reg(31);

    alias BIT_RUNNING                 : std_logic is status_reg(0);
    alias BIT_AXI_LITE_MASTER_ERR     : std_logic is status_reg(26);
    alias BIT_BRAM_OVERFLOW_ERR       : std_logic is status_reg(27);
    alias BIT_OUT_REG_UNDERFLOW_ERR_L : std_logic is status_reg(28);
    alias BIT_OUT_REG_OVERFLOW_ERR_L  : std_logic is status_reg(29);
    alias BIT_OUT_REG_UNDERFLOW_ERR_R : std_logic is status_reg(30);
    alias BIT_OUT_REG_OVERFLOW_ERR_R  : std_logic is status_reg(31);

    constant IP_VERSION : std_logic_vector(31 downto 0) := x"DEAD_BEEF";

    alias BIT_CONV_OP_L : std_logic is converter_setup_reg(0);
    alias BIT_CONV_OP_R : std_logic is converter_setup_reg(1);

    alias BIT_WRITE_REQUEST   : std_logic is master_lite_wr_setup_reg(0);
    alias BIT_WRITE_DONE      : std_logic is master_lite_wr_setup_reg(31);
    alias BIT_READ_REQUEST    : std_logic is master_lite_rd_setup_reg(0);
    alias BIT_READ_DATA_VALID : std_logic is master_lite_rd_setup_reg(31);


    -- Procedures
    procedure add_bit (signal sigH : in std_logic; signal bitpos : out std_logic) is
    begin
        if (sigH) then bitpos <= '1';
        else bitpos           <= '0';
        end if;
    end procedure add_bit;


begin

    s_axi_awready <= axi_awready;
    s_axi_wready  <= axi_wready;
    s_axi_bresp   <= axi_bresp;
    s_axi_bvalid  <= axi_bvalid;
    s_axi_arready <= axi_arready;
    s_axi_rdata   <= axi_rdata;
    s_axi_rresp   <= axi_rresp;
    s_axi_rvalid  <= axi_rvalid;

    slv_reg_wren <= axi_wready and s_axi_wvalid and axi_awready and s_axi_awvalid;
    slv_reg_rden <= axi_arready and s_axi_arvalid and (not axi_rvalid);


    -- Implement axi_awready generation
    -- axi_awready is asserted for one S_AXI_ACLK clock cycle when both
    -- S_AXI_AWVALID and S_AXI_WVALID are asserted. axi_awready is
    -- de-asserted when reset is low.
    axi_awready_proc : process (s_axi_aclk)
    begin
        if rising_edge(s_axi_aclk) then
            if (s_axi_aresetn = '0' or soft_reset_i = '1') then
                axi_awready <= '0';
            else
                if (axi_awready = '0' and s_axi_awvalid = '1' and s_axi_wvalid = '1') then
                    axi_awready <= '1';
                else
                    axi_awready <= '0';
                end if;
            end if;
        end if;
    end process axi_awready_proc;


    -- Implement axi_awaddr latching
    -- This process is used to latch the address when both
    -- S_AXI_AWVALID and S_AXI_WVALID are valid.
    axi_awaddr_proc : process (s_axi_aclk)
    begin
        if rising_edge(s_axi_aclk) then
            if (s_axi_aresetn = '0' or soft_reset_i = '1') then
                axi_awaddr <= (others => '0');
            elsif (axi_awready = '0' and s_axi_awvalid = '1' and s_axi_wvalid = '1') then
                axi_awaddr <= s_axi_awaddr;
            end if;
        end if;
    end process axi_awaddr_proc;


    -- Implement axi_wready generation
    -- axi_wready is asserted for one S_AXI_ACLK clock cycle when both
    -- S_AXI_AWVALID and S_AXI_WVALID are asserted. axi_wready is
    -- de-asserted when reset is low.
    axi_wready_proc : process (s_axi_aclk)
    begin
        if rising_edge(s_axi_aclk) then
            if (s_axi_aresetn = '0' or soft_reset_i = '1') then
                axi_wready <= '0';
            else
                if (axi_wready = '0' and s_axi_wvalid = '1' and s_axi_awvalid = '1') then
                    axi_wready <= '1';
                else
                    axi_wready <= '0';
                end if;
            end if;
        end if;
    end process axi_wready_proc;


    -- Implement write response logic generation
    -- The write response and response valid signals are asserted by the slave
    -- when axi_wready, S_AXI_WVALID, axi_wready and S_AXI_WVALID are asserted.
    -- This marks the acceptance of address and indicates the status of
    -- write transaction.
    axi_bvalid_proc : process (s_axi_aclk)
    begin
        if rising_edge(s_axi_aclk) then
            if (s_axi_aresetn = '0' or soft_reset_i = '1') then
                axi_bvalid <= '0';
                axi_bresp  <= "00";
            else
                if (axi_awready = '1' and s_axi_awvalid = '1' and axi_wready = '1' and s_axi_wvalid = '1' and axi_bvalid = '0') then
                    axi_bvalid <= '1';
                    axi_bresp  <= "00";
                elsif (s_axi_bready = '1' and axi_bvalid = '1') then
                    axi_bvalid <= '0';
                end if;
            end if;
        end if;
    end process axi_bvalid_proc;


    -- Implement axi_arready generation
    -- axi_arready is asserted for one S_AXI_ACLK clock cycle when
    -- S_AXI_ARVALID is asserted. axi_awready is
    -- de-asserted when reset (active low) is asserted.
    -- The read address is also latched when S_AXI_ARVALID is
    -- asserted. axi_araddr is reset to zero on reset assertion.
    axi_arready_proc : process (s_axi_aclk)
    begin
        if rising_edge(s_axi_aclk) then
            if (s_axi_aresetn = '0' or soft_reset_i = '1') then
                axi_arready <= '0';
                axi_araddr  <= (others => '1');
            else
                if (axi_arready = '0' and s_axi_arvalid = '1') then
                    axi_arready <= '1';
                    axi_araddr  <= s_axi_araddr;
                else
                    axi_arready <= '0';
                end if;
            end if;
        end if;
    end process axi_arready_proc;


    -- Implement axi_arvalid generation
    -- axi_rvalid is asserted for one S_AXI_ACLK clock cycle when both
    -- S_AXI_ARVALID and axi_arready are asserted. The slave registers
    -- data are available on the axi_rdata bus at this instance. The
    -- assertion of axi_rvalid marks the validity of read data on the
    -- bus and axi_rresp indicates the status of read transaction.axi_rvalid
    -- is deasserted on reset (active low). axi_rresp and axi_rdata are
    -- cleared to zero on reset (active low).
    axi_rvalid_proc : process (s_axi_aclk)
    begin
        if rising_edge(s_axi_aclk) then
            if (s_axi_aresetn = '0' or soft_reset_i = '1') then
                axi_rvalid <= '0';
                axi_rresp  <= "00";
            else
                if (axi_arready = '1' and s_axi_arvalid = '1' and axi_rvalid = '0') then
                    axi_rvalid <= '1';
                    axi_rresp  <= "00";
                elsif (axi_rvalid = '1' and s_axi_rready = '1') then
                    axi_rvalid <= '0';
                end if;
            end if;
        end if;
    end process axi_rvalid_proc;


    -- Output register or memory read data
    axi_rdata_proc : process(s_axi_aclk) is
    begin
        if (rising_edge (s_axi_aclk)) then
            if (s_axi_aresetn = '0' or soft_reset_i = '1') then
                axi_rdata <= (others => '0');
            else
                if (slv_reg_rden = '1') then
                    axi_rdata <= reg_data_out;
                end if;
            end if;
        end if;
    end process axi_rdata_proc;


    -- Read address decoding
    address_decoding_proc : process (all)
        variable loc_addr : std_logic_vector(OPT_MEM_ADDR_BITS downto 0);
    begin
        loc_addr := axi_araddr(ADDR_LSB + OPT_MEM_ADDR_BITS downto ADDR_LSB);
        case loc_addr is
            when b"00000" =>
                reg_data_out <= control_reg;
            when b"00001" =>
                reg_data_out <= status_reg;
            when b"00010" =>
                reg_data_out <= version_reg;
            when b"00011" =>
                reg_data_out <= converter_setup_reg;
            when b"00100" =>
                reg_data_out <= mm2s_size_reg;
            when b"00101" =>
                reg_data_out <= master_lite_wr_setup_reg;
            when b"00110" =>
                reg_data_out <= master_lite_wr_add_reg;
            when b"00111" =>
                reg_data_out <= master_lite_wr_data_reg;
            when b"01000" =>
                reg_data_out <= master_lite_rd_setup_reg;
            when b"01001" =>
                reg_data_out <= master_lite_rd_add_reg;
            when b"01010" =>
                reg_data_out <= master_lite_rd_data_reg;
            when b"01011" =>
                reg_data_out <= count_lch_reg;
            when b"01100" =>
                reg_data_out <= pattern_count_lch_reg;
            when b"01101" =>
                reg_data_out <= count_rch_reg;
            when b"01110" =>
                reg_data_out <= pattern_count_rch_reg;
            when others =>
                reg_data_out <= (others => '0');
        end case;
    end process address_decoding_proc;


    -- Implement memory mapped register select and write logic generation
    -- The write data is accepted and written to memory mapped registers when
    -- axi_awready, S_AXI_WVALID, axi_wready and S_AXI_WVALID are asserted. Write strobes are used to
    -- select byte enables of slave registers while writing.
    -- These registers are cleared when reset (active low) is applied.
    -- Slave register write enable is asserted when valid address and data are available
    -- and the slave is ready to accept the write address and write data.
    read_write_regs_proc : process (s_axi_aclk)
        variable loc_addr : std_logic_vector(OPT_MEM_ADDR_BITS downto 0);
    begin
        if rising_edge(s_axi_aclk) then
            if (s_axi_aresetn = '0') then
                control_reg              <= (others => '0');
                converter_setup_reg      <= (others => '0');
                mm2s_size_reg            <= (others => '0');
                master_lite_wr_setup_reg <= (others => '0');
                master_lite_wr_add_reg   <= (others => '0');
                master_lite_wr_data_reg  <= (others => '0');
                master_lite_rd_setup_reg <= (others => '0');
                master_lite_rd_add_reg   <= (others => '0');
            else
                loc_addr := axi_awaddr(ADDR_LSB + OPT_MEM_ADDR_BITS downto ADDR_LSB);
                if (slv_reg_wren = '1') then
                    case loc_addr is
                        when b"00000" =>
                            for byte_index in 0 to (C_S_AXI_DATA_WIDTH/8-1) loop
                                if (s_axi_wstrb(byte_index) = '1') then
                                    control_reg(byte_index*8+7 downto byte_index*8) <= s_axi_wdata(byte_index*8+7 downto byte_index*8);
                                end if;
                            end loop;

                        when b"00011" =>
                            for byte_index in 0 to (C_S_AXI_DATA_WIDTH/8-1) loop
                                if (s_axi_wstrb(byte_index) = '1') then
                                    converter_setup_reg(byte_index*8+7 downto byte_index*8) <= s_axi_wdata(byte_index*8+7 downto byte_index*8);
                                end if;
                            end loop;

                        when b"00100" =>
                            for byte_index in 0 to (C_S_AXI_DATA_WIDTH/8-1) loop
                                if (s_axi_wstrb(byte_index) = '1') then
                                    mm2s_size_reg(byte_index*8+7 downto byte_index*8) <= s_axi_wdata(byte_index*8+7 downto byte_index*8);
                                end if;
                            end loop;

                        when b"00101" =>
                            for byte_index in 0 to (C_S_AXI_DATA_WIDTH/8-1) loop
                                if (s_axi_wstrb(byte_index) = '1') then
                                    master_lite_wr_setup_reg(byte_index*8+7 downto byte_index*8) <= s_axi_wdata(byte_index*8+7 downto byte_index*8);
                                end if;
                            end loop;

                        when b"00110" =>
                            for byte_index in 0 to (C_S_AXI_DATA_WIDTH/8-1) loop
                                if (s_axi_wstrb(byte_index) = '1') then
                                    master_lite_wr_add_reg(byte_index*8+7 downto byte_index*8) <= s_axi_wdata(byte_index*8+7 downto byte_index*8);
                                end if;
                            end loop;

                        when b"00111" =>
                            for byte_index in 0 to (C_S_AXI_DATA_WIDTH/8-1) loop
                                if (s_axi_wstrb(byte_index) = '1') then
                                    master_lite_wr_data_reg(byte_index*8+7 downto byte_index*8) <= s_axi_wdata(byte_index*8+7 downto byte_index*8);
                                end if;
                            end loop;

                        when b"01000" =>
                            for byte_index in 0 to (C_S_AXI_DATA_WIDTH/8-1) loop
                                if (s_axi_wstrb(byte_index) = '1') then
                                    master_lite_rd_setup_reg(byte_index*8+7 downto byte_index*8) <= s_axi_wdata(byte_index*8+7 downto byte_index*8);
                                end if;
                            end loop;

                        when b"01001" =>
                            for byte_index in 0 to (C_S_AXI_DATA_WIDTH/8-1) loop
                                if (s_axi_wstrb(byte_index) = '1') then
                                    master_lite_rd_add_reg(byte_index*8+7 downto byte_index*8) <= s_axi_wdata(byte_index*8+7 downto byte_index*8);
                                end if;
                            end loop;

                        when others => null;

                    end case;
                end if;

                -- Soft reset bit auto-clear after specified amount of cycles
                if (soft_reset_cnt = SOFT_RESET_CYCLES - 1) then
                    BIT_SOFT_RESET <= '0';
                end if;

                -- Axi lite master read/write requests should last one cycle
                if (read_request) then
                    BIT_READ_REQUEST <= '0';
                end if;

                if (read_data_valid) then
                    BIT_READ_DATA_VALID <= '1';
                end if;

                if (write_request) then
                    BIT_WRITE_REQUEST <= '0';
                end if;

                if (write_done) then
                    BIT_WRITE_DONE <= '1';
                end if;



            end if;
        end if;
    end process read_write_regs_proc;



    read_only_regs_proc : process (s_axi_aclk) is
    begin
        if rising_edge(s_axi_aclk) then
            if (s_axi_aresetn = '0' or soft_reset_i = '1') then
                status_reg              <= (others => '0');
                version_reg             <= IP_VERSION;
                count_lch_reg           <= (others => '0');
                pattern_count_lch_reg   <= (others => '0');
                count_rch_reg           <= (others => '0');
                pattern_count_rch_reg   <= (others => '0');
                master_lite_rd_data_reg <= (others => '0');
            else
                -- Status REG
                add_bit(system_running, BIT_RUNNING);
                add_bit(transaction_error, BIT_AXI_LITE_MASTER_ERR);
                add_bit(bram_overflow_error, BIT_BRAM_OVERFLOW_ERR);
                add_bit(out_reg_underflow_error_l, BIT_OUT_REG_UNDERFLOW_ERR_L);
                add_bit(out_reg_overflow_error_l, BIT_OUT_REG_OVERFLOW_ERR_L);
                add_bit(out_reg_underflow_error_r, BIT_OUT_REG_UNDERFLOW_ERR_R);
                add_bit(out_reg_overflow_error_r, BIT_OUT_REG_OVERFLOW_ERR_R);
                -- Version REG
                version_reg             <= IP_VERSION;
                -- Pattern counters
                count_lch_reg           <= std_logic_vector(count_lch);
                pattern_count_lch_reg   <= std_logic_vector(pattern_count_lch);
                count_rch_reg           <= std_logic_vector(count_rch);
                pattern_count_rch_reg   <= std_logic_vector(pattern_count_rch);
                -- Axi lite master read data
                master_lite_rd_data_reg <= read_data;
            end if;
        end if;
    end process read_only_regs_proc;



    control_reg_signals_proc : process (s_axi_aclk) is
        procedure add_out_sigH (signal sigH : out std_logic; signal bitpos : in std_logic) is
        begin
            if (bitpos) then sigH <= '1';
            else sigH             <= '0';
            end if;
        end procedure add_out_sigH;

    begin
        if rising_edge(s_axi_aclk) then
            if (s_axi_aresetn = '0' or soft_reset_i = '1') then
                system_enable <= '0';
            else
                add_out_sigH(system_enable, BIT_ENABLE);
            end if;
        end if;
    end process control_reg_signals_proc;



    soft_reset_out_proc : process (s_axi_aclk) is
    begin
        if (rising_edge(s_axi_aclk)) then
            if (s_axi_aresetn = '0') then
                soft_reset     <= '0';
                soft_reset_i   <= '0';
                soft_reset_cnt <= 0;
            else
                if (BIT_SOFT_RESET = '1') then
                    soft_reset_i   <= '1';
                    soft_reset_cnt <= soft_reset_cnt + 1;
                else
                    soft_reset_i   <= '0';
                    soft_reset_cnt <= 0;
                end if;

                soft_reset <= soft_reset_i;
            end if;
        end if;
    end process soft_reset_out_proc;


    mm2s_proc : process (s_axi_aclk) is
    begin
        if (rising_edge(s_axi_aclk)) then
            if (s_axi_aresetn = '0' or soft_reset_i = '1') then
                conv_op_lch <= '0';
                conv_op_rch <= '0';
                read_size_l <= (others => '0');
                read_size_r <= (others => '0');
            else
                add_bit(BIT_CONV_OP_L, conv_op_lch);
                add_bit(BIT_CONV_OP_R, conv_op_rch);
                read_size_l <= unsigned(mm2s_size_reg(15 downto 0));
                read_size_r <= unsigned(mm2s_size_reg(31 downto 16));
            end if;
        end if;
    end process mm2s_proc;


    master_lite_proc : process (s_axi_aclk) is
    begin
        if (rising_edge(s_axi_aclk)) then
            if (s_axi_aresetn = '0' or soft_reset_i = '1') then
                write_address <= (others => '0');
                write_data    <= (others => '0');
                write_request <= '0';
                read_address  <= (others => '0');
                read_request  <= '0';
            else
                write_address <= master_lite_wr_add_reg;
                write_data    <= master_lite_wr_data_reg;
                write_request <= BIT_WRITE_REQUEST;
                read_address  <= master_lite_rd_add_reg;
                read_request  <= BIT_READ_REQUEST;
            end if;
        end if;
    end process master_lite_proc;



end RTL;
