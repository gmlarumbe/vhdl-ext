-------------------------------------------------------------------------------
-- Title      : Sexp Tests
-- Project    : vhdl-ext
-------------------------------------------------------------------------------
-- File       : sexp.vhd
-- Author     : Gonzalo Larumbe  <gonzalomlarumbe@gmail.com>
-- Company    : gmlarumbe
-- Created    : 2023-06-10
-- Last update: 2023-06-16
-- Platform   : Arch
-- Standard   : VHDL'08
-------------------------------------------------------------------------------
-- Description:
-------------------------------------------------------------------------------
-- Copyright (c) 2023 gmlarumbe
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author  Description
-- 2023-06-10  1.0      gonz    Created
-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;

-- Entities
entity foo is
    port (
        clk    : in std_logic;
        resetn : in std_logic
    );

end entity foo;

entity foo2 is
    port (
        clk    : in std_logic;
        resetn : in std_logic
    );

end foo2;


-- Architectures/functions/components/processes/procedures/generates
architecture RTL of foo is

    signal soft_reset : std_logic;

    component blk_mem_gen_0
        port (
            clka  : in  std_logic;
            doutb : out std_logic_vector(63 downto 0)
        );
    end component;

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
    end function clogb2;

begin

    main : process

        procedure init_values is
        begin
            resetn     <= '0';
            soft_reset <= '0';
        end procedure init_values;

    begin
    end process main;

    gen_mem_sel : if (USER_NUM_MEM >= 1) generate
        begin
        mem_select  <= "1";
    end generate gen_mem_sel;

end architecture RTL;



architecture RTL of foo is

    signal soft_reset : std_logic;

    function clogb2 (bit_depth : integer) return integer is
        variable depth : integer := bit_depth;
        variable count : integer := 1;
    begin
        for clogb2 in 1 to bit_depth loop
            if (bit_depth <= 2) then
                count := 1;
            else
                if (depth <= 1) then
                    for j in 1 to 10 loop
                        count := count;
                    end loop;
                else
                    depth := depth / 2;
                    count := count + 1;
                end if;
            end if;
        end loop;
        return (count);
    end;

begin

    main : process

        procedure init_values is
        begin
            resetn     <= '0';
            soft_reset <= '0';
        end init_values;

    begin
    end process main;

    gen_mem_sel : if (USER_NUM_MEM >= 1) generate
        begin
        mem_select  <= "1";
    end generate;

end RTL;



-- Packages
package foo is

    procedure foo2(
        signal common_in   : in  mf_common_response_r_type_in
    );

end package foo;

package foo is

    procedure foo2(
        signal common_in   : in  mf_common_response_r_type_in
    );

end;


package body foo is

    procedure foo (
        signal common_in   : in  mf_common_response_r_type_in
    ) is
    begin
        common_out.m_axi_arready <= '1';
    end procedure foo;

end package body foo;


package body foo is

    procedure foo (
        signal common_in   : in  mf_common_response_r_type_in
    ) is
    begin
        common_out.m_axi_arready <= '1';
    end  foo;

end;


-- Configuration
configuration foo of foo2 is

end configuration foo;

configuration foo of foo2 is

end;


-- Contexts
context foo is

end context foo;

context foo is

end;
