library ieee;
use ieee.std_logic_1164.all;
use IEEE.numeric_std.all;

entity lvds_data_gen is
    port (
        clk_i   : in std_logic;
        reset_i : in std_logic;

        lvds0   : out std_logic;
        lvds1   : out std_logic;
        valid_o : out std_logic
    );
end lvds_data_gen;

architecture rtl of lvds_data_gen is
    signal fast_cnt_reg : std_logic_vector(6 downto 0);
    signal slow_cnt_reg : std_logic_vector(6 downto 0);
    signal lvds0_reg    : std_logic_vector(6 downto 0);
    signal lvds1_reg    : std_logic_vector(6 downto 0);

    signal fast_cnt_next : std_logic_vector(6 downto 0);
    signal slow_cnt_next : std_logic_vector(6 downto 0);
    signal lvds0_next    : std_logic_vector(6 downto 0);
    signal lvds1_next    : std_logic_vector(6 downto 0);

    type pattern_array is array (6 downto 0) of
    std_logic_vector(6 downto 0);
    signal pattern : pattern_array;

begin

    pattern <= (
        "1000001", --6
        "1000001", --5
        "1000001", --4
        "1000001", --3
        "1001111", --2
        "1001111", --1
        "1001111"  --0
        -----6543210
        );

    process (clk_i, reset_i)
    begin
        if reset_i = '1' then
            fast_cnt_reg <= (others => '0');
            slow_cnt_reg <= (others => '0');
            lvds0_reg    <= (others => '0');
            lvds1_reg    <= (others => '0');
        elsif rising_edge(clk_i) then
            fast_cnt_reg <= fast_cnt_next;
            slow_cnt_reg <= slow_cnt_next;
            lvds0_reg    <= lvds0_next;
            lvds1_reg    <= lvds1_next;
        end if;
    end process;

    nextlogicfor_fast_cnt : process (all)
    begin
        if to_integer(unsigned(fast_cnt_reg)) >= 6 then
            fast_cnt_next <= std_logic_vector(to_unsigned(0, 7));
        else
            fast_cnt_next <= std_logic_vector(unsigned(fast_cnt_reg) + 1);
        end if;
    end process nextlogicfor_fast_cnt;

    nextlogicfor_slow_cnt : process (all)
    begin
        if (to_integer(unsigned(fast_cnt_reg)) = 6) and (to_integer(unsigned(slow_cnt_reg)) < 6) then
            slow_cnt_next                                                                          <= std_logic_vector(unsigned(slow_cnt_reg) + 1);
        elsif (to_integer(unsigned(fast_cnt_reg)) < 6) and (to_integer(unsigned(slow_cnt_reg)) <= 6) then
            slow_cnt_next                                                                          <= slow_cnt_reg;
        else
            slow_cnt_next <= std_logic_vector(to_unsigned(0, 7));
        end if;
    end process nextlogicfor_slow_cnt;

    nextlogicfor_lvds0_reg : process (all)
    begin
        if ((to_integer(unsigned(slow_cnt_reg)) < to_integer(unsigned(slow_cnt_next))) and to_integer(unsigned(fast_cnt_reg)) = 6) or
            (to_integer(unsigned(slow_cnt_next)) = 0 and to_integer(unsigned(fast_cnt_reg)) = 6) then
            case to_integer(unsigned(slow_cnt_reg)) is
                when 0 =>
                                ---6543210
                    lvds1_next <= "0000001";
                    lvds0_next <= pattern(0);
                when 1 =>
                    lvds1_next <= "0000001";
                    lvds0_next <= pattern(1);
                when 2 =>
                    lvds1_next <= "0000001";
                    lvds0_next <= pattern(2);
                when 3 =>
                    lvds1_next <= "0000001";
                    lvds0_next <= pattern(3);
                when 4 =>
                    lvds1_next <= "0000001";
                    lvds0_next <= pattern(4);
                when 5 =>
                    lvds1_next <= "0000001";
                    lvds0_next <= pattern(5);
                when 6 =>
                    lvds1_next <= "0000001";
                    lvds0_next <= pattern(6);
                when others =>
            end case;
        else
            lvds0_next(6) <= '0';
            for i in 5 downto 0 loop
                lvds0_next(i) <= lvds0_reg(i + 1);
            end loop;

            lvds1_next(6) <= '0';
            for i in 5 downto 0 loop
                lvds1_next(i) <= lvds1_reg(i + 1);
            end loop;
        end if;
    end process nextlogicfor_lvds0_reg;

    -- next_state_logic_forlvdsregs : process (all)
    -- begin
    --     if (to_integer(unsigned(fast_cnt_reg)) = 6) then
    --                     ---6543210
    --         lvds1_next <= "0000001";
    --         lvds0_next <= "1000001";
    --     else
    --         lvds0_next(6) <= '0';
    --         for i in 5 downto 0 loop
    --             lvds0_next(i) <= lvds0_reg(i + 1);
    --         end loop;

    --         lvds1_next(6) <= '0';
    --         for i in 5 downto 0 loop
    --             lvds1_next(i) <= lvds1_reg(i + 1);
    --         end loop;
    --     end if;
    -- end process next_state_logic_forlvdsregs;

    lvds0 <= lvds0_reg(0);
    lvds1 <= lvds1_reg(0);
end architecture;