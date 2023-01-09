library ieee;
use ieee.std_logic_1164.all;
use IEEE.numeric_std.all;

entity top_tx_dphy is
    port (
        async_reset_n_i : in std_logic;
        LVDS_p0         : in std_logic;
        LVDS_n0         : in std_logic;
        LVDS_p1         : in std_logic;
        LVDS_n1         : in std_logic;
        LVDS_p2         : in std_logic;
        LVDS_n2         : in std_logic;
        LVDS_p3         : in std_logic;
        LVDS_n3         : in std_logic;
        LVDS_pclk       : in std_logic;
        LVDS_nclk       : in std_logic;

        cmos_test_o : out std_logic_vector(7 downto 0);

        fv_oo : out std_logic;
        lv_oo : out std_logic;

        clk_p_o    : inout std_logic;
        clk_n_o    : inout std_logic;
        mipi_d_p_o : inout std_logic_vector (3 downto 0);--
        mipi_d_n_o : inout std_logic_vector (3 downto 0) --

    );
end top_tx_dphy;

architecture struct of top_tx_dphy is
    signal cmos_px_data    : std_logic_vector(15 downto 0);
    signal serdes_high_clk : std_logic;
    signal cmos_clk        : std_logic;
    signal pll_lock_hc     : std_logic;
    signal lvds0           : std_logic;
    signal lvds1           : std_logic;
    signal serdes_slow_clk : std_logic;

    -- p2b
    signal fv_start                   : std_logic;
    signal fv_end                     : std_logic;
    signal txfr_req                   : std_logic;
    signal packet_sender_byte_data_en : std_logic;
    signal byte_data                  : std_logic_vector(31 downto 0);--

    -- delays
    type byte_data_delay is array (natural range 5 downto 0) of
    std_logic_vector(31 downto 0);--
    signal byte_data_next : byte_data_delay;
    signal byte_data_reg  : byte_data_delay;

    signal byte_data_en_next : std_logic_vector(4 downto 0);
    signal byte_data_en_reg  : std_logic_vector(5 downto 0);

    -- tx dphy 
    signal byte_clk   : std_logic;
    signal txfr_en    : std_logic;
    signal tx_c2d_rdy : std_logic;
    signal sp_en      : std_logic;

    signal sp_en_next_0d : std_logic;
    signal sp_en_reg_0d  : std_logic;

    signal lp_en : std_logic;
    signal dt    : std_logic_vector(5 downto 0);

    signal wc : std_logic_vector(15 downto 0);
    signal vc : std_logic_vector(1 downto 0);

    signal byte_data_en      : std_logic;
    signal tx_dphy_byte_data : std_logic_vector(31 downto 0);--

    signal pd_dphy       : std_logic;
    signal pll_lock      : std_logic;
    signal pix2byte_rstn : std_logic;

    signal phdr_xfr_done       : std_logic;
    signal vc_tx               : std_logic_vector(1 downto 0);
    signal dt_tx               : std_logic_vector(5 downto 0);
    signal wc_tx               : std_logic_vector(15 downto 0);
    signal lp_en_next_0d       : std_logic;
    signal lp_en_reg_0d        : std_logic;
    signal phdr_xfr_done_next0 : std_logic;
    signal phdr_xfr_done_reg0  : std_logic;
    signal phdr_xfr_done_next1 : std_logic;
    signal phdr_xfr_done_reg1  : std_logic;
    signal vc_valid_next       : std_logic_vector(1 downto 0);
    signal vc_valid_reg        : std_logic_vector(1 downto 0);
    signal dt_valid_next       : std_logic_vector(5 downto 0);
    signal dt_valid_reg        : std_logic_vector(5 downto 0);
    signal wc_valid_next       : std_logic_vector(15 downto 0);
    signal wc_valid_reg        : std_logic_vector(15 downto 0);

    signal fast_clk       : std_logic;
    signal clock_gen_lock : std_logic;

    signal osc_clk    : std_logic;
    signal dphy_ready : std_logic;

    signal pll_clock : std_logic;

    signal px_clk : std_logic;
    signal dvalid : std_logic;
    signal fv     : std_logic;
    signal lv     : std_logic;
    component mipi_tx_dphy_csi2 is
        port (
            reset_n_i             : in std_logic;
            pd_dphy_i             : in std_logic;
            byte_or_pkt_data_i    : in std_logic_vector(31 downto 0);--
            byte_or_pkt_data_en_i : in std_logic;
            ready_o               : out std_logic;
            vc_i                  : in std_logic_vector(1 downto 0);
            dt_i                  : in std_logic_vector(5 downto 0);
            wc_i                  : in std_logic_vector(15 downto 0);
            clk_hs_en_i           : in std_logic;
            d_hs_en_i             : in std_logic;
            tinit_done_o          : out std_logic;
            pll_lock_o            : out std_logic;
            pix2byte_rstn_o       : out std_logic;
            pkt_format_ready_o    : out std_logic;
            d_hs_rdy_o            : out std_logic;
            byte_clk_o            : out std_logic;
            c2d_ready_o           : out std_logic;
            phdr_xfr_done_o       : out std_logic;
            ld_pyld_o             : out std_logic;
            clk_p_io              : inout std_logic;
            clk_n_io              : inout std_logic;
            d_p_io                : inout std_logic_vector(3 downto 0);--
            d_n_io                : inout std_logic_vector(3 downto 0);--
            pll_clkop_i           : in std_logic;
            pll_lock_i            : in std_logic;
            sp_en_i               : in std_logic;
            lp_en_i               : in std_logic
        );
    end component;

    component pll_highclk is
        port (
            clki_i  : in std_logic;
            rstn_i  : in std_logic;
            clkop_o : out std_logic;
            clkos_o : out std_logic;
            lock_o  : out std_logic
        );
    end component;

    component pll_mipi is
        port (
            clki_i  : in std_logic;
            rstn_i  : in std_logic;
            clkop_o : out std_logic;
            lock_o  : out std_logic
        );
    end component;
    component pll_sample is
        port (
            clki_i  : in std_logic;
            rstn_i  : in std_logic;
            clkop_o : out std_logic;
            lock_o  : out std_logic
        );
    end component;

    component osc1 is
        port (
            hf_out_en_i  : in std_logic;
            hf_clk_out_o : out std_logic
        );
    end component;

    signal sample_clk    : std_logic;
    signal masterrst_reg : std_logic;
    signal resff_reg     : std_logic;

    signal fv_s_reg  : std_logic_vector (640 downto 0);
    signal fv_s_next : std_logic_vector (639 downto 0);
    signal tmp1      : std_logic_vector (640 downto 0);

    signal fv_e_reg  : std_logic_vector (900 downto 0);
    signal fv_e_next : std_logic_vector (899 downto 0);
    signal tmp2      : std_logic_vector (900 downto 0);

    signal data_delayed : std_logic_vector(15 downto 0);
    signal valid        : std_logic;

    type sync is array(2 downto 0) of std_logic_vector(15 downto 0);
    signal sync_regs : sync;
    signal sync_next : sync;
    begin

    -- fv_oo <= fv;
    -- lv_oo <= lv;
    -- -- async resets synchronizer - for antimetastable deassert 
    async_reset : process (osc_clk, async_reset_n_i)
    begin
        if async_reset_n_i = '0' then
            resff_reg     <= '0';
            masterrst_reg <= '0';
        elsif rising_edge(osc_clk) then
            resff_reg     <= '1';
            masterrst_reg <= resff_reg;
        end if;
    end process async_reset;

    byte_data_en_reg(5) <= packet_sender_byte_data_en;
    byte_data_en        <= byte_data_en_reg(0);
    byte_en_delay : for i in 4 downto 0 generate
        process (byte_clk, masterrst_reg)
        begin
            if masterrst_reg = '0' then
                byte_data_en_reg(i) <= '0';
            elsif rising_edge(byte_clk) then
                byte_data_en_reg(i) <= byte_data_en_next(i);
            end if;
        end process;
        byte_data_en_next(i) <= byte_data_en_reg(i + 1);
    end generate byte_en_delay;

    process (byte_clk, masterrst_reg)
    begin
        if masterrst_reg = '0' then
            phdr_xfr_done_reg0 <= '0';
            phdr_xfr_done_reg1 <= '0';
            lp_en_reg_0d       <= '0';
            sp_en_reg_0d       <= '0';
            wc_valid_reg       <= (others => '0');
            dt_valid_reg       <= (others => '0');
            vc_valid_reg       <= (others => '0');
        elsif rising_edge(byte_clk) then
            phdr_xfr_done_reg0 <= phdr_xfr_done_next0;
            phdr_xfr_done_reg1 <= phdr_xfr_done_next1;
            lp_en_reg_0d       <= lp_en_next_0d;
            sp_en_reg_0d       <= sp_en_next_0d;
            wc_valid_reg       <= wc_valid_next;
            dt_valid_reg       <= dt_valid_next;
            vc_valid_reg       <= vc_valid_next;
        end if;
    end process;

    -- signals for knowing that fv_start were close 
    fv_s_reg(640) <= fv_start;
    tmp1(640)     <= fv_s_reg(640);
    fv_s : for i in 639 downto 0 generate
        process (byte_clk, masterrst_reg) begin --byteclk
            if masterrst_reg <= '0' then
                fv_s_reg(i)      <= '0';
            elsif rising_edge(byte_clk) then
                fv_s_reg(i) <= fv_s_next(i);
            end if;
        end process;
        fv_s_next(i) <= fv_s_reg(i + 1);
        tmp1(i)      <= tmp1(i + 1) or fv_s_reg(i);
    end generate fv_s;

    -- signals for knowing that fv_end were close 
    fv_e_reg(900) <= fv_end;
    tmp2(900)     <= fv_e_reg(900);
    fv_e : for i in 899 downto 0 generate
        process (byte_clk, masterrst_reg) begin --byteclk
            if masterrst_reg <= '0' then
                fv_e_reg(i)      <= '0';
            elsif rising_edge(byte_clk) then
                fv_e_reg(i) <= fv_e_next(i);
            end if;
        end process;
        fv_e_next(i) <= fv_e_reg(i + 1);
        tmp2(i)      <= tmp2(i + 1) or fv_e_reg(i);
    end generate fv_e;

    dvalid <= fv and lv;

    phdr_xfr_done_next0 <= phdr_xfr_done;
    phdr_xfr_done_next1 <= phdr_xfr_done_reg0;
    sp_en_next_0d       <= sp_en;
    lp_en_next_0d       <= lp_en;

    lp_en <= packet_sender_byte_data_en and (not byte_data_en_reg(4));
    vc    <= (others => '0');
    wc    <= "0000111100000000"; -- 3840 8 bit words

    process (all) begin
        if tmp1(0) = '1' then
            dt <= (others => '0');
        elsif tmp2(0) = '1' then
            dt <= "000001";
        else
            dt <= "011110"; --1E
        end if;
    end process;

    process (all) begin
        if sp_en = '1' then
            wc_valid_next <= (others => '0');
            dt_valid_next <= dt;
            vc_valid_next <= vc;
        elsif lp_en = '1' then
            wc_valid_next <= wc;
            dt_valid_next <= dt;
            vc_valid_next <= vc;
        else
            wc_valid_next <= wc_valid_reg;
            dt_valid_next <= dt_valid_reg;
            vc_valid_next <= vc_valid_reg;
        end if;
    end process;

    wc_tx <= wc_valid_reg when (not(phdr_xfr_done_reg0 and not phdr_xfr_done_reg1)
        or byte_data_en) else
        (others => '0');
    dt_tx <= dt_valid_reg when (not(phdr_xfr_done_reg0 and not phdr_xfr_done_reg1)
        or byte_data_en) else
        (others => '0');
    vc_tx <= vc_valid_reg when (not(phdr_xfr_done_reg0 and not phdr_xfr_done_reg1)
        or byte_data_en) else
        (others => '0');

    LVDS_deserializer0 : entity work.lvds_double_buffer port map(
        lvds_in0_p => LVDS_p0,
        -- lvds_in0_p => LVDS_p1,
        lvds_in1_p => LVDS_p1,
        -- lvds_in1_p => LVDS_p0,
        high_clk_i => serdes_high_clk,
        reset_i    => not masterrst_reg,
        cmos_o     => sync_next(2)(15 downto 8)
        -- cmos_o => cmos_test_o
        );

    LVDS_deserializer1 : entity work.lvds_double_buffer port map(
        lvds_in0_p => LVDS_p2,
        lvds_in1_p => LVDS_p3,
        high_clk_i => serdes_high_clk,
        reset_i    => not masterrst_reg,
        cmos_o     => sync_next(2)(7 downto 0)
        -- cmos_o => cmos_test_o
        );

    -- data_generator : entity work.lvds_data_gen port map(
    --     clk_i =>serdes_high_clk,
    --     reset_i => not masterrst_reg,
    --     valid_o => valid,
    --     lvds0 =>lvds0,
    --     lvds1 =>lvds1
    --     );

    process(serdes_slow_clk, masterrst_reg)
    begin
        if masterrst_reg = '0' then
            for i in 2 downto 0 loop
                sync_regs(i) <= (others => '0');
            end loop;
        elsif rising_edge(serdes_slow_clk) then
            for i in 2 downto 0 loop
                sync_regs(i) <= sync_next(i);
            end loop;
        end if;
    end process;

   process(all)
   begin
        for i in 1 downto 0 loop 
            sync_next(i) <= sync_regs(i+1);
        end loop;
   end process;

    syncs: entity work.detector_sync_generator port map(
        -- parallel_data_in => cmos_px_data,
        parallel_data_in => sync_regs(0),
        lvds_clk_in      => serdes_slow_clk, -- cmos_clk should be generated from same PLL as "serdes_high_clk"
        reset_i          => not masterrst_reg,

        lv_reg_o       => lv,
        fv_reg_o       => fv,
        delayed_data_o => data_delayed
        );

    tx_dphy : mipi_tx_dphy_csi2 port map(
        reset_n_i             => masterrst_reg,
        pd_dphy_i             => not masterrst_reg,
        byte_or_pkt_data_i    => byte_data,
        byte_or_pkt_data_en_i => byte_data_en,
        ready_o               => dphy_ready,
        vc_i                  => vc_tx,
        dt_i                  => dt_tx,
        wc_i                  => wc_tx,
        clk_hs_en_i           => txfr_req,
        d_hs_en_i             => txfr_req,
        pll_lock_o            => open,
        pix2byte_rstn_o       => pix2byte_rstn,
        pkt_format_ready_o    => open,
        d_hs_rdy_o            => txfr_en,
        byte_clk_o            => byte_clk,
        c2d_ready_o           => tx_c2d_rdy,
        phdr_xfr_done_o       => phdr_xfr_done,
        ld_pyld_o             => open,
        clk_p_io              => clk_p_o,
        clk_n_io              => clk_n_o,
        d_p_io                => mipi_d_p_o,
        d_n_io                => mipi_d_n_o,
        sp_en_i               => sp_en_reg_0d,
        lp_en_i               => lp_en_reg_0d,
        tinit_done_o          => open,
        pll_clkop_i           => pll_clock,
        pll_lock_i            => pll_lock
    );

    byte_packets : entity work.packet_sender port map(
        reset_i      => not masterrst_reg,
        px_data      => data_delayed,
        px_clk       => cmos_clk,
        fv_i         => fv,
        lv_i         => lv,
        d_hs_ready_i => txfr_en,
        byte_clk     => byte_clk,
        c2d_ready_i  => tx_c2d_rdy,
        phdr_i       => phdr_xfr_done,
        fv_e_r_i     => tmp2(0),
        fv_s_r_i     => tmp1(0),
        d_hs_en_o    => txfr_req,
        packet_en_o  => packet_sender_byte_data_en,
        packets_o    => byte_data,
        lv_start_o   => open,
        lv_end_o     => open,
        fv_start_o   => fv_start,
        fv_end_o     => fv_end,
        sp_en_o      => sp_en
        );

    PLL_serdes : pll_highclk port map(
        clki_i  => LVDS_pclk,
        rstn_i  => masterrst_reg,
        clkop_o => serdes_slow_clk,
        clkos_o => serdes_high_clk,
        lock_o  => pll_lock_hc
    );
    PLL_external : pll_mipi port map(
        clki_i  => LVDS_pclk,
        rstn_i  => masterrst_reg,
        clkop_o => pll_clock,
        lock_o  => pll_lock
    );

    -- osc and pll just for sample
    pll_reveal : pll_sample port map(
        clki_i  => osc_clk,
        rstn_i  => '1',
        clkop_o => sample_clk,
        lock_o  => clock_gen_lock
    );

    sample_clk_osc : osc1 port map(
        hf_out_en_i  => '1',
        hf_clk_out_o => osc_clk
    );

end architecture struct;