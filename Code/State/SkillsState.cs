namespace StardewValleyStonks
{
    public class SkillsState
    {
        public int FarmLvl { get; set; }
        public int FarmBuff { get; set; }
        public int BuffedFarmLvl => FarmLvl + FarmBuff;

        public double[] Quality { get; } 

        public Multiplier Agriculturist { get; }
        public PriceMultiplier Artisan { get; }
        public PriceMultiplier Tiller { get; }

        public SkillsState()
        {
            FarmLvl = 0;
            FarmBuff = 0;

            Quality = new double[] { 1, 1.25, 1.5, 2 };

            Tiller = new PriceMultiplier(
                1.1,
                new FarmLvlCondition(5),
                new IProfession[] { Agriculturist, Artisan });

            FarmLvlCondition lvl10 = new FarmLvlCondition(10);
            IProfession[] NeedsTiller = new IProfession[] { Tiller };
            Artisan = new PriceMultiplier(
                1.4,
                lvl10,
                null,
                NeedsTiller,
                new IProfession[] { Agriculturist });
            Agriculturist = new Multiplier(
                0.1,
                lvl10,
                null,
                NeedsTiller,
                new IProfession[] { Artisan });
        }
    }
}
