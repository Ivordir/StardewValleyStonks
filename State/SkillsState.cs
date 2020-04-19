namespace StardewValleyStonks
{
    public class SkillsState
    {
        public int FarmLvl { get; set; } = 0;
        public int FarmBuff { get; set; } = 0;
        public int BuffedFarmLvl => FarmLvl + FarmBuff;

        public double[] Quality { get; } = new double[] { 1, 1.25, 1.5, 2 };

        public Multiplier Artisan { get; } = new Multiplier(1.4);
        public Multiplier Tiller { get; } = new Multiplier(1.1);

        public ActiveItem Agriculturist { get; } = new ActiveItem();
    }
}
