namespace StardewValleyStonks
{
    public abstract class BaseCropAmount : ICropAmount
    {
        private readonly SettingsState Settings;

        private readonly double ExtraCrops;
        private readonly bool DoubleChance;
        private readonly bool Giant;

        public abstract void SetAmounts(int fertQuality = 0);

        protected double AvgCrops => 1 - GiantCrops;
        protected double AvgExtraCrops => (DoubleChance ? 0 : (ExtraCrops + AvgCrops) * Settings.DoubleCropChance) + ExtraCrops + GiantCrops;
        protected double GiantCrops => Giant ? 2 * (1 - System.Math.Pow(0.99, Settings.GiantCropChecksPerTile)) : 0;
    }
}
