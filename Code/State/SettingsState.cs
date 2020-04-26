namespace StardewValleyStonks
{
    public class SettingsState
    {
        List<Crop>
        List<RegrowCrop>
        List<Fertilizer>
        List<ProductSource> QualityProducts
        List<PaddyCrop>

        public bool SpecialCharm { get; set; } = false; 
        public int LuckBuff { get; set; } = 0;
        public double DoubleCropChance => 0.0000999999974737875 + LuckBuff / 1500.0 + (SpecialCharm ? 0.0234375005122273718774982435775 / 1200 : 0);
        //public double SeedChance { get; set; } = 0.975;
        //public double CropsNeededForSeeds => 1 / (2 * SeedChance);
        public bool GreenhouseMode { get; set; } = false;
        //public bool QualityProducts { get; set; } = false;
        public bool TrelisPenalty;
        public Fertilizer StaringFert { get; set; } = null;
        
        public SettingsState(bool test)
        {

        }
    }
}
