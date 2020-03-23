namespace StardewValleyStonks
{
    public class SettingsState
    {
        public bool Irrigated { get; set; } = false;
        public bool SpecialCharm { get; set; } = false;
        public int LuckBuff { get; set; } = 0;
        public double DoubleCropChance
        {
            get
            {
                return 0.0000999999974737875 + LuckBuff / 1500.0 + (SpecialCharm ? 0.0234375005122273718774982435775 / 1200 : 0);
            }
        }

        public bool GreenhouseMode { get; set; } = false;
        public bool QualityProducts { get; set; } = false;
        public Fertilizer StaringFert { get; set; } = Fertilizer.None;
    }
}
