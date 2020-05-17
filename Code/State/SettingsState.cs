namespace StardewValleyStonks
{
    public class SettingsState
    {
        public bool SpecialCharm { get; set; }
        public int LuckBuff
        {
            get => _LuckBuff;
            set => _LuckBuff = value.WithMin(0);
        }
        public double DoubleCropChance => 0.0000999999974737875 + _LuckBuff / 1500.0 + (SpecialCharm ? 0.0234375005122273718774982435775 / 1200 : 0);
        
        public double SeedsFromSeedMaker
        {
            get => _SeedsFromSeedMaker;
            set
            {
                _SeedsFromSeedMaker = value;
                foreach(Reference<double> reference in QualitySeedsFromSeedMaker)
                {
                    reference.Value = value;
                }
            }
        }
        public bool QualitySeedMaker { get; set; }
        public Reference<double>[] QualitySeedsFromSeedMaker;

        public double GiantCropChecksPerTile
        {
            get => _GiantCropChecksPerTile;
            set => _GiantCropChecksPerTile = value.InRange(0, 9);
        }
        public bool GreenhouseMode { get; set; }
        //public bool TrelisPenalty;
        public Fertilizer StaringFert { get; set; }

        private int _LuckBuff;
        private double _GiantCropChecksPerTile, _SeedsFromSeedMaker;

        public SettingsState()
        {
            StaringFert = null;
            SpecialCharm = false;
            _LuckBuff = 0;
            _GiantCropChecksPerTile = 9;
            GreenhouseMode = false;
            _SeedsFromSeedMaker = 2;
            QualitySeedsFromSeedMaker = new Reference<double>[4];
            for(int quality = 0; quality < 4; quality++)
            {
                QualitySeedsFromSeedMaker[quality] = new Reference<double>(_SeedsFromSeedMaker);
            }
        }
    }
}
