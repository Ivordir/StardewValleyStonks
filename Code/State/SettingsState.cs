using System.Collections.Generic;

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
                _SeedsFromSeedMaker = value.WithMin(0);
                foreach(Amount amount in _SeedsByQuality.Values)
                {
                    amount.Value = _SeedsFromSeedMaker;
                }
            }
        }
        public double SeedProbability
        {
            get => _SeedProbability.Value;
            set => _SeedProbability.Value = value.InRange(0, 1);
        }
        public bool QualitySeedMaker { get; set; }
        public double SeedsByQuality(Quality quality)
        {
            return _SeedsByQuality[quality].Value;
        }
        public void SetSeedsByQuality(Quality quality, double value)
        {
            _SeedsByQuality[quality].Value = value.WithMin(0);
        }
        public Dictionary<Quality, MultiplierAmount> SeedAmounts { get; }

        public double GiantCropChecksPerTile
        {
            get => _GiantCropChecksPerTile;
            set => _GiantCropChecksPerTile = value.InRange(0, 9);
        }

        public bool GreenhouseMode { get; set; }
        public FertilizerDIO StaringFert { get; set; }

        private int _LuckBuff;
        private double _GiantCropChecksPerTile, _SeedsFromSeedMaker;
        private readonly Amount _SeedProbability;
        private readonly Dictionary<Quality, Amount> _SeedsByQuality;

        public SettingsState(DataState data)
        {
            StaringFert = null;
            SpecialCharm = false;
            _LuckBuff = 0;
            _GiantCropChecksPerTile = 9;
            GreenhouseMode = false;
            _SeedsFromSeedMaker = 2;
            _SeedsByQuality = new Dictionary<Quality, Amount>();
            foreach(Quality quality in data.Qualities)
            {
                _SeedsByQuality.Add(quality, new Amount(_SeedsFromSeedMaker));
            }
            _SeedProbability = new Amount(0.975);
            SeedAmounts = new Dictionary<Quality, MultiplierAmount>();
            foreach (Quality quality in data.Qualities)
            {
                SeedAmounts.Add(quality, new MultiplierAmount(
                    _SeedsByQuality[quality],
                    _SeedProbability));
            }
        }
    }
}
