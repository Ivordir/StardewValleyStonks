using System.Collections.Generic;
using ExtentionsLibrary.Validation;

namespace StardewValleyStonks
{
    public class Settings
    {
        public bool SpecialCharm { get; set; }
        public int LuckBuff
        {
            get => (int)_LuckBuff;
            set => _LuckBuff = value.WithMin(0);
        }
        public double DoubleCropProb => 0.0000999999974737875 + _LuckBuff / 1500 + (SpecialCharm ? 0.025000000372529 : 0);

        public int SeedsFromSeedMaker
        {
            get => (int)_SeedsFromSeedMaker;
            set
            {
                _SeedsFromSeedMaker = value.WithMin(0);
                foreach(RefValue amount in _SeedsByQuality.Values)
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
        public int SeedsByQuality(Quality quality)
            => (int)_SeedsByQuality[quality].Value;
        public void SetSeedsByQuality(Quality quality, int value)
            => _SeedsByQuality[quality].Value = value.WithMin(0);
        public Dictionary<Quality, IValue> SeedAmounts { get; }

        public double GiantCropChecksPerTile
        {
            get => _GiantCropChecksPerTile;
            set
            {
                _GiantCropChecksPerTile= value.InRange(0, 9);
            }
        }
        public double NoGiantCropProb => System.Math.Pow(1 - GiantCropChance, _GiantCropChecksPerTile);


        public bool GreenhouseMode { get; set; }
        public FertilizerDIO StaringFert { get; set; }

        private int _LuckBuff;
        private double _SeedsFromSeedMaker, _GiantCropChecksPerTile;
        private readonly double GiantCropChance;
        private readonly RefValue _SeedProbability;
        private readonly Dictionary<Quality, RefValue> _SeedsByQuality;

        public Settings()
        {
            Quality[] qualities;
            qualities = new Quality[0];

            GiantCropChance = 0.01;

            _SeedsFromSeedMaker = 2;
            _SeedsByQuality = new Dictionary<Quality, RefValue>();
            foreach(Quality quality in qualities)
            {
                _SeedsByQuality.Add(quality, new RefValue(_SeedsFromSeedMaker));
            }
            _SeedProbability = new RefValue(0.975);
            SeedAmounts = new Dictionary<Quality, IValue>();
            foreach (Quality quality in qualities)
            {
                SeedAmounts.Add(quality, new Term(new IValue[]
                {
                    _SeedsByQuality[quality],
                    _SeedProbability
                }));
            }

            StaringFert = null;
        }
    }
}
