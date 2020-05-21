using System.Collections.Generic;

namespace StardewValleyStonks
{
    public class SettingsState
    {
        public bool SpecialCharm
        {
            get => _SpecialCharm;
            set
            {
                _SpecialCharm = value;
                SpecialCharmValue.Value = value ? 0.025000000372529 / 1200 : 0;
            }
        }
        public int LuckBuff
        {
            get => (int)_LuckBuff.Value;
            set => _LuckBuff.Value = value.WithMin(0);
        }
        public IValue DoubleCropProb { get; }

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
            get => _GiantCropChecksPerTile.Value;
            set
            {
                _GiantCropChecksPerTile.Value = value.InRange(0, 9);
            }
        }
        public IValue GiantCrops { get; }
        public IValue NoGiantCropProb { get; }

        public bool GreenhouseMode { get; set; }
        public FertilizerDIO StaringFert { get; set; }

        private bool _SpecialCharm;
        private readonly RefValue SpecialCharmValue;
        private readonly RefValue _LuckBuff;
        private double _SeedsFromSeedMaker;
        private readonly RefValue _SeedProbability, _GiantCropChecksPerTile;
        private readonly Dictionary<Quality, RefValue> _SeedsByQuality;

        public SettingsState()
        {
            Quality[] qualities;
            qualities = new Quality[0];
            //P(doublecrop) = 0.0000999999974737875 + LuckBuff / 1500 + (SpecialCharm ? 0.025000000372529 : 0)
            DoubleCropProb = new Expression(new IValue[]
            {
                new RefValue(0.0000999999974737875),
                new Term(new IValue[]
                {
                    _LuckBuff,
                    new RefValue(1.0 / 1500),
                }),
                SpecialCharmValue
            });

            RefValue GiantCropChance = new RefValue(0.01);
            _GiantCropChecksPerTile = new RefValue(9);
            //P(NoGiantCrop) = (1 - GiantCropChance) ^ ChecksPerTile
            NoGiantCropProb = new Exponent(
                new InverseProb(GiantCropChance),
                _GiantCropChecksPerTile);
            IValue GiantCropProb = new InverseProb(NoGiantCropProb);
            //each GiantCrop takes up 9 tiles and gives an average of 18 crops
            //18/9 = 2 crops per tile
            //GiantCrops = 2 * P(giantcrop)
            GiantCrops = new Term(new IValue[]
            {
                new RefValue(2),
                GiantCropProb
            });

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
