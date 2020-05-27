using System.Collections.Generic;
using System.Linq;

namespace StardewValleyStonks
{
    public class FertilizerDIO : DataTableItem
    {
        public int Quality { get; }
        public double Speed { get; }
        public override bool Active => Selected && HasPrice;
        public override List<Warning> Warnings
        {
            get
            {
                _Warnings.Clear();
                if (!HasPrice)
                {
                    NoSource.SubWarnings.Clear();
                    foreach (Price price in Prices)
                    {
                        NoSource.SubWarnings.AddRange(price.Warnings);
                    }
                    _Warnings.Add(NoSource);
                }
                return _Warnings;
            }
        }

        readonly List<Warning> _Warnings;
        readonly Warning NoSource;

        public Fertilizer ToFertilizer(int farmBuffLevel) => new Fertilizer(
            Name,
            Quality,
            Speed,
            Price,
            BestPrices.Select(p => p.Source).ToArray(),
            farmBuffLevel);

        public FertilizerDIO(
            string name,
            int quality,
            double speed,
            Dictionary<Source, Price> priceFrom)
            : base(name, priceFrom)
        {
            Quality = quality;
            Speed = speed;
            NoSource = new Warning($"{ Name } cannot be bought from anywhere:");
            _Warnings = new List<Warning>();
        }
    }
}
