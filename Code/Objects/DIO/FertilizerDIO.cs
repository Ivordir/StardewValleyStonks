using System.Collections.Generic;

namespace StardewValleyStonks
{
    public class FertilizerDIO : DataTableItem
    {
        public int Quality { get; }
        public double Speed { get; }
        public override bool Active => Selected && PriceManager.HasBestItem;
        public override List<Warning> Warnings
        {
            get
            {
                _Warnings.Clear();
                if (!PriceManager.HasBestItem)
                {
                    NoSource.SubWarnings.Clear();
                    foreach(BuyPrice price in PriceManager.Values)
                    {
                        NoSource.SubWarnings.AddRange(price.Warnings);
                    }
                    _Warnings.Add(NoSource);
                }
                return _Warnings;
            }
        }

        private readonly List<Warning> _Warnings;
        private readonly Warning NoSource;

        public FertilizerDIO(
            string name,
            int quality,
            double speed,
            BestDict<Source, BuyPrice> priceManager)
            : base(name, priceManager)
        {
            Quality = quality;
            Speed = speed;
            NoSource = new Warning($"{ Name } cannot be bought from anywhere:");
            _Warnings = new List<Warning>();
        }
    }
}
