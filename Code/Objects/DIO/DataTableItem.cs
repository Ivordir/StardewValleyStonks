using System.Collections.Generic;

namespace StardewValleyStonks
{
	public abstract class DataTableItem : ISelectable, IWarn
	{
		public string Name { get; }
        public bool Selected { get; set; }
        public abstract bool Active { get; }
        public bool HasPrice => BestPrices.Count > 0;
        public int Price => BestPrices[0].Value;
        public bool HasPriceFrom(Source source) => _PriceFrom.ContainsKey(source);
        public Price PriceFrom(Source source) => _PriceFrom[source];
        public Dictionary<Source, Price>.KeyCollection Sources => _PriceFrom.Keys;
        public Dictionary<Source, Price>.ValueCollection Prices => _PriceFrom.Values;
        public List<Price> BestPrices
        {
            get
            {
                _BestPrices.Clear();
                foreach (Price price in _PriceFrom.Values)
                {
                    if (price.Active)
                    {
                        if (_BestPrices.Count == 0 || Compare(price) == 0)
                        {
                            _BestPrices.Add(price);
                        }
                        else if (Compare(price) == -1) //source is better than BestSource
                        {
                            _BestPrices.Clear();
                            _BestPrices.Add(price);
                        }
                    }
                }
                return _BestPrices;
            }
        }
        public abstract List<Warning> Warnings { get; }
		public string DisplayWarnings
		{
			get
			{
				string display = "";
				foreach (Warning warning in Warnings)
				{
					display += warning.Display() + "\n";
				}
				return display;
			}
		}
        public virtual string Image => Name + ".png";

        private readonly Dictionary<Source, Price> _PriceFrom;
        private readonly List<Price> _BestPrices;

        private int Compare(Price price) => _BestPrices[0].CompareTo(price);

        public DataTableItem(
			string name,
			Dictionary<Source, Price> priceFrom)
		{
			Name = name;
            _PriceFrom = priceFrom;
            _BestPrices = new List<Price>();
			Selected = true;
		}
    }
}
