using System.Collections.Generic;
using System.Linq;
using ExtentionsLibrary.Collections;

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
				return _PriceFrom.Values
					.Where(p => p.Active)
					.MaxElements(_BestPrices);
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

        public DataTableItem(
			string name,
			Dictionary<Source, Price> priceFrom)
		{
			Name = name;
            _PriceFrom = priceFrom;
            _BestPrices = new List<Price>(1);
			Selected = true;
		}
    }
}
