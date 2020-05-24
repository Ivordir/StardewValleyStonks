using System.Collections.Generic;

namespace StardewValleyStonks
{
	public abstract class DataTableItem : ISelectable, IWarn
	{
		public string Name { get; }
		public int Price => BestPrice.Price();
		public Dictionary<Source, BuyPrice>.KeyCollection Sources => BestPrice.Keys;
		public bool HasPrice => BestPrice.Exists;
		public List<BuyPrice> BestPrices => BestPrice.BestItems;
		public BestDict<Source, BuyPrice> BestPrice { get; }
		public bool Selected { get; set; }
		public abstract bool Active { get; }
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

		public DataTableItem(
			string name,
			BestDict<Source, BuyPrice> bestPrice,
			bool selected = true)
		{
			Name = name;
			BestPrice = bestPrice;
			Selected = selected;
		}

		public virtual string Image => Name + ".png";
	}
}
