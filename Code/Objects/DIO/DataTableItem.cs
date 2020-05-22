using System.Collections.Generic;

namespace StardewValleyStonks
{
	public abstract class DataTableItem : ISelectable, IWarn
	{
		public string Name { get; }
		public int Price => PriceManager.Price();
		public Dictionary<Source, BuyPrice>.KeyCollection Sources => PriceManager.Keys;
		public bool HasPrice => PriceManager.HasBestItem;
		public List<BuyPrice> BestPrices => PriceManager.BestItems;
		public BestDict<Source, BuyPrice> PriceManager { get; }
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
			BestDict<Source, BuyPrice> priceManager,
			bool selected = true)
		{
			Name = name;
			PriceManager = priceManager;
			Selected = selected;
		}

		public virtual string Image => Name + ".png";

		//string IWarn.DisplayWarnings { get; }
	}
}
