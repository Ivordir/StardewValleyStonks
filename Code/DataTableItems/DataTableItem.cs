namespace StardewValleyStonks
{
	public abstract class DataTableItem
	{
		public string Name { get; }
		public int Price => PriceManager.Price();
		public BestFinder<Source, BuyPrice> PriceManager { get; }
		public bool Enabled { get; set; }
		public abstract bool Active { get; }

		public DataTableItem(
			string name,
			BestFinder<Source, BuyPrice> priceManager,
			bool enbabled = true)
		{
			Name = name;
			PriceManager = priceManager;
			Enabled = enbabled;
		}

		public virtual string Image => Name + ".png";
	}
}
