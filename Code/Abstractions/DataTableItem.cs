namespace StardewValleyStonks
{
	public abstract class DataTableItem
	{
		public string Name { get; }
		public bool Enabled { get; set; }
		public abstract bool Active { get; }
		public IManager<ISource, IPricedItem> PriceManager;

		public DataTableItem(string name, IManager<ISource, IPricedItem> priceManager)
		{
			Name = name;
			PriceManager = priceManager;
		}

		public virtual string Image
		{
			get
			{
				return Name + ".png";
			}
		}
	}
}
