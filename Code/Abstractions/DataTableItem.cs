namespace StardewValleyStonks
{
	public abstract class DataTableItem
	{
		public string Name { get; }
		public bool Enabled { get; set; }
		public abstract bool Active { get; }

		public DataTableItem(string name)
		{
			Name = name;
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
