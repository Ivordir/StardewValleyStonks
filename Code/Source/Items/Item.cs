namespace StardewValleyStonks
{
	public class Item : IItem
	{
		public string Name { get; }
		public int Price { get; }

		public Item(
			string name,
			int price)
		{
			Name = name;
			Price = price;
		}
	}
}