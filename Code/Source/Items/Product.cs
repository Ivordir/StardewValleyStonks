namespace StardewValleyStonks
{
	public class Product : IItem
	{
		public string Name { get; }
		public int Price => Multiplier.ApplyTo(BasePrice);
		public bool Qualities { get; set; }

		private readonly int BasePrice;
		private readonly IPriceMultiplier Multiplier;

		public Product(
			string name,
			int basePrice,
			IPriceMultiplier multiplier)
		{
			Name = name;
			BasePrice = basePrice;
			Multiplier = multiplier;
		}
	}
}