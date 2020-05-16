namespace StardewValleyStonks
{
	public class Product : IProduct
	{
		public string Name { get; }
		public bool Qualities { get; set; }
		private readonly int BasePrice;
		private readonly IPriceMultiplier Multiplier;

		public Product(
			string name,
			int basePrice,
			IPriceMultiplier multiplier = null)
		{
			Name = name;
			BasePrice = basePrice;
			Multiplier = multiplier;
		}

		public int Price => Multiplier == null ? BasePrice : Multiplier.ApplyTo(BasePrice);
	}
}