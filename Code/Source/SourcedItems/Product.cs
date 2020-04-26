namespace StardewValleyStonks
{
	public class Product : ActiveItem, IProduct
	{
		public string Name { get; }
		public double OutputPerInput { get; }

		private readonly int BasePrice;
		private readonly IPriceMultiplier Multiplier;

		public Product(
			string name,
			int basePrice,
			IPriceMultiplier multiplier = null,
			double outputPerInput = 1,
			bool enabled = true,
			ICondition[] conditions = null)
			: base(enabled, conditions)
		{
			Name = name;
			BasePrice = basePrice;
			Multiplier = multiplier;
			OutputPerInput = outputPerInput;
		}

		public double Price => OutputPerInput * UnitPrice;

		public double QualityPrice(double quality) => OutputPerInput * (int)(quality * UnitPrice);

		public int UnitPrice => Multiplier == null ? BasePrice : Multiplier.ApplyTo(BasePrice);
	}
}