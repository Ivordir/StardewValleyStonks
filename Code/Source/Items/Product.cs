namespace StardewValleyStonks
{
	public class Product : IItem
	{
		public string Name { get; }
		public int Price => Multiplier.Active ? (int)(Multiplier.Value * BasePrice) : BasePrice;
		public bool Qualities => Processor.PreservesQuality;

		private readonly int BasePrice;
		private readonly Processor Processor;
		private readonly IMultiplier Multiplier;

		public Product(
			string name,
			int basePrice,
			Processor processor,
			IMultiplier multiplier)
		{
			Name = name;
			BasePrice = basePrice;
			Processor = processor;
			Multiplier = multiplier;
		}
	}
}