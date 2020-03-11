public class Product
{
	public static Product Oil;

	static Product()
	{
		Oil = new Product("Oil", 100);
	}

	public string Name { get; }

	private readonly int BasePrice;
	private readonly bool UsesArtisan;

	public Product(string name, int basePrice, bool usesArtisan = false)
	{
		Name = name;
		BasePrice = basePrice;
		UsesArtisan = usesArtisan;
	}

	public int Price
	{
		get
		{
			return Arti && UsesArtisan ? (int)(BasePrice * 1.4) : BasePrice;
		}
	}
}
