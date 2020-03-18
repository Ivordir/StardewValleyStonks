[System.Flags]
public enum ProductType //as what products can this crop sell as
{
	Crop = 1,
	Jar = 1 << 1,
	Keg = 1 << 2,
	Oil = 1 << 3,
	Mill = 1 << 4
}