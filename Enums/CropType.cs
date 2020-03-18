[System.Flags]
public enum CropType //what bonuses do/do not apply to this crop and what products can this crop sell as unless overridden
{
	Tiller = 1,
	FruitFlag = 1 << 1,
	Fruit = FruitFlag | Tiller,
	VegeFlag = 1 << 2,
	Vege = VegeFlag | Tiller,
	ScytheFlag = 1 << 3,
	Scythe = ScytheFlag | Vege,
	Trelis = 1 << 4,
	PaddyFlag = 1 << 5,
	Paddy = PaddyFlag | Scythe,
	IndoorOnly = 1 << 6
}
