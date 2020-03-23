[System.Flags]
public enum CropType //what bonuses do/do not apply to this crop and what products can this crop sell as unless overridden
{
	Tiller = 1,
	Fruit = 1 << 1,
	Vege = 1 << 2,
	Scythe = 1 << 3,
	Trelis = 1 << 4,
	Paddy = 1 << 5,
	IndoorOnly = 1 << 6,
	CommonFruit = Fruit | Tiller,
	CommonVege = Vege | Tiller,
	CommonScythe = Scythe | Vege,
	CommonPaddy = Paddy | Scythe
}
