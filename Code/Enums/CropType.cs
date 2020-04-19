[System.Flags]
public enum CropFlags //deprecated
{
	Tiller = 1, //nope
	Fruit = 1 << 1, //nope
	Vege = 1 << 2, //nope
	Scythe = 1 << 3, //nope
	Trelis = 1 << 4, //change to bool
	Paddy = 1 << 5, //changed to type of Crop
	IndoorOnly = 1 << 6, //???
}
