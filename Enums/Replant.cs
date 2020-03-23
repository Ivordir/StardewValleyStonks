[System.Flags]
public enum Replant
{
	BoughtSeeds = 1,
	SeedMaker = 1 << 1, 
	Replant = 1 << 2, //coffee and sunflower
	Common = BoughtSeeds | SeedMaker
}