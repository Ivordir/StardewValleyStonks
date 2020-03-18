[System.Flags]
public enum Replant
{
	BoughtSeeds = 1,
	SeedMaker = 1 << 1, 
	Common = BoughtSeeds | SeedMaker,
	Replant = 1 << 2, //coffee and sunflower
}