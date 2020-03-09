using System;

[Flags]
public enum Replant : byte //in what ways can this crop be planted
{
	BoughtSeeds = 1, //always an option unless user says no
	SeedMaker = 1 << 1, //always unless tea, coffee, no seedmaker, or user says no. seed chance = 0.975 unless ancient fruit where seed chance = 0.98
	Common = BoughtSeeds | SeedMaker,
	Replant = 1 << 2, //coffee and sunflower
}