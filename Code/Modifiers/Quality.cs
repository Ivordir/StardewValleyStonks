namespace StardewValleyStonks
{
    public readonly struct Quality
    {
        public readonly string Name { get; }

        public static int operator *(Quality quality, int basePrice) => (int)(quality.Multiplier * basePrice);

        private readonly double Multiplier;

        public Quality(
            string name,
            double multiplier)
        {
            Name = name;
            Multiplier = multiplier;
        }

        public static Quality Get(int quality) => Qualities[quality];

        private static readonly Quality[] Qualities = new Quality[]
        {
            new Quality("Normal", 1), //no reason to be accessing normal quality
            new Quality("Silver", 1.25),
            new Quality("Gold", 1.5),
            new Quality("Iridium", 2)
        };
    }
}
