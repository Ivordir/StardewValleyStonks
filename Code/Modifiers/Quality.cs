using System;
using System.Collections.Generic;
using System.Linq;

namespace StardewValleyStonks
{
    public readonly struct Quality
    {
        public static IReadOnlyCollection<Quality> Qualities { get; } 

        public static int operator *(Quality quality, int basePrice) => (int)(quality.Multiplier * basePrice);

        public static Quality Get(int index) => Qualities.ElementAt(index);

        static Quality()
        {
            Qualities = Array.AsReadOnly(new Quality[]
            {
                new Quality("Normal", 0),
                new Quality("Silver", 1),
                new Quality("Gold", 2),
                new Quality("Iridium", 4)
            });
        }

        public readonly string Name { get; }

        readonly double Multiplier;

        public Quality(
            string name,
            int multiplierLevel)
        {
            Name = name;
            Multiplier = 1 + multiplierLevel * 0.25;
        }
    }
}
