namespace StardewValleyStonks
{
    public static class Extentions
    {
        public static int Price(this BestFinder<ISource, BuyPrice> bestFinder)
        {
            return bestFinder.HasBestItem ? bestFinder.BestItems[0].Price : -1;
        }

        public static int InRange(this int value, int min, int max)
        {
            return value.WithMin(min).WithMax(max);
        }

        public static int WithMin(this int value, int min)
        {
            if (value < min)
            {
                return min;
            }
            return value;
        }

        public static int WithMax(this int value, int max)
        {
            if (value > max)
            {
                return max;
            }
            return value;
        }

        public static double InRange(this double value, int min, int max)
        {
            return value.WithMin(min).WithMax(max);
        }

        public static double WithMin(this double value, int min)
        {
            if (value < min)
            {
                return min;
            }
            return min;
        }
        public static double WithMax(this double value, int max)
        {
            if (value > max)
            {
                return max;
            }
            return value;
        }
    }
}
